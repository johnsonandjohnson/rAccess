
#' @title User-interface definition of module_sub_iam
#'
#' @description This is a Shiny module used by the main shiny web application
#'
#' @param id Module's ID
#'
#' @importFrom shiny uiOutput NS fluidPage fluidRow column actionButton
#' @importFrom  shinyjs useShinyjs
#' @importFrom DT DTOutput
module_sub_iam_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  fluidPage(
    class = "px-0",
    useShinyjs(),
    includeCSS(system.file("css/custom.css", package = "rAccess")),
    fluidRow(
      column(
        12, actionButton(class = "mr-1", ns("btnAddUsr"), "Add User"),
        actionButton(ns("btnEditUsr"), "Edit User Access"),
        actionButton(ns("btnDwnAccList"), "Download Access List",
                     icon = icon("download"))
      )
    ),
    hr(),
    fluidRow(
      class = "rAccess-table",
      DTOutput(ns("accessDT"))
    )
  )
}

#' @title Server logic of module_sub_iam
#'
#' @param id Module's ID
#' @param access_panel_id Access panel ID
#' @param rAccess_obj New instance of rAccess(R6 object)
#'
#' @importFrom shiny renderUI reactiveValues withProgress showNotification
#'  observeEvent showModal modalDialog uiOutput tagList div isolate
#'  getDefaultReactiveDomain selectInput icon modalButton  actionButton em
#'  htmlOutput removeModal hr p tags req br moduleServer addResourcePath
#'  selectizeInput downloadButton downloadHandler
#' @importFrom DT renderDT datatable
#' @importFrom shinyWidgets searchInput switchInput
#' @importFrom pins pin_write pin_list
#' @importFrom htmlwidgets JS
#' @importFrom tibble as_tibble
#' @importFrom utils write.csv
module_sub_iam_server <- function(id, access_panel_id, rAccess_obj) {
  # add resource path to rAccess package
  addResourcePath(prefix = "png",
                  directoryPath = system.file("png", package = "rAccess"))
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      panel_id <- access_panel_id
      # Fetch data from RS Connect pin board
      df_list <- rAccess_obj$pin_board %>% pin_list()
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(rAccess_obj$pin_name, "_access_board")
      # Check if pin board exists
      if (pin_name %in% df_list) {
        access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
        access_pin_cols <- colnames(access_pin)
        access_panels <- rAccess_obj$access_panels
        curr_accessunits <-
          access_pin_cols[!(access_pin_cols %in%
                            c("AccessPanel",
                              "UserName",
                              "UserID",
                              access_panels))]
        new_accessunits <- unlist(rAccess_obj$access_units)
        if (!identical(curr_accessunits[order(curr_accessunits)],
                       new_accessunits[order(new_accessunits)])) {
          accessunits_rem <- setdiff(curr_accessunits, new_accessunits)
          if (length(accessunits_rem) > 0) {
            access_pin <- access_pin[, !names(access_pin) %in%
                                       c(accessunits_rem)]
          }
          accessunits_add <- setdiff(new_accessunits, curr_accessunits)
          if (length(accessunits_add) > 0) {
            if (nrow(access_pin) > 0) {
              access_pin[accessunits_add] <- "FALSE"
            } else {
              access_units <- c(colnames(access_pin), accessunits_add)
              access_unitsdf <- t(as.data.frame(access_units))
              colnames(access_unitsdf) <- access_units
              access_unitsdf <- access_unitsdf[-1, ]
              access_pin <- as.data.frame(access_unitsdf)
            }
          }
          if (is.null(rAccess_obj$pin_board$path)) {
            rAccess_obj$pin_board %>%
              pin_write(access_pin,
                        name = pin_name,
                        description = paste0("Edited by ",
                                             session$user))
          } else {
            rAccess_obj$pin_board %>%
              pin_write(access_pin,
                        name = pin_name,
                        description = paste0("Edited by ",
                                             session$user))
          }
        }
        access_pin <- access_pin[access_pin$"AccessPanel" == panel_id, ]
        access_pin_panels <-
          access_pin[-which(names(access_pin) == "AccessPanel")]
        if (nrow(access_pin_panels) > 0) {
          row.names(access_pin_panels) <- seq.int(nrow(access_pin_panels))
        }
      } else {
        # Create and save an empty access pin board if it not already exists
        if (length(rAccess_obj$access_units) > 0) {
          access_panels <- rAccess_obj$access_panels
          if (is.list(rAccess_obj$access_units)) {
            access_units <- unlist(rAccess_obj$access_units)
          } else {
            access_units <- rAccess_obj$access_units
          }
          access_units <- c("AccessPanel", "UserName", "UserID",
                            access_units, access_panels)
          access_unitsdf <- t(as.data.frame(access_units))
          colnames(access_unitsdf) <- access_units
          access_unitsdf <- access_unitsdf[-1, ]
          access_pin <- as.data.frame(access_unitsdf)
          if (is.null(rAccess_obj$pin_board$path)) {
            rAccess_obj$pin_board %>%
              pin_write(access_pin,
                        name = pin_name,
                        description = paste0("Edited by ",
                                             session$user))
          } else {
            rAccess_obj$pin_board %>%
              pin_write(access_pin,
                        name = pin_name,
                        description = paste0("Edited by ",
                                             session$user))
          }
          access_pin_panels <- access_pin[, -1]
        }
      }
      reactive_vals <- reactiveValues(
        raccess_pin = access_pin_panels,
        add_acc_buttons = list(),
        addAccSel = c(),
        addAccSelDropDn = c(),
        editAccButtons = list(),
        editAccSel = c(),
        editAccSelDropDn = c(),
        editrow = NULL,
        rUserList = NULL,
        rUserEditList = NULL,
        raddUsrInfoAlert = tags$span(""),
        raddUsrAlert = tags$span(""),
        reditUsrInfoAlert = tags$span(""),
        reditUsrAlert = tags$span(""),
        rdelUsrInfoAlert = tags$span(""),
        rdelUsrAlert = tags$span(""),
        rdelAdminAlert = tags$span(""),
        accessUnit_type = NULL,
        dwn_access_pin = NULL
      )
      # Access Data Table
      output$accessDT <- renderDT(server = FALSE, {
        access_df <- reactive_vals$raccess_pin
        access_df[access_df == TRUE] <- "\u2713"
        access_df[access_df == FALSE] <- ""
        access_panels <- rAccess_obj$access_panels
        if (panel_id == "ADMIN") {
          access_df <- access_df[, c("UserName", "UserID", access_panels)]
        } else if (is.list(rAccess_obj$access_units)) {
          access_df <- access_df[, c("UserName",
                                     "UserID",
                                     rAccess_obj$access_units[[panel_id]])]
        } else {
          access_df <- access_df[, !colnames(access_df) %in% c(access_panels)]
        }
        datatable(access_df,
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE),
          callback = JS(
            paste0(
              "table.on('dblclick', 'td',",
              "  function() {",
              "    var rowID =  table.row(this).data()[0];",
              "    Shiny.setInputValue('", ns("dt_dblclick_qa"), "', {dt_row: rowID});",
              "  }",
              ");"
            )
          )
        )
      })
      # Click and edit functionality
      observeEvent(input$"dt_dblclick_qa", {
        reactive_vals$reditUsrInfoAlert <- tags$span("")
        reactive_vals$reditUsrAlert <- tags$span("")
        isolate({
          reactive_vals$editrow <-
            which(reactive_vals$raccess_pin == input$"dt_dblclick_qa"$dt_row)[1]
          editrowidx <- reactive_vals$editrow
        })
        if (!is.null(editrowidx)) {
          access_df <- reactive_vals$raccess_pin
          editrow <- access_df[editrowidx, , drop = FALSE]
          # Show Edit User Modal
          show_edit_modal(editrow)
        } else {
          reactive_vals$reditUsrInfoAlert <- tags$span("")
          reactive_vals$reditUsrAlert <-
            tags$span(tags$img(src = "png/caution-icon.png",
                               class = "img-10px pr-2"),
                      "No row selected to edit")
        }
      })
      # Add user functionality
      # List of access units
      output$addaccessList <- renderUI({
        req(input$btnAddUsr)
        access_df <- reactive_vals$raccess_pin
        access_panels <- rAccess_obj$access_panels
        if (panel_id == "ADMIN") {
          label_ <- "Select Access Panels: "
          access_df <- access_df[, c("UserName", "UserID", access_panels)]
        } else if (is.list(rAccess_obj$access_units)) {
          access_df <- access_df[, c("UserName",
                                     "UserID",
                                     rAccess_obj$access_units[[panel_id]])]
          label_ <- "Select access units: "
        } else {
          access_df <- access_df[, !colnames(access_df) %in% c(access_panels)]
          label_ <- "Select access units: "
        }
        access_units <- colnames(access_df)[-c(1:2)]
        add_access_list_ <- tagList()
        if (rAccess_obj$unit_display == "dropdown") {
          if (length(access_units) > 0) {
            add_access_list_ <- selectizeInput(
              inputId = ns("selUnits"),
              label = label_,
              multiple = TRUE,
              choices = access_units,
              selected = NULL,
              options = list(
                create = FALSE,
                placeholder = "Search...",
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          }
        } else {
          if (length(access_units) > 0) {
            for (i in seq_along(access_units)) {
              add_access_unit_ <- shinyWidgets::switchInput(
                inputId = ns(paste0("addbtnsrc", i)),
                onLabel = access_units[i],
                offLabel = access_units[i],
                size = rAccess_obj$switch_size,
                inline = TRUE,
                labelWidth = "100px",
                handleWidth = "100px"
              )
              add_access_list_ <- tagList(add_access_list_, add_access_unit_)
              isolate({
                reactive_vals$add_acc_buttons[[i]] <- add_access_unit_
              })
            }
          }
          if (length(add_access_list_) > 0) {
            add_access_list_ <- tagList(p(class = "col-sm-12",
                                          style = "padding: 0;",
                                          tags$b("Select access units: ")),
                                        add_access_list_)
          }
        }
        add_access_list_
      })
      # List of users with matching text
      output$userList <- renderUI({
        reactive_vals$raddUsrInfoAlert <- tags$span("")
        reactive_vals$raddUsrAlert <- tags$span("")
        username_ <- input$txtUsername
        if (nchar(trimws(username_)) >= 1) {
          withProgress(message = "Fetching list of users...", {
            if (!is.null(rAccess_obj$user_df)) {
              choices_ <- rAccess_obj$matched_users(username_)
            } else {
              choices_ <- NULL
            }
            userdf_choices_ <- choices_
            if (Sys.getenv("SHINY_PORT") != "") {
              if (rAccess_obj$use_rconnect_users) {
                rconnect_choices_ <- rAccess::get_user_api(
                  contact_info = username_,
                  url = session$clientData$url_hostname,
                  api_key = NULL
                )
                choices_ <- rbind(rconnect_choices_, userdf_choices_)
              } else {
                choices_ <- userdf_choices_
              }
            }
            if (!is.null(choices_)) {
              choices_ <- choices_[!duplicated(tolower(choices_$userid)), ]
              choices_temp <- choices_$userid
              names(choices_temp) <- paste0(choices_$userid,
                                            " (", choices_$username, ")")
              user_df <- data.frame(
                UserName = choices_$username,
                User = choices_temp
              )
              reactive_vals$rUserList <- user_df
            } else {
              user_df <- NULL
              choices_temp <- NULL
              tags$span(tags$img(src = "png/caution-icon.png",
                                 class = "img-10px pr-2"), "No users found")
            }
          })
        } else {
          user_df <- NULL
          choices_temp <- NULL
          tags$span(tags$img(src = "png/caution-icon.png",
                             class = "img-10px pr-2"), "Please enter text")
        }
        reactive_vals$rUserList <- user_df
        selectInput(ns("pduserList"), "", choices = choices_temp,
                    selected = choices_temp[1])
      })
      output$btnaddUsrInfoAlert <- renderUI({
        reactive_vals$raddUsrInfoAlert
      })
      output$btnaddUsrAlert <- renderUI({
        reactive_vals$raddUsrAlert
      })
      # List of selected access units in case of dropdown
      observeEvent(input$selUnits, {
        reactive_vals$addAccSelDropDn <- input$selUnits
      })
      # Show Modal on clicking add user
      observeEvent(input$btnAddUsr, {
        reactive_vals$raddUsrInfoAlert <- tags$span("")
        reactive_vals$raddUsrAlert <- tags$span("")
        # Show Add User Modal
        showModal(modalDialog(
          title = "ADD USER",
          tagList(
            div(
              class = "rAccess-board",
              shinyWidgets::searchInput(
                inputId = ns("txtUsername"),
                label = "Enter a name (or Everyone): ",
                placeholder = "Type-in...",
                btnSearch = icon("magnifying-glass",
                                 class = NULL, lib = "font-awesome"),
                btnReset = icon("xmark", class = NULL,
                                lib = "font-awesome"),
                width = "300px"
              ),
              uiOutput(ns("userList")),
              uiOutput(ns("addaccessList"))
            )
          ),
          footer = tagList(uiOutput(class = "valid",
                                    ns("btnaddUsrInfoAlert")),
                           uiOutput(class = "invalid",
                                    ns("btnaddUsrAlert")),
                           actionButton(ns("btnaddUsrSubmit"), "Save"),
                           modalButton("Close"))
        ))
      })
      # Observe event for list of access unit inputs
      observeEvent(reactive_vals$add_acc_buttons, {
        access_df <- reactive_vals$raccess_pin
        if (panel_id == "ADMIN") {
          access_df <- access_df[, c("UserName", "UserID", access_panels)]
        } else {
          access_df <- access_df[, !colnames(access_df) %in% c(access_panels)]
        }
        access_units <- colnames(access_df)[-c(1:2)]
        for (ii in seq_along(reactive_vals$add_acc_buttons)) {
          local({
            i <- ii
            observeEvent(input[[paste0("addbtnsrc", i)]], {
              add_acc_sel_buttons_ <- reactive_vals$addAccSel
              val_ <- input[[paste0("addbtnsrc", i)]]
              if (val_) {
                add_acc_sel_buttons_ <- c(add_acc_sel_buttons_, access_units[i])
                reactive_vals$addAccSel <- unique(add_acc_sel_buttons_)
              } else {
                add_acc_sel_buttons_ <-
                  add_acc_sel_buttons_[!add_acc_sel_buttons_ %in%
                                       c(access_units[i])]
                reactive_vals$addAccSel <- unique(add_acc_sel_buttons_)
              }
            })
          })
        }
      })
      # Submit addition of user
      observeEvent(input$btnaddUsrSubmit, {
        if (panel_id == "ADMIN") {
          shinyjs::showElement("iam-btnRefreshAcess", asis = TRUE)
        }
        if (rAccess_obj$unit_display == "dropdown") {
          selectedaccess <- reactive_vals$addAccSelDropDn
        } else {
          selectedaccess <- reactive_vals$addAccSel
        }
        access_df <- reactive_vals$raccess_pin
        accesslistbool <- colnames(access_df)[-c(1:2)] %in% selectedaccess
        userid <- input$pduserList
        userdf <- reactive_vals$rUserList
        username <- userdf[userdf$User == userid, "UserName"]
        # Remove NAs
        username <- username[!is.na(username)]
        if (rAccess_obj$access_mode == "default") {
          if (!is.null(userid)) {
            if (userid != "") {
              if (!userid %in% access_df$UserID) {
                # There should be at least one access unit selected if its for
                # a non-admin panel
                if (TRUE %in% accesslistbool) {
                  accesslistbool <- c(username, userid, accesslistbool)
                  # Adding new user access row to reactive access data frame
                  access_df[nrow(access_df) + 1, ] <- accesslistbool
                  row.names(access_df)[nrow(access_df)] <- nrow(access_df)
                  reactive_vals$raccess_pin <- access_df
                  access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
                  accesslistbool <- c(panel_id, accesslistbool)
                  access_pin[nrow(access_pin) + 1, ] <- accesslistbool
                  row.names(access_pin)[nrow(access_pin)] <- nrow(access_pin)
                  access_pin <- access_pin[order(access_pin$"AccessPanel"), ]
                  row.names(access_pin) <- seq.int(nrow(access_pin))
                  rAccess_obj$pin_board %>%
                    pin_write(access_pin,
                              name = pin_name,
                              description = paste0("Edited by ", session$user))
                  reactive_vals$raddUsrInfoAlert <-
                    tags$span(tags$img(src =
                                         "png/check-selected-icon-s.png",
                                       class = "img-10px pr-2"),
                              "User access added successfully")
                  reactive_vals$raddUsrAlert <- tags$span("")
                } else {
                  reactive_vals$raddUsrInfoAlert <- tags$span("")
                  reactive_vals$raddUsrAlert <-
                    tags$span(tags$img(src = "png/caution-icon.png",
                                       class = "img-10px pr-2"),
                              "Unable to add user as no access units are
                              selected")
                }
              } else {
                reactive_vals$raddUsrInfoAlert <- tags$span("")
                reactive_vals$raddUsrAlert <-
                  tags$span(tags$img(src = "png/caution-icon.png",
                                     class = "img-10px pr-2"),
                            "This user already exists")
              }
            } else {
              reactive_vals$raddUsrInfoAlert <- tags$span("")
              reactive_vals$raddUsrAlert <-
                tags$span(tags$img(src = "png/caution-icon.png",
                                   class = "img-10px pr-2"),
                          "Please enter valid user name")
            }
          }
        } else {
          if (rAccess_obj$access_mode == "single unit" &
                length(selectedaccess) > 1 & panel_id != "ADMIN") {
            reactive_vals$raddUsrInfoAlert <- tags$span("")
            reactive_vals$raddUsrAlert <- tags$span(
              tags$img(src = "png/caution-icon.png", class = "img-10px pr-2"),
              "Only one unit allowed in the selected access mode"
            )
          } else {
            if (!is.null(userid)) {
              if (userid != "") {
                if (!userid %in% access_df$UserID) {
                  if (TRUE %in% accesslistbool) {
                    accesslistbool <- c(username, userid, accesslistbool)
                    # Adding new user access row to reactive access data frame
                    access_df[nrow(access_df) + 1, ] <- accesslistbool
                    row.names(access_df)[nrow(access_df)] <- nrow(access_df)
                    reactive_vals$raccess_pin <- access_df
                    access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
                    accesslistbool <- c(panel_id, accesslistbool)
                    access_pin[nrow(access_pin) + 1, ] <- accesslistbool
                    row.names(access_pin)[nrow(access_pin)] <- nrow(access_pin)
                    access_pin <- access_pin[order(access_pin$"AccessPanel"), ]
                    row.names(access_pin) <- seq.int(nrow(access_pin))
                    # Adding new user access to pin_board
                    rAccess_obj$pin_board %>%
                      pin_write(access_pin,
                                name = pin_name,
                                description = paste0("Edited by ",
                                                     session$user))
                    reactive_vals$raddUsrInfoAlert <-
                      tags$span(tags$img(src =
                                           "png/check-selected-icon-s.png",
                                         class = "img-10px pr-2"),
                                "User access added successfully")
                    reactive_vals$raddUsrAlert <- tags$span("")
                  } else {
                    reactive_vals$raddUsrInfoAlert <- tags$span("")
                    reactive_vals$raddUsrAlert <-
                      tags$span(tags$img(
                                         src = "png/caution-icon.png",
                                         class = "img-10px pr-2"),
                      "Unable to add user as no access units are selected")
                  }
                } else {
                  reactive_vals$raddUsrInfoAlert <- tags$span("")
                  reactive_vals$raddUsrAlert <-
                    tags$span(tags$img(src = "png/caution-icon.png",
                                       class = "img-10px pr-2"),
                              "This user already exists")
                }
              } else {
                reactive_vals$raddUsrInfoAlert <- tags$span("")
                reactive_vals$raddUsrAlert <-
                  tags$span(tags$img(src = "png/caution-icon.png",
                                     class = "img-10px pr-2"),
                            "Please enter valid user name")
              }
            }
          }
        }
      })
      # Edit user functionality
      # List of access units
      output$editaccessList <- renderUI({
        access_df <- reactive_vals$raccess_pin
        if (panel_id == "ADMIN") {
          access_df <- access_df[, c("UserName", "UserID", access_panels)]
          label_ <- "Select Access Panels: "
        } else if (is.list(rAccess_obj$access_units)) {
          access_df <- access_df[, c("UserName", "UserID",
                                     rAccess_obj$access_units[[panel_id]])]
          label_ <- "Select access units: "
        } else {
          access_df <- access_df[, !colnames(access_df) %in% c(access_panels)]
          label_ <- "Select access units: "
        }
        access_units <- colnames(access_df)[-c(1:2)]
        editrowidx <- reactive_vals$editrow
        edit_accesslist_ <- tagList()
        message(paste0("editrow: ", editrowidx))
        if (rAccess_obj$unit_display == "dropdown") {
          if (editrowidx %in% rownames(access_df)) {
            editrow <- access_df[editrowidx, , drop = FALSE]
            editaccess_units <-
              colnames(editrow[, editrow == TRUE, drop = FALSE])
            reactive_vals$editAccSel <- editaccess_units
            if (length(access_units) > 0) {
              edit_accesslist_ <- selectizeInput(
                inputId = ns("edtUnits"),
                label = label_,
                multiple = TRUE,
                choices = access_units,
                selected = editaccess_units,
                options = list(
                  create = FALSE,
                  placeholder = "Search...",
                  onType =
                    I("function (str) {if (str === \"\") {this.close();}}")
                )
              )
            }
          }
        } else {
          if (editrowidx %in% rownames(access_df)) {
            editrow <- access_df[editrowidx, , drop = FALSE]
            editaccess_units <- colnames(editrow[,
                                                 editrow == TRUE,
                                                 drop = FALSE])
            reactive_vals$editAccSel <- editaccess_units
            if (length(access_units) > 0) {
              for (i in seq_along(access_units)) {
                if (access_units[i] %in% editaccess_units) {
                  value_ <- TRUE
                } else {
                  value_ <- FALSE
                }
                edit_access_unit_ <- shinyWidgets::switchInput(
                  inputId = ns(paste0("editbtnsrc", i)),
                  onLabel = access_units[i],
                  offLabel = access_units[i],
                  size = rAccess_obj$switch_size,
                  labelWidth = "100px",
                  handleWidth = "100px",
                  inline = TRUE,
                  value = value_
                )
                edit_accesslist_ <- tagList(edit_accesslist_, edit_access_unit_)
                isolate({
                  reactive_vals$editAccButtons[[i]] <- edit_access_unit_
                })
              }
            }
            if (length(edit_accesslist_) > 0) {
              edit_accesslist_ <-
                tagList(p(tags$b("Select access units: ")), edit_accesslist_)
            }
          }
        }
        edit_accesslist_
      })
      # Display user id
      output$txtEditUsername <- renderUI({
        editrowidx <- reactive_vals$editrow
        access_df <- reactive_vals$raccess_pin
        editrow <- access_df[editrowidx, , drop = FALSE]
        editrow$UserID
      })
      # Display access mode
      output$txtAccessMode <- renderUI({
        rAccess_obj$access_mode
      })
      output$UIbtneditUsrSubmit <- renderUI({
        actionButton(class = "mr-1", ns("btneditUsrSubmit"), "Save")
      })
      output$btneditUsrInfoAlert <- renderUI({
        reactive_vals$reditUsrInfoAlert
      })
      output$btneditUsrAlert <- renderUI({
        reactive_vals$reditUsrAlert
      })
      # Edit user Modal
      show_edit_modal <- function(editrow) {
        reactive_vals$reditUsrInfoAlert <- tags$span("")
        reactive_vals$reditUsrAlert <- tags$span("")
        showModal(modalDialog(
          title = "EDIT USER",
          div(class = "col-sm-12 py-2 px-0 font-weight-bold", "User :",
              em(class = "d-inline-block", htmlOutput(ns("txtEditUsername")))),
          div(class = "col-sm-12 py-2 px-0 font-weight-bold", "Access Mode :",
              em(class = "d-inline-block", htmlOutput(ns("txtAccessMode")))),
          uiOutput(ns("editaccessList")),
          footer = tagList(
            uiOutput(class = "valid", ns("btneditUsrInfoAlert")),
            uiOutput(class = "invalid", ns("btneditUsrAlert")),
            uiOutput(class = "invalid", ns("btndelAdminAlert")),
            uiOutput(ns("UIbtneditUsrSubmit")),
            actionButton(ns("btndelUsr"), "Delete User"),
            modalButton("Close")
          )
        ))
      }
      # Show Modal on clicking Edit user
      observeEvent(input$btnEditUsr, {
        reactive_vals$reditUsrInfoAlert <- tags$span("")
        reactive_vals$reditUsrAlert <- tags$span("")
        reactive_vals$rdelAdminAlert <- tags$span("")
        isolate({
          reactive_vals$editrow <- input$accessDT_rows_selected
          editrowidx <- reactive_vals$editrow
        })
        if (!is.null(editrowidx)) {
          access_df <- reactive_vals$raccess_pin
          editrow <- access_df[editrowidx, , drop = FALSE]
          # Show Edit User Modal
          show_edit_modal(editrow)
        } else {
          reactive_vals$reditUsrInfoAlert <- tags$span("")
          reactive_vals$reditUsrAlert <-
            tags$span(tags$img(src = "png/caution-icon.png",
                               class = "img-10px pr-2"),
                      "No row selected to edit")
        }
      })
      # observe event for list of access unit inputs
      observeEvent(reactive_vals$editAccButtons, {
        access_df <- reactive_vals$raccess_pin
        if (panel_id == "ADMIN") {
          access_df <- access_df[, c("UserName", "UserID", access_panels)]
        } else {
          access_df <- access_df[, !colnames(access_df) %in% c(access_panels)]
        }
        access_units <- colnames(access_df)[-c(1:2)]
        for (ii in seq_along(reactive_vals$editAccButtons)) {
          local({
            i <- ii
            observeEvent(input[[paste0("editbtnsrc", i)]], {
              edit_acc_sel_buttons_ <- reactive_vals$editAccSel
              val_ <- input[[paste0("editbtnsrc", i)]]
              if (val_) {
                edit_acc_sel_buttons_ <-
                  c(edit_acc_sel_buttons_, access_units[i])
                reactive_vals$editAccSel <- unique(edit_acc_sel_buttons_)
              } else {
                edit_acc_sel_buttons_ <-
                  edit_acc_sel_buttons_[!edit_acc_sel_buttons_ %in%
                                        c(access_units[i])]
                reactive_vals$editAccSel <- unique(edit_acc_sel_buttons_)
              }
            })
          })
        }
      })
      observeEvent(input$edtUnits, {
        reactive_vals$editAccSelDropDn <- input$edtUnits
      })
      # Submit after editing of user
      observeEvent(input$btneditUsrSubmit, {
        reactive_vals$rdelAdminAlert <- tags$span("")
        if (panel_id == "ADMIN") {
          shinyjs::showElement("iam-btnRefreshAcess", asis = TRUE)
        }
        reactive_vals$reditUsrInfoAlert <- tags$span("")
        reactive_vals$reditUsrAlert <- tags$span("")
        isolate({
          if (rAccess_obj$unit_display == "dropdown") {
            selectedaccess <- input$edtUnits
          } else {
            selectedaccess <- reactive_vals$editAccSel
          }
          access_df <- reactive_vals$raccess_pin
          editrowidx <- reactive_vals$editrow
        })
        accesslistbool <- colnames(access_df)[-c(1:2)] %in% selectedaccess
        userid <- access_df[editrowidx, "UserID"]
        username <- access_df[editrowidx, "UserName"]
        # Check access mode ----
        if (rAccess_obj$access_mode == "default") {
          if (!is.null(userid)) {
            if (userid != "") {
              otheruserlist <-
                access_df[!(row.names(access_df) %in% editrowidx), "UserID"]
              if (!userid %in% otheruserlist) {
                if (TRUE %in% accesslistbool) {
                  accesslistbool <- c(username, userid, accesslistbool)
                  if (!identical(as.character(access_df[editrowidx, ]),
                                 accesslistbool)) {
                    access_df[editrowidx, ] <- accesslistbool
                    reactive_vals$raccess_pin <- access_df
                    access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
                    edit_db_rowidx <-
                      which(((access_pin$UserID == userid) &
                               (access_pin$"AccessPanel" == panel_id)), )
                    accesslistbool <- c(panel_id, accesslistbool)
                    access_pin[edit_db_rowidx, ] <- accesslistbool
                    access_pin <- access_pin[order(access_pin$"AccessPanel"), ]
                    row.names(access_pin) <- seq.int(nrow(access_pin))
                    rAccess_obj$pin_board %>%
                      pin_write(access_pin,
                                name = pin_name,
                                description = paste0("Edited by ",
                                                     session$user))
                    reactive_vals$reditUsrInfoAlert <-
                      tags$span(
                                tags$img(src =
                                           "png/check-selected-icon-s.png",
                                         class = "img-10px pr-2"),
                                "User access edited successfully")
                    reactive_vals$reditUsrAlert <- tags$span("")
                  } else {
                    reactive_vals$reditUsrInfoAlert <- tags$span("")
                    reactive_vals$reditUsrAlert <-
                      tags$span(tags$img(src = "png/caution-icon.png",
                                         class = "img-10px pr-2"),
                                "No changes made")
                  }
                } else {
                  # ----
                  access_df <- reactive_vals$raccess_pin
                  # Subset pin data frame for General ADMIN users
                  access_df_ga <- access_df[access_df$ADMIN == TRUE, ]
                  selrowidx <- which(access_df$ADMIN == TRUE, )
                  delrowidx <- reactive_vals$editrow
                  if ((nrow(access_df_ga) == 1) &&
                        (panel_id == "ADMIN") &&
                        (selrowidx == delrowidx)) {
                    reactive_vals$rdelUsrInfoAlert <- tags$span("")
                    reactive_vals$rdelUsrAlert <-
                      reactive_vals$rdelUsrAlert <-
                      tags$span(tags$img(src = "png/caution-icon.png",
                                         class = "img-10px pr-2"),
                        "Note: Empty Admin lists enable everyones to
                                 make changes to all access panels"
                      )
                    reactive_vals$rdelAdminAlert <- tags$span(
                      tags$img(src = "png/caution-icon.png",
                               class = "img-10px pr-2"),
                      "ADMIN Panel cannot be empty!!", br(),
                      "Please add another Admin to remove this user"
                    )
                  } else {
                    reactive_vals$rdelUsrInfoAlert <- tags$span("")
                    reactive_vals$rdelUsrAlert <- tags$span("")
                    showModal(
                      modalDialog(
                        title = "Delete Confirmation",
                        easyClose = TRUE,
                        size = c("s"),
                        fade = TRUE,
                        p("Are you sure you want to delete the selected user?"),
                        footer = tagList(
                          uiOutput(class = "valid", ns("btndelUsrInfoAlert")),
                          uiOutput(class = "invalid", ns("btndelUsrAlert")),
                          actionButton(ns("btnDelOK"), "Yes"),
                          actionButton(ns("btnDelClose"), "No"),
                          modalButton("Close")
                        )
                      )
                    )
                  }
                  # ----
                }
              } else {
                reactive_vals$reditUsrInfoAlert <- tags$span("")
                reactive_vals$reditUsrAlert <-
                  tags$span(tags$img(src = "png/caution-icon.png",
                                     class = "img-10px pr-2"),
                            "This user already exists")
              }
            } else {
              reactive_vals$reditUsrInfoAlert <- tags$span("")
              reactive_vals$reditUsrAlert <-
                tags$span(tags$img(src = "png/caution-icon.png",
                                   class = "img-10px pr-2"),
                          "Please enter valid user name")
            }
          }
        } else {
          if (rAccess_obj$access_mode == "single unit" &
                length(selectedaccess) > 1 & panel_id != "ADMIN") {
            reactive_vals$reditUsrInfoAlert <- tags$span("")
            reactive_vals$reditUsrAlert <- tags$span(
              tags$img(src = "png/caution-icon.png", class = "img-10px pr-2"),
              "Only one unit allowed in the selected access mode"
            )
          } else {
            if (!is.null(userid)) {
              if (userid != "") {
                otheruserlist <-
                  access_df[!(row.names(access_df) %in% editrowidx), "UserID"]
                if (!userid %in% otheruserlist) {
                  if (TRUE %in% accesslistbool) {
                    accesslistbool <- c(username, userid, accesslistbool)
                    if (!identical(as.character(access_df[editrowidx, ]),
                                   accesslistbool)) {
                      access_df[editrowidx, ] <- accesslistbool
                      reactive_vals$raccess_pin <- access_df
                      access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
                      edit_db_rowidx <-
                        which(((access_pin$UserID == userid) &
                                 (access_pin$"AccessPanel" == panel_id)), )
                      accesslistbool <- c(panel_id, accesslistbool)
                      access_pin[edit_db_rowidx, ] <- accesslistbool
                      access_pin <-
                        access_pin[order(access_pin$"AccessPanel"), ]
                      row.names(access_pin) <- seq.int(nrow(access_pin))
                      rAccess_obj$pin_board %>%
                        pin_write(access_pin,
                                  name = pin_name,
                                  description = paste0("Edited by ",
                                                       session$user))
                      reactive_vals$reditUsrInfoAlert <-
                        tags$span(
                                  tags$img(src =
                                             "png/check-selected-icon-s.png",
                                           class = "img-10px pr-2"),
                                  "User access edited successfully")
                      reactive_vals$reditUsrAlert <- tags$span("")
                    } else {
                      reactive_vals$reditUsrInfoAlert <- tags$span("")
                      reactive_vals$reditUsrAlert <-
                        tags$span(tags$img(src = "png/caution-icon.png",
                                           class = "img-10px pr-2"),
                                  "No changes made")
                    }
                  } else {
                    reactive_vals$reditUsrInfoAlert <- tags$span("")
                    reactive_vals$reditUsrAlert <-
                      tags$span(tags$img(
                                         src = "png/caution-icon.png",
                                         class = "img-10px pr-2"),
                      "Unable to save as no access units are selected")
                  }
                } else {
                  reactive_vals$reditUsrInfoAlert <- tags$span("")
                  reactive_vals$reditUsrAlert <-
                    tags$span(tags$img(src = "png/caution-icon.png",
                                       class = "img-10px pr-2"),
                              "This user already exists")
                }
              } else {
                reactive_vals$reditUsrInfoAlert <- tags$span("")
                reactive_vals$reditUsrAlert <-
                  tags$span(tags$img(src = "png/caution-icon.png",
                                     class = "img-10px pr-2"),
                            "Please enter valid user name")
              }
            }
          }
        }
      })
      output$btndelUsrInfoAlert <- renderUI({
        reactive_vals$rdelUsrInfoAlert
      })
      output$btndelUsrAlert <- renderUI({
        reactive_vals$rdelUsrAlert
      })
      output$btndelAdminAlert <- renderUI({
        reactive_vals$rdelAdminAlert
      })
      # Delete user functionality
      observeEvent(input$btndelUsr, {
        reactive_vals$reditUsrAlert <- tags$span("")
        access_df <- reactive_vals$raccess_pin
        # Subset pin data frame for General ADMIN users
        access_df_ga <- access_df[access_df$ADMIN == TRUE, ]
        selrowidx <- which(access_df$ADMIN == TRUE, )
        delrowidx <- reactive_vals$editrow
        if ((nrow(access_df_ga) == 1) &&
              (panel_id == "ADMIN") &&
              (selrowidx == delrowidx)) {
          reactive_vals$rdelUsrInfoAlert <- tags$span("")
          reactive_vals$rdelUsrAlert <- tags$span(
            tags$img(src = "png/caution-icon.png", class = "img-10px pr-2"),
            "Note: Empty Admin lists enable everyones to update all accesslists"
          )
          reactive_vals$rdelAdminAlert <- tags$span(
            tags$img(src = "png/caution-icon.png", class = "img-10px pr-2"),
            "ADMIN Panel cannot be empty!!", br(),
            "Please add another Admin to remove this user"
          )
          message(reactive_vals$rdelAdminAlert)
        } else {
          reactive_vals$rdelUsrInfoAlert <- tags$span("")
          reactive_vals$rdelUsrAlert <- tags$span("")
          showModal(
            modalDialog(
              title = "Delete Confirmation",
              easyClose = TRUE,
              size = c("s"),
              fade = TRUE,
              p("Are you sure you want to delete the selected user?"),
              footer = tagList(
                uiOutput(class = "valid", ns("btndelUsrInfoAlert")),
                uiOutput(class = "invalid", ns("btndelUsrAlert")),
                actionButton(ns("btnDelOK"), "Yes"),
                actionButton(ns("btnDelClose"), "No"), modalButton("Close")
              )
            )
          )
        }
      })
      # Confirm delete OK functionality
      observeEvent(input$btnDelOK, {
        if (panel_id == "ADMIN") {
          shinyjs::showElement("iam-btnRefreshAcess", asis = TRUE)
        }
        reactive_vals$rdelUsrInfoAlert <- tags$span("")
        reactive_vals$rdelUsrAlert <- tags$span("")
        access_df <- reactive_vals$raccess_pin
        editrowidx <- reactive_vals$editrow
        if (!is.null(editrowidx)) {
          userid <- access_df[editrowidx, "UserID"]
          access_df <- access_df[-editrowidx, , drop = FALSE]
          if (nrow(access_df) > 0) {
            row.names(access_df) <- seq.int(nrow(access_df))
          }
          reactive_vals$raccess_pin <- access_df
          access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
          edit_db_rowidx <-
            which(((access_pin$UserID == userid) &
                     (access_pin$"AccessPanel" == panel_id)), )
          access_pin <- access_pin[-edit_db_rowidx, , drop = FALSE]
          access_pin <- access_pin[order(access_pin$"AccessPanel"), ]
          if (nrow(access_pin) > 0) {
            row.names(access_pin) <- seq.int(nrow(access_pin))
          }
          rAccess_obj$pin_board %>%
            pin_write(access_pin,
                      name = pin_name,
                      description = paste0("Edited by ", session$user))
          reactive_vals$rdelUsrInfoAlert <- tags$span("")
          reactive_vals$rdelUsrAlert <- tags$span("")
          showModal(
            modalDialog(
              title = "User access deleted successfully",
              easyClose = TRUE,
              size = c("s"),
              fade = TRUE,
              footer = modalButton("Close")
            )
          )
        } else {
          reactive_vals$rdelUsrInfoAlert <- tags$span("")
          reactive_vals$rdelUsrAlert <-
            tags$span(tags$img(src = "png/caution-icon.png",
                               class = "img-10px pr-2"),
                      "No row selected to delete")
        }
      })
      # Confirm delete Cancel functionality
      observeEvent(input$btnDelClose, {
        isolate({
          reactive_vals$editrow <- input$accessDT_rows_selected
          editrowidx <- reactive_vals$editrow
        })
        if (!is.null(editrowidx)) {
          access_df <- reactive_vals$raccess_pin
          editrow <- access_df[editrowidx, , drop = FALSE]
          # Show Edit User Modal
          show_edit_modal(editrow)
        }
      })
      # Download access list
      observeEvent(input$btnDwnAccList, {
        # Fetch data from RS Connect pin board
        df_list <- rAccess_obj$pin_board %>% pin_list()
        # Remove user name in list of pins
        df_list <- gsub(".*[/]", "", df_list)
        pin_name <- paste0(rAccess_obj$pin_name, "_access_board")
        # Check if pin board exists
        if (pin_name %in% df_list) {
          access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
          if (nrow(access_pin) > 0) {
            reactive_vals$dwn_access_pin <- access_pin
            showModal(
              modalDialog(
                title = "Download Access Data",
                downloadButton(ns("dwnRds"), label = ".rds"),
                downloadButton(ns("dwncsv"), label = ".csv")
              )
            )
          } else {
            showNotification("Access list not available!!",
                             closeButton = TRUE, type = c("warning"))
          }
        }
      })
      output$dwnRds <- downloadHandler(
        filename = function() {
          paste0(rAccess_obj$pin_name, "_access_board", ".rds")
        },
        content = function(file) {
          saveRDS(reactive_vals$dwn_access_pin, file)
          showNotification("Access list downloaded successfully!!",
                           type = "message")
          removeModal()
        }
      )
      output$dwncsv <- downloadHandler(
        filename = function() {
          paste0(rAccess_obj$pin_name, "_access_board", ".csv")
        },
        content = function(file) {
          utils::write.csv(reactive_vals$dwn_access_pin, file)
          showNotification("Access list downloaded successfully!!",
                           type = "message")
          removeModal()
        }
      )
    }
  )
}
