#
# This is a Shiny module used by the main shiny web application.
#
# To understand Shiny modules, check the how module_iam_ui() and
# module_iam_server() functions are used in ./app_ui.R and ./app_server.R
#

# User-interface definition of module_iam
#' @title User-interface definition of module_iam
#' @description This is a Shiny module used by the main shiny web application
#' @param id User ID
#'
#' @importFrom shiny uiOutput NS fluidPage addResourcePath
#' @export
module_iam_ui <- function(id) {
  ns <- NS(id)
  addResourcePath(prefix = "png",
                  directoryPath = system.file("png", package = "rAccess"))

  fluidPage(
    class = "px-0",
    useShinyjs(),
    includeCSS(system.file("css/custom.css", package = "rAccess")),
    fluidRow(
      column(
        12,
        actionButton(
          ns("btnRefreshAcess"),
          tags$img(src = "png/refresh-icon.png", class = "img-30px")
        ),
        uiOutput(ns("tabIAM"))
      )
    )
  )
}

#' @title Server logic of module_iam
#' @param id Module's ID
#'
#' @param rAccess_obj New instance of rAccess(R6 object)
#'
#' @importFrom shiny renderUI tabPanel tabsetPanel moduleServer
#' @export
module_iam_server <- function(id, rAccess_obj) {
  ns <- NS(id)
  stopifnot(!is.null(rAccess_obj))
  # Call module_sub_iam_server for all access panels
  for (i in seq_along(rAccess_obj$access_panels)) {
    module_sub_iam_server(ns(paste0("iam", i)),
                          rAccess_obj$access_panels[i],
                          rAccess_obj)
  }
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      user_id <- rAccess_obj$user
      user_id <- ifelse(is.null(user_id), session$user, user_id)
      # Tabset panel with tab panels for all access panels
      output$tabIAM <- renderUI({
        input$btnRefreshAcess
        shinyjs::hideElement("iam-btnRefreshAcess", asis = TRUE)
        tab_item_list <- list()
        panels_withaccess <- c()
        # Fetch data from RS Connect pin board
        df_list <- rAccess_obj$pin_board %>% pin_list()
        # Remove user name in list of pins
        df_list <- gsub(".*[/]", "", df_list)
        pin_name <- paste0(rAccess_obj$pin_name, "_access_board")
        # Check if pin board exists
        if (pin_name %in% df_list) {
          access_pin <- rAccess_obj$pin_board %>% pin_read(pin_name)
          if (nrow(access_pin) > 0) {
            accessdf <-
              access_pin[(access_pin$AccessPanel == "ADMIN" &
                          tolower(access_pin$UserID) == tolower(user_id)),
                         colnames(access_pin) %in% rAccess_obj$access_panels,
                         drop = FALSE]
            if (nrow(accessdf) > 0) {
              panels_withaccess_ <- colnames(accessdf[, accessdf[1, ] == TRUE,
                                                      drop = FALSE])
              panels_withaccess <- c(panels_withaccess, panels_withaccess_)
            }
          }
        }
        # At the start, only ADMIN panel should be visible
        # OR when there are no general admins present
        if ((nrow(access_pin) == 0) || (rAccess_obj$no_admin())) {
          panels_withaccess <- c("ADMIN", panels_withaccess)
        }
        for (i in seq_along(rAccess_obj$access_panels)) {
          if (rAccess_obj$access_panels[i] %in% panels_withaccess) {
            tab_item_ <- tabPanel(
              paste0(rAccess_obj$access_panels[i]),
              module_sub_iam_ui(ns(paste0("iam", i)))
            )
            tab_item_list[[i]] <- tab_item_
          }
        }
        do.call(tabsetPanel, tab_item_list)
      })
    }
  )
}
