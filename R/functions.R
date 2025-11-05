#' @title rAccess module functions - R6 object
#'
#' @description
#' The `rAccess` class encapsulates various methods used in the IAM (Identity
#'  and Access Management) module.
#' It provides functionalities to check user permissions, verify admin status,
#'  retrieve user details,
#' and interact with pin boards for access control configuration.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{user}}{User ID}
#'   \item{\code{app_name}}{Application Name}
#'   \item{\code{pin_name}}{Pin name}
#'   \item{\code{pin_list}}{List of pins}
#'   \item{\code{pin_board}}{Pin board object}
#'   \item{\code{access_panels}}{Available access panels}
#'   \item{\code{access_units}}{Available access units}
#'   \item{\code{access_mode}}{Access mode, e.g., "default", "single unit"}
#'   \item{\code{user_df}}{Data frame of user IDs and names}
#'   \item{\code{switch_size}}{Size of UI switch elements
#'    (e.g., "small", "large", "default", "mini", "small", "normal", "large")}
#'   \item{\code{unit_display}}{Display type for access units
#'    ("switch", "dropdown")}
#'   \item{\code{board_type}}{Type of pin board ("local", "s3", "rconnect")}
#'   \item{\code{local_board_path}}{Local path for local pin boards}
#'   \item{\code{s3_bucket}}{S3 bucket name}
#'   \item{\code{s3_access_key}}{S3 access key}
#'   \item{\code{s3_secret_key}}{S3 secret key}
#'   \item{\code{use_rconnect_users}}{Boolean, use rconnect users in
#'    conjunction with user_df}
#'   \item{\code{config}}{Configuration file content}
#'   \item{\code{data}}{List of data paths from config}
#'   \item{\code{verbose}}{Boolean, whether to print logs}
#'   \item{\code{panel_config}}{Complete panel structure}
#'   \item{\code{secure_mode}}{Boolean, enforce access requirement}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(user, ...)}}{Constructor to create an instance with
#'    specified parameters.}
#'   \item{\code{check_access(user_id, access_panel)}}{Checks user access rights
#'    for a given panel.}
#'   \item{\code{is_admin()}}{Checks if the current user is an admin.}
#'   \item{\code{no_admin()}}{Checks if there are no admins in the admin panel.}
#'   \item{\code{get_userlist_unit(access_panel, access_unit)}}{Gets list of
#'    users with access to the specified unit.}
#'   \item{\code{rAccessThemes()}}{Includes custom CSS themes for the app.}
#'   \item{\code{get_user_accesslist(user_id)}}{Lists access units available to
#'    a user, including "everyone".}
#'   \item{\code{get_superAdmins()}}{Returns list of admin user IDs.}
#' }
#'
#' @importFrom tibble tibble
#' @importFrom jsonlite fromJSON
#' @importFrom pins pin_read
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom shiny includeScript includeCSS isolate
#' @importFrom config get
#' @importFrom stats setNames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom here here
#' @importFrom yaml read_yaml
#' @importFrom shinyalert shinyalert
#'
#' @export
rAccess <- R6Class(
  "rAccess",
  # ============================
  # Fields
  # ============================
  public = list(
    #' @field user User ID
    user = NULL,
    #' @field app_name App Name
    app_name = NULL,
    #' @field pin_name A field that takes the argument pin_name
    pin_name = NULL,
    #' @field pin_list A field that takes the argument pin_list
    pin_list = NULL,
    #' @field pin_board A field that takes the argument pin_board
    pin_board = NULL,
    #' @field access_panels A field that takes the argument access_panels
    access_panels = NULL,
    #' @field access_units A field that takes the argument access_units
    access_units = NULL,
    #' @field access_mode Enables user to select access modes.Available access
    #'  modes are :
    #' default - Allows access to multiple access panels and multiple access
    #'  units, single unit - Allows access to single access unit within an
    #'  access panel.
    access_mode = "NULL",
    #' @field user_df A data.frame with user id and user name
    user_df = NULL,
    #' @field switch_size Takes values : 'default', 'mini', 'small', 'normal',
    #'  'large'. Determines the size of access unit switches used in the module.
    switch_size = NULL,
    #' @field unit_display Takes values : 'switch', 'dropdown'. Determines
    #' the type of display for access units. Defaults to 'switch'.
    unit_display = NULL,
    #' @field board_type Board type. Takes values "local", "s3", "rconnect"
    board_type = NULL,
    #' @field local_board_path Local board path.
    local_board_path = NULL,
    #' @field s3_bucket S3 bucket
    s3_bucket = NULL,
    #' @field s3_access_key S3 Access Key
    s3_access_key = NULL,
    #' @field s3_secret_key S3 Secret Key
    s3_secret_key = NULL,
    #' @field use_rconnect_users If true then rconnect users will be combined
    #'  with the given user_df
    use_rconnect_users = TRUE,
    #' @field config rAccess configuration file
    config = NULL,
    #' @field data Lists all datapaths in config file
    data = NULL,
    #' @field verbose If TRUE, prints all data base updates in the log
    verbose = FALSE,
    #' @field panel_config A list with entire panel structure including
    #'  datapaths
    panel_config = list(),
    #' @field secure_mode If TRUE, then user should have access to at least one
    #'  access_unit/access_panel to use the app.
    secure_mode = FALSE,

    # ============================
    # Methods
    # ============================

    #' @description Constructor to initialize an rAccess object
    #' @param user User ID
    #' @param pin_name Pin name
    #' @param app_name App name
    #' @param pin_board Pin board
    #' @param access_panels Access panels
    #' @param access_units Access units
    #' @param access_mode Access mode
    #' @param user_df Data Frame with username and userid
    #' @param switch_size Determines size of access unit switches : `default`,
    #'  `mini`, `small`, `normal`, `large`
    #' @param unit_display Determines the type of display for access units :
    #'  `switch`, `dropdown`.
    #' @param board_type Pin board type: `local`, `s3`, `rconnect`
    #' @param local_board_path Local path to save pin_board when board_type is
    #'  `local`.
    #' @param s3_bucket S3 bucket id
    #' @param s3_access_key Access key to S3 bucket
    #' @param s3_secret_key Secret Key to S3 bucket
    #' @param verbose If TRUE, prints all data base updates in the log
    #' @param config rAccess configuration file
    #' @param use_rconnect_users If TRUE, then rconnect users will be combined
    #'  with user_df when deployed.
    #' @param secure_mode If TRUE, then user should have access to at least one
    #'  access_unit/access_panel to use the app.
    #'
    #' @return pin_name, access_panels, access_units, Pin_board ...
    #'
    #' @export
    initialize = function(user = NULL,
                          pin_board = NULL,
                          app_name = NULL,
                          pin_name = NULL,
                          access_panels,
                          access_units = NULL,
                          access_mode = "default",
                          user_df = NULL,
                          switch_size = NULL,
                          unit_display = "switch",
                          board_type = NULL,
                          local_board_path = NULL,
                          s3_bucket = NULL,
                          s3_access_key = NULL,
                          s3_secret_key = NULL,
                          use_rconnect_users = TRUE,
                          config = NULL,
                          verbose = FALSE,
                          secure_mode = FALSE) {
      if (!is.null(config)) {
        yaml_file <- here::here(config)
        raccess_config <- yaml::read_yaml(file = yaml_file, eval.expr = TRUE)
        params <- raccess_config$parameters
        app_name <- params$app_name
        pin_name <- params$pin_name
        board_type <- params$board_type
        access_mode <- params$access_mode
        user_df <- params$user_df
        switch_size <- params$switch_size
        unit_display <- params$unit_display
        local_board_path <- params$local_board_path
        s3_bucket <- params$s3_bucket
        s3_access_key <- params$s3_access_key
        s3_secret_key <- params$s3_secret_key
        use_rconnect_users <- params$use_rconnect_users
        verbose <- params$verbose
        secure_mode <- params$secure_mode
        self$config <- raccess_config
        self$data <- raccess_config$data

        # get access panel list from config
        if (length(raccess_config$panel_str)) {
          access_panels <- list()
          panel_name <- c()
          panel_config <- list()
          for (i in seq_along(raccess_config$panel_str)) {
            panel_name[i] <- raccess_config$panel_str[[i]]$access_panel
            if (is.null(raccess_config$panel_str[[i]]$access_units)) {
              access_panels[[i]] <- NULL
            } else {
              access_panels[[i]] <- sapply(
                seq_along(raccess_config$panel_str[[i]]$access_units),
                function(x) {
                  raccess_config$panel_str[[i]]$access_units[[x]]$unit
                }
              )
              panel_config[[i]] <- lapply(
                seq_along(raccess_config$panel_str[[i]]$access_units),
                function(x) {
                  raccess_config$panel_str[[i]]$access_units[[x]]$data
                }
              )
              names(panel_config[[i]]) <- access_panels[[i]]
            }
          }
          names(access_panels) <- names(panel_config) <- panel_name
          self$panel_config <- panel_config
          if (verbose) {
            print(access_panels)
            print(panel_config)
          }
        }
      }

      stopifnot(!is.null(access_panels))
      stopifnot(!is.null(user))
      stopifnot(is.null(access_mode) | access_mode %in%
                  c("single unit", "default"))
      stopifnot(is.null(switch_size) | switch_size
                %in% c("default", "mini", "small", "normal", "large"))
      stopifnot(unit_display %in% c("switch", "dropdown"))
      stopifnot(board_type %in% c("local", "s3", "rconnect", NULL))
      if (is.null(board_type) & is.null(pin_board)) {
        stop("board_type must be provided if pin_board is not assigned!!")
      }

      # Assign app name to pin_name
      if (!is.null(app_name)) pin_name <- app_name
      # Check Access panel type
      if (is.list(access_panels)) {
        stopifnot(!is.null(names(access_panels)))
        access_units <- access_panels
        access_panels <- unique(c("ADMIN", names(access_panels)))
      }

      # If board_type exists
      if (!is.null(board_type)) {
        if (interactive()) {
          if (board_type == "rconnect")
            stop("board_type = 'rconnect' cannot be used in interactive mode")
        }
        # Create pin_board based on board_type
        if (is.null(pin_board)) {
          if (board_type == "local") {
            local_board_path <- ifelse(is.null(local_board_path),
                                       "./data/", local_board_path)
            pin_board <- pins::board_folder(path = file.path(local_board_path))
          } else if (board_type == "s3") {
            stopifnot(!is.null(s3_bucket) &
                        !is.null(s3_access_key) &
                        !is.null(s3_secret_key))
            s3_config(
              access_key = s3_access_key,
              secret_key = s3_secret_key
            )
            pin_board <- pins::board_s3(
              bucket = s3_bucket,
              prefix = pin_name,
              access_key = s3_access_key,
              secret_access_key = s3_secret_key
            )
          } else if (board_type == "rconnect") {
            pin_board <- pins::board_connect(
              auth = "envvar",
              key = Sys.getenv("CONNECT_API_KEY"),
              server = Sys.getenv("CONNECT_SERVER")
            )
          }
        }
        pin_list <- pin_board %>% pin_list()
      }

      if (is.null(access_mode)) access_mode <- "default"

      self$pin_name <- ifelse(is.null(pin_name), NULL, pin_name)
      self$app_name <- app_name
      self$pin_list <- pin_list
      self$pin_board <- pin_board
      self$access_panels <- access_panels
      self$access_units <- access_units
      self$access_mode <- access_mode
      self$user_df <- user_df
      self$switch_size <- switch_size
      self$unit_display <- unit_display
      self$board_type <- board_type
      self$local_board_path <- local_board_path
      self$s3_bucket <- s3_bucket
      self$s3_access_key <- s3_access_key
      self$s3_secret_key <- s3_secret_key
      self$use_rconnect_users <- use_rconnect_users
      self$verbose <- verbose
      self$user <- user
      self$secure_mode <- secure_mode
    },

    #' @description Find users matching search input in self$user_df
    #'
    #' @param contact_info User entered search text
    #'
    #' @return A data.frame
    #'
    matched_users = function(contact_info) {
      contact_info <- trimws(contact_info)
      if (!tolower(contact_info) %in% c("everyone", "Everyone", "All", "all")) {
        # Convert user entered text to lower case
        contact_info <- trimws(tolower(contact_info))
        # Subset users that matches user entered text
        user_df <-
          as.data.frame(self$user_df[grepl(contact_info,
                                           tolower(self$user_df$username)) |
                                       grepl(contact_info,
                                             tolower(self$user_df$userid)),
                                     c("userid", "username")])
        if (nrow(user_df) > 0) {
          return_vals <- user_df
        } else {
          return_vals <- NULL
        }
      } else {
        # Add Everyone to the list with id user_id of Everyone
        return_vals <- data.frame(userid = "Everyone", username = "Everyone")
      }
      return(return_vals)
    },

    #' @description To check user access rights to an access unit within a
    #' particular access panel. Returns access details from the app's access
    #' pin board that matches given user_id and access panel
    #'
    #' @param user_id User ID
    #' @param access_panel Access Panel name
    #'
    #' @export
    check_access = function(user_id = self$user, access_panel) {
      df_list <- self$pin_list
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(self$pin_name, "_access_board")
      # Check if pin board exists
      if (pin_name %in% df_list) {
        access_pin <- self$pin_board %>% pin_read(pin_name)
        # Subset pin data frame for given access_panel
        access_pin <- access_pin[access_pin$"AccessPanel" == access_panel, ]
        access_pin_access_panel <- subset(access_pin, select = -c(AccessPanel))
      } else {
        access_pin_access_panel <- NULL
        return(NULL)
      }
      if (nrow(access_pin_access_panel) > 0) {
        # Subset for given user id
        accessrow_ <-
          access_pin_access_panel[tolower(access_pin_access_panel$UserID) %in%
                                  tolower(user_id), ]
        # Subset for "Everyone"
        everyone_accessrow_ <-
          access_pin_access_panel[tolower(access_pin_access_panel$UserID) %in%
                                  "everyone", ]
        # combine user access with everyone's access (logical or)
        combine_accessrow_ <- as.logical(everyone_accessrow_) |
          as.logical(accessrow_)
        names(combine_accessrow_) <- names(accessrow_)
        combine_accessrow_ <- data.frame(t(combine_accessrow_))
        combine_accessrow_$UserID <- ifelse(nrow(accessrow_) > 0,
                                            accessrow_$UserID,
                                            "")
        combine_accessrow_$UserName <- ifelse(nrow(accessrow_) > 0,
                                              accessrow_$UserName,
                                              "")
        accessrow_ <- combine_accessrow_
        if (all(is.na(accessrow_[-c(1, 2)]))) {
          accessrow_ <- NULL
        } else {
          accessrow_[is.na(accessrow_)] <- FALSE
        }
      } else {
        accessrow_ <- NULL
      }
      return(accessrow_)
    },

    #' @description To check if the user is Admin in order to provide access to
    #' IAM module. Returns TRUE if it is an admin user.
    #'
    #' @export
    is_admin = function() {
      access_pin <- NULL
      is_admin <- FALSE
      access_pin_access_panel <- data.frame("UserName" = character(0),
                                            "UserID" = character(0))
      df_list <- self$pin_list
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(self$pin_name, "_access_board")
      if (pin_name %in% df_list) {
        access_pin <- self$pin_board %>% pin_read(pin_name)
      }

      if (!is.null(access_pin)) {
        # Subset pin data frame for ADMIN users
        access_pin <- access_pin[access_pin$"AccessPanel" == "ADMIN", ]
        access_pin_access_panel <- subset(access_pin, select = -c(AccessPanel))
      }

      if (nrow(access_pin_access_panel) > 0) {
        # Subset for given user id
        accessrow_ <-
          access_pin_access_panel[tolower(access_pin_access_panel$UserID) %in%
                                  tolower(self$user), , drop = FALSE]
        if (nrow(accessrow_) > 0) {
          is_admin <- ifelse(accessrow_$ADMIN == "TRUE", TRUE, FALSE)
        }
      }
      return(is_admin)
    },

    #' @description check if there is no user in the ADMIN panel
    #' Returns FALSE if there is one or more admins.
    #'
    #' @export
    no_admin = function() {
      flg_no_admin <- TRUE
      access_pin <- NULL
      df_list <- self$pin_list
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(self$pin_name, "_access_board")
      if (pin_name %in% df_list) {
        access_pin <- self$pin_board %>% pin_read(pin_name)
      }
      if (!is.null(access_pin)) {
        # Subset access data frame for ADMIN users
        access_pin <- access_pin[access_pin$"AccessPanel" == "ADMIN", ]
        access_pin_access_panel <- subset(access_pin, select = -c(AccessPanel))
        admin_access_list <- access_pin_access_panel$ADMIN
        if (TRUE %in% admin_access_list) {
          flg_no_admin <- FALSE
        }
      }
      return(flg_no_admin)
    },

    #' @description Gets user list filtered by given access unit
    #'
    #' @param access_panel Access panel name
    #' @param access_unit Access unit name
    #'
    #' @export
    get_userlist_unit = function(access_panel, access_unit) {
      # Fetch data from RS Connect pin board
      df_list <- self$pin_board %>% pin_list()
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(self$pin_name, "_access_board")
      # Check if pin board exists
      if (pin_name %in% df_list) {
        access_pin <- self$pin_board %>% pin_read(pin_name)
        # Subset pin data frame for ADMIN users
        access_pin <- access_pin[access_pin$"AccessPanel" %in% access_panel, ]
        access_pin_access_panel <-
          access_pin[, c("UserName", "UserID", access_unit)]
        userlistdf <-
          access_pin_access_panel[access_pin_access_panel[[access_unit]] %in%
                                  c(TRUE), ]
        uid <- userlistdf$UserID
        uname <- userlistdf$UserName
        user_list <- setNames(uid, paste(uid, uname, sep = " - "))
      } else {
        user_list <- NULL
      }
      return(user_list)
    },

    #' @description Function to inline js/css into the main app’s HTML
    #' @export
    rAccessThemes = function() {
      return(list(
        includeCSS(system.file("css/custom.css", package = "rAccess"))
      ))
    },

    #' @description Function to get list of access units for a given user.
    #' Note that it will contain access units that are accessible by `everyone`.
    #'
    #' @param user_id User ID
    #'
    #' @return A list
    #' @export
    get_user_accesslist = function(user_id = self$user) {
      acc_list <- list()
      # Fetch data from RS Connect pin board
      df_list <- self$pin_board %>% pin_list()
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(self$pin_name, "_access_board")
      if (pin_name %in% df_list) {
        access_pin <- self$pin_board %>% pin_read(pin_name)
        if (nrow(access_pin) > 0) {
          acc_dat <- access_pin %>%
            tidyr::pivot_longer(
              cols = setdiff(names(access_pin),
                             c("AccessPanel", "UserName", "UserID")),
              names_to = "item", values_to = "access"
            )
          acc_dat <- acc_dat %>%
            dplyr::filter(
              tolower(UserID) == tolower(user_id) |
                tolower(UserID) == "everyone",
              access == TRUE
            )
          if (nrow(acc_dat) > 0) {
            acc_list <- lapply(
              unique(acc_dat$AccessPanel),
              function(x) acc_dat$item[acc_dat$AccessPanel == x]
            )
            names(acc_list) <- unique(acc_dat$AccessPanel)
          }
        }
      }
      if (!self$no_admin() && length(acc_list) < 1 && self$secure_mode) {
        shinyalert::shinyalert(
          paste0(
            "You do not have access to this application.",
            " Please contact the admins for access request!"
          ),
          html = TRUE,
          timer = 13000,
          size = "l",
          type = "error",
          showConfirmButton = FALSE
        )
        Sys.sleep(10)
        stopApp()
      }
      return(acc_list)
    },

    #' @description Function to get app admins
    #'
    #' @return ADMIN user ids
    #' @export
    get_superAdmins = function() {
      # Fetch data from RS Connect pin board
      df_list <- self$pin_board %>% pin_list()
      # Remove user name in list of pins
      df_list <- gsub(".*[/]", "", df_list)
      pin_name <- paste0(self$pin_name, "_access_board")
      # Check if pin board exists
      if (pin_name %in% df_list) {
        access_pin <- self$pin_board %>% pin_read(pin_name)
        # Subset pin data frame for ADMIN users
        access_pin <- access_pin[access_pin$"AccessPanel" %in% "ADMIN", ]
        access_pin_access_panel <-
          access_pin[, c("UserName", "UserID", "ADMIN")]
        userlistdf <-
          access_pin_access_panel[access_pin_access_panel[["ADMIN"]] %in%
                                  c(TRUE), ]
        uid <- userlistdf$UserID
        uname <- userlistdf$UserName
        admin_list <- setNames(uid, paste(uid, uname, sep = " - "))
      } else {
        admin_list <- NULL
      }
      return(admin_list)
    }
  )
)

#' Function to add rAccess configuration file to the given directory
#'
#' Copies a configuration file (.yml) from the package's config directory to a
#'  specified path.
#'
#' @param file_name Config file name with .yml extension
#' @param path Directory to which config file is to be added
#'
#' @export
use_config <- function(file_name = "rAccess.yml", path = getwd()) {
  config_path <- system.file("config", package = "rAccess", mustWork = TRUE)
  if (file_name %in% list.files(config_path)) {
    file.copy(file.path(config_path, file_name),
      file.path(path, file_name),
      overwrite = TRUE
    )
  } else {
    message("Config file NOT AVAILABLE")
  }
}

#' Set environmental variables to connect to AWS S3 bucket
#'
#' @param access_key Character. AWS access key.
#' @param secret_key Character. AWS secret key.
#' @param region Character. AWS region. Defaults to `"us-east-1"`.
#'
#' @export
s3_config <- function(access_key, secret_key, region = "us-east-1") {
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = access_key,
    "AWS_SECRET_ACCESS_KEY" = secret_key,
    "AWS_DEFAULT_REGION" = region
  )
}

#' Gather pin_board elements
#'
#' @param pin_board pin board
#' @param pin_name pin name
#'
#' @importFrom pins pin_read pin_list pin_versions
#' @importFrom magrittr %>%
#'
#' @return A list with access_panels, access_units, access_df, access_list
#' @export
get_board <- function(pin_board, pin_name) {
  pin_name <- paste0(pin_name, "_access_board")
  access_df <- pin_read(board = pin_board, pin_name)
  key_cols <- c("AccessPanel", "UserName", "UserID")
  access_panels <- unique(access_df$AccessPanel)
  access_units <- setdiff(names(access_df), c(key_cols, access_panels))
  access_list <- list()
  if (pin_board$versioned) {
    pins_vn <- pin_board %>% pin_versions(pin_name)
    versns <- pins_vn$version
    access_list <- lapply(versns, function(x) {
      pin_read(pin_board, pin_name, version = x)
    })
    names(access_list) <- versns
  }
  out <- list(
    access_panels = access_panels,
    access_units = access_units,
    access_df = access_df,
    access_list = access_list
  )
  return(out)
}

#' Gets the list of admins
#'
#' @param pin_board Pin board
#' @param admin_panel Admin panel name
#' @param pin_name pin name
#'
#' @importFrom pins pin_read pin_list pin_versions
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#'
#' @export
get_admins <- function(pin_board, pin_name, admin_panel) {
  # Fetch data from RS Connect pin board
  df_list <- pin_board %>% pin_list()
  # Remove user name in list of pins
  df_list <- gsub(".*[/]", "", df_list)
  pin_name <- paste0(pin_name, "_access_board")
  # Check if pin board exists
  if (pin_name %in% df_list) {
    access_pin <- pin_board %>% pin_read(pin_name)
    # Subset pin data frame for ADMIN users
    access_pin <- access_pin[access_pin$"AccessPanel" %in% admin_panel, ]
    access_pin_access_panel <- access_pin[, c("UserName", "UserID", "ADMIN")]
    userlistdf <-
      access_pin_access_panel[access_pin_access_panel[["ADMIN"]] %in% c(TRUE), ]
    uid <- userlistdf$UserID
    uname <- userlistdf$UserName
    admin_list <- setNames(uid, paste(uid, uname, sep = " - "))
  } else {
    admin_list <- NULL
  }
  return(admin_list)
}

#' Get access list at a given date or in a specific time period
#'
#' @param pin_board Pin board
#' @param datemin Date in "YYYY-MM-DD" format
#' @param datemax Date, either NA or in "YYYY-MM-DD" format
#' @param pin_name pin name
#'
#' @importFrom pins pin_read pin_list pin_versions pin_fetch
#' @importFrom magrittr %>%
#'
#' @return A list of dataframes with access details
#' @export
get_accesslist <- function(pin_board, pin_name, datemin, datemax = NA) {
  stopifnot(!is.na(as.Date(datemin, "%Y-%m-%d")))

  # Fetch data from RS Connect pin board
  df_list <- pin_board %>% pin_list()
  # Remove user name in list of pins
  df_list <- gsub(".*[/]", "", df_list)
  pin_name <- paste0(pin_name, "_access_board")
  # Check if pin board exists
  if (pin_name %in% df_list) {
    version_list <- pin_board %>% pin_versions(pin_name)
    if (is.na(datemax)) {
      date_ <- as.Date(datemin, format = "%Y-%m-%d")
      version_dates <- as.Date(version_list$created, format = "%Y-%m-%d")
      date_nn <- version_dates[abs(version_dates - date_) ==
                                 min(abs(version_dates - date_))]
      versions <- version_list$version[as.Date(version_list$created,
                                               format = "%Y-%m-%d") %in%
                                         date_nn]
      names_ <- version_list$created[version_list$version %in% versions]
    } else {
      stopifnot(!is.na(as.Date(datemax, "%Y-%m-%d")))
      datemin <- as.Date(datemin, format = "%Y-%m-%d")
      datemax <- as.Date(datemax, format = "%Y-%m-%d")
      version_dates <- as.Date(version_list$created, format = "%Y-%m-%d")
      versions <-
        version_list$version[as.Date(version_list$created,
                                     format = "%Y-%m-%d") <= datemax &
                             as.Date(version_list$created,
                                     format = "%Y-%m-%d") >= datemin]
      names_ <- version_list$created[version_list$version %in% versions]
    }
    if (length(versions) > 0) {
      access_list <- lapply(versions, function(x) {
        pin_board %>% pin_read(pin_name, version = x)
      })
      names(access_list) <- names_
    } else {
      access_list <- NULL
    }
  } else {
    access_list <- NULL
  }
  return(access_list)
}

#' Get all access list till date
#'
#' @param pin_name Pin Name
#' @param pin_board Pin board
#'
#' @importFrom pins pin_read pin_list pin_versions pin_fetch
#' @importFrom magrittr %>%
#'
#' @export
get_accesshistory <- function(pin_board, pin_name) {
  # Fetch data from RS Connect pin board
  df_list <- pin_board %>% pin_list()
  # Remove user name in list of pins
  df_list <- gsub(".*[/]", "", df_list)
  pin_name <- paste0(pin_name, "_access_board")
  # Check if pin board exists
  if (pin_name %in% df_list) {
    version_list <- pin_board %>% pin_versions(pin_name)
    versions <- version_list$version
    names_ <- version_list$created
    if (length(versions) > 0) {
      access_list <- lapply(versions, function(x) {
        pin_board %>% pin_read(pin_name, version = x)
      })
      desc_ <- lapply(versions, function(x) {
        (pin_board %>% pin_fetch(pin_name, version = x))$description
      })
      names(access_list) <- paste(names_, desc_)
    } else {
      access_list <- NULL
    }
  } else {
    access_list <- NULL
  }
  return(access_list)
}

#' Get user data using API
#'
#' @param contact_info User entered search text
#' @param url URL
#' @param api_key Valid api key or NULL
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET add_headers
#' @importFrom tibble tibble
#'
#' @return A tibble with user id and username
#' @export
get_user_api <- function(contact_info, url, api_key = NULL) {
  if (is.null(url)) {
    return(NULL)
  }

  contact_info <- trimws(contact_info)
  url <- sprintf("https://%s/__api__/v1/users/remote", url)

  if (tolower(contact_info) != "everyone") {
    # Convert user entered text to lower case
    contact_info <- trimws(tolower(contact_info))
    auth <- ifelse(is.null(api_key),
      paste("Key", Sys.getenv("CONNECT_API_KEY")),
      api_key
    )

    user_info <- jsonlite::fromJSON(
      paste(
        httr::GET(url,
          query = list(prefix = contact_info),
          httr::add_headers(Authorization = auth)
        ),
        collapse = " "
      )
    )

    user_dat <- user_info$results
    if (!is.null(user_dat) && !is.null(nrow(user_dat))) {
      user_email <- unique(user_dat$email)[user_dat$email != ""]
      user_id <- unlist(lapply(strsplit(user_email, "@"), function(x) {
        x[1]
      }))
      user_id <- user_id[!is.na(user_id)]
      user_dat <- user_dat[tolower(user_dat$username) %in% tolower(user_id),
                           c("username", "first_name", "last_name")]
      if (nrow(user_dat) > 0) {
        return_vals <- tibble::tibble(
          userid = user_dat$username,
          username = paste(user_dat$first_name, user_dat$last_name)
        )
      } else {
        return_vals <- NULL
      }
    } else {
      return_vals <- NULL
    }
  } else {
    # Add Everyone to the list with id user_id of Everyone
    return_vals <- tibble::tibble(userid = "Everyone", username = "Everyone")
  }
  return(return_vals)
}

#' Helper function to get access units/panels with access for a user from an
#'  existing pin board
#'
#' @param user_id user id
#' @param pin_board pin board
#' @param pin_name pin name
#'
#' @importFrom pins pin_list
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#'
#'
#' @return A vector with access units
#' @export
get_granted_units <- function(user_id, pin_board, pin_name) {
  # Fetch data from RS Connect pin board
  df_list <- pin_board %>% pin_list()
  # Remove user name in list of pins
  df_list <- gsub(".*[/]", "", df_list)
  pin_name <- paste0(pin_name, "_access_board")

  # Check if pin board exists
  if (pin_name %in% df_list) {
    access_pin <- tibble::as_tibble(pin_board %>% pin_read(pin_name))
    if (nrow(access_pin) > 0) {
      access_pin_user <-
        access_pin[tolower(access_pin$UserID) == tolower(user_id), ]
      if (nrow(access_pin_user) > 0) {
        access_pin_user <-
          access_pin_user[, apply(access_pin_user, 2,
                                  function(x)  any(x %in% c(TRUE)))]
        user_panels <- names(access_pin_user)
      } else {
        user_panels <- NULL
      }
      ret_panels <- unique(user_panels)
    } else {
      ret_panels <- NULL
    }
  } else {
    ret_panels <- NULL
  }
  return(ret_panels)
}

#' Function to create rconnect pinboard
#'
#' @param server rconnect server
#' @param key  API KEY to connect with rconnect servers
#'
#' @importFrom pins board_connect pin_list pin_read
#'
#' @return a pin_board
#' @export
rconnect_pin_board <- function(server, key) {
  pin_board <- pins::board_connect(
    auth = "rsconnect",
    server = server,
    key = key
  )
  return(pin_board)
}

#' Function to create s3 pin_board
#'
#' @param s3_bucket s3 bucket name
#' @param s3_access_key access key to connect to s3 bucket
#' @param s3_secret_key secret key to connect to s3 bucket
#' @param s3_region s3 bucket region
#' @param s3_prefix Prefix to
#'
#' @importFrom pins board_s3
#'
#' @return s3 pin board
#' @export
s3_pinboard <- function(s3_bucket, s3_access_key, s3_secret_key,
                        s3_region = "us-east-1", s3_prefix) {
  # configure s3 bucket
  s3_config(
    access_key = s3_access_key,
    secret_key = s3_secret_key,
    region = s3_region
  )
  pin_board_s3 <- pins::board_s3(
    bucket = s3_bucket,
    prefix = s3_prefix,
    access_key = s3_access_key,
    secret_access_key = s3_access_key
  )
  return(pin_board_s3)
}
