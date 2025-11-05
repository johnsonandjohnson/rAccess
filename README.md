# rAccess <img src="man/figures/raccess.png" align="right" height="200" style="float:right; height:100px;"/>

<!-- badges: start -->

[![Test
coverage](https://raw.githubusercontent.com/johnsonandjohnson/rAccess/coverage/badges/coverage.svg)](https://github.com/johnsonandjohnson/rAccess/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/johnsonandjohnson/rAccess/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johnsonandjohnson/rAccess/actions/workflows/R-CMD-check.yaml)
[![CRAN
Version](https://www.r-pkg.org/badges/version/rAccess?color=green)](https://cran.r-project.org/package=rAccess)

<!-- badges: end -->

`rAccess` is an R package that offers a flexible framework for in-app
access control, allowing local and/or remote storage, organization, and
retrieval of access lists. It features a pluggable shiny module to
create and manage access lists for individual Shiny applications. It is
built on top of the Posit Connect Access Management, which means no
credentials are collected or stored.

A friendly user interface enables the app Admin to easily manage user
access permission for respective access units.

The parameters to the `rAccess` object can either be passed directly as
arguments to the new instance of `rAccess` or could be defined within a
configuration yaml file.

## Core Concepts

**Access Panels** serve as organizational containers that group related *Access 
Units* together, enabling you to implement hierarchical access control 
structures. In the rAccess user interface, each *Access Panel* appears as a 
separate tab where administrators can navigate to configure permissions. This 
tabbed structure allows admins to logically organize access controls by 
functional areas - for example, having separate panels for "Data Visualization",
 "Report Generation", and "Administrative Functions". Each panel can contain
  multiple related units that share similar access requirements or belong to 
the same feature set.

**Access Units** represent the most granular level of access control in your 
application - these are the specific elements that users can be granted or denied 
access to. They could be individual shiny modules, specific UI components (like 
download buttons or input panels), particular data views, or any discrete 
functionality within your application. In the rAccess user interface, *Access 
Units* appear as individual toggles or dropdown selections within their 
respective *Access Panel* tabs, allowing administrators to precisely control which
 users can access each specific feature or component of the application.

## Installation

```         
#install.packages("pak")
pak::pak("johnsonandjohnson/rAccess")
```

## Usage

`rAccess` includes server and ui modules for access management tab which
could be used within a main Shiny web application.

```         
library(rAccess)
```

## Adding rAccess config file

The package includes a template configuration file, making it simple for
users to get started. It contains all the necessary parameters for
rAccess and can be customized to suit individual needs.

To add a config file template to your project directory, use:

```         
rAccess::use_config(file_name = "rAccess.yml")
```

## Config file structure

```         
module: rAccess
parameters:
  app_name:                 # application name *
  board_type:               # local/rconnect/s3 *   
  access_mode:              # default/single unit
  unit_display:             # dropdown/switch
  switch_size:              # default/mini/small/normal/large
  user_df: !expr tibble::tribble(          
    ~userid, ~username,
    "UserID", "User Name")        # Sample user_df
  use_rconnect_users: TRUE        # TRUE - combine rconnect userlist with user_df
  secure_mode: FALSE              # TRUE - Access will be denied to users with no access to atleast one panel/unit
  local_board_path:               # Path to save local pin_board when board_type = "local"    
  s3_bucket:                      # s3 bucket name when board_type = "s3"      
  s3_access_key:                  # s3 access key when board_type = "s3" 
  s3_secret_key:                  # s3 secret key when board_type = "s3" 
  verbose: FALSE                  # If TRUE prints all updates in the log

data:                             # Study data path if available  
  datapath1:     
  datapath2: 

panel_str:                        # Panel structure to be defined by the developer
  - access_panel: ADMIN           # Every App should have an ADMIN panel.(mandatory)

  - access_panel:                 # Access Panel name *
    access_units:
      - unit:                     # Access unit name  *
      - unit:                     # Access unit name

  - access_panel:                 # Access Panel name 
    access_units:
      - unit:
        data:                     # datapath associated with access unit  
      - unit:                     
        data:                     # datapath associated with access unit
```

**For detailed description on the config file components refer vignette
: Tutorial.**

### Creating a new instance of rAccess

Once the configuration file is ready the user can create a new instance
of rAccess as below:

```         
newIAM <- rAccess$new(user = "UserID", config = "rAccess.yml")
```

### Creating a new instance of rAccess without a config file

If there is no config file in place, user can also pass the rAccess
parameters as arguments to the new instance of the `rAccess` object.

```         
access_panels <- list(
  `ADMIN` = NULL,
  `Access Panel 1` = c("Unit 1", "Unit 2"),
  `Access Panel 2` = c("Unit 3", "Unit 4"),
  `Access Panel 3` = c("Unit 5", "Unit 6")
)

# User list
user_df <- tibble::tribble(          
  ~userid, ~username,
  "UserID1", "User Name 1",
  "UserID2", "User Name 2")

newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "local",
                      local_board_path = "./data/",
                      pin_name = pin_name,
                      access_panels = access_panels,
                      access_mode = "default",
                      user_df = user_df)

newIAM$access_panels
newIAM$access_units
newIAM$access_mode
```

### board_type options in rAccess

There three pin board options available for the users: "local", "s3",
and "rconnect". This can be specified with the parameter `board_type` of
the rAccess object.

-   `"local"` : Local folder will be used as a pin_board. The user must
    also specify the "local_board_path".

```         
newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "local",
                      local_board_path = "./data/",
                      pin_name = pin_name,
                      access_panels = access_panels,
                      access_mode = "default",
                      user_df = user_df)
```

-   `"s3"` : S3 bucket will be used as a pin_board. When board_type is
    "s3", the user must give the s3 credentials.

```         
newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "s3",
                      s3_bucket: ""
                      s3_access_key: ""
                      s3_secret_key: "",
                      access_panels = access_panels,
                      access_mode = "default",
                      user_df = user_df)
```

-   `"rconnect"` : Posit Connect pin_board will be utilized. If the
    pin_board does not already exist, it will be created and deployed on
    the same Posit Connect server where the app is hosted.

```         
newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "rconnect",
                      access_panels = access_panels,
                      access_mode = "default",
                      user_df = user_df)
```

### Creating user list

The user list for rAccess could either be supplied as the `user_df`
argument of the `rAccess` object or could be fetched from the rconnect
user list.

**Creating user_df**\*

```         
user_df <- tibble::tribble(
  ~userid, ~username,
  "UserId1", "User Name 1",
  "UserId2", "User Name 2",
  "UserId3", "User Name 3",
  "UserId4", "User Name 4",
  "UserId5", "User Name 5"
)

newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "local",
                      local_board_path = "./data/",
                      access_panels = access_panels,
                      access_mode = "default",
                      user_df = user_df)
```

**Using API to fetch user data** If you have access to your
organization’s user directory via an API, you might want to first fetch
the data and then prepare the user_df, similar to the example below

```         
api_url <- "<user-directory-api>"
users <- jsonlite::fromJSON(api_url)
user_df <- tibble::tibble(userid = users$USERID, username = users$USERNAME)

newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "local",
                      local_board_path = "./data/",
                      access_panels = access_panels,
                      access_mode = "default",
                      user_df = user_df)
```

**Using User list from rconnect**

User list will be automatically fetched from the Posit Connect servers
when deployed. Users must make sure that `use_rconnect_users` parameter
is set as `TRUE` to get users from Posit Connect.

```         
# When deployed
newIAM <- rAccess$new(user = "<userid>",
                      app_name = "testApp",
                      board_type = "rconnect",
                      access_panels = access_panels,
                      access_mode = "default",
                      use_rconnect_users = TRUE)
```

## Example

```         
library(DT)
library(pins)
library(shiny)
library(rAccess)

ui <- navbarPage(
  id = "mainpage",
  title = "Demo!",
  tabPanel(
    "Plot",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "plotType", "Plot type",
          c("Scatter" = "p", "Line" = "l")
        )
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  ),
  tabPanel(
    "Summary",
    verbatimTextOutput("summary")
  ),
  tabPanel(
    "Access Control",
    rAccess::module_iam_ui("iam")
  )
)

server <- function(input, output, session) {
  access_panels <- list(
    `ADMIN` = NULL,
    `View` = c("view-plots", "view-summary")
  )
  user_df <- data.frame(
    userid = "UserID",
    username = "User Name"
  )
  user_id <- ifelse(!exists("session$user"), "UserID", session$user)
  
  # Create new instance of rAccess
  newIAM <- rAccess$new( user = user_id,
                         board_type = "local",
                         local_board_path = "./data/",
                         app_name = "test2",
                         access_panels = access_panels,
                         access_mode = "default",
                         switch_size = "small",
                         unit_display = "dropdown",
                         user_df = user_df,
                         secure_mode = FALSE
  )
  
  if (newIAM$no_admin() || newIAM$is_admin()) {
    showTab("mainpage", target = "Access Control")
  } else {
    hideTab("mainpage", target = "Access Control")
  }
  
  module_iam_server("iam", newIAM)
  
  # Get panels with access
  user_access_list <- newIAM$get_user_accesslist()
  
  # Show/Hide: plot tab
  if (!"view-plots" %in% user_access_list$View) {
    hideTab("mainpage", target = "Plot")
  } else {
    showTab("mainpage", target = "Plot")
  }
  
  # Show/Hide: Summary Tab
  if (!"view-summary" %in% user_access_list$View) {
    hideTab("mainpage", target = "Summary")
  } else {
    showTab("mainpage", target = "Summary")
  }
  
  output$plot <- renderPlot({
    plot(cars, type = input$plotType)
  })
  output$summary <- renderPrint({
    summary(cars)
  })
}

shinyApp(ui, server)
```

![App recording](man/figures/readme_rAccess.gif)
