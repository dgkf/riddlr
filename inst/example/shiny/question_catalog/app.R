library(riddlr)
library(shiny)

deps <- c(
  "shinythemes",
  "shinydashboard",
  "shinycssloaders",
  "shinyAce",
  "DT",
  "dplyr",
  "tibble",
  "purrr",
  "tidyr"
)

loaded <- vapply(deps, FUN.VALUE = logical(1L), function(dep) tryCatch(
  require(dep, character.only = TRUE, quietly = TRUE, ),
  error = function(e) FALSE,
  warning = function(e) FALSE
))

if (!all(loaded))
  stop(call. = FALSE, paste0(
    "This demo app requires additional packages: ",
    paste0("'", deps[!loaded], "'", collapse = ", ")
  ))

# ensure interactive console width won't affect output
options(width = 80)

catalog <- system.file("example", "questions", package = "riddlr") %>%
  parse_riddlr_dir_headers() %>%
  lapply(new_tibble, nrow = 1) %>%
  enframe() %>%
  mutate(value = map(value, . %>% mutate(tags = paste(tags, collapse = "; ")))) %>%
  unnest("value") %>%
  select(difficulty, title, details, tags, filepath)

ui <- dashboardPage(
  dashboardHeader(title = "riddlr"),
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    uiOutput("userpanel"), # The dynamically-generated user panel
    uiOutput('sidebar_menu')),
  dashboardBody(
    tabItems(
      tabItem("catalog", fluidPage(dataTableOutput("riddle_catalog"))),
      tabItem("riddle", fluidPage(withSpinner(uiOutput("riddle_tab"))))
    )))

server <- function(input, output, session) {
  catalog_html <- reactive({
    catalog %>%
      # convert difficulty to dot scale
      mutate(difficulty = pmax((difficulty * 5) %% 5, 0.5)) %>%
      mutate(difficulty = map_chr(difficulty, function(n) {
        do.call(tagList, if_else(
          n >= 1:5,       list(icon("circle")), if_else(
          n >= 1:5 - 0.5, list(icon("adjust")),
          list(shiny::tags$i(class = 'far fa-circle'))
          ))) %>%
        span(style = "white-space: nowrap;") %>%
        as.character()
      })) %>%

      # add buttons
      mutate(button = map_chr(row_number(), ~{
        as.character(modifyCssClasses(actionButton(
          inputId = paste0("riddle_start_btn_", .),
          label = icon("terminal"),
          width = "100%",
          class = "btn btn-primary",
          riddlr_rownum = .,
          onclick = 'Shiny.onInputChange("selected_riddle_rownum",
            Number($("#" + this.id).first().attr("riddlr_rownum")))'
        ), -btn-default, btn-primary))
      })) %>%
      select(button, everything()) %>%

      # add tags
      mutate(tags = map_chr(tags, ~paste(
        map(trimws(strsplit(., ";")[[1]]), ~
          as.character(shiny::tags$span(., class = "badge badge-primary"))),
        collapse = ""))) %>%

      # bold riddle titles
      mutate(title = map_chr(title, ~as.character(shiny::tags$strong(.)))) %>%

      # reorder columns
      select(button, title, details, difficulty, tags, everything())
  })

  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span(icon("user-circle"), session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })

  output$riddle_catalog <- renderDataTable({
    catalog_html() %>%
      select(-filepath) %>%
      datatable(
        escape = FALSE,
        colnames = c(" " = "button"),
        rownames = FALSE,
        autoHideNavigation = TRUE,
        selection = "none")
  })

  updateTabItems(session, "tabs", "catalog")
  output$sidebar_menu <- renderUI({
    catalog <- menuItem("Catalog", tabName = "catalog", icon = icon("list"))
    riddle <- menuItem("Riddle", tabName = "riddle", icon = icon("question-circle"))

    items <- c(
      list(catalog = catalog),
      if (!is.null(input$selected_riddle_rownum)) list(riddle = riddle))

    updateTabItems(session, "tabs", selected = "riddle")
    do.call(sidebarMenu, append(
      list(id = "tabs"),
      Filter(Negate(is.null), unname(items))))
  })

  riddle_spec <- eventReactive(input$selected_riddle_rownum, {
    updateTabItems(session, "tabs", selected = "riddle")
    r_filepath <- catalog_html()$filepath[[input$selected_riddle_rownum]]
    parse_riddlr_rmd(r_filepath)
  })

  output$riddle_tab <- renderUI({
    r <- riddle_spec()
    riddle_ui("riddle", question_ui = r$prompt, metadata = r$metadata)
  })

  observe({
    r <- riddle_spec()
    callModule(riddle, "riddle",
      solution = r$grader$solution,
      quoted = TRUE,
      test_inputs = r$grader$test_inputs,
      test_timeouts = r$grader$test_timeouts,
      clear = riddle_spec) # clear output reactively upon update
  })
}

shinyApp(ui, server)
