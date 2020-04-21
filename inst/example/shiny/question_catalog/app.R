library(riddlr)
library(shiny)

deps <- c("shinythemes", "shinydashboard", "shinycssloaders", "shinyAce",
    "DT", "markdown", "dplyr", "tibble", "purrr", "tidyr")

loaded <- vapply(deps, FUN.VALUE = logical(1L), function(dep) tryCatch(
  require(dep, character.only = TRUE, quietly = TRUE, ),
  error = function(e) FALSE,
  warning = function(e) FALSE))

if (!all(loaded))
  stop(call. = FALSE, paste0(
    "This demo app requires additional packages: ",
    paste0("'", deps[!loaded], "'", collapse = ", ")))

# ensure interactive console width won't affect output
options(width = 80)

catalog <- system.file("example", "questions", package = "riddlr") %>%
  parse_riddlr_dir_headers() %>%
  lapply(new_tibble, nrow = 1) %>%
  enframe() %>%
  mutate(value = map(value, . %>% mutate(tags = paste(tags, collapse = "; ")))) %>%
  unnest(value) %>%
  select(difficulty, title, details, tags, filepath)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "riddlr"),
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    uiOutput("userpanel"), # The dynamically-generated user panel
    uiOutput('sidebar_menu')),
  dashboardBody(
    riddlr_css(),
    tabItems(
      tabItem("welcome", fluidPage(
        tags$h1("riddlr"),
        tags$a(href="https://github.com/dgkf/riddlr",
          tags$img(src = "riddlr/hex-riddlr.png", align = "right", style = "margin: 2em; margin-top: -2em;")),
        includeMarkdown(system.file(
          "example", "shiny", "question_catalog", "welcome.md",
          package = "riddlr"))
      )),
      tabItem("catalog", fluidPage(
        tags$h2("Riddle Catalog"),
        tags$p(
          "Explore a list of available questions. Click on the   ",
          tags$span(icon("terminal"), style = "margin: 0.5em"),
          "   to launch a riddle."),
        dataTableOutput("riddle_catalog")
      )),
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
        as.character(actionButton(
          inputId = paste0("riddle_start_btn_", .),
          label = NULL,
          icon = icon("terminal")))
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
        style = "bootstrap",
        escape = FALSE,
        colnames = c(" " = "button"),
        rownames = FALSE,
        autoHideNavigation = TRUE,
        selection = "single")
  })

  updateTabItems(session, "tabs", "welcome")
  output$sidebar_menu <- renderUI({
    welcome <- menuItem("Welcome", tabName = "welcome", icon = icon("play"))
    catalog <- menuItem("Catalog", tabName = "catalog", icon = icon("list"))
    riddle <- menuItem("Riddle", tabName = "riddle", icon = icon("question-circle"))

    items <- c(
      list(
        welcome = welcome,
        catalog = catalog),
      if (!is.null(riddle_num())) list(riddle = riddle))

    updateTabItems(session, "tabs", selected = "riddle")
    do.call(sidebarMenu, append(
      list(id = "tabs"),
      Filter(Negate(is.null), unname(items))))
  })

  riddle_num <- reactiveVal()
  observeEvent(input$riddle_catalog_rows_selected, ignoreNULL = TRUE, {
    updateTabItems(session, "tabs", selected = "riddle")
    riddle_num(input$riddle_catalog_row_last_clicked)
    selectRows(dataTableProxy("riddle_catalog"), c())
  })

  riddle_spec <- eventReactive(riddle_num(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    parse_riddlr_rmd(catalog_html()$filepath[[riddle_num()]])
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
