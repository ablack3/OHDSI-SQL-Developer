library(shiny)
library(shinydashboard)
library(SqlRender)

menuItemFileInput <- function(inputId, text, icon = shiny::icon("file-text-o")) {
  script <- "document.getElementById('%id%').click(); return false;"
  script <- gsub("%id%", inputId, script)
  list(div(fileInput(inputId, ""), style = "display: none;"),
       tags$li(class = "treeview", a(href = "#", onclick = script, icon, text)))
}

menuItemDownloadLink <- function(inputId, label, icon = shiny::icon("floppy-o")) {
  tags$li(class = "treeview",
          tags$a(id = inputId,
                 class = "shiny-download-link",
                 href = "",
                 target = "_blank",
                 download = NA,
                 icon,
                 label))
}

menuItemCopyTextAreaToClipboard <- function(textAreaId, label, icon = shiny::icon("clipboard")) {
  script <- "
  element = $('<textarea>').appendTo('body').val(document.getElementById('%id%').value).select();
  document.execCommand('copy');
  element.remove();
  return false;
  "
  script <- gsub("%id%", textAreaId, script)
  tags$li(class = "treeview", a(href = "#", onclick = script, icon, label))
}

menuItemCopyDivToClipboard <- function(divId, label, icon = shiny::icon("clipboard")) {
  script <- "
  element = $('<textarea>').appendTo('body').val(document.getElementById('%id%').textContent).select();
  document.execCommand('copy');
  element.remove();
  return false;
  "
  script <- gsub("%id%", divId, script)
  tags$li(class = "treeview", a(href = "#", onclick = script, icon, label))
}

ui <- dashboardPage(
  dashboardHeader(title = "SqlRender Developer"),
  dashboardSidebar(
    sidebarMenu(
      menuItemFileInput("open", "Open file", icon = shiny::icon("folder-open")),
      menuItemDownloadLink("save", "Save", icon = shiny::icon("save")),
      menuItem("Open new tab", href = "", icon = shiny::icon("plus-square")),
      menuItemCopyTextAreaToClipboard("source", "Copy source to clipboard"),
      menuItemCopyDivToClipboard("target", "Copy target to clipboard")
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 9, 
             box(
               title = "Source: OHDSI SQL", width = NULL, status = "primary",
               textAreaInput("source", NULL, width = "100%", height = "300px")
             ), 
             box(
               title = "Target: Rendered translation", width = NULL,
               # tags$table(width = "100%",
               #            tags$tr(
               #              tags$td(align = "left", actionButton("renderTranslate", "Render and translate")),
               #              tags$td(align = "right", checkboxInput("continuous", "Auto render and translate")))),
               pre(textOutput("target"))
             )
      ),
      column(width = 3,
             box(background = "light-blue",
                 h4("Target dialect"), width = NULL,
                 selectInput("dialect", NULL, choices = c("BigQuery", "Impala", "Netezza", "Oracle", "PDW", "PostgreSQL", "RedShift", "SQL Server", "SQLite", "Hive", "Spark", "Snowflake", "Synapse"), selected = "SQL Server"),
                 h4("Temp emulation schema"),
                 textInput("tempEmulationSchema", NULL),
                 h4("Parameters"),
                 uiOutput("parameterInputs"),
                 textOutput("warnings")
             )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # cache <- reactiveValues(target = '', clicks = 0, parameters = NULL)
  
  parameters <- reactive({
    params <- regmatches(input$source, gregexpr("@[a-zA-Z0-9_]+", input$source))[[1]]
    params <- unique(params)
    params <- params[order(params)]
    params <- substr(params, 2, nchar(params))
    return(params)
  })
  
  output$target <- renderText({
    parameterValues <- list()
    for (param in parameters()) {
      value <- input[[param]]
      if (!is.null(value)) {
        parameterValues[[param]] <- value
      }
    }
    sql <- do.call("render", append(input$source, parameterValues))
    warningString <- c()
    handleWarning <- function(e) {
      output$warnings <- e$message
    }
    tempEmulationSchema <- input$tempEmulationSchema
    if (tempEmulationSchema == "")
      tempEmulationSchema <- NULL
    sql <- withCallingHandlers(suppressWarnings(translate(sql,
                                                          targetDialect = tolower(input$dialect),
                                                          tempEmulationSchema = tempEmulationSchema)), warning = handleWarning)
    if (!is.null(warningString))
      output$warnings <- warningString
    return(sql)
  })
  
  output$parameterInputs <- renderUI({
    params <- parameters()
    sourceSql <- input$source
    
    createRow <- function(param, sourceSql) {
      # Get current values if already exists:
      value <- isolate(input[[param]])
      
      if (is.null(value)) {
        # Get default values:
        value <- regmatches(sourceSql,
                            regexpr(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=[^}]+}"), sourceSql))
        if (length(value) == 1) {
          value <- sub(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=\\s*"), "", sub("\\}$", "", value))
        } else {
          value <- ""
        }
      }
      textInput(param, param, value = value)
    }
    lapply(params, createRow, sourceSql = sourceSql)
  })
  
  observeEvent(input$open, {
    sql <- SqlRender::readSql(input$open$datapath)
    updateTextAreaInput(session, "source", value = sql)
  })
  
  output$save <- downloadHandler(filename = function() {
    paste("query-", Sys.Date(), ".sql", sep = "")
  }, content = function(con) {
    SqlRender::writeSql(sql = input$source, targetFile = con)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
