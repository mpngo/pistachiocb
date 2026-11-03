# This helper sources all model files from the R/ directory so the Shiny app
# can call the same analysis functions used by the package code.
source_model_files <- function(r_dir) {
  model_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  for (file in sort(model_files)) {
    source(file, local = FALSE)
  }
}

# Load the carbon budget model functions into the app session.
source_model_files(file.path(getwd(), "R"))

# Load the packages used by both the model code and the Shiny interface.
library(dplyr)
library(readr)
library(bigleaf)
library(tibble)
library(rlang)
library(magrittr)
library(shiny)
library(plotly)

# Format large numeric outputs for summary tables in the UI.
fmt_num <- function(x) {
  format(round(x, 2), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Format Date columns for table display so Shiny does not print internal date numbers.
format_table_dates <- function(df) {
  if ("Date" %in% names(df)) {
    df$Date <- format(as.Date(df$Date), "%Y-%m-%d")
  }
  df
}

# Convert carbon from micromoles to milligrams using the molar mass of carbon.
umol_to_mg_c <- function(x) {
  x * 12e-3
}

# Format the final carbon budget for display without changing the underlying model output.
format_budget_display <- function(df, unit = "umol") {
  out <- df
  if (identical(unit, "mg")) {
    out$CB_display <- umol_to_mg_c(out$CB_umol_day)
    names(out)[names(out) == "CB_display"] <- "CB_mg_day"
  } else {
    out$CB_display <- out$CB_umol_day
    names(out)[names(out) == "CB_display"] <- "CB_umol_day_display"
  }
  out
}

# Build the user interface: sidebar inputs on the left and tabbed outputs on the right.
ui <- fluidPage(
  titlePanel("Pistachio Branch Carbon Budget Model"),
  sidebarLayout(
    sidebarPanel(
      # File upload and model inputs let the user define one carbon budget run.
      fileInput(
        "cimis_file",
        "CIMIS hourly CSV",
        accept = c(".csv")
      ),
      numericInput("leaf_num", "Leaf count", value = 7, min = 1, step = 100),
      numericInput("fruit_num", "Fruit count", value = 56, min = 0, step = 50),
      selectInput(
        "cb_unit",
        "Final carbon budget display unit",
        choices = c("Micromoles C/day" = "umol", "Milligrams C/day" = "mg"),
        selected = "umol"
      ),
      actionButton("run_model", "Run model", class = "btn-primary"),
      tags$hr(),
      tags$p(
        "Expected columns include Date, Hour (PST), Sol Rad (Ly/day), and Air Temp (F)."
      ),
      downloadButton("download_budget", "Download budget CSV")
    ),
    mainPanel(
      # Tabs separate the high-level budget view from raw tables and inputs.
      tags$p(
        "Upload a CIMIS weather file, set tree counts, and inspect the daily carbon budget and its component curves."
      ),
      verbatimTextOutput("status_text"),
      tabsetPanel(
        tabPanel(
          "How the model works",
          tags$h3("What the pistachio carbon budget model calculates"),
          tags$p(
            "This application estimates the daily carbon balance of a pistachio system by combining carbon supply from leaf assimilation with carbon use by respiration and growth."
          ),
          tags$h4("Model workflow"),
          tags$ol(
            tags$li(
              "Weather import: the model reads hourly CIMIS weather data and converts solar radiation and air temperature into units used by the physiological calculations."
            ),
            tags$li(
              "Light absorption and thermal time: incoming radiation is scaled by hourly canopy absorption fractions, then daily growing degree days are calculated and accumulated across the season."
            ),
            tags$li(
              "Leaf assimilation: absorbed light is combined with fitted Marshall-Biscoe photosynthesis parameters to estimate daily leaf carbon assimilation."
            ),
            tags$li(
              "Leaf area scaling: assimilation is first estimated per unit leaf area and then scaled to the whole tree using the modeled leaf area curve and the user-provided leaf count."
            ),
            tags$li(
              "Respiration demand: vegetative and reproductive respiration are calculated from hourly temperature and stage-specific respiration parameters, then scaled by modeled fresh weight."
            ),
            tags$li(
              "Growth demand: leaf and fruit dry-weight curves are converted to carbon, and daily growth demand is estimated from the day-to-day increase in structural carbon."
            ),
            tags$li(
              "Carbon budget assembly: the final daily balance is calculated as assimilation minus respiration and growth terms."
            )
          ),
          tags$h4("Main outputs"),
          tags$ul(
            tags$li("A_umol_day: daily assimilated carbon supply."),
            tags$li("R_leaves_umol_day: daily leaf respiration demand."),
            tags$li("R_fruits_umol_day: daily fruit respiration demand."),
            tags$li("G_leaves_umol_day: daily carbon required for leaf growth."),
            tags$li("G_fruits_umol_day: daily carbon required for fruit growth."),
            tags$li("CB_umol_day: net daily carbon budget after subtracting modeled demands.")
          ),
          tags$h4("Interpretation"),
          tags$p(
            "Positive carbon budget values indicate days when modeled carbon supply exceeds the modeled respiration and growth demands included in the framework. Negative values indicate days when demand is greater than supply."
          ),
          tags$p(
            "The current implementation focuses on leaves and fruit. Shoot or other storage terms are not explicitly included in the final daily budget shown here."
          )
        ),
        tabPanel(
          "Budget overview",
          div(style = "padding-top: 12px;", plotlyOutput("budget_plot", height = "420px")),
          tableOutput("summary_table")
        ),
        tabPanel(
          "Components",
          div(style = "padding-top: 12px;", plotlyOutput("component_plot", height = "420px")),
          div(
            style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            tableOutput("component_table")
          )
        ),
        tabPanel(
          "Daily results",
          div(
            style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            tableOutput("budget_table")
          )
        ),
        tabPanel(
          "Hourly inputs",
          div(
            style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            tableOutput("hourly_table")
          )
        )
      )
    )
  )
)

# Server logic controls when the model runs and how each output is rendered.
server <- function(input, output, session) {
  # Run the model only when the user clicks the button, not on every input change.
  model_run <- eventReactive(input$run_model, {
    req(input$cimis_file)

    validate(
      need(input$leaf_num > 0, "Leaf count must be greater than zero."),
      need(input$fruit_num >= 0, "Fruit count cannot be negative.")
    )

    pistachio_budget_components_from_csv(
      cimis_csv_path = input$cimis_file$datapath,
      leaf_num = input$leaf_num,
      fruit_num = input$fruit_num,
      A_is_net = FALSE,
      clamp_growth = TRUE
    )
  })

  # Show a compact status message describing the current model result.
  output$status_text <- renderText({
    req(model_run())
    budget <- model_run()$budget
    paste(
      "Loaded", nrow(budget), "daily records from",
      format(min(budget$Date), "%Y-%m-%d"), "to",
      format(max(budget$Date), "%Y-%m-%d"), "\n",
      "Final budget includes the separate leaf respiration submodel."
    )
  })

  # Summarize the main carbon budget totals across the uploaded season.
  output$summary_table <- renderTable({
    req(model_run())
    budget <- model_run()$budget
    cb_total <- sum(budget$CB_umol_day, na.rm = TRUE)
    cb_label <- if (identical(input$cb_unit, "mg")) {
      "Net carbon budget (mg C)"
    } else {
      "Net carbon budget (umol)"
    }
    cb_value <- if (identical(input$cb_unit, "mg")) {
      fmt_num(umol_to_mg_c(cb_total))
    } else {
      fmt_num(cb_total)
    }

    data.frame(
      Metric = c(
        "Total assimilation (umol)",
        "Total leaf respiration (umol)",
        "Total fruit respiration (umol)",
        "Total leaf growth demand (umol)",
        "Total fruit growth demand (umol)",
        cb_label
      ),
      Value = c(
        fmt_num(sum(budget$A_umol_day, na.rm = TRUE)),
        fmt_num(sum(budget$R_leaves_umol_day, na.rm = TRUE)),
        fmt_num(sum(budget$R_fruits_umol_day, na.rm = TRUE)),
        fmt_num(sum(budget$G_leaves_umol_day, na.rm = TRUE)),
        fmt_num(sum(budget$G_fruits_umol_day, na.rm = TRUE)),
        cb_value
      ),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "m")

  # Preview the main daily component columns that feed the final budget.
  output$component_table <- renderTable({
    req(model_run())
    format_table_dates(model_run()$budget[, c(
      "Date", "Cum_GDD_base7", "A_umol_day", "R_leaves_umol_day",
      "R_fruits_umol_day", "G_leaves_umol_day", "G_fruits_umol_day"
    )])
  }, striped = TRUE, spacing = "xs")

  # Show the full daily budget table for inspection or quick copy/paste.
  output$budget_table <- renderTable({
    req(model_run())
    budget_display <- format_budget_display(model_run()$budget, unit = input$cb_unit)
    format_table_dates(budget_display)
  }, striped = TRUE, spacing = "xs")

  # Show the first day of hourly inputs used by the assimilation and respiration models.
  output$hourly_table <- renderTable({
    req(model_run())
    format_table_dates(model_run()$hourly)
  }, striped = TRUE, spacing = "xs")

  # Plot daily assimilation against the final carbon budget to show seasonal surplus or deficit.
  output$budget_plot <- renderPlotly({
    req(model_run())
    budget <- model_run()$budget
    hover_dates <- format(budget$Date, "%Y-%m-%d")
    use_mg <- identical(input$cb_unit, "mg")
    assimilation_values <- if (use_mg) umol_to_mg_c(budget$A_umol_day) else budget$A_umol_day
    cb_values <- if (use_mg) umol_to_mg_c(budget$CB_umol_day) else budget$CB_umol_day
    y_axis_title <- if (use_mg) "mg C day-1" else "umol C day-1"
    assimilation_label <- if (use_mg) "Assimilation: " else "Assimilation: "
    cb_label <- if (use_mg) "Carbon budget: " else "Carbon budget: "
    unit_suffix <- if (use_mg) " mg C day-1" else " umol C day-1"

    plot_ly(budget, x = ~Date, hovermode = "x unified") %>%
      add_lines(
        y = assimilation_values,
        name = "Assimilation",
        line = list(color = "#1b9e77", width = 3),
        text = paste0(
          "Date: ", hover_dates,
          "<br>", assimilation_label, fmt_num(assimilation_values), unit_suffix
        ),
        hoverinfo = "text"
      ) %>%
      add_lines(
        y = cb_values,
        name = "Carbon budget",
        line = list(color = "#d95f02", width = 3),
        text = paste0(
          "Date: ", hover_dates,
          "<br>", cb_label, fmt_num(cb_values), unit_suffix
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Daily assimilation and carbon budget",
        margin = list(t = 70),
        xaxis = list(title = "Date"),
        yaxis = list(title = y_axis_title, zeroline = TRUE),
        hoverlabel = list(align = "left")
      )
  })

  # Plot the individual sources and sinks that make up the daily carbon budget.
  output$component_plot <- renderPlotly({
    req(model_run())
    budget <- model_run()$budget
    hover_dates <- format(budget$Date, "%Y-%m-%d")

    plot_ly(budget, x = ~Date, hovermode = "x unified") %>%
      add_lines(
        y = ~A_umol_day,
        name = "Assimilation",
        line = list(color = "#1b9e77", width = 2),
        text = ~paste0(
          "Date: ", hover_dates,
          "<br>Assimilation: ", fmt_num(A_umol_day), " umol C day-1"
        ),
        hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~R_leaves_umol_day,
        name = "Leaf respiration",
        line = list(color = "#7570b3", width = 2),
        text = ~paste0(
          "Date: ", hover_dates,
          "<br>Leaf respiration: ", fmt_num(R_leaves_umol_day), " umol C day-1"
        ),
        hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~R_fruits_umol_day,
        name = "Fruit respiration",
        line = list(color = "#e7298a", width = 2),
        text = ~paste0(
          "Date: ", hover_dates,
          "<br>Fruit respiration: ", fmt_num(R_fruits_umol_day), " umol C day-1"
        ),
        hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~G_leaves_umol_day,
        name = "Leaf growth",
        line = list(color = "blue", width = 2),
        text = ~paste0(
          "Date: ", hover_dates,
          "<br>Leaf growth: ", fmt_num(G_leaves_umol_day), " umol C day-1"
        ),
        hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~G_fruits_umol_day,
        name = "Fruit growth",
        line = list(color = "#e6ab02", width = 2),
        text = ~paste0(
          "Date: ", hover_dates,
          "<br>Fruit growth: ", fmt_num(G_fruits_umol_day), " umol C day-1"
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Daily carbon budget components",
        margin = list(t = 70),
        xaxis = list(title = "Date"),
        yaxis = list(title = "umol C day-1", zeroline = TRUE),
        hoverlabel = list(align = "left")
      )
  })

  # Allow the modeled daily budget table to be downloaded as a CSV file.
  output$download_budget <- downloadHandler(
    filename = function() {
      paste0("pistachio-carbon-budget-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(model_run())
      write.csv(model_run()$budget, file, row.names = FALSE)
    }
  )
}

# Launch the Shiny application.
shinyApp(ui = ui, server = server)
