# Shiny app for haiInsight
# - Uses package-bundled data (no read.csv)
# - Interactivity: infection type, sex, proportions, and summary mode
# - Uses summarise_hai() and plot_hai_totals()

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(haiInsight)
library(scales)

pal <- list(
  bg    = "#f8f9fa",
  panel = "#FFFFFF",
  bar   = "#2C3E50",
  accent= "#598392",
  deep  = "#004D40",
  grid  = "#E6EEF0",
  text  = "#1B2631")

app_theme <- bslib::bs_theme(
  bootswatch   = "flatly",
  primary      = pal$accent,
  secondary    = pal$bar,
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter Tight"))

data("hai_data_clean", package = "haiInsight")

ui <- fluidPage(
  theme = app_theme,
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  titlePanel("Healthcare-Associated Infections - Germany, 2011 (haiInsight)"),

  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      selectInput(
        "type", "Infection type",
        choices = c("All", sort(unique(hai_data_clean$Infection_Type))),
        selected = "All"),

      selectInput(
        "sex", "Sex",
        choices = c("All", sort(unique(hai_data_clean$Sex))),
        selected = "All"),

      checkboxInput("show_prop", "Show proportions instead of counts", FALSE),
      hr(),

      selectInput(
        "view_mode", "View totals by:",
        choices = c("Infection_Type", "Age_Group", "Sex"),
        selected = "Age_Group"),

      hr(),
      helpText(
        strong("Field meanings:"),
        tags$ul(
          tags$li(tags$code("Infection type"), "- HAP (pneumonia), UTI (urinary tract), SSI (surgical site), BSI (bloodstream), CDI (C. difficile)."),
          tags$li(tags$code("Age group"), "- Age ranges in years, e.g. [70;74]."),
          tags$li(tags$code("Sex"), "- F (female), M (male).")),

        br(),
        strong("How to interpret outputs:"),
        "Use 'View totals by' to explore totals from different perspectives.",
        "Bars represent absolute infection counts.")
      ),

    mainPanel(
      class = "mainPanel",
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Plot",
          plotOutput("plot_summary", height = 600),
          br(),
          textOutput("narrative")),
        tabPanel(
          "Table",
          uiOutput("tbl_summary")))
      )
    )
  )

server <- function(input, output, session) {

  filtered_data <- reactive({
    df <- hai_data_clean
    if (!is.null(input$type) && input$type != "All") {
      df <- df %>% filter(Infection_Type == input$type)
      }
    if (!is.null(input$sex) && input$sex != "All") {
      df <- df %>% filter(Sex == input$sex)
      }
    df
    })

  output$plot_summary <- renderPlot({
    df <- filtered_data()
    req(input$view_mode)
    validate(need(nrow(df) > 0, "No data available for this selection."))

    plot_hai_totals(
      df,
      by        = input$view_mode,
      show_prop = isTRUE(input$show_prop),
      pal       = pal)
    })

  output$tbl_summary <- renderUI({
    df <- filtered_data()
    tbl_html <- summarise_hai(
      df,
      by       = input$view_mode,
      as_kable = TRUE,
      caption  = paste("Summary of HAI cases by", input$view_mode))
    HTML(tbl_html)
    })

  output$narrative <- renderText({
    df <- filtered_data()
    by <- input$view_mode
    req(nrow(df) > 0)

    sums <- df %>% count(.data[[by]], wt = Count, name = "Total")
    tot_all <- sum(sums$Total, na.rm = TRUE)
    top <- slice_max(sums, Total, n = 1, with_ties = FALSE)

    if (isTRUE(input$show_prop)) {
      pct <- if (tot_all > 0) top$Total / tot_all else 0
      paste0("Top ", by, ": ", top[[by]], " (", percent(pct, accuracy = 0.1), ").")
      } else {
      paste0("Top ", by, ": ", top[[by]], " (", top$Total, " cases).")
        }
  })
  }

shinyApp(ui, server)
