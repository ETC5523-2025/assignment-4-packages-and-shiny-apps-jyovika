# Shiny app for haiInsight
# - Uses package-bundled data (no read.csv)
# - Includes interactivity for infection type, sex, and summary mode
# - Uses summarise_hai() and plot_hai_totals() from your package
# - Styled with bslib and kableExtra

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(haiInsight)

data("hai_data_clean", package = "haiInsight")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Healthcare-Associated Infections - Germany, 2011 (haiInsight)"),

  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      selectInput(
        "type", "Infection type",
        choices = c("All", sort(unique(hai_data_clean$Infection_Type))),
        selected = "All"
      ),
      selectInput(
        "sex", "Sex",
        choices = c("All", sort(unique(hai_data_clean$Sex))),
        selected = "All"
      ),
      checkboxInput("show_prop", "Show proportions instead of counts", FALSE),
      hr(),
      selectInput(
        "view_mode", "View totals by:",
        choices = c("Infection_Type", "Age_Group", "Sex"),
        selected = "Age_Group"
      ),
      hr(),
      helpText(
        strong("Field meanings:"),
        tags$ul(
          tags$li(tags$code("Infection type"), "- HAP (pneumonia), UTI (urinary tract), SSI (surgical site), BSI (bloodstream), CDI (C. difficile)."),
          tags$li(tags$code("Age group"), "- Age ranges in years, e.g. [70;74]."),
          tags$li(tags$code("Sex"), "- F (female), M (male).")
        ),
        br(),
        strong("How to interpret outputs:"),
        "Use 'View totals by' to explore totals from different perspectives.",
        "Bars represent absolute infection counts."
      )
    ),

    mainPanel(
      style = "background-color:#f8f9fa; border-radius:10px; padding:20px;",
      h4("HAI Summary Explorer"),
      plotOutput("plot_summary", height = "600px"),
      hr(),
      h5("Summary Table"),
      uiOutput("tbl_summary")
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    df <- hai_data_clean

    # Infection type filter (skip if "All")
    if (!is.null(input$type) && input$type != "All") {
      df <- df %>% filter(Infection_Type == input$type)
    }

    # Sex filter (skip if "All")
    if (!is.null(input$sex) && input$sex != "All") {
      df <- df %>% filter(Sex == input$sex)
    }

    df
  })

  output$plot_summary <- renderPlot({
    df <- filtered_data()
    req(input$view_mode)
    validate(need(nrow(df) > 0, "No data available for this selection."))

    plot_hai_totals(df, by = input$view_mode)
  })

  output$tbl_summary <- renderUI({
    df <- filtered_data()

    tbl_html <- summarise_hai(
      df,
      by = input$view_mode,
      as_kable = TRUE,
      caption = paste("Summary of HAI cases by", input$view_mode)
    )

    HTML(tbl_html)
  })
}

shinyApp(ui, server)
