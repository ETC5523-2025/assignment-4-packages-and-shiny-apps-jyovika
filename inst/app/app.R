library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(haiInsight)

# Load data
data("hai_data_clean", package = "haiInsight")
data("hai_headlines",   package = "haiInsight")

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Healthcare-Associated Infections - Germany, 2011 (haiInsight)"),

  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      selectInput(
        "type", "Infection type",
        choices = sort(unique(hai_data_clean$Infection_Type)),
        selected = "HAP"
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
          tags$li(tags$code("Age group"), "- age ranges in years, e.g. [70;74]."),
          tags$li(tags$code("Sex"), "- F (female), M (male).")
        ),
        strong("How to interpret outputs:"),
        "Use 'View totals by' to switch perspective.",
        "Each bar represents total or proportional infections within that group."
      )
    ),

    mainPanel(
      style = "background-color:#f8f9fa; border-radius:10px; padding:20px;",
      h4("HAI Summary Plots"),
      plotOutput("plot_summary", height = "600px"),
      hr(),
      h5("Summary Table"),
      tableOutput("tbl_summary")
    )
  )
)

# SERVER
server <- function(input, output, session) {

  filt <- reactive({
    df <- hai_data_clean %>% filter(Infection_Type == input$type)
    if (input$sex != "All") df <- df %>% filter(Sex == input$sex)
    df
  })

  output$plot_summary <- renderPlot({
    data_to_plot <- if (input$view_mode == "Infection_Type") {
      hai_data_clean
    } else if (input$view_mode == "Age_Group") {
      filt()
    } else {
      filt()
    }

    plot_hai_totals(data_to_plot, by = input$view_mode)
  })

  output$tbl_summary <- renderTable({
    summarise_hai(hai_data_clean, by = input$view_mode)
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
