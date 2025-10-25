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
      helpText(
        strong("Field meanings:"),
        tags$ul(
          tags$li(tags$code("Infection type"), "— HAP, UTI, SSI, BSI, CDI"),
          tags$li(tags$code("Age group"), "— bracketed lower;upper, e.g. ", tags$code("[70;74]")),
          tags$li(tags$code("Sex"), "— F (female), M (male)")
        ),
        br(),
        strong("How to read the chart:"),
        "Bars show total infections (or proportions if toggled).",
        " Age bands are ordered from ", strong("youngest (top) → oldest (bottom)"), "."
      )
    ),

    mainPanel(
      h4("Infections by Age Group"),
      plotOutput("plot_age", height = "600px"),
      hr(),
      tableOutput("tbl_age_note"),
      hr(),
      h5("Totals by infection type"),
      tableOutput("tbl_types"),
      hr(),
      h5("Totals by sex"),
      tableOutput("tbl_sex"),
      hr(),
      h5("Headline figures"),
      tableOutput("tbl_headlines")
    )
  )
)

# SERVER
server <- function(input, output, session) {

  filt <- reactive({
    df <- hai_data_clean %>% filter(Infection_Type == input$type)
    if (input$sex != "All") df <- df %>% filter(Sex == input$sex)
    df})

  output$plot_age <- renderPlot({
    df <- filt()
    validate(need(nrow(df) > 0, "No data for this selection."))

    sums <- df %>%
      group_by(Age_Group) %>%
      summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

    if (sum(sums$Total) == 0) {
      plot.new()
      title(main = paste("No non-zero data for", input$type, input$sex))
      return(NULL)
    }

    if (isTRUE(input$show_prop)) {
      total_all <- sum(sums$Total)
      sums <- mutate(sums, Value = Total / total_all)
      y_lab <- "Proportion of infections"
    } else {
      sums <- mutate(sums, Value = Total)
      y_lab <- "Total infections"
    }

    sums <- sums %>%
      mutate(
        Lower = suppressWarnings(as.numeric(sub("\\[(\\d+);.*", "\\1", Age_Group))),
        Lower = ifelse(grepl("Inf", Age_Group), 999, Lower)
      ) %>%
      arrange(Lower)

    ggplot(sums, aes(x = reorder(Age_Group, Lower), y = Value)) +
      geom_col(fill = "#2C3E50") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0(input$type, if (input$sex != "All") paste0(" (", input$sex, ")")),
        x = "Age group (years)",
        y = y_lab
      )
  })

  output$tbl_age_note <- renderTable({
    data.frame(Note = "Age bands ordered by lower bound: youngest at top → oldest at bottom.")
  }, striped = TRUE, bordered = TRUE, colnames = FALSE)

  output$tbl_types <- renderTable({
    hai_data_clean %>%
      group_by(Infection_Type) %>%
      summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total))
  }, striped = TRUE, bordered = TRUE)

  output$tbl_sex <- renderTable({
    filt() %>%
      group_by(Sex) %>%
      summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total))
  }, striped = TRUE, bordered = TRUE)

  output$tbl_headlines <- renderTable({
    hai_headlines
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
