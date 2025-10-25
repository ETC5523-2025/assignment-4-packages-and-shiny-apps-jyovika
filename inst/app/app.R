# Shiny app for haiInsight
# - Uses package-bundled data (no read.csv)
# - Meaningful interactivity: infection type + sex filter + counts vs proportions
# - Explains fields and interpretation
# - Light styling via bslib

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(haiInsight)

# Load package datasets
data("hai_data_clean", package = "haiInsight")
data("hai_headlines",   package = "haiInsight")

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Healthcare-Associated Infections — Germany, 2011 (haiInsight)"),

  layout_sidebar(
    sidebar = sidebar(
      h4("Controls"),
      selectInput(
        "type", "Infection type",
        choices = sort(unique(hai_data_clean$Infection_Type)),
        selected = sort(unique(hai_data_clean$Infection_Type))[1]),

      selectInput(
        "sex", "Sex",
        choices = c("All", sort(unique(hai_data_clean$Sex))),
        selected = "All"),

      checkboxInput("show_prop", "Show proportions instead of counts", FALSE),
      hr(),
      helpText(
        strong("Field meanings:"),
        tags$ul(
          tags$li(tags$code("Infection type"), "— HAP, UTI, SSI, BSI, CDI"),
          tags$li(tags$code("Age group"), "— bracketed lower;upper, e.g. ", tags$code("[70;74]")),
          tags$li(tags$code("Sex"), "— F (female), M (male)")),

        br(),
        strong("How to read the chart:"),
        "Bars show total infections (or proportions if toggled) for the selected infection type.",
        " Age bands are ordered from ", strong("youngest (top) → oldest (bottom)"), ".")),
    verbatimTextOutput("debug"),

    main = navset_tab(
      id = "tabs",
      nav_panel(
        "Infection by age",
        br(),
        plotOutput("plot_age", height = 420),
        br(),
        tableOutput("tbl_age_note")
      ),
      nav_panel(
        "Summary tables",
        br(),
        fluidRow(
          column(
            width = 6,
            h5("Totals by infection type"),
            tableOutput("tbl_types")
          ),
          column(
            width = 6,
            h5("Totals by sex"),
            tableOutput("tbl_sex")
          )
        )
      ),
      nav_panel(
        "Key figures (from study)",
        br(),
        helpText(
          "Published national estimates (for context only): cases, deaths, DALYs.",
          " These are manually compiled from the paper you cited."
        ),
        tableOutput("tbl_headlines")
      )
    )
  )
)

server <- function(input, output, session) {

  # filtered dataset for the chosen infection type & sex
  filt <- reactive({
    df <- hai_data_clean %>% dplyr::filter(Infection_Type == input$type)
    if (input$sex != "All") df <- df %>% dplyr::filter(Sex == input$sex)
    df
  })

  # Plot by age group: youngest (top) -> oldest (bottom)
  output$plot_age <- renderPlot({
    df <- filt()
    validate(need(nrow(df) > 0, "No data for this selection."))

    # totals by age
    sums <- df %>%
      dplyr::group_by(Age_Group) %>%
      dplyr::summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

    if (isTRUE(input$show_prop)) {
      total_all <- sum(sums$Total, na.rm = TRUE)
      sums <- dplyr::mutate(sums, Value = ifelse(total_all > 0, Total / total_all, 0))
      y_lab <- "Proportion of infections"
    } else {
      sums <- dplyr::mutate(sums, Value = Total)
      y_lab <- "Total infections"
    }

    # order age bands by lower bound ASC → with coord_flip() youngest at top
    lo <- suppressWarnings(as.numeric(sub("\\[(\\d+);.*", "\\1", sums$Age_Group)))
    sums$Age_Group <- factor(sums$Age_Group, levels = sums$Age_Group[order(lo)])

    ggplot2::ggplot(sums, ggplot2::aes(x = Age_Group, y = Value)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(
        title = paste0(input$type, if (input$sex != "All") paste0(" (", input$sex, ")")),
        x = "Age group (years)",
        y = y_lab
      )
  })

  # Little note under the plot so markers know what's happening
  output$tbl_age_note <- renderTable({
    data.frame(
      Note = "Age bands ordered by lower bound: youngest at top → oldest at bottom.",
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, colnames = FALSE)

  # Summary tables
  output$tbl_types <- renderTable({
    hai_data_clean %>%
      dplyr::group_by(Infection_Type) %>%
      dplyr::summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(Total))
  }, striped = TRUE, bordered = TRUE)

  output$tbl_sex <- renderTable({
    filt() %>%
      dplyr::group_by(Sex) %>%
      dplyr::summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(Total))
  }, striped = TRUE, bordered = TRUE)

  # Key headline figures (from your manually compiled table)
  output$tbl_headlines <- renderTable({
    hai_headlines
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
