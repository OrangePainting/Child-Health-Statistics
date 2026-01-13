# Child Health Statistics

library(shiny)
library(ggplot2)
library(dplyr)
source("R/helper_functions.R")

# Loading the data
health_data = read.csv("data/child_health_data.csv")
health_data[["Percentage"]] = clean_percentage(health_data[["Percentage"]])
health_data = health_data %>% filter(!is.na(Percentage), Percentage < 777) # For some reason 777 is a special code for N/A idk
health_data[["Category"]] = sapply(health_data[["Group"]], categorize_group)
outcomes = sort(unique(health_data[["Outcome..or.Indicator."]])) # Why are parenthesis dots? Check later

comparison_choices = c("Sex (Male/Female)" = "Sex",
                       "Age Group" = "Age Group",
                       "Race/Ethnicity" = "Race/Ethnicity",
                       "Family Structure" = "Family Structure",
                       "Parent Education" = "Parental Education",
                       "Income Level" = "Poverty Level",
                       "Insurance Type" = "Insurance Type")

# UI
ui = navbarPage(title = "Child Health Statistics", theme = NULL,
  # First Main Section
  tabPanel(title = "Health Data",
    sidebarLayout(
      # Inputs Panel
      sidebarPanel(width = 4,
        h3("Select What to Analyze"),
        selectInput(inputId = "outcome",
          label = strong("Choose Health Outcome:"), choices = outcomes),
        radioButtons(inputId = "category", label = strong("Choose Comparison:"),
          choices = comparison_choices), 
        uiOutput("group_select")
      ),
      # Outputs Panel
      mainPanel(width = 8,
        fluidRow(column(12,
                 h2(textOutput("outcome_title"), style = "color: #000000;"),
                 h6(style = "color: #888888;",
                   "National Center for Health Statistics. NHIS Child Summary Health Statistics."),
                 h6(style = "color: #888888;",
                    "Data accessed December 16, 2025. Available from https://data.cdc.gov/d/wxz7-ekz9."))),
        fluidRow(column(12,
                 div(
                   style = "border: 1px solid #000000; border-radius: 20px; padding: 10px; background-color: white;",
                   plotOutput("health_plot", height = "500px")
                 ))),
        # Table section
        fluidRow(column(12,
                 h4("Detailed Numbers", style = "color: #2c3e50;"),
                 h6(style = "color: #7f8c8d;",
                   "This table contains percentages and confidence intervals."),
                 dataTableOutput("health_table")))))),
  # Second Visualization
  tabPanel(title = "Compare All Outcomes",
    fluidRow(column(12,
             h2("Most Common Health Issues:", style = "color: #000000;"),
             h4(style = "color: #888888;",
               "Health outcomes ranked by overall percentage of children affected."))),
    fluidRow(column(12,
             div(
               style = "border: 1px solid #000000; border-radius: 20px; padding: 5px; background-color: white;",
               plotOutput("comparison_plot", height = "500px")))),
    fluidRow(column(12,
             h4("Detailed Numbers", style = "color: #2c3e50;"),
             h6(style = "color: #7f8c8d;",
               "Overall percentage for each health outcome across all children."),
             dataTableOutput("comparison_table")))),
  tabPanel(
    title = "About This App",
    htmlOutput("about_page")))

# Server
server = function(input, output, session) {
  filtered_data = reactive({data = health_data %>%
      filter(Outcome..or.Indicator. == input[["outcome"]]) %>%
      filter(Category == input[["category"]]) # Based on the user's two inputs
    
    # For Male/Female/Total
    if (input[["category"]] != "Sex") {data = data %>% filter(Group != "Total")}
    
    # Conditional Input with Checkboxes
    if (!is.null(input[["groups"]])) {
      if (length(input[["groups"]]) > 0){
        data = data %>%filter(Group %in% input[["groups"]])
      }
    }

    data = data %>% distinct(Group, .keep_all = TRUE) %>%
      filter(Percentage <= 100)
    return(data)
  })
  
  # Output that's also an input?
  # Cuz it's dependent on the first two choices
  output[["group_select"]] = renderUI({
    current_groups = health_data %>% filter(Outcome..or.Indicator. == input[["outcome"]]) %>%
      filter(Category == input[["category"]]) %>%
      pull(Group) %>% unique()
    
    # For Male/Female/Total
    if (input[["category"]] != "Sex") {current_groups = current_groups[current_groups != "Total"]}
    
    #  if 0 total groups available
    current_groups = sort(current_groups)
    if (length(current_groups) == 0) {
      return(
        div(style = "background-color: #000000; padding: 10px; border-radius: 5px;",
          h6(style = "color: #888888; margin: 0;",
            "No data available. Please try something else.")))
    }
    checkbox_label = paste("Choose Groups: (", length(current_groups), " available)", sep = "")
    
    div(
      strong(checkbox_label),
      checkboxGroupInput(
        inputId = "groups",
        label = NULL,
        choices = current_groups))
  })
  
  output[["outcome_title"]] = renderText({
    paste(input[["outcome"]], "Categorized by", input[["category"]])
  })
  
  # Bar Chart (Visualization 1)
  output[["health_plot"]] = renderPlot({
    data = filtered_data()
    
    #  if 0 rows available
    if (nrow(data) == 0) {
      plot_blank = ggplot() +
        annotate("text", x = 1, y = 1, 
                 label = "No data available. Please choose something else.", 
                 size = 8, color = "#888888") + theme_bw()
      return(plot_blank)
    }

    data[["Group"]] = order_groups_by_category(data[["Group"]], input[["category"]])
    
    max_percentage = max(data[["Percentage"]], na.rm = TRUE)
    min_percentage = min(data[["Percentage"]], na.rm = TRUE)
    y_max = max_percentage * (1 + (max_percentage - min_percentage)/100)
    y_min = min_percentage * (1 - (max_percentage - min_percentage)/100)
    
    data[["formatted_pct"]] = format_percentage(data[["Percentage"]])
    plot = ggplot(data, aes(x = reorder(Group, -Percentage), y = Percentage, fill = Group)) +
      geom_col(color = "#000000", linewidth = 0.3, show.legend = FALSE) +
      geom_text(aes(label = formatted_pct), vjust = -0.5, size = 5, fontface = "bold", color = "#000000") + 
      scale_fill_brewer(palette = "Set3")

    plot_title = paste(input[["outcome"]])
    plot_subtitle = paste("Categorized by", input[["category"]])
    plot = plot + labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = input[["category"]],
      y = "Percentage of Children (%)",
      caption = "National Center for Health Statistics. NHIS Child Summary Health Statistics.
Data accessed December 16, 2025. Available from https://data.cdc.gov/d/wxz7-ekz9.")

    plot = plot + theme_minimal(base_size = 14) + theme(
      plot.title = element_text(size = 18, face = "bold", color = "#222222"),
      plot.subtitle = element_text(size = 14, color = "#444444", margin = margin(b = 15)),
      plot.caption = element_text(size = 10, color = "#444444", hjust = 0),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "#222222"),
      axis.text.y = element_text(size = 12, color = "#222222"),
      axis.title = element_text(size = 13, face = "bold", color = "#222222"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#DDDDDD", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA)
    ) + scale_y_continuous(limits = c(y_min, y_max), oob = scales::oob_keep)
    return(plot)
  })
  
  # Table for Bar Chart
  output[["health_table"]] = renderDataTable({
    data = filtered_data()
    #  if 0 rows available
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data available. Please choose something else."))
    }
    result = data %>% select(Group, Percentage, Confidence.Interval, Year) %>%
      arrange(desc(Percentage)) %>%
      mutate(Percentage = format_percentage(Percentage), Confidence.Interval = ifelse(
          is.na(Confidence.Interval) | Confidence.Interval == "", "N/A", Confidence.Interval)
      ) %>% rename("Demographic Group" = Group, "Percentage (%)" = Percentage,
        "95% Confidence Interval" = Confidence.Interval,"Data Year" = Year)
    return(result)
  }, options = list(pageLength = 15, scrollX = TRUE,
    language = list(info = "Showing _START_ to _END_ of _TOTAL_ groups",
      infoEmpty = "No groups to show")))
  
  # Comparison Plot (Visualization 2)
  output[["comparison_plot"]] = renderPlot({
    comparison_data = health_data %>% filter(Group == "Total") %>%
      select(Outcome..or.Indicator., Percentage) %>%
      distinct(Outcome..or.Indicator., .keep_all = TRUE) %>%
      arrange(desc(Percentage)) %>%
      head(15)

    comparison_data[["formatted_pct"]] = format_percentage(comparison_data[["Percentage"]])

    plot = ggplot(comparison_data, aes(x = reorder(Outcome..or.Indicator., Percentage), y = Percentage)) +
      geom_col(aes(fill = Percentage), color = "black", linewidth = 0.1) +scale_fill_gradient(low = "#00B0B0", high = "#008080", name = "Percentage") +
      geom_text(aes(label = formatted_pct), hjust = -0.1, size = 4, fontface = "bold") + coord_flip() +
      labs(title = "Top 15 Health Outcomes by Overall Prevalence",
           subtitle = "Percentage of all children affected", x = "",
           y = "Percentage of Children (%)",
           caption = "National Center for Health Statistics. NHIS Child Summary Health Statistics.
Data accessed December 16, 2025. Available from https://data.cdc.gov/d/wxz7-ekz9.") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(size = 18, face = "bold", color = "#222222"),
      plot.subtitle = element_text(size = 14, color = "#444444", margin = margin(b = 15)),
      plot.caption = element_text(size = 10, color = "#444444", hjust = 0),
      axis.text.y = element_text(size = 11, color = "#222222"),
      axis.text.x = element_text(size = 11, color = "#222222"),
      axis.title = element_text(size = 12, face = "bold", color = "#222222"),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#DDDDDD", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA)) +
      scale_y_continuous(limits = c(0, 100), oob = scales::oob_keep)

    return(plot)
  })
  
  # Table for Comparison Plot
  output[["comparison_table"]] = renderDataTable({
    comparison_data = health_data %>% filter(Group == "Total") %>%
      select(Outcome..or.Indicator., Percentage) %>%
      distinct(Outcome..or.Indicator., .keep_all = TRUE) %>%
      arrange(desc(Percentage)) %>%
      mutate(Percentage = format_percentage(Percentage)) %>%
    
    return(comparison_data)}, options = list(
    pageLength = 15, scrollX = TRUE,
    language = list(search = "Search outcomes:", info = "Showing _START_ to _END_ of _TOTAL_ outcomes")))
  
  # About page
  output$about_page = renderUI({HTML(markdown::renderMarkdown(file = "about.Rmd"))})
}

shinyApp(ui = ui, server = server)