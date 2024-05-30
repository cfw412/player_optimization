library(shiny)
library(ggplot2)
library(plotly)
library(reactable)
library(dplyr)
library(tidyr)
library(htmltools)

# Sample data for illustration
detail_dataframe <- data.frame(
  row_id = 1:3,
  total_goals_added = c(30, 25, 28),
  sum_total_salary = c(150000, 200000, 170000),
  player_name = c("Player1, Player2", "Player3, Player4", "Player5, Player6"),
  position = c("Forward, Midfield", "Midfield, Defense", "Forward, Defense"),
  log_goals_added = c("10, 12", "9, 16", "14, 14"),
  guaranteed_compensation = c("50000, 75000", "60000, 90000", "70000, 100000")
)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Scatterplot of Player Data"),
  sidebarLayout(
    sidebarPanel(
      reactableOutput("table")  # Output for the reactable table
    ),
    mainPanel(
      plotlyOutput("scatterPlot")  # Output for the scatterplot
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Render the scatterplot with Plotly for hover functionality
  output$scatterPlot <- renderPlotly({
    # Flatten the detail dataframe for scatterplot data
    plot_data <- detail_dataframe %>%
      mutate(player_name = strsplit(as.character(player_name), ",\\s*"),
             position = strsplit(as.character(position), ",\\s*"),
             log_goals_added = lapply(strsplit(as.character(log_goals_added), ",\\s*"), as.numeric),
             guaranteed_compensation = lapply(strsplit(as.character(guaranteed_compensation), ",\\s*"), as.numeric)) %>%
      unnest(c(player_name, position, log_goals_added, guaranteed_compensation))
    
    p <- ggplot(plot_data, aes(x = guaranteed_compensation, y = log_goals_added, text = player_name)) +
      geom_point() +
      labs(x = "Total Comp", y = "Goals Added") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # Render the outer reactable table
  output$table <- renderReactable({
    reactable(detail_dataframe %>% 
                mutate(details = NA) %>% 
                select(total_goals_added, sum_total_salary, details), 
              columns = list(
                sum_total_salary = colDef(
                  format = colFormat(prefix = "$", separators = TRUE, digits = 0)
                ),
                details = colDef(
                  name = "", 
                  sortable = FALSE, 
                  cell = function(index) {
                    tags$button(
                      "Show on plot",
                      onclick = sprintf("Shiny.setInputValue('show_on_plot', %d, {priority: 'event'})", index)
                    )
                  }
                )
              ),
              highlight = TRUE, 
              compact = TRUE,
              onClick = "select",
              details = function(index) {
                row_detail_data <- detail_dataframe[detail_dataframe$row_id == detail_dataframe$row_id[index], ]
                
                player_name <- strsplit(as.character(row_detail_data$player_name), ",\\s*")[[1]]
                position <- strsplit(row_detail_data$position, ",\\s*")[[1]]
                log_goals_added <- as.numeric(strsplit(as.character(row_detail_data$log_goals_added), ",\\s*")[[1]])
                guaranteed_compensation <- as.numeric(strsplit(as.character(row_detail_data$guaranteed_compensation), ",\\s*")[[1]])
                
                row_detail_data <- data.frame(
                  player_name = player_name,
                  position = position,
                  log_goals_added = log_goals_added,
                  guaranteed_compensation = guaranteed_compensation
                )
                
                # Creating a detail table for the selected row
                htmltools::div(style = "padding: 1rem",
                               reactable(row_detail_data, 
                                         columns = list(guaranteed_compensation = colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0))),
                                         outlined = TRUE, 
                                         highlight = TRUE, 
                                         compact = TRUE))
              })
  })
  
  # Observe button clicks and highlight corresponding points in the scatterplot
  observeEvent(input$show_on_plot, {
    selected_index <- input$show_on_plot
    selected_row <- detail_dataframe[selected_index, ]
    
    selected_player_names <- strsplit(as.character(selected_row$player_name), ",\\s*")[[1]]
    
    session$sendCustomMessage(type = "highlightPoints", message = list(players = selected_player_names))
  })
}

# JavaScript to highlight the points in the scatterplot
js <- "
Shiny.addCustomMessageHandler('highlightPoints', function(message) {
  var points = document.getElementsByClassName('point');
  
  Array.from(points).forEach(function(point) {
    var pointText = point.textContent;
    if (message.players.some(player => pointText.includes(player))) {
      point.style.fill = 'red';
      point.style.stroke = 'black';
      point.style.strokeWidth = '2px';
    } else {
      point.style.fill = '';
      point.style.stroke = '';
      point.style.strokeWidth = '';
    }
  });
});
"

# Run the application
shinyApp(ui = ui, server = server, options = list(shiny.head = tags$script(HTML(js))))
