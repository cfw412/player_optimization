library(shiny)
library(reactable)

# Sample data
df <- data.frame(
  id = 1:5,
  value = c("A", "B", "C", "D", "E"),
  detail_btn = NA
)

# UI
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('click', '.show-on-plot-btn', function() {
        var id = $(this).data('id');
        Shiny.setInputValue('button_clicked', id, {priority: 'event'});
      });
    "))
  ),
  reactableOutput("table"),
  verbatimTextOutput("df_output")
)

# Server
server <- function(input, output, session) {
  # Reactive dataframe
  rv <- reactiveVal(df)
  
  # Render reactable
  output$table <- renderReactable({
    reactable(
      rv(),
      columns = list(
        id = colDef(),
        value = colDef(),
        detail_btn = colDef(name = "", sortable = FALSE, cell = function(value, index) {
          htmltools::tags$button(
            "Show on plot",
            class = "show-on-plot-btn",
            `data-id` = rv()$id[index]
          )
        })
      )
    )
  })
  
  # Update dataframe when button is clicked
  observeEvent(input$button_clicked, {
    updated_df <- rv()
    row_index <- which(updated_df$id == input$button_clicked)
    updated_df$value[row_index] <- "Updated"
    rv(updated_df)
  })
  
  # Display updated dataframe
  output$df_output <- renderPrint({
    rv()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
