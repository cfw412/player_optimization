
if (!require(rsconnect)){
  install.packages('rsconnect')
}
library('rsconnect')

if (!require(devtools)){
  install.packages('devtools')
}
library(devtools)

if (!require(shiny)){
  install.packages('shiny')
  library(shiny)
}

# show_modal_spinner and report_warning
if (!require(shinybusy)){
  install.packages('shinybusy')
}
library(shinybusy)

# currencyInput
if (!require(shinyWidgets)){
  install.packages('shinyWidgets')
}
library(shinyWidgets)

if (!require(DT)){
  install.packages('DT')
}
library(DT)

if (!require(htmlwidgets)){
  install.packages('htmlwidgets')
}
library(htmlwidgets)

if (!require(GA)){
  install.packages('GA')
}
library(GA)

if (!require(tidyverse)){
  install.packages('tidyverse')
}
library(tidyverse)

if (!require(plotly)){
  install.packages('plotly')
}
library(plotly)

if (!require(RColorBrewer)){
  install.packages('RColorBrewer')
}
library(RColorBrewer)

if (!require(forcats)){
  install.packages('forcats')
}
library(forcats)

if (!require(scales)){
  install.packages('scales')
}
library(scales)

# ASA's package to connect to their API
if (!require(itscalledsoccer)){
  install.packages('itscalledsoccer')
}
library(itscalledsoccer)

if (!require(reactable)){
  install.packages('reactable')
}
library('reactable')

### Import and Clean Data ###

# American Soccer Analysis API client
asa_client = AmericanSoccerAnalysis$new()

goals_added = asa_client$get_player_goals_added(leagues = "mls", 
                                                    season_name = "2022") %>% 
  tidyr::unnest("data") %>%
  tidyr::unnest("team_id") %>%
  select(-c(team_id)) %>%
  distinct()

# aggregating rows by goals_added per player
goals_added = goals_added %>% 
  group_by(player_id, general_position) %>% 
  summarise(across(c(goals_added_raw, goals_added_above_avg),sum), 
            .groups = 'drop') %>% 
  as.data.frame()

salaries = asa_client$get_player_salaries(leagues = "mls", season_name = "2022")

teams = asa_client$get_teams(leagues = "mls") 

players = asa_client$get_players(leagues = "mls")

player_salaries = merge(x = salaries, y = players, by = "player_id")

# TODO: dynamically select the max date and filter on that instead
player_salaries = merge(x = player_salaries, y = teams, by = "team_id") %>%
  filter(mlspa_release == "2022-09-02") %>% 
  select(c(player_id, 
           player_name, 
           team_abbreviation, 
           base_salary, 
           guaranteed_compensation, 
           mlspa_release))

player_data = merge(x = goals_added, y = player_salaries, by = "player_id") %>%
  arrange(desc(goals_added_raw)) %>%
  rename("position" = "general_position")

rm(player_salaries)
rm(players)
rm(teams)
rm(salaries)
rm(goals_added)

# Getting the log of the Goals Added metric so that it is on a 0 to 1 scale
shifted_values = player_data$goals_added_raw - min(player_data$goals_added_raw)
transformed_values = log1p(shifted_values)
normalized_values = (transformed_values - min(transformed_values)) / (max(transformed_values) - min(transformed_values))
##################
###### TODO: Change below equation to .9998
##################
normalized_values = normalized_values * (0.9999 - 0.0001) + 0.0001

player_data = data.frame(player_data %>% 
                           mutate(log_goals_added = normalized_values)) %>% 
  mutate(across('log_goals_added', round, 4))

### Shiny App ###
# UI function
ui = fluidPage(
  
  # changing the color of the background
  tags$head( 
    tags$style(
      HTML('
         #sidebarPanel {
            background-color: #582C8350;
         }
        
         #title {
          color: #380F55;
         }')
    )
  ),
  
  # Title
  titlePanel("", windowTitle = "Player Optimization App"),
  
  h2(id = "title", "Player Optimization App"),
  
  h4("Select the number of players at each position:"),
  
  verticalLayout(
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          # Numeric inputs for minimum and maximum players per position 
          uiOutput("position_inputs")
          ),
        
        fluidRow(
          currencyInput(inputId = "max_salary", 
                        label = "What is the salary limit?", 
                        format = "dollar", 
                        value = 1000000,
                        align = "center",
                        width = "160px"), 
          actionButton(inputId = "btn_run", 
                       label = "Run")
          ),
        ),
      
      # Code for scatterplot and styling options
      mainPanel(plotlyOutput("scatterPlot")
                ),
      
      position = "left",
      fluid = FALSE
    ),
    
    fluidRow(
      column(
        reactableOutput('table'),
        width = 9)
    ),
  )
) #add comma here if changing shinyApp function
  
# Server function 
server = function(input, output, session) {
  
  # reactive value for the max salary
  max_salary = reactive({input$max_salary})
  
  # unique list of positions from the dataset
  position_list = c(unique(player_data$position))

  # Code for the default scatterplot
  # output$plot_output = renderPlot({
  #   ggplot(data.frame(x = c(1, 2, 3), y = c(10, 20, 30)), aes(x = x, y = y)) +
  #     geom_bar(stat = "identity", fill = "steelblue")
  # })
  
  # Code for dynamically up
  # color_palette <- reactive({
  #   switch(input$color_palette,
  #          "Viridis" = scales::viridis_pal(),
  #          "RdYlBu" = scales::RdYlBu,
  #          "Blues" = scales::blues)
  # })
  
  # dynamically create and render numericInputs for the player position inputs
  output$position_inputs = renderUI({
    selected_position_values_list = 
      lapply(position_list,
             function(value) {
               div(style = paste0("display: inline-block; width: ", 
                                  length(position_list) * 9, 
                                  "px; text-align: center;"),
                   numericInput(paste0("input_", value),
                                label = value,
                                value = 0,
                                min = 0)
               )
             })
    tagList(selected_position_values_list)
  })
  
  # formatting the max_salary input field
  output$max_salary_input = renderUI({
    tags$style(type = "text/css", "#max_salary { width: 145px; }"
    )
  })
  
  # Define a function to get the input values
  get_input_value = function(input_name) {
    input = input[[input_name]]
    return(input)
  }
  
  # display data on 'btn_run' click
  observeEvent(input$btn_run, {
    
    extract_and_convert <- function(string_values) {
      numeric_values <- as.numeric(strsplit(string_values, ", ")[[1]])
      if (length(numeric_values) > 1) {
        return(numeric_values)
      } else {
        return(NA)
      }
    }
    
    # creating a list of names for numericInput objects to be created below
    numeric_input_position_objects_names = paste0("input_", position_list)
    
    # get the input values for each input name
    numeric_input_position_values = lapply(numeric_input_position_objects_names, 
                                           get_input_value)
    
    # remove positions from input values where the value is 0
    position_values = data.frame(position = position_list,
                                 input_value = 
                                   unlist(numeric_input_position_values)) %>%
      filter(input_value > 0)
    
    # check if there are 2 or more positions selected
    if(nrow(position_values) == 0 | sum(position_values$input_value) == 1)
    {
      # display warning if there are less than 2 positions selected
      report_warning("Select more players", 
                     "There must be 2 or more total players selected", 
                     button = "Ok",
                     config_report(
                       svgColor = "#582C83",
                       buttonBackground = "#582C83",
                       backOverlayColor = "#582C8390"
                     ))
    }else
    {
      # add modal spinner while data is loading
      show_modal_spinner(
        spin = 'semipolar',
        color = '#582C83',
        text = 'Please wait'
      ) 
      
      # turn the position_values dataframe into a table() format
      positions_required = position_values %>% 
        spread(position, input_value, fill = 0)
      
      # setting the population equal to the number of players entered
      sel_pop = sum(position_values$input_value)
      
      # setting the max salary equal to the input value
      max_salary = max_salary()
      
      # filter the Player_Data dataframe on the players in the selected positions
      filtered_player_data = 
        player_data %>%
        filter(position %in% position_values$position) %>%
        select(c('player_name',
                 'position',
                 'guaranteed_compensation',
                 'log_goals_added'))
      
      ### GA Functions
      ### Initialize population
      initializePopulation = function(k){

        function(GA){
          mat = matrix(0, ncol = GA@nBits, nrow = GA@popSize)

          for(i in seq_len(GA@popSize))
            mat[i, sample(GA@nBits, k)] = 1

          for (i in 1:length(position_values$input_value)) {
            if (position_values$input_value[i] > 0) {
              ind = which(player_data$position
                          == names(position_values$input_value)[i])
              mat[, ind[sample(length(ind), 1)]] = 1
            }
          }

          mat
        }
      }

      ### Crossover function
        # custom function that performs crossover while keeping the number of
        # selected bits equal to the selected population
      crossoverFunction = function(GA, parents){

        parents = GA@population[parents,] %>%
          apply(1, function(x) which(x == 1)) %>%
          t()

        parents_diff = list("vector", 2)
        parents_diff[[1]] = setdiff(parents[2,], parents[1,])
        parents_diff[[2]] = setdiff(parents[1,], parents[2,])

        children_ind = list("vector", 2)
        for(i in 1:2){
          k = length(parents_diff[[i]])
          change_k = sample(k, sample(ceiling(k/2), 1))
          children_ind[[i]] = if(length(change_k) > 0){
            c(parents[i, -change_k], parents_diff[[i]][change_k])
          } else {
            parents[i,]
          }
        }

        children = matrix(0, nrow = 2, ncol = GA@nBits)
        for(i in 1:2)
          children[i, children_ind[[i]]] = 1

        list(children = children, fitness = c(NA, NA))
      }

      ### Mutation function
        # custom function that performs mutation while keeping the number of
        # selected bits equal to the selected population
      mutationFunction = function(GA, parent){
        ind = which(GA@population[parent,] == 1)
        n_change = sample(sel_pop, 1)
        ind[sample(length(ind), n_change)] =
          sample(setdiff(seq_len(GA@nBits), ind), n_change)
        parent = integer(GA@nBits)
        parent[ind] = 1

        parent
      }

      ### Fitness function
      # objective function that sums the goals_added value and checks the
      # constraint of number of players and their positions
      fitness = function(x)
      {
        current_goals_added=x%*%filtered_player_data$log_goals_added
        current_salary=x%*%filtered_player_data$guaranteed_compensation
        positions = filtered_player_data$position[x == 1] # positions of selected players
        positions_count = table(positions) # count of players in each position
        
        if(current_salary<max_salary & all(positions_count == positions_required)) {
          return(current_goals_added) 
        }else {
          current_goals_added = 0
          return(current_goals_added)
        }
      }
      
      # Genetic algorithm function
      GA = ga(
        type = "binary",
        fitness = fitness,
        nBits = nrow(filtered_player_data),
        population = initializePopulation(sel_pop),
        crossover = crossoverFunction,
        mutation = mutationFunction,
        run = 125,
        pmutation = .25,
        maxiter = 300,
        popSize = 30,
        monitor = FALSE
      )

      ############################################################
      # Combine fitness values with the population matrix
      population_df = data.frame(fitness = GA@fitness, 
                                  population = GA@population)
      
      # Sort the combined data frame based on fitness values in descending order
      sorted_population = unique(population_df[order(-population_df$fitness), ] %>% 
                                    filter(fitness > 0))
      
      # Number of rows
      n = 5
      sorted_pop_rows = nrow(sorted_population)
      
      if(n > sorted_pop_rows){
        n = sorted_pop_rows
      }
      
      # Create an empty list to store the filtered data frames
      filtered_data_frames = list()
      
      ## Removed below since it creates redundant steps
      # summary_dataframe = data.frame(total_goals_added = double(), 
      #                                sum_total_salary = integer())
      
      # Loop through each binary list and filter the rows accordingly
      ########################################################################
      ## TO:DO - Remove for loop and add all data to the dataframe then apply 
      ## vectorized functions
      ########################################################################
      for (r in 1:n) {
        row_list = as.list(sorted_population[r, ] %>% select(-c("fitness")))
        filtered_rows = filtered_player_data[row_list == 1, ]
        total_goals_added = sum(as.numeric(filtered_rows$log_goals_added))
        sum_total_salary = sum(as.numeric(filtered_rows$guaranteed_compensation))
        concatenated_row = apply(filtered_rows, 2, function(x) paste(x, collapse = ", "))
        concatenated_row = c(concatenated_row, total_goals_added, sum_total_salary)
        filtered_data_frames[[r]] = concatenated_row
        ## Removed below since its redundant
        #summary_dataframe = rbind(summary_dataframe, c(total_goals_added, sum_total_salary)) 
      }
      
      # Combine the filtered data frames into a single dataframe
      detail_dataframe = do.call(rbind, filtered_data_frames)
     
      detail_dataframe = data.frame(detail_dataframe) %>%
        rename('total_goals_added' = 'V5',
               'sum_total_salary' = 'V6') 
      
      detail_dataframe = detail_dataframe %>%
        rowwise() %>%
        mutate(std_dev = sd(na.omit(extract_and_convert(log_goals_added)))) %>%
        mutate(across('std_dev', round, 4)) %>%
        select(c(
          'player_name',
          'position',
          'log_goals_added',
          'total_goals_added',
          'guaranteed_compensation',
          'sum_total_salary',
          'std_dev'
        ))
      
      # displaying the algorithm's results on the UI
      # output$debug_table = renderDataTable(detail_dataframe,
      #                               # renamed_player_data[GA@solution == 1,],
      #                                options = list(
      #                                  searching = FALSE,
      #                                  lengthChange = FALSE,
      #                                  dom = "lfrt"
      #                                ))
      
      detail_dataframe$player_name = fct_reorder(detail_dataframe$player_name,
                                                detail_dataframe$total_goals_added,
                                                .desc = TRUE)
      
      detail_dataframe$sum_total_salary = as.numeric(detail_dataframe$sum_total_salary)
      detail_dataframe$row_id = seq_len(nrow(detail_dataframe))
      
      table_width = 1000
      
      ############
      #TODO:  
      # 1. Tie "Show on plot" button to the index of detail_dataframe
      # 2. Create a dataframe with filtered_player_data that has a column to indicate color
      # 3. Merge values from the selected record from detail dataframe and change those to a different color
      #     - May need to move the plotly code outside the first observe event function
      #     - May need to make an additional plot that overwrites the original one
      ############
      
      output$table <- renderReactable({
        reactable(detail_dataframe %>% mutate(detail_btn = NA) %>%
                    select(total_goals_added, sum_total_salary, detail_btn),
                  columns = list(sum_total_salary = colDef(format =
                                                             colFormat(prefix = "$",
                                                                       separators = TRUE,
                                                                       digits = 0)),
                                 detail_btn = colDef(name = "", 
                                                     sortable = FALSE, 
                                                     cell = function() htmltools::tags$button("Show on plot"))),
                  highlight = TRUE,
                  compact = TRUE,
                  width = table_width,
                  onClick = "select",
                  details = function(index) {
                    row_detail_data = detail_dataframe[detail_dataframe$row_id == detail_dataframe$row_id[index], ]

                    # Split the concatenated 'position' and 'guaranteed_compensation' fields into separate rows
                    player_name = strsplit(as.character(row_detail_data$player_name), ",\\s*")[[1]]
                    position = strsplit(row_detail_data$position, ",\\s*")[[1]]
                    log_goals_added = strsplit(as.character(row_detail_data$log_goals_added), ",\\s*")[[1]]
                    guaranteed_compensation = strsplit(as.character(row_detail_data$guaranteed_compensation), ",\\s*")[[1]]

                    row_detail_data = data.frame(
                      player_name = player_name,
                      position = position,
                      log_goals_added = log_goals_added,
                      guaranteed_compensation = guaranteed_compensation
                    )

                    #Convert to numeric for formatting purposes
                    row_detail_data$guaranteed_compensation = as.numeric(row_detail_data$guaranteed_compensation)

                    # Creating a detail table for the selected row
                    htmltools::div(style = "padding: 1rem",
                                   reactable(row_detail_data,
                                             columns = list(guaranteed_compensation =
                                                              colDef(format =
                                                                       colFormat(prefix = "$",
                                                                                 separators = TRUE,
                                                                                 digits = 0))),
                                             outlined = TRUE,
                                             highlight = TRUE,
                                             compact = TRUE,
                                             width = table_width)
                    )
                  })
        })
      
      output$scatterPlot <- renderPlotly({
        p <- ggplot(filtered_player_data, aes(x = guaranteed_compensation, y = log_goals_added, text = player_name)) +
          geom_point() +
          labs(x = "Total Comp", y = "Goals Added") +
          theme_minimal()

        ggplotly(p, tooltip = c("text", "x", "y"))
      })
      
      # removing the modal
      remove_modal_spinner() # remove it when done

    } #if statement to evaluate invalid selections
  }) #observeEvent
} #server function

shinyApp(ui = ui, server = server)