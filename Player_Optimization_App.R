
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

if (!require(readxl)){
  install.packages('readxl')
}
library(readxl)

if (!require(GA)){
  install.packages('GA')
}
library(GA)

if (!require(httr)){
  install.packages('httr')
}
library(httr)

if (!require(jsonlite)){
  install.packages('jsonlite')
}
library(jsonlite)

if (!require(tidyverse)){
  install.packages('tidyverse')
}
library(tidyverse)

# ASA's package to connect to their API
if (!require(itscalledsoccer)){
  install.packages('itscalledsoccer')
}
library(itscalledsoccer)

### Import and Clean Data ###

# American Soccer Analysis API client
asa_client <- AmericanSoccerAnalysis$new()

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

# Getting the log of the Goals Added metric so that it is on a 0 to 1 scale
shifted_values <- player_data$goals_added_raw - min(player_data$goals_added_raw)
transformed_values <- log1p(shifted_values)
normalized_values <- (transformed_values - min(transformed_values)) / (max(transformed_values) - min(transformed_values))
normalized_values <- normalized_values * (0.9999 - 0.0001) + 0.0001

player_data = data.frame(player_data %>% 
                           mutate(log_goals_added = normalized_values)) %>% 
  mutate(across('log_goals_added', round, 4))

### Shiny App ###

unique(player_data$position)

shinyApp(
  
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
    
    # Set the input fields
    sidebarLayout(
      sidebarPanel(id = "sidebarPanel",
                   h4("Select the number of players at each position:"), 
                   
                   # Numeric inputs for minimum and maximum players per position 
                   uiOutput("position_inputs"), 
                   
                   currencyInput(inputId = "max_salary", 
                                 label = "What is the salary limit?", 
                                 format = "dollar", 
                                 value = 1000000,
                                 align = "center"), 
                   
                   actionButton(inputId = "btn_run", 
                                label = "Run"), 
                   
                   width = 6
      ), 
      
      mainPanel( 
        fluidRow( 
          column(6, uiOutput("max_salary_input")),
          column(12, dataTableOutput('table'))
          )
        )
      )
    ),
  
  # Server function 
  server = function(input, output, session) {
    
    # reactive value for the max salary
    max_salary = reactive({input$max_salary})
    
    # unique list of positions from the dataset
    position_list = c(unique(player_data$position))
    
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
          
          if(current_salary>max_salary) {
            return(0) # Penalty for exceeding the max salary
          }
          else if (all(positions_count == positions_required)) {
            return(current_goals_added) # meets the players and positions constraint
          }
          else {
            return(0)
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
        
        #
        renamed_player_data = filtered_player_data %>%
          select(c('player_name',
                   'position',
                   'log_goals_added',
                   'guaranteed_compensation')) %>%
          rename('Goals Added' = 'log_goals_added',
                 'Total Salary' = 'guaranteed_compensation')
  
        # displaying the algorithm's results on the UI
        output$table = renderDataTable(renamed_player_data[GA@solution == 1,],
                                       options = list(
                                         searching = FALSE,
                                         lengthChange = FALSE,
                                         dom = "lfrt"
                                       ))
        
        # removing the modal
        remove_modal_spinner() # remove it when done
      }
    })
  }
)