
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

if (!require(shinybusy)){
  install.packages('shinybusy')
}
library(shinybusy)

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

### Import and Clean Data ###

# Steps to pull files from repo
owner = "cfw412"
repo = "player_optimization"
path = "Data"

# Construct the URL for the API endpoint with the main branch
url = paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=main")

# Make a GET request to the API endpoint and convert the response to a data frame
response = GET(url)
data = content(response, "text")
df = fromJSON(data)

# Load the American Soccer Analysis goals added and salary data
ga_salary_data = data.frame()
ga_salary_data_url = df %>%
  filter(name == "2022_MLS_GoalsAdded_and_Salary_Data.xlsx") %>%
  select('download_url') %>%
  pull()

temp_file = tempfile()
download.file(ga_salary_data_url, temp_file, mode = "wb")
Player_Goals_Added = read_excel(temp_file, sheet = "Player_Goals_Added")
Player_Salary = read_excel(temp_file, sheet = "Player_Salary") %>%
  select(-c("Season"))

# Data cleansing and wrangling for MLSPA salary data
MLSPA_Player_Salary_202209 =
  read_csv("http://s3.amazonaws.com/mlspa/9_2_2022-Roster-Freeze-Salary-List.csv?mtime=20221017131703") %>%
  select(-c("Nickname")) %>%
  mutate(`2022 Base Salary` = floor(as.numeric(gsub("[$,]", "", `2022 Base Salary`)))) %>%
  mutate(`2022 Guar. Comp.` = floor(as.numeric(gsub("[$,]", "", `2022 Guar. Comp.`)))) %>%
  mutate(Source = 9)

MLSPA_Player_Salary202204 = 
  read_csv("http://s3.amazonaws.com/mlspa/2022_salary_list_4.15__for_site.csv?mtime=20220517164257") %>%
  mutate(`2022 Base Salary` = floor(as.numeric(gsub("[$,]", "", `2022 Base Salary`)))) %>%
  mutate(`2022 Guar. Comp.` = floor(as.numeric(gsub("[$,]", "", `2022 Guar. Comp.`)))) %>%
  mutate(Source = 4)

MLSPA_Player_Salary202204 = rename(MLSPA_Player_Salary202204, "Position" = "Playing Position")

MLSPA_Player_Salary =
  union(MLSPA_Player_Salary_202209, MLSPA_Player_Salary202204)

MLSPA_Player_Salary = MLSPA_Player_Salary %>%
  mutate(Player = paste(MLSPA_Player_Salary$'First Name', MLSPA_Player_Salary$'Last Name', sep = " "))

MLSPA_Player_Salary = MLSPA_Player_Salary %>%
  relocate(c("Player", "Club", "Position", "2022 Base Salary", "2022 Guar. Comp."))

MLSPA_Player_Salary = rename(MLSPA_Player_Salary,
                             "Team" = "Club",
                             "Base Salary" = "2022 Base Salary",
                             "Guaranteed Compensation" = "2022 Guar. Comp.") %>%
  select(-c("Last Name", "First Name", Team, Position))

MLSPA_Player_Salary = MLSPA_Player_Salary %>%
  arrange(desc(Player)) %>%
  group_by(Player) %>%
  mutate(Rank = row_number(desc(Source))) %>%
  filter(Rank == 1)

Salary_Data =
  full_join(Player_Salary, MLSPA_Player_Salary %>% select(-c(Source, Rank)), by = 'Player')

Player_Data = Player_Goals_Added %>% 
  left_join(Salary_Data, by = "Player")

# Renaming fields
Player_Data = Player_Data %>% 
  mutate('Guaranteed Compensation.x' 
         = coalesce(Player_Data$'Guaranteed Compensation.x',Player_Data$'Guaranteed Compensation.y')) %>%
  mutate('Base Salary.x' 
         = coalesce(Player_Data$'Base Salary.x',Player_Data$'Base Salary.y')) %>%
  mutate('Position.x' 
         = coalesce(Player_Data$'Position.x',Player_Data$'Position.y')) %>%
  mutate('Team.x' 
         = coalesce(Player_Data$'Team.x',Player_Data$'Team.y')) %>%
  select(-c("Team.y",
            "Position.y",
            "Base Salary.y", 
            "Guaranteed Compensation.y")) %>% 
  rename("Position" = "Position.x",
         "Team" = "Team.x",
         "Base_Salary" = "Base Salary.x",
         "Guaranteed_Compensation" = "Guaranteed Compensation.x",
         "Goals_Added" = "Goals Added") %>% 
  drop_na(Base_Salary)

# Removing rows where there is no salary
Player_Data %>%
  filter(is.na(Base_Salary))

# Getting the log of the Goals Added metric so that it is on a 0 to 1 scale
Player_Data = data.frame(Player_Data %>% 
                           mutate(Log_Goals_Added = 
                                    log10(Goals_Added+(abs(min(Player_Data$Goals_Added))+1.01)))) %>%
  mutate(across('Log_Goals_Added', round, 4))

### Shiny App ###

shinyApp(
  
  # UI function
  ui = fluidPage(
    
    # changing the color of the background
    tags$head(tags$style(
      HTML('
         #sidebarPanel {
            background-color: #582C8350;
         }
        
         #title {
          color: #380F55;
         }')
    )),
    
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
                                 # max = 
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
    position_list = c(unique(Player_Data$Position))
    
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
        Filtered_Player_Data = 
          Player_Data %>%
          filter(Position %in% position_values$position) %>%
          select(c('Player',
                   'Position',
                   'Guaranteed_Compensation',
                   'Log_Goals_Added'))
        
        ### GA Functions
        ### Initialize population
        initializePopulation = function(k){
          
          function(GA){
            mat = matrix(0, ncol = GA@nBits, nrow = GA@popSize)
            
            for(i in seq_len(GA@popSize))
              mat[i, sample(GA@nBits, k)] = 1
            
            for (i in 1:length(position_values$input_value)) {
              if (position_values$input_value[i] > 0) {
                ind = which(Filtered_Player_Data$Position 
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
          current_goals_added=x%*%Filtered_Player_Data$Log_Goals_Added
          current_salary=x%*%Filtered_Player_Data$Guaranteed_Compensation
          positions = Filtered_Player_Data$Position[x == 1] # positions of selected players
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
          nBits = nrow(Filtered_Player_Data),
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
        Renamed_Player_Data = Filtered_Player_Data %>%
          select(c('Player',
                   'Position',
                   'Log_Goals_Added',
                   'Guaranteed_Compensation')) %>%
          rename('Goals Added' = 'Log_Goals_Added',
                 'Total Salary' = 'Guaranteed_Compensation')
  
        # displaying the algorithm's results on the UI
        output$table = renderDataTable(Renamed_Player_Data[GA@solution == 1,],
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