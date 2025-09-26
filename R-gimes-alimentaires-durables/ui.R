

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
fluidPage(
  
  tags$style(HTML("
      @media (min-width: 768px) {
        .col-sm-4 {
          flex: 0 0 20% !important;
          max-width: 20% !important;
        }
        .col-sm-8 {
          flex: 0 0 80% !important;
          max-width: 80% !important;
        }
      }
    ")),
  
  # Application title
  #titlePanel("Sustainable diet"),
  
  navbarPage(
    title = "Sustainable diet",
    tabPanel(title="Accueil",
             "Hereisthesummary: du texte, que du texte,
               \n Data source:							
\n Springmann et al, Lancet Planetary Health 2018, The health and nutritional aspects of sustainable diet strategies and their relationship to environmental impacts – a comparative global modelling analysis with country-level detail							
							
\n Tabs:							
							
\n \n> General:							
Scenarios: description of scenarios							
Regions: regional aggregation							
							
> Nutrition-related:							
nutrition_notes: sources and units for nutrient analysis							
nutrition_table: pivot table with results of nutrient analysis							
nutrition_data: raw data of nutrient analysis							
							
> Mortality-related:							
health_table: pivot table with results of mortality analysis							
health_data: raw data of mortality analysis							
							
> Environment-related:							
environment_table: pivot table with results of environmental analysis							
environment_data: raw data of environmental analysis"),
    tabPanel(title="Onglet 2", 
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 20,
                             value = 5),
                 
                 # 1
                 h4("Pays", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "levels", 
                   label = "Régions éco", 
                   choices = NULL,
                   options = pickerOptions(
                     actionsBox = FALSE,
                     liveSearch = TRUE,
                     noneSelectedText = "j",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
                 
                 # 2
                 h4("Pays", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "levels", 
                   label = "Régions éco", 
                   choices = NULL,
                   options = pickerOptions(
                     actionsBox = FALSE,
                     liveSearch = TRUE,
                     noneSelectedText = "j",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
                 
                 # 3
                 h4("Pays", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "levels", 
                   label = "Régions éco", 
                   choices = NULL,
                   options = pickerOptions(
                     actionsBox = FALSE,
                     liveSearch = TRUE,
                     noneSelectedText = "j",
                     size = 5
                   ), 
                   multiple = TRUE
                 )
               ), 
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Accueil", plotOutput("distPlot")),
                   tabPanel("Summary",verbatimTextOutput("summary")),
                   tabPanel("Table", tableOutput("table"))
                 )
               ))
    ),
    tabPanel(title="Onglet 3",
             "Hereisthesummary"),
    
    
  )
)

