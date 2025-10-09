

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
fluidPage(
  
  theme = shinytheme("flatly"),
  

  
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
  
  # tags$head(tags$link (rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),
  #   tags$style(HTML("
  #     html, body, .tab-content, .tab-pane, .mainPanel {
  #       height: 100% !important;
  #     }
  #   "))
  # ),
  
  # Application title
  #titlePanel("Sustainable diet"),
  
  ######
  navbarPage(
    title = "Sustainable diet",
    
    tabPanel(
      title = "Introduction",
      fluidPage(
        tags$div(
          style = "max-width: 1500px; margin: auto; font-size: 16px; line-height: 1.6;",
          
          # --- Title ---
          tags$h2("Sustainable Diets: Conciliating nutritional aspects, health concerns, and environmental impacts to go towards tomorrow's diet"),
          tags$p(
            "Diet plays a crucial role in both human health and planetary well-being. 
        In the article, ",
            tags$em("“Health and nutritional aspects of sustainable diet strategies and their association with environmental impacts: a global modelling analysis with country-level detail”"),
            " (Springmann et al., 2018, ",
            tags$em("The Lancet Planetary Health"),
            "), the authors investigate how changes in diet across more than 150 countries 
        could lead to better nutrition, lower diet-related mortality, and reduced environmental pressures."
          ),
          tags$p(
            "This Shiny app makes those findings interactive and explorable — allowing users 
        to visualize, for each country, how adopting alternative diet scenarios could 
        simultaneously improve health, nutrition, and environmental outcomes."
          ),
          tags$br(), 
          tags$em("Data from:"),
          tags$blockquote(
            em("Springmann et al., Lancet Planetary Health (2018) - The health and nutritional aspects of sustainable diet strategies and their relationship to environmental impacts – a comparative global modelling analysis with country-level detail")
          ),
          br(),
          
          tabsetPanel(
            tabPanel("Study Objectives", 
                     tags$p("The study aimed to integrate three domains:"),
                     tags$ul(
                       tags$li(tags$b("Nutrition:"), " evaluate dietary adequacy and nutrient intake."),
                       tags$li(tags$b("Health:"), " estimate mortality changes linked to diet and body weight."),
                       tags$li(tags$b("Environment:"), " assess resource use and emissions from food production.")
                     ),
                     tags$p(
                       "The analysis compares current national diets with sustainable diet strategies 
        that could meet global health and environmental targets."
                     )
                     ),
          
           tabPanel(" Dietary Scenario", 
                    tags$p("The study modeled three broad strategies for transitioning toward sustainable diets:"),
                    tags$ol(
                      tags$li(tags$b("Environmental focus –"), 
                              " reducing animal-source foods by 25–100% to lower emissions and land use."),
                      tags$li(tags$b("Food-security focus –"), 
                              " reducing underweight, overweight, and obesity by 25–100%."),
                      tags$li(tags$b("Health focus –"), 
                              " adopting balanced diets (flexitarian, pescatarian, vegetarian, or vegan).")
                    ),
                    tags$p(
                      "All scenarios assume energy-balanced diets, adjusted to local food availability 
        and consumption patterns."
                    )),
           
           tabPanel("Health Impacts", 
                    tags$p(
                      "Health effects were estimated using a comparative risk assessment model 
        including nine diet- and weight-related risk factors 
        (e.g., fruit, vegetable, meat, sugar intake, and body weight distribution)."
                    )), 
           
           
           tabPanel(" Environmental Impact Indicators", 
                    tags$p("Five environmental dimensions were analyzed:"),
                    tags$ul(
                      tags$li("Greenhouse gas emissions (CO₂ eq)"),
                      tags$li("Cropland use"),
                      tags$li("Freshwater use"),
                      tags$li("Nitrogen application"),
                      tags$li("Phosphorus application")
                    )
                    ), 
           
           tabPanel("Purpose of This App", 
                    tags$p("This Shiny application was built to:"),
                    tags$ul(
                      tags$li("Display the country-level results from Springmann et al. (2018)."),
                      tags$li("Let users select a country and explore baseline health, nutrition, and environmental data."),
                      tags$li("Apply different diet scenarios and visualize projected outcomes."),
                      tags$li("Compare results across strategies, highlighting synergies and trade-offs."),
                      tags$li("Facilitate understanding of how dietary transitions can contribute to a healthier population and a more sustainable planet.")
                      )
            
            )

          


          )
        )
      )
    )
    ,
    tabPanel(title="Studied Countries", 
             
             h3("Interactive map of studied countries", 
                style = "text-align:center; font-weight:bold; margin-bottom:20px;"),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 #h4("Continent", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "continent", 
                   label = "Continent",
                   choices = sort(unique(world$region_un)), #levels(factor(world$region_un)), # valeurs initiales
                   # selected = "Africa",
                   options = pickerOptions(
                     actionsBox = TRUE,
                     liveSearch = TRUE,
                     noneSelectedText = "Select one or more continents",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
                 
                 #h4("Pays", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "country", 
                   label = "Country", 
                   choices = NULL, # rempli dynamiquement dans le serveur
                   options = pickerOptions(
                     actionsBox = TRUE,
                     liveSearch = TRUE,
                     noneSelectedText = "Select one or more countries",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
                 
                 #h4("Région économique", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "reg_eco", 
                   label = "Economical regions", 
                   choices = NULL, # rempli dynamiquement
                   options = pickerOptions(
                     actionsBox = TRUE,
                     liveSearch = TRUE,
                     noneSelectedText = "Select one or more economical regions",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
               ), 
               # Show a plot
               mainPanel(
                 tabsetPanel(
                   tabPanel("Studied Countries", leafletOutput("map", height = "500px"))

                 )
               ))),
    
    # onglets 1 et 2 
    ########
    
    tabPanel(title="Nutritional aspects",
               
             mainPanel(
               width = 12,
             
               tabsetPanel(
                 tabPanel("Data Manipulation",
                          
                          h3("Explore nutritional dataset", 
                             style = "text-align:center; font-weight:bold; margin-bottom:20px;"),
                          
                          sidebarLayout(
                            sidebarPanel(   # side bar dans
                              width = 3,
                          h3("Use this tool bar to navigate through nutritional data"),

                          # pickerInput(
                          #   inputId = "columnsdatanutri",
                          #   label = "Columns",
                          #   choices = names(nutri_new),
                          #   selected = names(nutri_new),
                          #   options = pickerOptions(
                          #     actionsBox = TRUE,
                          #     liveSearch = TRUE,
                          #     noneSelectedText = "Select printed columns",
                          #     size = 5
                          #   ),
                          #   multiple = TRUE  # plusieurs colonnes possibles
                          # ),
                          
                          tags$br(),
                          tags$p("How to navigate data:"),
                          tags$p("Item :"),
                          tags$ul(
                            tags$li("abs : value in units"),
                            tags$li("% rec : nutrient coverage in %, from nutritional recommandation"),
                            tags$li("pct : % of change from BMK")
                          ), 
                          tags$p("Diet Scenario :"),
                          tags$ul(
                            tags$li("BMK: current diet"),
                            tags$li("ani-25 => ani-100 : replacement from 25 to 100 % of animal-source foods with plant-based foods"),
                            tags$li("kcal-25 => kcal-100: reduced levels of underweight, overweight, and obesity by 25–100%"), 
                            tags$li("FLX: Flexitarian, PSC: Pescovegetarian, VEG: Vegetarian, VGN: Vegan")
                            
                          ), 
                          
                          ),
                          
                          mainPanel (
                            tabPanel("Data Manipulation",DTOutput("datatablenutri"))
                          

                          ))
                  ),
                 
                   tabPanel("PCA", 
                            
                            
                            h3("Principal Composant Analysis of nutrients values in choosen country", 
                               style = "text-align:center; font-weight:bold; margin-bottom:20px;"),
                            
                            sidebarLayout(
                              sidebarPanel(   # side bar dans
                                width = 2,
                                h3("Use this tool choose a country"),
                                
                                pickerInput(
                                  inputId = "country_pca_nutri",
                                  label = "Country",
                                  choices = sort(unique(nutri_new$code_pays)),
                                  selected = "FRP",
                                  options = pickerOptions(
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    noneSelectedText = "Select a country",
                                    size = 5
                                  ),
                                  multiple = FALSE  # plusieurs colonnes possibles
                                )),
                              
                              mainPanel (
                                tabPanel("PCA", 
                                         fluidRow(
                                           column(
                                             width = 6,
                                             plotOutput("plot1", height = "600px", width = "100%")
                                           ),
                                           column(
                                             width = 6,
                                             plotOutput("plot2", height = "600px", width = "100%")
                                           )
                                         )), 
                                width = 9,
                                
                                
                              )
                   )),
                 
                 tabPanel("Nutritional qualities of diets",
                          
                          h3("Visualize differences in nutritional qualities of diets in chosen country", 
                             style = "text-align:center; font-weight:bold; margin-bottom:20px;"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              h3("Use this tool bar to print graphs showing per country the % of nutrient coverage of different diets scenari"),
              
                             pickerInput(
                                inputId = "paysnutri", 
                                label = "Country",
                                selected= "FRP",
                                choices = sort(unique(nutri_new$code_pays)), 
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Select a studied country",
                                  size = 5
                                ), 
                                multiple = FALSE  # On ne peut chosisir qu'un pays 
                              ),
                              
                              pickerInput(
                                inputId = "nutriments_nutri", 
                                label = "Nutrient",
                                selected = names(nutri_new[,4:15]),                                
                                choices = names(nutri_new[,4:27]), 
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Select one or more nutrients",
                                  size = 5
                                ), 
                                multiple = TRUE
                              ),
                              
                              pickerInput(
                                inputId = "diets_nutri", 
                                label = "Diets",
                                selected = c("ani-100","ani-25","ani-50","ani-75","BMK","FLX","kcal-100","kcal-25","kcal-50" ,"kcal-75","PSC","VEG","VGN"),
                                choices = levels(nutri_new$diet.scenario), 
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Select one or more diets",
                                  size = 5
                                ), 
                                multiple = TRUE
                              ),
                              
                              pickerInput(
                                inputId = "ncolnutri", 
                                label = "Number of columns",
                                selected = 3, 
                                choices = c(1,2,3,4,5,6), 
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Pick the number of columns to display graphs",
                                  size = 5
                                ), 
                                multiple = FALSE
                              )
                              
                              
                              
                              
                            ),
                            mainPanel(
                              tabPanel("Nutritional qualities of diets", plotOutput("plot", height = "600px")), 
                              width = 9, 
                            )
                          )
                 )
               )
    )
             ),
    
    ###############################################################
    # Onglet ENVIRONMENTAL ASPECTS
    ###############################################################
    
    tabPanel(title="Environmental aspects",
             
             mainPanel(
               width = 12,
               
               tabsetPanel(
                 tabPanel(
                   "Data Manipulation",
                   
                   h3(
                     "Explore environmental dataset", 
                     style = "text-align:center; font-weight:bold; margin-bottom:20px;"
                   ),
                   
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       
                       h3("Use this tool bar to navigate through nutritional data"),
                       tags$br(),
                       
                       tags$p("How to navigate data:"),
                       
                       # section socio-econ scenario
                       tags$p("Socio-econ scenario :"),
                       tags$ul(
                         tags$li("SSP2 : middle-of-the-road socio-economic development pathway"),
                         tags$li("SSP1 : pathway with greater income and lower population growth"),
                         tags$li("SSP3 : pathway with greater population and lower income growth")
                       ),
                       
                       # Item
                       tags$p("Item :"),
                       tags$ul(
                         tags$li("abs : value in units"),
                         tags$li("% rec : nutrient coverage in %, from nutritional recommendation"),
                         tags$li("pct : % of change from BMK")
                       ),
                       
                       # Diet scenario
                       tags$p("Diet Scenario :"),
                       tags$ul(
                         tags$li("BMK: current diet"),
                         tags$li("ani-25 => ani-100 : replacement from 25 to 100 % of animal-source foods with plant-based foods"),
                         tags$li("kcal-25 => kcal-100: reduced levels of underweight, overweight, and obesity by 25–100%"),
                         tags$li("FLX: Flexitarian, PSC: Pescovegetarian, VEG: Vegetarian, VGN: Vegan")
                       )
                     ),
                     
                     mainPanel(
                       width = 9,
                       DTOutput("datatable_env")
                     )
                   )
                 ),
                 
                 tabPanel("PCA",
                          
                          # Titre
                          tags$div(
                            "Principal Component Analysis of Environmental Indicators",
                            style = "text-align: center; 
             font-weight: bold; 
             font-size: 20px; 
             color: black; 
             margin-bottom: 25px;"
                          ),
                          
                          sidebarLayout(
                            # Barre latérale
                            sidebarPanel(
                              h4("Use this tool to choose a country"),
                              selectInput(
                                inputId = "country_env",
                                label = "Country",
                                choices = unique(env_new$code_pays),
                                selected = "FRP"
                              )
                            ),
                            
                            # Graphique principal
                            mainPanel(
                              plotOutput("pca_env_plot", height = "600px")
                            )
                          )
                 ),
                 
                 
                 tabPanel("Environmental data Vizualisation",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              
                              pickerInput(
                                inputId = "paysenv", 
                                label = "Country",
                                selected = "FRP",
                                choices = sort(unique(env_new$code_pays)),
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Select one or more countries"
                                ),
                                multiple = FALSE
                              ),
                              
                              pickerInput(
                                inputId = "indicateurs_env",
                                label = "Environmental indicators",
                                selected = c("GHGe", "land", "water", "nitr", "phos"),
                                choices = c("GHGe", "land", "water", "nitr", "phos"),
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Select one or more indicators"
                                ),
                                multiple = TRUE
                              ),
                              
                              pickerInput(
                                inputId = "diets_env",
                                label = "Diets",
                                selected = c("ani-100","ani-25","ani-50","ani-75","FLX","kcal-100","kcal-25","kcal-50","kcal-75", 
                                             "PSC","VEG","VGN"),
                                choices = c("ani-100","ani-25","ani-50","ani-75","FLX","kcal-100","kcal-25","kcal-50","kcal-75", 
                                            "PSC","VEG","VGN"),
                                options = pickerOptions(
                                  actionsBox = TRUE,
                                  liveSearch = TRUE,
                                  noneSelectedText = "Select one or more diets"
                                ),
                                multiple = TRUE
                              ),
                              
                              pickerInput(
                                inputId = "ncolenv",
                                label = "Number of columns",
                                selected = 3,
                                choices = c(1, 2, 3, 4, 5),
                                multiple = FALSE
                              )
                            ),
                            
                            mainPanel(
                              tabPanel("Environmental data Vizualisation", plotOutput("plot_env", height = "600px")), 
                              width = 9
                            
                          )
                 )
               )
             )
    )

    
    
    
  ),
  ###############################################################
  # Onglet GLOBAL ANALYSIS
  ###############################################################
  
  tabPanel("Global Analysis",
           
           
           h3("Multiple Factor Analysis (MFA) on All Domains: Nutritional, Environmental, and Health", 
              style = "text-align:center; font-weight:bold; margin-bottom:20px;"),
           
           
           sidebarLayout(
             sidebarPanel(
               h4("Use this tool to choose a country"),
               selectInput(
                 inputId = "country_mfa",
                 label = "Country",
                 choices = unique(env_new$code_pays),
                 selected = "FRP"
               )
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Individuals", plotOutput("plot_mfa_ind")),
                 tabPanel("Groups", plotOutput("plot_mfa_group")),
                 tabPanel("Correlation Circle", plotOutput("plot_mfa_score")),
                 tabPanel("Partial Individuals", plotOutput("plot_mfa_partial"))
               )
             )
           )
  )
  
  
  
  
)

)
