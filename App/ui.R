

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
    tabPanel(title = "Main",
             titlePanel("Bienvenue dans l'application Sustainable Diet"),
             
             fluidRow(
               column(
                 width = 10,
                 h3("Présentation de l'application"),
                 p("Cette application interactive présente les résultats d'une étude sur les régimes alimentaires durables."),
                 p("Elle se base sur les données de l'étude suivante :"),
                 tags$blockquote(
                   em("Springmann et al., Lancet Planetary Health (2018) - The health and nutritional aspects of sustainable diet strategies and their relationship to environmental impacts – a comparative global modelling analysis with country-level detail")
                 ),
                 br(),
                 
                 h4("Structure de l'application"),
                 
                 tags$ul(
                   tags$li(strong("Général :"), " Scenarios et regroupements régionaux."),
                   tags$li(strong("Nutrition :"),
                           tags$ul(
                             tags$li("Notes sur les sources et unités d'analyse nutritionnelle"),
                             tags$li("Tableau croisé des résultats nutritionnels"),
                             tags$li("Données brutes de l'analyse nutritionnelle")
                           )
                   ),
                   tags$li(strong("Santé / Mortalité :"),
                           tags$ul(
                             tags$li("Tableau croisé des résultats de mortalité"),
                             tags$li("Données brutes de l'analyse de mortalité")
                           )
                   ),
                   tags$li(strong("Environnement :"),
                           tags$ul(
                             tags$li("Tableau croisé des résultats environnementaux"),
                             tags$li("Données brutes de l'analyse environnementale")
                           )
                   )
                 ),
                 br(),
                 h5("Navigation"),
                 p("Utilisez les onglets ci-dessus pour explorer les différentes dimensions de l'analyse.")
               )
             )
    ),
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
                          
                          h3("Explore nutritional data", 
                             style = "text-align:center; font-weight:bold; margin-bottom:20px;"),
                          
                          sidebarLayout(
                            sidebarPanel(   # side bar dans
                              width = 2,
                          h3("Use this tool bar to print nutritional data"),

                          pickerInput(
                            inputId = "columnsdatanutri",
                            label = "Columns",
                            choices = names(nutri_new),
                            selected = names(nutri_new),
                            options = pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              noneSelectedText = "Select printed columns",
                              size = 5
                            ),
                            multiple = TRUE  # plusieurs colonnes possibles
                          )),
                          
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
                 tabPanel("Data Manipulation",
                          fluidRow(
                            column(
                              width = 12,
                              style = "padding-left:40px; padding-right:40px;",
                              DTOutput("datatable_env", width = "100%")
                            )
                          )
                 ),
                 
                 tabPanel("PCA",
                          
                          # Titre du sous-onglet
                          titlePanel("Principal Component Analysis of Environmental Indicators"),
                          
                          sidebarLayout(
                            # Barre à gauche
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
                              h3("Relationships and projections of environmental indicators by diet"),
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

    
    
    
  )
)

)
