

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
  
  # tags$head(
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
    tabPanel(title = "Accueil",
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
                     noneSelectedText = "Sélectionnez un continent",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
                 
                 #h4("Pays", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "country", 
                   label = "Pays", 
                   choices = NULL, # rempli dynamiquement dans le serveur
                   options = pickerOptions(
                     actionsBox = TRUE,
                     liveSearch = TRUE,
                     noneSelectedText = "Sélectionnez un pays",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
                 
                 #h4("Région économique", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "reg_eco", 
                   label = "Région économique", 
                   choices = NULL, # rempli dynamiquement
                   options = pickerOptions(
                     actionsBox = TRUE,
                     liveSearch = TRUE,
                     noneSelectedText = "Sélectionnez une région éco",
                     size = 5
                   ), 
                   multiple = TRUE
                 ),
               ), 
               # Show a plot
               mainPanel(
                 tabsetPanel(
                   tabPanel("Carte", leafletOutput("map", height = "500px")),
                   tabPanel("Other", tableOutput("table")),
                   tabPanel("Table", tableOutput("table"))
                 )
               ))
    ),
    
    # onglets 1 et 2 
    ########
    
    tabPanel(title="Nutritional aspects", 
             
             mainPanel(
               width= 12, 
               
               
               tabsetPanel(
                 tabPanel("Data Manipulation",
                          h3("Contenu de l'onglet 1"),
                          p("Pas de sidebar ici")
                 ),
                 
                 tabPanel("PCA"),
                 
                 tabPanel("Nutritional qualities of diets",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              
                              pickerInput(
                                inputId = "paysnutri", 
                                label = "Pays",
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
                                label = "Nutriment",
                                selected = names(nutri_new[,5:14]),                                
                                choices = names(nutri_new[,5:14]), 
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
             
    )
  )
)

