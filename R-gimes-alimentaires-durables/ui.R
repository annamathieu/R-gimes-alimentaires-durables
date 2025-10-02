

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
    tabPanel(title="Onglet 2", 
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 #h4("Continent", class = "mt-3 mb-2 text-secondary"),
                 pickerInput(
                   inputId = "continent", 
                   label = "Continent",
                   choices = sort(unique(world$region_un)), #levels(factor(world$region_un)), # valeurs initiales
                   selected = "Africa",
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
                 )
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
    tabPanel(title="Onglet 3",
             "Hereisthesummary"),
    
  )
)

