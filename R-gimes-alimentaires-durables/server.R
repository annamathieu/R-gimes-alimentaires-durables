#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  # Réactif qui filtre world_new selon le continent sélectionné
  filtered_data <- reactive({
    req(input$continent)
    world[world$region_un %in% input$continent, ]
  })
  
  # Met à jour dynamiquement les pays en fonction du continent
  observeEvent(input$continent, {
    updatePickerInput(session, "country",
                      choices = as.list(levels(factor(filtered_data()$nom_pays))))
  })
  
  # Met à jour dynamiquement les régions économiques
  observeEvent(input$continent, {
    updatePickerInput(session, "reg_eco",
                      choices = as.list(levels(factor(filtered_data()$nom_pays))))
  })
  
  # Utilise les valeurs sélectionnées pour appeler la fonction
  selected_data <- reactive({
    studiedcountries(
      conti = conti,
      contin = input$continent,
      country = input$country,
      reg_eco = input$reg_eco
    )
  })
  
  # Affichage de la carte
  output$map <- renderLeaflet({
    selected_data()
  })

}
