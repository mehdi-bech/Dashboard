source("imports.R")

ui <- fluidPage(

  titlePanel("Dashboard"),

  navbarPage("Menu",
############################################## Page ##############################################  

    tabPanel("Objectif tyehd",
      sidebarLayout(

        sidebarPanel( 
          h5(
          includeHTML("Objectif text.html"),          
          ),
        ),
        mainPanel(
          h5(
          includeHTML("Table.html"),
          h2("\n"),
          actionButton("go", "Show")
            ),
                  )             
                    ),

            ),

############################################## Page ##############################################
    
    tabPanel("Analyse exploratoire des données",
      tabsetPanel(
        tabPanel("Analyse unidimensionnelle","Empty"),
        tabPanel('Analyse bidimensionnelle', "Empty")
                  )
            ),
############################################## Page ##############################################  
    
    tabPanel("Entraınement de modeles",
      tabsetPanel(
        tabPanel("Classification supervisee","Empty"),
        tabPanel('Evaluation', "Empty")
                  )
            )
    )
)
