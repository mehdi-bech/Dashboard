source("imports.R")

ui <- fluidPage(

  titlePanel("Dashboard"),

  navbarPage("Menu",
    
############################################## Page ##############################################  
    tabPanel("Objectif",
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
        tabPanel("Analyse unidimensionnelle",
          sidebarLayout(
            sidebarPanel(
              selectInput("SelectUniv",
                          label = h4("Sélectionner une variable pour l'analyse univariée"),
                          choices = sort(names(df)),
                          selected=1
                                )
              ),
            mainPanel(
              fluidRow(
                  column(6, plotOutput(outputId = "oplot_box_bar")),
                  column(6, plotOutput(outputId = "oplot_histo_pie"))
                      ),
              fluidRow(
                  column(6, plotlyOutput(outputId = "oplot_Cummul")),
                  column(6, tableOutput(outputId = "oStat"))
                      )
            )
          )
        ),
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
