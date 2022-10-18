source("imports.R")
source("random_forest.R")

ui <- fluidPage(

  titlePanel("Diagnostic des inflammations aiguës de la vessie urinaire"),
  br(),
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("PT Serif", local = TRUE)),
  
  navbarPage(
    "Menu",
    
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
    
    tabPanel("Modèles de classification supervisée",
      tabsetPanel(
        tabPanel("Régression logistique",
                 sidebarLayout(
                   sidebarPanel(
                     h3('Contexte :'),
                     br(),
                     htmlOutput(outputId = 'classif_info'),
                     br()),
                   mainPanel(
                     fluidRow(
                       column(6, plotOutput(outputId = "ROC1")),
                       column(6, plotOutput(outputId = "AUC1"))
                     ),
                     fluidRow(
                       column(6, plotOutput(outputId = "param1"))
                       )
                   )
                 )
        ),
        tabPanel("Forêt d'arbres de décision (Random Forest)", 
                 sidebarLayout(
                   sidebarPanel(
                     h3('Contexte :'),
                     br(),
                     htmlOutput(outputId = 'classif_info'),
                     br(),
                     sliderInput(
                       'ka',
                       strong("Selectionner le Nombre d'arbres de décision :"),
                       value = 10,
                       min = 1,
                       max = 100)),
                   mainPanel(
                     fluidRow(
                       column(6, plotOutput(outputId = "ROC2")),
                       column(6, plotOutput(outputId = "AUC2"))
                     ),
                     fluidRow(
                       column(6, tableOutput(outputId = "param2"))
                     )
                   )
                 )
        ),
        tabPanel('Machine à vecteurs de support (SVM)', 
                 "Empty")
                  )
            )
      )
)
