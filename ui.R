source("imports.R")
source("random_forest.R")
source("regression_logistique.R")
source("SVM.R")
choi=function()
{
  a=sort(names(df)) != input$SelectBiv1
  return(a)
}
ui <- fluidPage(

  titlePanel(title = span(img(src = "urinary-removebg-preview.png", height = 90), 
                          "Diagnostic des inflammations aiguës de la vessie urinaire")),
  br(),
  theme = bs_theme(bootswatch = "united",
                   base_font = font_google("Montserrat", local = TRUE)),
  
  navbarPage(
    "MENU",
    
############################################## Page ##############################################  
    tabPanel("Objectif",
      sidebarLayout(

        sidebarPanel( 
          
          includeHTML( "Objectif text.html")         
          ,
        ),
        mainPanel(
          h4(
          includeHTML("table.html"),
          h3("\n"),
          actionButton("go", "Afficher la Base de Données")
            )
                  )             
                    )

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
        tabPanel('Analyse bidimensionnelle', 
          selectInput("SelectBiv1",
                          label = h4("Sélectionner la première variable pour l'analyse bivariée"),
                          choices = a,
                          selected=1
                                ),
          selectInput("SelectBiv2",
                          label = h4("Sélectionner la seconde variable pour l'analyse bivariée"),
                          choices =  a,
                          selected=2
                                )
          )
                  )
            ),
############################################## Page ##############################################  
    
    tabPanel("Modèles de classification supervisée",
      tabsetPanel(
        tabPanel("Régression logistique",
                 sidebarLayout(
                   sidebarPanel(
                     h4('Contexte :'),
                     br(),
                     htmlOutput(outputId = 'rl_info'),
                     br()),
                   mainPanel(
                     fluidRow(
                       column(6, plotOutput(outputId = "ROC1")),
                       column(6, plotOutput(outputId = "AUC1"))
                     ),
                     fluidRow(
                       column(6,tableOutput(outputId = "mc1")),
                       column(6,tableOutput(outputId = "tablemet1")),
                       h2("\n"),
                       actionButton("go1", "Afficher les interprétations"),
                     )
                   )
                 )
        ),
        tabPanel("Forêt d'arbres de décision (Random Forest)", 
                 sidebarLayout(
                   sidebarPanel(
                     h4('Contexte :'),
                     br(),
                     htmlOutput(outputId = 'rf_info'),
                     br(),
                     sliderInput(
                       'ka',
                       strong("Selectionner le Nombre d'arbres de décision :"),
                       value = 10,
                       min = 1,
                       max = 40)),
                   mainPanel(
                     fluidRow(
                       column(6, plotOutput(outputId = "ROC2")),
                       column(6, plotOutput(outputId = "AUC2")),
                     ),
                     fluidRow(
                         column(6,tableOutput(outputId = "mc2")),
                         column(6,tableOutput(outputId = "tablemet2")),
                         h2("\n"),
                         actionButton("go2", "Afficher les interprétations"),
                     )
                   )
                 )
        ),
        tabPanel('Machine à vecteurs de support (SVM)', 
                 sidebarLayout(
                   sidebarPanel(
                     h4('Contexte :'),
                     br(),
                     htmlOutput(outputId = 'svm_info'),
                     br()),
                   mainPanel(
                     fluidRow(
                       column(6, plotOutput(outputId = "ROC3")),
                       column(6, plotOutput(outputId = "AUC3"))
                     ),
                     fluidRow(
                       column(6,tableOutput(outputId = "mc3")),
                       column(6,tableOutput(outputId = "tablemet3")),
                       h2("\n"),
                       actionButton("go3", "Afficher les interprétations"),
                     )
                   )
                 )
        )
        
      )
    ),
inverse = T)
)

