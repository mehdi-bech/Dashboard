
source("ML\\imports.R")
source("EDA\\univariate.R")
source("EDA\\Bivariate.R")
source("ML\\regression_logistique.R")
source("ML\\SVM.R")
source("ML\\random_forest.R")

choi=function()
{
  a=sort(names(df)) != input$SelectBiv1
  return(a)
}
ui <- fluidPage(

  titlePanel(title = span(img(src = "urinary-removebg-preview.png", height = 100), 
                          "Diagnostic des inflammations aiguës de la vessie urinaire")),
  br(),
  theme = bs_theme(bootswatch = "united",
                   base_font = font_google("Montserrat", local = TRUE)),
  
  navbarPage(
    "MENU",
    
############################################## Page ##############################################  
    tabPanel("Données et objectif",
      sidebarLayout(

        sidebarPanel( 
          
          includeHTML( "html files\\Objectif text.html")         
          ,
        ),
        mainPanel(
          h4(
          includeHTML("html files\\table.html"),
          h3("\n"),
          actionButton("go", "Afficher la Dataset")
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
                  column(4, plotOutput(outputId = "oplot_box_bar")),
                  column(4, plotOutput(outputId = "oplot_histo_pie")),
                  column(4, plotlyOutput(outputId = "oplot_Cummul")),
                  
                  hr(),
                  br()
                      ),
              fluidRow(
                  column(6,offset = 3, tableOutput(outputId = "oStat")),
                  h2("\n"),
                  actionButton("goUni", "Afficher les interprétations"),
                      )
            )
          )
        ),
        tabPanel('Analyse bidimensionnelle', 
          sidebarLayout(
            sidebarPanel(
              selectInput("SelectBiv1",
                          label = h4("Sélectionner la première variable :"),
                          choices = a,
                          selected=1
                                ),
              selectInput("SelectBiv2",
                          label = h4("Sélectionner la seconde variable :"),
                          choices =  a,
                          selected=2
                                )
          ),
            mainPanel(
              fluidRow(
                  column(5,offset= 3, plotOutput(outputId = "plt_box_bar")),
                  
                  hr(),
                      ),
              fluidRow(
                  column(5,offset = 2, tableOutput(outputId = "describetable")),
                  column(2,tableOutput(outputId = "tab_test")),
                  h2("\n"),
                  actionButton("goBiv", "Afficher les interprétations"),
              )
            )
            ))
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
                       column(6, plotOutput(outputId = "AUC1")),
                       hr(),
                     ),
                     fixedRow(
                       column(6, offset= 2, tableOutput(outputId = "mc1")),
                       column(3, tableOutput(outputId = "tablemet1")),
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
                       hr(),
                     ),
                     fluidRow(
                         column(6, offset= 2,tableOutput(outputId = "mc2")),
                         column(3,tableOutput(outputId = "tablemet2")),
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
                       column(6, plotOutput(outputId = "AUC3")),
                       hr(),
                     ),
                     fluidRow(
                       column(6, offset= 2,tableOutput(outputId = "mc3")),
                       column(3,tableOutput(outputId = "tablemet3")),
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

