source("imports.R")
source("univariate.R")
source("regression_logistique.R")

server <- function(input, output) {
  
  # Affichage de la base de donnees
  observeEvent(input$go, {
    showModal(modalDialog(
      size='l',
      renderDataTable(df),
      footer = NULL,
      easyClose = TRUE
    ))
  })

  # Occurences (effectifs) plot for univariate analyzis
    # Depending on the variable type, it is either a boxplot or a barplot
    output$oplot_box_bar <- renderPlot({
        plot_box_bar(input$SelectUniv)
    })
    
    # Frequencies plot
    # Depending on variable type, it is either a barplot or a pie chart
    output$oplot_histo_pie <- renderPlot({
        plot_histo_pie(input$SelectUniv)
    })
    
    # Cumulative occurences plot
    output$oplot_Cummul <- renderPlotly({
        plot_Cummul(input$SelectUniv)
    })
    
    output$oStat <- renderTable({
        statQ(input$SelectUniv)
    })
    
    # Contexte 
    output$rl_info = renderText({
      context()
    })
 
    # Contexte 
    output$rf_info = renderText({
      context()
    })
    
    # precision 
    output$pre2 = renderText({
      pre()
    })
    
    # Recall
    output$rec2 = renderText({
      rec()
    })
    
    # fscore
    output$fsco2 = renderText({
      fsco()
    })
    
    # accuracy
    output$acc2 = renderText({
      acc()
    })

    # Parametres d'evaluation du modèle 
    output$param2 = renderTable({
      car2()
    })
    
    # ROC random Forest
    output$ROC2 = renderPlot({
      roc2(input$ka)
    })
    
    # Illustration de la valeur AUC
    output$AUC2 = renderPlot({
      auc2(input$ka)
    })
    
    # ROC random Forest
    output$ROC1 = renderPlot({
      roc1()
    })
    
    # Illustration de la valeur AUC
    output$AUC1 = renderPlot({
      auc1()
    })
    

    


}