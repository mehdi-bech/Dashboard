source("imports.R")
source("univariate.R")
source("regression_logistique.R")
source("SVM.R")

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
      context1()
    })
    output$rf_info = renderText({
      context2()
    })
    output$svm_info = renderText({
      context3()
    })
    
    # precision 
    output$pre1 = renderText({
      pre1()
    })
    output$pre2 = renderText({
      pre2()
    })
    output$pre3 = renderText({
      pre3()
    })
    
    # Recall
    output$rec1 = renderText({
      rec1()
    })
    output$rec2 = renderText({
      rec2()
    })
    output$rec3 = renderText({
      rec3()
    })
    
    # fscore
    output$fsco1 = renderText({
      fsco1()
    })
    output$fsco2 = renderText({
      fsco2()
    })
    output$fsco3 = renderText({
      fsco3()
    })
    
    # accuracy
    output$acc1 = renderText({
      acc1()
    })
    output$acc2 = renderText({
      acc2()
    })
    output$acc3 = renderText({
      acc3()
    })

    # Matrice de confusion 
    output$mc1 = renderTable({
      cm1()
    })
    output$mc2 = renderTable({
      cm2()
    })
    output$mc3 = renderTable({
      cm3()
    })
    
    # ROC 
    output$ROC1 = renderPlot({
      roc1()
    })
    output$ROC2 = renderPlot({
      roc2(input$ka)
    })
    output$ROC3 = renderPlot({
      roc3()
    })
    
    # Illustration de la valeur AUC
    output$AUC1 = renderPlot({
      auc1()
    })
    output$AUC2 = renderPlot({
      auc2(input$ka)
    })
    output$AUC3 = renderPlot({
      auc3()
    })
    
    # Table des metriques
    output$tablemet1 = renderTable({
      table_metr1()
    })
    output$tablemet2 = renderTable({
      table_metr2()
    })
    output$tablemet3 = renderTable({
      table_metr3()
    })
    
    # Affichage des interprétations
    observeEvent(input$go1, {
      showModal(modalDialog(
        size='l',
        renderText("interp1.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    observeEvent(input$go2, {
      showModal(modalDialog(
        size='l',
        renderText("interp2.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    observeEvent(input$go3, {
      showModal(modalDialog(
        size='l',
        renderText("interp3.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    

}