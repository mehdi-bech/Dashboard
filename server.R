source("ML\\imports.R")
source("EDA\\univariate.R")
source("EDA\\Bivariate.R")
source("ML\\regression_logistique.R")
source("ML\\SVM.R")
source("ML\\random_forest.R")

server <- function(input, output, session) {
  
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

    output$describetable <- renderTable({
        describetable(input$SelectBiv1,input$SelectBiv2)
    })

    output$tab_test <- renderTable({
        tab_test(input$SelectBiv1,input$SelectBiv2)
    })

    output$plt_box_bar <- renderPlot({
        plt_box_bar(input$SelectBiv1,input$SelectBiv2)
    })
    
    # Contextes
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
      pre2(input$ka)
    })
    output$pre3 = renderText({
      pre3()
    })
    
    # Recall
    output$rec1 = renderText({
      rec1()
    })
    output$rec2 = renderText({
      rec2(input$ka)
    })
    output$rec3 = renderText({
      rec3()
    })
    
    # fscore
    output$fsco1 = renderText({
      fsco1()
    })
    output$fsco2 = renderText({
      fsco2(input$ka)
    })
    output$fsco3 = renderText({
      fsco3()
    })
    
    # spécifité
    output$spe1 = renderText({
      spe1()
    })
    output$spe2 = renderText({
      spe2(input$ka)
    })
    output$spe3 = renderText({
      spe3()
    })
    
    # accuracy
    output$acc1 = renderText({
      acc1()
    })
    output$acc2 = renderText({
      acc2(input$ka)
    })
    output$acc3 = renderText({
      acc3()
    })

    # Matrice de confusion 
    output$mc1 = renderTable({
      cm1()
    })
    output$mc2 = renderTable({
      cm2(input$ka)
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
    
    # AUC
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
      table_metr2(input$ka)
    })
    output$tablemet3 = renderTable({
      table_metr3()
    })
    
    # Affichage des interprétations
    
    observeEvent(input$goUni, {
      showModal(modalDialog(
        size='l',
        includeHTML("Interpretations\\Uni.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    observeEvent(input$goBiv, {
      showModal(modalDialog(
        size='l',
        includeHTML("Interpretations\\Biv.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$go1, {
      showModal(modalDialog(
        size='l',
        includeHTML("Interpretations\\Inter1.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    observeEvent(input$go2, {
      showModal(modalDialog(
        size='l',
        includeHTML("Interpretations\\Inter2.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$go3, {
      showModal(modalDialog(
        size='l',
        includeHTML("Interpretations\\Inter3.html"),
        footer = NULL,
        easyClose = TRUE
      ))
    })
  observe({
      
      b=input$SelectBiv1
      updateSelectInput(session, "SelectBiv2",
                               choices = a[a != b])
                            
    
})
}