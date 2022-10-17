source("imports.R")
source("univariate.R")

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


}