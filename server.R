source("imports.R")

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

  # Affichage de la table statistiques pour les donees quantitatives
  quantitativeTabStats <- reactive({
      data.frame(
          stat = c('Min', 'Quantile 25%', 'Médiane', 'Quantile 75%', 'Max', 'Moyenne', 'Écart type'),
          values = c(quantile(df[, input$univariateSelect]), mean(df[, input$univariateSelect]), sd(df[, input$univariateSelect]))
      )
  })

      # Reactive variable giving frequency measures on modalities of qualitative variables
    qualitativeTabStats <- reactive({
        table.tmp <- as.data.frame(table(df[, input$univariateSelect]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        table.tmp <- cbind(table.tmp,
                           table.tmp[[2]]/nrow(df)*100,
                           table.tmp[[3]]/nrow(df)*100)
        colnames(table.tmp) <- c(input$univariateSelect, "Effectifs", "Effectifs Cum.",
                                 "Fréquences", "Fréquences Cum.")
        table.tmp
    })  


}