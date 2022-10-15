# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- read_csv("Data\\Inflammation.csv")

  observeEvent(input$go, {
    showModal(modalDialog(
      size='l',
      renderDataTable(df),
      footer = NULL,
      easyClose = TRUE
    ))
  })
}