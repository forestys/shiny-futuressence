# Define server logic required to draw a histogram
function(input, output, session) {
  
  # link to www directory and objects
  addResourcePath("www", system.file("www", package="spfuturessence"))
  output$img_logo<-renderUI(
    img(src='www/img/logo_forestys.png',height='100',width='200')
  )
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}