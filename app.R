library(shinyWidgets)
library(psych)
options(mc.cores =1)


ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    fileInput(
      'file1',
      'Choose file to upload',
      accept = c('text/csv',
                 'text/comma-separated-values',
                 '.csv',
                 '.tsv')
    ),
    awesomeRadio(
      "separator",
      "Separator: ",
      choices = c(
        Semicolon = ';',
        Comma = ',',
        Colon = ':',
        Tab = '\t'
      ),
      selected = ',',
      inline = TRUE,
      status = "success"
    ),
    p(
      "Are the table values quoted (e.g.; \"1\",\"2\",...)? Indicate below.",
      class = "instructText"
    ),
    awesomeRadio(
      'quote',
      'Quote',
      c(
        None = '',
        'Double Quote' = '"',
        'Single Quote' = "'"
      ),
      '',
      inline = TRUE,
      status = "success"
    ),
    p(
      "Does your data have a header column with the variable names listed?",
      class = "instructText"
    ),
    awesomeCheckbox("header", "Header", TRUE, status = "success"),
    
    p(
      "Choose the correlation method you wish to use",
      class = "instructText"
    ),
   
    awesomeRadio(
      "method",
      "Method: ",
      choices = c(
        "Pearson" = 'pearson',
        "Spearman" = 'spearman',
        "Kendall Tau" = 'kendall'
      ),
      selected = "pearson" ,
      inline = TRUE,
      status = "success"
    ),
    
    p("Do you want to bootstrap the confidence intervals?"),
    awesomeCheckbox("btstpY", "Bootstrapped CIs", FALSE, status = "success"),
 
    conditionalPanel(
      condition = "input.btstpY == 1",
      
        "Bootstrap Samples",
        sliderInput(
          "btsn",
          label = "Set the number of bootstrap samples",
          value = 5000,
          step = 100,
          min = 200,
          max = 5000,
      ),

      ),
    p(
      "Once you have entered all the data for your test, click the \"Submit\" button. The application",
      span("will not", class = "warn"),
      "run  until you",
      span(" click \"submit\".", class = "warn"),
      class = "instructText"
    ),
    actionButton(
      inputId = "submit_data",
      label = "Submit",
      lib = "glyphicon",
      icon = icon("circle-play")
    ),
    
    HTML("<br /><br />"),
  
     conditionalPanel(
       condition = "input.submit_data",
       awesomeCheckbox("showDataTable", "Display Data", TRUE, status = "success"),
       conditionalPanel(
    p("Your dataset is displayed below. If you do not want to show this, uncheck the", em("Display Data"), " box."),
    
   
    
   
      condition = "input.showDataTable == 1",
    tableOutput("tableView"),
    ),
     )
  ),
  mainPanel(
    h3(htmlOutput("head3")),
    verbatimTextOutput("desStats"),
    
    h3(htmlOutput("head1")),
    
    plotOutput("pairspanels"),
    
    
    verbatimTextOutput("correl.out"),
    
    conditionalPanel(
      condition = "input.btstpY == 1",
    
    h3(htmlOutput("head2")),
    
    verbatimTextOutput("correlCI.out"),
    plotOutput("corBsCiPlot"),
    )

    
  )
))

server <- function(input, output) {
  data <- reactive({
    req(input$file1)
    
    inFile <- input$file1
    
    df <-
      read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    
    return(df)
    
  })
  
 
  output$tableView <- renderTable({ data() },
                                  rownames = TRUE,
                                  )
  
  
  observeEvent(eventExpr = input[["submit_data"]],
               handlerExpr = {
                 
                 ## Use corr.test from psych package
                 
                 correl <- reactive({
                   x <- as.matrix(data())
                   cR <-  corr.test(x, method = input$method)
                 })
                 
                 ## Print just the correlation results
                 
                 output$correl.out <- renderPrint({
                   round(correl()$r, 3)
                 })
                 
                 ## Print the CIs
                 
                 output$correlCi.out <- renderPrint({
                   round(correl()$ci2, 3)
                 })
                 
                 
                 ## Bootstrapped CIs
                 
                 if (input$btstpY == 1) {
                   output$head2 <- renderText(
                     paste("Bootstrapped confidence intervals - bootstrap level = ",input$btsn)
                   )
                   
                 correlCI <- reactive({
                   x <- as.matrix(data())
                   corCi(x, n.iter = input$btsn, method = input$method)
                  })

                 
                 output$correlCI.out <- renderPrint({
                   round(correlCI()$ci, 3)
                 })
                 
                 
                 ## Bootstrapped plot
                 
                 makecorPlot <- reactive ({
                   x <- as.matrix(data())
                   corPlotUpperLowerCi(correlCI(), main = "Bootstapped CIs for correlation")
                 })
                 
                 
                 output$corBsCiPlot <- renderPlot({
                   print(makecorPlot())
                 })
                 
                 }
                 
                 ## Pairs panels
                 
                 makepairsPanels <- reactive ({
                   x <- as.matrix(data())
                   pairs.panels(x, method = input$method)
                 })
                 
                 output$pairspanels <- renderPlot({
                   print(makepairsPanels())
                 })
                 
                 descript <- reactive({
                   x <- as.matrix(data())
                   describe(x)
                   
                 })
                 
                 output$desStats <- renderPrint({
                   print(descript())
                 })
                 
                 ## Various Text for display
                
                 output$head1 <- renderText(
                   paste("Correlation results using ", input$method, " correlation")
                 )
                 
                 output$head3 <- renderText(
                   paste("Descriptive Statistics")
                 )
                 
               })
  
  ## Regression
  
  lmModel <- reactive({
    req(data(), input$xvar, input$yvar)
    #x <- as.numeric(data()[[as.name(input$xvar)]])
    #y <- as.numeric(data()[[as.name(input$yvar)]])
    current_formula <-
      paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <-
      lm(current_formula, data = data(), na.action = na.exclude)
    return(model)
  })
  
  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())
  })
  output$diagnosticPlot <- renderPlot({
    req(lmModel())
    par(mfrow = c(2, 2))
    plot(lmModel())
  })
  
  
}

shinyApp(ui, server)
