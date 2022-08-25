library(shinyWidgets)
library(psych)
options(mc.cores =1)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    #href = "https://r-pc.net/shiny/rstudio/www/app.css"
  ),
  titlePanel("Correlation Analysis"),
  sidebarLayout(
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


       awesomeCheckbox("showDataTable", "Display Data", FALSE, status = "success"),
       conditionalPanel(
         condition = "input.showDataTable == 1",

         p("Your dataset is displayed below. If you do not want to show this, uncheck the", em("Display Data"), " box."),
         tableOutput("tableView"),
    ),
  ),

  # Main panel for displaying output

  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Results Output",
    h3(htmlOutput("head3")),
    verbatimTextOutput("desStats"),

    h3(htmlOutput("head1")),

    plotOutput("pairspanels"),
    h4(htmlOutput(("corrText"))),
    verbatimTextOutput("correlCi.out"),

    conditionalPanel(
      condition = "input.btstpY == 1",
      h3(htmlOutput("head2")),
      verbatimTextOutput("correlCI.out"),
      plotOutput("corBsCiPlot"),
    ),
      ),
    tabPanel(
      "Instructions",
      HTML(
        "<h4>Data Input</h4>
        <p>To enter your data, you must first save it as a <code>comma separated (csv)</code> or <code>tab-separated (tsv)</code>  file. You can prepare the data file in a spreadsheet application such as Microsoft Excel or Apple Numbers, and then export or save to the format you prefer.</p>
<p>In your spreadsheet application set up the data with 1 column for each group. For example, if you wanted to correlate data from four different groups with a maximum total of 30 variables in each group 1...group 4, you would enter the group names in the top row to give four columns, and then fill each column with the data for the respective groups, as such:</p>
   <div class=\"row\">
        <div class=\"col-md-3\">
<table class=\"table table-responsive\">
<thead>
<tr>
<th scope=\"col\">#</th>
<th scope=\"col\">Group 1</th>
<th scope=\"col\">Group 2</th>
<th scope=\"col\">Group 3</th>
<th scope=\"col\">Group 4</th>
</tr>
</thead>
<tbody>
<tr>
<th scope=\"row\">1</th>
<td>67</td>
<td>70</td>
<td>68</td>
<td>NA</td>
</tr>
<tr>
<th scope=\"row\">2</th>
<td>56</td>
<td>68</td>
<td>52</td>
<td>71</td>
</tr>
<tr>
<th scope=\"row\">.<br>.<br>.</th>
<td>.<br>.<br>.</td>
<td>.<br>.<br>.</td>
<td>.<br>.<br>.</td>
<td>.<br>.<br>.</td>
</tr>
<tr>
<th scope=\"row\">30</th>
<td>NA</td>
<td>87</td>
<td>52</td>
<td>69</td>
</tr>
</tbody>
</table>
</div>
</div>

<p><b>Note:</b> If you have any <u>missing data or empty cells</u>, enter <code>NA</code> as in the case of Group 4 #1 and Group 1 #30 above.</p>
<p>Once you have the data ready, you can then import it into the application and run the correlation. To do so, follow these steps:</p>
<ul>
<li>Under <i>Choose file to upload</i> click <i>Browse...</i>. This will allow you to locate the file on your computer and upload the data to the application. Once you have done the upload, you'll see the name of the file and <i>Upload complete</i> displayed on the screen. You can check your data has been properly imported by checking the <i>Display data</i> checkbox under the <i>Submit</i> button.</li>
"
      ),


             ),
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


                 ## Print the correlations and CIs

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
                   paste("Correlation results for ", input$method, " correlation")
                 )

                 output$head3 <- renderText(
                   paste("Descriptive statistics")
                 )

                 output$corrText <- renderText(
                   paste("Correlation coefficients (\"r\") and confidence intervals (\"lower\"; \"upper\").")
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
