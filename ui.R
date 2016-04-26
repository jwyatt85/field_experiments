shinyUI(
  fluidPage(theme = "bootstrap2.css", 
            sidebarLayout(
              sidebarPanel(
                h3("ITT Analysis"),
                wellPanel("Upload a .CSV",
                          fileInput('file1', 'Choose CSV File',
                                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                          uiOutput("outcome"),
                          uiOutput("contact"),
                          uiOutput("treatment")
                ),
                
                wellPanel("Summary of Input",
                          tableOutput("summary_table_2")  
                ),
                
                br(),
                h3("Randomization"),
                h6("_______________"),
                h5("Directions:"),
                h6("Column 1 must be the ID (i.e: DWID)"),
                h6("Columns 2 through Xi must be balancing variables.  If you're
                   using 'Blocked' randomization the covariates must be numeric"),
      
                br(),
                # Copy the line below to make a slider bar 
                sliderInput("ConditionPercent", label = h5("Percent in Treatment"), min = 0, 
                            max = 100, value = 50),
                br(),
                # Copy the line below to make a set of radio buttons
                radioButtons("randtype", label = h5("Randomization Method"),
                             choices = list("Blocked (less than 2k rows)" = 1, "SRA (Large Data)" = 2), selected = 1),
                br(),
                h6("_______________"),
                
                wellPanel("Upload a .CSV for Randomization",
                          fileInput('file2', 'Choose CSV File',
                                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                ),
                br(),
                br(),
                h6("_______________"),
                h4("Download ITT/TOT Analysis Report"),
                radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                             inline = TRUE),
                downloadButton('downloadReport')
              ),
              
              mainPanel(position = "right",
                        tabsetPanel(
                          tabPanel("ITT / TOT",
                                   h3("Conditional Analysis Section", align= "center"),
                                   br(),
                                   plotOutput("out_plot"),
                                   #h3("Color of Plot"),
                                   #radioButtons("color","Select Color", choices = c("blue", "green", "purple"), selected = "blue" ),
                                   br(),
                                   h3("Statistical Results", align = "center"),
                                   tableOutput("out_table")
                          ),
                          tabPanel("Randomized List",
                                   h3("Randomization Section", align= "center"),
                                   br(),
                                   downloadButton('downloadData', 'Download Randomized List'),
                                   hr(),
                                   br(),
                                   tableOutput("out_table3")
                          ),
                          tabPanel("Randomization Balance",
                                   h3("Covariate Balance Across Conditions", align= "center"),
                                   br(),
                                   h5("This section analyzes whether any covariates
                                       predict condtion placement, which would indicate
                                       there's an imbalance between conditions.  If any p-value
                                       (that is not the intercept) is below, or equals, .05,
                                       then there may a possible imbalance between groups"),
                                   br(),
                                   tableOutput("out_logit"),
                                   br(),
                                   br(),
                                   plotOutput("out_plotLogit"),
                                   br(),
                                   br(),
                                   plotOutput("out_box")
                          ),
                          tabPanel("MDE",
                                   h3("Power Calculations (MDE) for Conditional Analysis", align= "center"),
                                   br(),
                                   fluidRow(
                                     column(4,
                                            sliderInput("percentintreatment", 
                                                        label = h5("Percent In Treatment"), min = 0, 
                                                        max = 100, value = 50)
                                     ),
                                     column(4,
                                            sliderInput("contactrate", label = h5("Contact Rate"), min = 0, 
                                                        max = 100, value = 100)
                                     ),
                                     column(3,
                                            numericInput("total", label = h5("Total N Size"), value = 30000))
                                   ),
                                   fluidRow(
                                     column(4,
                                            sliderInput("colrate", 
                                                        label = h5("DV Collection / Response Rate"), min = 0, 
                                                        max = 100, value = 100)
                                     ),
                                     column(4,
                                            sliderInput("controlrate", 
                                                        label = h5("Expected DV Rate in Control"), min = 0, 
                                                        max = 100, value = 50)
                                     ),
                                     column(4,
                                            h5("Assumptions: power = 80%"),
                                            h5("Confidence Interval = 90%"),
                                            h5("R-squared - Varience Explained by Covariates = 0")
                                     )),
                                   hr(),
                                   plotOutput("out_mde"),
                                   br(),
                                   br(),
                                   plotOutput("out_mde_span")
                                   
                                   
                                   ),#end of tab Panel
                          tabPanel("Sub-Group Analysis",
                                   h3("Subgroup Analysis", align= "center"),
                                   br(),
                                   wellPanel("Upload a .CSV",
                                             fileInput('file5', 'Choose CSV File',
                                                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                             uiOutput("Sub.DV"),
                                             uiOutput("Sub.Treat"),
                                             uiOutput("Sub.Group")
                                   ),
                                   br(),
                                   plotOutput("subgroup.plot"),
                                   br(),
                                   tableOutput("sub.group.table")
                          ),
                          tabPanel("Mapping",
                                   h3("State/County Mapping", align= "center"),
                                   br(),
                                   h5("Directions: The CSV file must contain the county fips, zip fips, or state name (if developing state-level estimates) as the first column.  The second column must be the value to plot"),
                                   radioButtons("maptype", label = h4("Choose your map type:"),
                                                choices = list("State" = "state", "County" = "county", "Zip" = "zip"), 
                                                selected = NULL),
                                   br(),
                                   textInput("maptitle", label=h4("Title of the Map:"), value = "", width = NULL, placeholder = NULL),
                                   textInput("maplegend", label=h4("Legend Title:"), value = "", width = NULL, placeholder = NULL),
                                   radioButtons("mapcolor", label = h4("Choose a Color"),
                                                choices = list("White - Blue" = "Blues", "Yellow - Red" = "YlOrRd", "Spectral" = "Spectral", "White - Green" = "Greens"), 
                                                selected = NULL),
                                   textInput("statezoom", label = h4("States to Zoom Into (seperated by commas)"), value = ""),
                                   wellPanel("Upload a .CSV",
                                             fileInput('mapcsv', 'Choose CSV File',
                                                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                             

                                   ),
                                   br(),
                                   plotOutput("maps")
                          )
                        )
              )  
      )
  )
)



