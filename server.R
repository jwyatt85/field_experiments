# server.R
source("helpers.R")
options(shiny.maxRequestSize = 30*1024^2)

shinyServer(function(input, output) {
  
  #make the plot - reactive so I can use the function
  makePlot <- reactive({
    inFile <- input$file1
    if(is.null(inFile)) return(NULL)
    
    dat<- read.csv(inFile$datapath)
    
    Y = dat[,input$Y]
    D = dat[,input$D]
    Z = dat[,input$Z]
    if(!is.numeric(Y)) stop("The dependent variable must be numeric.")
    if(!is_binary(D)) stop("The contact variable can only include 0's and 1's.")
    if(!is_binary(Z)) stop("The treatment variable can only include 0's and 1's.")
    
    df <- data.frame(Y, D, Z = ifelse(Z==1, "Treatment", "Control"))
    
    group_by(df, Z) %>% 
      summarize(y_bar = mean(Y),
                N = n(),
                se = sd(Y)/sqrt(N),
                ui = y_bar + 1.96*se,
                li = y_bar - 1.96*se) %>%
      ggplot(aes(x=Z, y=y_bar)) + 
      geom_pointrange(aes(ymin=li, ymax=ui, width = .04), color="blue", fill="white", shape=22, size=1.5) + 
      ylim(0, NA) +
      ggtitle("Difference Between Conditional Groups") + 
      xlab("Condition") + ylab("Average Outcome") +
      theme_bw()
  })
  
  # Create Plot
  output$out_plot <- renderPlot({
    makePlot()
  })
  
  
  
  # Upload a Dataset
  output$outcome <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat<- read.csv(inFile$datapath)
    vars <- names(dat)
    selectInput(inputId = "Y", "Dependent Variable", vars, selected=vars[1])
  })
  
  output$contact <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat<- read.csv(inFile$datapath)
    vars <- names(dat)
    selectInput(inputId = "D", "Treatment Received", vars, selected=vars[2])
  })
  
  output$treatment <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat<- read.csv(inFile$datapath)
    vars <- names(dat)
    selectInput(inputId = "Z", "Treatment Assigned", vars, selected=vars[3])
  })
  
  
  # Create summary table - left panel
  output$summary_table_2 <- renderTable(digits=3, expr = {
    inFile <- input$file1
    if(is.null(inFile)) return(NULL)
    
    dat<- read.csv(inFile$datapath)
    
    Y = dat[,input$Y]
    D = dat[,input$D]
    Z = dat[,input$Z]
    if(!is.numeric(Y)) stop("The dependent variable must be numeric.")
    if(!is_binary(D)) stop("The contact variable can only include 0's and 1's.")
    if(!is_binary(Z)) stop("The treatment variable can only include 0's and 1's.")
    
    xtable(table_maker_2(Y,D,Z))
  })
  
  
  #download Report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('reports.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reports.Rmd')
      
      library(rmarkdown)
      out <- render('reports.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  

  makeTable <- reactive({
    expr = {
      inFile <- input$file1
      if(is.null(inFile)) return(NULL)
      
      dat<- read.csv(inFile$datapath)
      Y = dat[,input$Y]
      D = dat[,input$D]
      Z = dat[,input$Z]
      if(!is.numeric(Y)) stop("The dependent variable must be numeric.")
      if(!is_binary(D)) stop("The contact variable can only include 0's and 1's.")
      if(!is_binary(Z)) stop("The treatment variable can only include 0's and 1's.")
      
      xtable(statistical_results_2(Y,D,Z), label="Test_table")
      
    }
  })
  
  output$out_table <- renderTable(
    digits=3,{
      makeTable()
    })

####### Randomization Section

output$out_table3 <- renderTable({
    xtable(head(makeExport()))
})


#export file
makeExport <- reactive({
  expr = {
    inFile2 <- input$file2
    if(is.null(inFile2)) return(NULL)
    x <- input$randtype
    
    CondPer <- input$ConditionPercent
    
    dat2<- read.csv(inFile2$datapath)
    dat2 <- na.omit(dat2) 
    colnames(dat2)[1] <- "ID"
    
    #if user chooses block Rand
    if(x == 1){
      out <- block(dat2, n.tr = 2, id.vars = colnames(dat2)[1], algorithm="optGreedy",
                   block.vars = colnames(dat2)[2:length(dat2)], distance =
                     "mahalanobis", level.two = FALSE, verbose = TRUE)
      
      assg <- assignment(out, namesCol=c("Treatment", "Control"), seed = runif(1, 1, 100))
      final.assignment <- as.data.frame(assg[1])
      colnames(final.assignment)[1] <- "Treatment"
      colnames(final.assignment)[2] <- "Control"
      colnames(final.assignment)[3] <- "Distance"
      
      row.names(final.assignment) <- NULL
      return(final.assignment)
    } # end of Blocked Rand
    
    #if User chooses SRA
    if(x == 2){
      dat2$Condition <- as.integer(ra(nrow(dat2),round(nrow(dat2)*(CondPer/100)),seed=runif(1, 1, 100))[,2])
      return(dat2)
    }
  }
  
})

output$downloadData <- downloadHandler(
  filename = function() {paste("Final.Randomized.List", '.csv', sep='') },
  content = function(file) {
  write.csv(makeExport(), file)
  }
)

makeLogit <- reactive({
  inFile2 <- input$file2
  if(is.null(inFile2)) return(NULL)
  
  df.original <- read.csv(inFile2$datapath)
  df.original <- na.omit(df.original)
  df.rando <- makeExport()
  
  x <- input$randtype
  seed1 <- input$seed
  
  if(x == 1){
  #checks to see if unique ID is in "Treatment"
  df.original$Condition <- as.integer(ifelse(df.original[,1] %in% df.rando[,1], 1,0))
  df.original$ID <- NULL
  #now run regression
  my.logit <- glm(Condition ~., data = df.original, family = binomial(link = "logit"))
  return(my.logit)
  }
  if(x == 2) {
  df.rando$ID <- NULL
  my.logit <- glm(Condition ~., data = df.rando, family = binomial(link = "logit"))
  return(my.logit)
  }
  
})

output$out_logit <- renderTable({
  xtable(makeLogit())
})

### Logit chart outputs
makeLogitPlot <- reactive({
    my.logit.for.plot <- makeLogit()

    sjp.glm(my.logit.for.plot, 
            axisLabels.y = predlab,
            type = "prob")
})

# Create Plot
output$out_plotLogit <- renderPlot({
  makeLogitPlot()
})


### Logit chart outputs
makeBoxPlot <- reactive({
  inFile2 <- input$file2
  if(is.null(inFile2)) return(NULL)
  
  df.original <- read.csv(inFile2$datapath)
  df.original <- na.omit(df.original)
  
  df.rando <- makeExport()
  
  x <- input$randtype
  
  if(x == 1) {
  #checks to see if unique ID is in "Treatment"
  df.original$Condition <- as.integer(ifelse(df.original[,1] %in% df.rando[,1], 1,0))
  df.original$ID <- NULL
  
  df.m <- melt(df.original, id.var = "Condition")
  p <- ggplot(df.m, aes(x=factor(variable), y=value)) + geom_boxplot(aes(fill=factor(Condition)))
  p <- p + theme_bw() + ggtitle("Box Plot Analysis Across Groups") + scale_fill_manual(values = alpha(c("dark blue", "red"), .90))
  return(p)
  
  }
  if(x == 2){
  df.rando$ID <- NULL
  df.m <- melt(df.rando, id.var = "Condition")
  p <- ggplot(df.m, aes(x=factor(variable), y=value)) + geom_boxplot(aes(fill=factor(Condition)))
  p <- p + theme_bw() + ggtitle("Box Plot Analysis Across Groups") + scale_fill_manual(values = alpha(c("dark blue", "red"), .90))
  return(p)
  }
})

output$out_box <- renderPlot({
  makeBoxPlot()
})

#make MDE Plots
### Logit chart outputs
makeMDEPlot <- reactive({
      treatment <- input$percentintreatment
      treatment <- treatment/100
      contactrate <- input$contactrate
      contactrate <- contactrate / 100
      colrate <- input$colrate
      colrate <- colrate/100
      conrate <- input$controlrate
      conrate <- conrate/100
      n <- input$total
      
      df.mde <- mde(conrate, n, 0, treatment, contactrate, colrate)
      
      p <- ggplot(df.mde, aes(fill=variable, y=value, x=variable), ymax = )
      p <- p + geom_bar(position="dodge", stat="identity",colour = "black") + theme_bw()
      p <- p + scale_fill_manual(values = alpha(c("dark blue", "red"), .90)) + ggtitle("MDE for ITT and TOT")
      p <- p + geom_text(aes(label=round(value, digits=5)), position=position_dodge(width=0.9), vjust=-0.25)
      return(p)
})
output$out_mde <- renderPlot({
  makeMDEPlot()
})

makeMDEspanPlot <- reactive({
  n <- input$total
  contactrate <- input$contactrate
  contactrate <- contactrate / 100
  colrate <- input$colrate
  colrate <- colrate/100
  
  my.mde.df <- mde.span(n, contactrate, colrate)
  
  p <- ggplot(my.mde.df, aes(fill=variable, x=per.treat2, y=value))
  p <- p + geom_bar(position="dodge", stat="identity",colour = "black") + theme_bw() + 
    geom_line(aes(fill=variable)) + geom_text(size = 4, aes(label=round(value, digits=3)), position=position_dodge(width=0.05), vjust=-0.25)
  p <- p + scale_fill_manual(values = alpha(c("dark blue", "red"), .90)) + ggtitle("ITT/TOT MDE Over Span")
  
  return(p)
})



output$out_mde_span <- renderPlot({
  makeMDEspanPlot()
})


makePlotSubGroups <- reactive({
  inFile <- input$file5
  if(is.null(inFile)) return(NULL)
  
  dat<- read.csv(inFile$datapath)
  
  colnames(dat)[1] <- "DV"
  colnames(dat)[2] <- "Treatment"
  colnames(dat)[2] <- "Subgroup"

})

# Create Plot
output$out_plot_subgroups <- renderPlot({
  makePlotSubGroups()
})



#mapping
output$maps <- renderPlot({
  makemap()
})

makemap <- reactive({
  expr = {
    inFile2 <- input$mapcsv
    if(is.null(inFile2)) return(NULL)
  
    dat2<- read.csv(inFile2$datapath)
    dat2 <- na.omit(dat2)
    if(length(names(dat2)) != 2){
      stop("You can only have 2 columns: fips/stname and value")
    }
    
    names(dat2) <- c("region", "value")
    color <- as.character(input$mapcolor)
    statezoom <- tolower(state.name)
    statezoom <- statezoom[-c(grep("alaska|hawaii", statezoom))]
    
    my_map <- county_choropleth(dat2,
                      title = input$maptitle,
                      legend = input$maplegend,
                      num_colors = 5,
                      state_zoom = statezoom ) + scale_fill_brewer(palette = color)
    
    return(my_map)
    }})



# Sub-group analysis
# Upload a Dataset
output$Sub.DV <- renderUI({
  inFile <- input$file5
  
  if (is.null(inFile))
    return(NULL)
  
  dat<- read.csv(inFile$datapath)
  vars <- names(dat)
  selectInput(inputId = "Sub.DV", "Dependent Variable", vars, selected=vars[1])
})

output$Sub.Treat <- renderUI({
  inFile <- input$file5
  
  if (is.null(inFile))
    return(NULL)
  
  dat<- read.csv(inFile$datapath)
  vars <- names(dat)
  selectInput(inputId = "Sub.Treat", "Treatment Received", vars, selected=vars[2])
})

output$Sub.Group <- renderUI({
  inFile <- input$file5
  
  if (is.null(inFile))
    return(NULL)
  
  dat<- read.csv(inFile$datapath)
  vars <- names(dat)
  selectInput(inputId = "Sub.Group", "Sub Group to Analyze", vars, selected=vars[3])
})





### make sub group plot
#make the plot - reactive so I can use the function
makeSubGroupPlot <- reactive({
  inFile <- input$file5
  if(is.null(inFile)) return(NULL)
  
  dat<- read.csv(inFile$datapath)
  
  DV = dat[,input$Sub.DV]
  Treatment = dat[,input$Sub.Treat]
  Sub.Group = dat[,input$Sub.Group]
  
  if(!is.numeric(DV)) stop("The dependent variable must be numeric: 1/0")
  if(!is.numeric(Treatment)) stop("The treatment variable can only be numeric")
  if(!is.numeric(Treatment)) stop("The Group variable can only include Numbers for specific Groups (Ex: 1=M; 2=F)")
  
  df <- data.frame(DV, Treatment, Sub.Group)
  
  #run logits
  df$DV <- as.factor(df$DV)
  df$Treatment <- as.factor(df$Treatment)
  df$Sub.Group <- as.factor(df$Sub.Group)
  my.logit <- glm(DV ~ Treatment + Treatment*Sub.Group, family=binomial(link="logit"), data=df)

  plot(allEffects(my.logit))
  
})

# Create Plot for Sub-groups
output$subgroup.plot <- renderPlot({
  makeSubGroupPlot()
})



#### --- Output the Logit Effects for the 
makeSubGroupTable <- reactive({
  inFile <- input$file5
  if(is.null(inFile)) return(NULL)
  
  dat<- read.csv(inFile$datapath)
  
  DV = dat[,input$Sub.DV]
  Treatment = dat[,input$Sub.Treat]
  Sub.Group = dat[,input$Sub.Group]
  
  if(!is.numeric(DV)) stop("The dependent variable must be numeric: 1/0")
  if(!is.numeric(Treatment)) stop("The treatment variable can only be numeric")
  if(!is.numeric(Treatment)) stop("The Group variable can only include Numbers for specific Groups (Ex: 1=M; 2=F)")
  
  df <- data.frame(DV, Treatment, Sub.Group)
  
  #run logits
  df$DV <- as.factor(df$DV)
  df$Treatment <- as.factor(df$Treatment)
  df$Sub.Group <- as.factor(df$Sub.Group)
  
  my.logit <- glm(DV ~ Treatment + Treatment*Sub.Group, family=binomial(link="logit"), data=df)
  eff <- allEffects(my.logit)
  
  my.list  <- eff$`Treatment:Sub.Group`
  my.list.df <- as.data.frame(my.list)
 
  return(my.list.df)
  
})

output$sub.group.table <- renderTable({
  xtable(makeSubGroupTable())
})



}) #end of shiny server everything above ^^


