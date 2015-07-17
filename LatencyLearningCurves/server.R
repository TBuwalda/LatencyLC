
load("/Users/tabuwalda/Documents/LearnLab Summer School/spatial-data/SILC Data/datShiny.Rdat")
load("/Users/tabuwalda/Documents/LearnLab Summer School/spatial-data/SILC Data/listKCShiny.Rdat")

shinyServer(function(input, output, session) {

  getData <- reactive({
    load("/Users/tabuwalda/Documents/LearnLab Summer School/spatial-data/SILC Data/datShiny.Rdat")
  })
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    getData()
    cutpoints <- with(dat, seq(0,max(get(paste(input$kc,".Opp",sep=""))+as.numeric(input$bins)),by=as.numeric(input$bins)))
    dat$binnedOpportunity <- with(dat, cut(get(paste(input$kc,".Opp",sep="")),cutpoints,include.lowest=TRUE,labels=FALSE))
    with(dat, aggregate(list(logRT=logRT),list(Opportunity=binnedOpportunity,KC=get(input$kc)),mean,na.rm=TRUE))
  })

  selectedType <- reactive({
    ifelse(input$type=="Line","l",ifelse(input$type=="Points","p","b"))
    #kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    tmp <- selectedData()
    tmp <- with(tmp, aggregate(list(logRT=logRT),list(Opportunity=Opportunity),mean))
    cat(unique(tmp$Opportunity))
    cat(range(tmp$logRT))

    kcOpp <- dat %>% group_by() %>% select(get(paste(input$kc,".Opp",sep=""))) %>% unlist() %>% as.vector()
    plot(tmp$Opportunity,tmp$logRT,type=selectedType(),col="#990000",lwd=2,xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),
        ylim=input$ylim,xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="log response times",
        main=gsub("KC.","",input$kc))
    #par(mar = c(5.1, 4.1, 0, 1))
    #plot(selectedData(),
    #     col = clusters()$cluster,
    #     pch = 20, cex = 3)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

  output$subPlot1 <- renderPlot({
    tmp <- selectedData()
    tmp2 <- tmp
    tmp <- tmp[tmp$KC == input$kc1,]

    plot(tmp$Opportunity,tmp$logRT,type=selectedType(),col=input$kc1col,lwd=2,
        xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),ylim=input$ylim,
        xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="log response times",
        main=gsub("KC.","",input$kc))
    lines(tmp2$Opportunity,tmp2$logRT,type=selectedType(),col=input$k1linecol,lwd=2)
  })

  output$subPlot2 <- renderPlot({
    tmp <- selectedData()
    tmp <- tmp[tmp$KC == input$kc2,]

    plot(tmp$Opportunity,tmp$logRT,type=selectedType(),col=input$kc2col,lwd=2,
        xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),ylim=input$ylim,
        xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="log response times",
        main=gsub("KC.","",input$kc))
  })

  output$subPlot3 <- renderPlot({
    tmp <- selectedData()
    tmp1 <- tmp[tmp$KC == input$kc3,]
    tmp2 <- tmp1[tmp$KC == input$kc3line,]
    cat(input$kc3line)
    plot(tmp1$Opportunity,tmp1$logRT,type=selectedType(),col=input$kc3col,lwd=2,
        xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),ylim=input$ylim,
        xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="log response times",
        main=gsub("KC.","",input$kc))
    lines(tmp2$Opportunity,tmp2$logRT,type=selectedType(),col=input$k3linecol,lwd=2)

  })

  observe({
      kcLevels <- dat %>% group_by() %>% select(get(input$kc)) %>% unique
      updateSelectInput(
        session,
        "kc1",
        choices=kcLevels)

    })

  observe({
      kcLevels <- dat %>% group_by() %>% select(get(input$kc)) %>% unique
      updateSelectInput(
        session,
        "kc2",
        choices=kcLevels)

    })

  observe({
      kcLevels <- dat %>% group_by() %>% select(get(input$kc)) %>% unique
      updateSelectInput(
        session,
        "kc3",
        choices=kcLevels)

    })

  observe({
      kcLevels <- dat %>% group_by() %>% select(get(input$kc)) %>% unique
      updateSelectInput(
        session,
        "kc1line",
        choices=kcLevels)

    })

  observe({
      kcLevels <- dat %>% group_by() %>% select(get(input$kc)) %>% unique
      updateSelectInput(
        session,
        "kc2line",
        choices=kcLevels)

    })

  observe({
      kcLevels <- dat %>% group_by() %>% select(get(input$kc)) %>% unique
      updateSelectInput(
        session,
        "kc3line",
        choices=kcLevels)

    })

})