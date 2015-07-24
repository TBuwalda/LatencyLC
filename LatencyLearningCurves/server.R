source("helpers.r")

shinyServer(function(input, output, session) {
  oldModels <- NULL
  load("./Data/datShiny.Rdat")

  loadData <- reactive({
    # Load the data
    load("./Data/datShiny.Rdat")
  })

  selectedData <- reactive({
    # Combine the selected variables into a new data frame  
    loadData()
    cutpoints <- with(dat, seq(0,max(get(paste(input$kc,".Opp",sep=""))+as.numeric(input$bins)),by=as.numeric(input$bins)))
    dat$binnedOpportunity <- with(dat, cut(get(paste(input$kc,".Opp",sep="")),cutpoints,include.lowest=TRUE,labels=FALSE))
    rt <- with(dat[dat$Acc == 1,], aggregate(list(logRT=logRT,RT=RT),list(Opportunity=binnedOpportunity,KC=get(input$kc)),mean,na.rm=TRUE))
    acc <- with(dat, aggregate(list(Acc=Acc),list(Opportunity=binnedOpportunity,KC=get(input$kc)),mean,na.rm=TRUE))
    full_join(rt,acc,by=c("Opportunity","KC"))
  })

  selectedLineType <- reactive({
    # Selected Line Type for Plots
    ifelse(input$type=="Line","l",ifelse(input$type=="Points","p","b"))
  })

  linearRegression <- reactive({
    # Perform Linear Regression on Selected Data
    loadData()
    dat$KC <- dat %>% group_by() %>% select(get(paste(input$kc,sep=""))) %>% unlist() %>% as.vector()
    dat$KC.Idx <- paste(dat$Subj,dat$KC.Similarity,sep="-")
    dat$tmpVar <- 1
    dat <- dat %>% 
      group_by(KC.Idx) %>%
      mutate(KC.Opp = cumsum(tmpVar)) # Opportunity
    model.lmer <- lmer(logRT~KC +
      KC:KC.Opp+(1|Subj),data=dat[!is.na(dat$logRT),]) # Regression
  })

  linearRegressionER <- reactive({
    # Perform Linear Regression on Selected Data
    loadData()
    dat$KC <- dat %>% group_by() %>% select(get(paste(input$kc,sep=""))) %>% unlist() %>% as.vector()
    dat$KC.Idx <- paste(dat$Subj,dat$KC.Similarity,sep="-")
    dat$ErrorRate <- 1-dat$Acc
    dat$tmpVar <- 1
    dat <- dat %>% 
      group_by(KC.Idx) %>%
      mutate(KC.Opp = cumsum(tmpVar)) # Opportunity
    model.lmer <- lmer(ErrorRate~KC +
      KC:KC.Opp+(1|Subj),data=dat[!is.na(dat$logRT),],family="binomial") # Regression
  })

  output$plot1 <- renderPlot({
    # Output for main plot
    tmp <- selectedData()
    opportunities <- with(tmp, aggregate(list(logRT=logRT,RT=RT,Acc=Acc),list(Opportunity=Opportunity), mean, na.rm=TRUE))
    opportunities$ErrorRate <- (1 - opportunities$Acc) * 10000

    kcOpp <- dat %>% group_by() %>% select(get(paste(input$kc,".Opp",sep=""))) %>% unlist() %>% as.vector()
    par(mar = c(5.1, 4.1, 4.1, 4.1))
    plot(opportunities$Opportunity,
         opportunities$RT,
         type=selectedLineType(),
         col="darkorange",
         lwd=2,
         xlim=c(min(opportunities$Opportunity),max(opportunities$Opportunity)),
         ylim=input$ylim,
         xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), 
         ylab="Response Time (ms)",
         main=gsub("KC.","",input$kc))
    lines(opportunities$Opportunity,
          opportunities$ErrorRate,
          type=selectedLineType(),
          col="darkgreen",
          lwd=2)
    ylab <- seq(par("yaxp")[[1]],par("yaxp")[[2]], length.out=par("yaxp")[[3]] + 1)
    axis(side=4,labels=as.character(ylab/10000),at=ylab)
    mtext("Error Rate",side=4,line=3)
    legend("topright", col=c("darkorange","darkgreen"), lwd=3, legend=c("Response Time", "Error Rate"),inset=0.01)

    #if(input$plotPredict1) {
    #  model <- linearRegression()
    #  lines(predict(model))
    #}
  })

  output$aicbic1 <- renderUI({
    model.lmer <- linearRegression()
    aic <- AIC(model.lmer)
    bic <- BIC(model.lmer)
    tmp <- data.frame(KC=as.character(gsub("KC.","",input$kc)),
          Type="RT",AIC=floor(aic),BIC=floor(bic))
    if(is.null(oldModels)){
      oldModels <<- tmp
    } else {
      oldModels <<- rbind(oldModels,tmp)
      oldModels <<- oldModels[order(oldModels$AIC),]
    }
    HTML(paste("AIC:",floor(aic),"<br/>BIC:",floor(bic)))
  })

  output$aicbic2 <- renderUI({
    model.lmer <- linearRegressionER()
    aic <- AIC(model.lmer)
    bic <- BIC(model.lmer)
    tmp <- data.frame(KC=as.character(gsub("KC.","",input$kc)),
          Type="Errors",AIC=floor(aic),BIC=floor(bic))
    if(is.null(oldModels)){
      oldModels <<- tmp
    } else {
      oldModels <<- rbind(oldModels,tmp)
      oldModels <<- oldModels[order(oldModels$AIC,decreasing=TRUE),]
    }
    HTML(paste("AIC:",floor(aic),"<br/>BIC:",floor(bic)))
  })

  output$oldModels <- renderDataTable({
      input$kc
      options = list(pageLength=5)
      if(is.null(oldModels)) {
        return()
      } else {
        return(unique(oldModels))
      }
    })

  output$subPlot1 <- renderPlot({
    # Output for subPlot1
    tmp <- selectedData()
    tmp1 <- tmp[tmp$KC == input$kc1,]
    tmp2 <- tmp[tmp$KC == input$kc1line,]

    plot(tmp1$Opportunity,tmp1$RT,type=selectedLineType(),col="darkblue",lwd=2,
        xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),ylim=input$ylim,
        xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="Response Times (ms)",
        main=gsub("KC.","",input$kc))
    lines(tmp2$Opportunity, tmp2$RT, type=selectedLineType(), col="darkred", lwd=2)
  })

  output$subPlot2 <- renderPlot({
    # Output for subPlot2
    tmp <- selectedData()
    tmp1 <- tmp[tmp$KC == input$kc2,]
    tmp2 <- tmp[tmp$KC == input$kc2line,]

    plot(tmp1$Opportunity,tmp1$RT,type=selectedLineType(),col="darkblue",lwd=2,
        xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),ylim=input$ylim,
        xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="Response Times (ms)",
        main=gsub("KC.","",input$kc))
    lines(tmp2$Opportunity, tmp2$RT, type=selectedLineType(), col="darkred", lwd=2)
  })

  output$subPlot3 <- renderPlot({
    # Output for subPlot3
    tmp <- selectedData()
    tmp1 <- tmp[tmp$KC == input$kc3,]
    tmp2 <- tmp[tmp$KC == input$kc3line,]

    plot(tmp1$Opportunity,tmp1$RT,type=selectedLineType(),col="darkblue",lwd=2,
        xlim=c(min(tmp$Opportunity),max(tmp$Opportunity)),ylim=input$ylim,
        xlab=paste("Opportunity (bin size = ", input$bins, ")", sep=""), ylab="Response Times (ms)",
        main=gsub("KC.","",input$kc))
    lines(tmp2$Opportunity,tmp2$RT,type=selectedLineType(),col="darkred",lwd=2)

  })

  # Functions for reactive UI
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