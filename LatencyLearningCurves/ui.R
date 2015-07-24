
rm(list = setdiff(ls(), lsf.str()))
setwd("/Volumes/Double-Whopper/Trudy/2015_LearnLabSummerSchool/ShinyApp/LatencyLearningCurves/")

library(lme4)
library(dplyr)
options(dplyr.width = Inf)

load("./Data/listKCShiny.Rdat")
cat("\n\n\n")

colors <- c("darkblue","darkred","darkorange","darkgreen")


shinyUI(fluidPage(
    title = "Mental Rotation",

    titlePanel("DataShop / R-Version"),
    h6("Trudy Buwalda, 2015"),
    h3("Transfer between Mental Rotation and Paper Folding"),

    fluidRow(
      column(8, plotOutput("plot1")),
      column(2, wellPanel(
        h5("Linear Regression Model - log(Response Time)"),
        #checkboxInput('plotPredict1', 'Plot Prediction', value = FALSE),
        htmlOutput("aicbic1")
      )),
      column(2, wellPanel(
        h5("Logistic Regression Model - Error Rate"),
        #checkboxInput('plotPredict1', 'Plot Prediction', value = FALSE),
        htmlOutput("aicbic2"))),
      column(4, wellPanel(
        h5("Previous Models"),
        dataTableOutput('oldModels')
        ))
      #column(2, h5("Linear Regression Models")),
      #column(2, checkboxInput('plotPredict1', 'Plot Prediction', value = FALSE)),
      #column(1, h6("AIC: ")),
      #column(1, h6(textOutput("aic1"))),
      #column(2, h6("BIC: ")),
      #column(3,h5("Exponential Regression Models"),
      #  checkboxInput('plotPredict1', 'Plot Prediction', value = FALSE),
      #  h6("AIC: "),
        #h6(textOutput("aic1")),
      #  h6(paste("BIC: "))
      #)
    ),

    fluidRow(
      column(2,
        #h4('Mental Rotation - KC models'),
        selectInput('kc', 'KC model', listKC)
      ),
      column(1,
        selectInput('type', 'Plot Type', c("Line","Points","Both"))
      ),
      column(1,
        numericInput('bins', 'Bin Size', value=1,min=1,max=100)
      ),
      column(3,
        sliderInput('ylim', 'Y-axis',min=0,max=10000, step=1,value=c(0,10000))
      )#,
      #column(2,
      #  selectInput('distance','Bin Distance',c('Cosine','Dot Product'))
      #)

    ),

    hr(),
    fluidRow(
      column(4,
        selectInput('kc1',label='Plot 1: KC level','')
      ),
      column(4,
        selectInput('kc2',label='Plot 2: KC level','')
      ),
      column(4,
        selectInput('kc3',label='Plot 3: KC level','')
      )
    ),

    fluidRow(
      column(4,
        selectInput('kc1line',label='','')
      ),
      column(4,
        selectInput('kc2line',label='','')
      ),
      column(4,
        selectInput('kc3line',label='','')
      )
    ),

    fluidRow(
      column(4,
        wellPanel(
          plotOutput('subPlot1'))),
      column(4,
        wellPanel(
          plotOutput('subPlot2'))),
      column(4,
        wellPanel(
          plotOutput('subPlot3')))
    )

))