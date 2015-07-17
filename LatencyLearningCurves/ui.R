library(shiny)

#dataset <- load(data)

shinyUI(fluidPage(
    title = "Mental Rotation",

    titlePanel("DataShop - R-Version"),
    h6("Trudy Buwalda & Alina Nazareth, 2015"),
    h3("Transfer between Mental Rotation and Paper Folding"),

    plotOutput('plot1'),

    fluidRow(
      column(2,
        #h4('Mental Rotation - KC models'),
        selectInput('kc', 'KC model', listKC)
      ),
      column(1,
        selectInput('type', 'Plot Type', c("Line","Points","Both"))
      ),
      column(1,
        numericInput('bins', 'Bin Size', value=10,min=1,max=100)
      ),
      column(3,
        sliderInput('ylim', 'Y-axis',min=0,max=12, step=0.5,value=c(6,8))
      ),
      column(2,
        selectInput('distance','Bin Distance',c('Cosine','Dot Product'))
      )

    ),

    hr(),
    fluidRow(
      column(2,
        selectInput('kc1',label='Plot 1: KC level','')
      ),
      column(2,
        selectInput('kc1col',label='Color',c("#990000","#EFEFEF"))
      ),
      column(2,
        selectInput('kc2',label='Plot 2: KC level','')
      ),
      column(2,
        selectInput('kc2col',label='Color',c("#EFEFEF","#990000"))
      ),
      column(2,
        selectInput('kc3',label='Plot 3: KC level','')
      ),
      column(2,
        selectInput('kc3col',label='Color',c("#EFEFEF","#990000"))
      )
    ),

    fluidRow(
      column(2,
        selectInput('kc1line',label='','')
      ),
      column(2,
        selectInput('kc1linecol',label='Color',c("#EFEFEF","#990000"))
      ),
      column(2,
        selectInput('kc2line',label='','')
      ),
      column(2,
        selectInput('kc2linecol',label='Color',c("#EFEFEF","#990000"))
      ),
      column(2,
        selectInput('kc3line',label='','')
      ),
      column(2,
        selectInput('kc3linecol',label='Color',c("#EFEFEF","#990000"))
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