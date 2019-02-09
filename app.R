#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(neuralnet)
load('only nn.RData')
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Neural Network Emotion Calculator Demo"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("rele",
                  "Relevance",
                  min = 1,
                  max = 9,
                  value = 5),
      sliderInput("cong",
                  "Congruence",
                  min = 1,
                  max = 9,
                  value = 5),
      sliderInput("self",
                  "Self-accountability",
                  min = 1,
                  max = 9,
                  value = 5),
      sliderInput("other",
                  "Other-accountability",
                  min = 1,
                  max = 9,
                  value = 5),
      sliderInput("futu",
                  "Future expectancy",
                  min = 1,
                  max = 9,
                  value = 5),
      sliderInput("pfcp",
                  "Problem-focused coping",
                  min = 1,
                  max = 9,
                  value = 5),
      sliderInput("afcp",
                  "Accomodative-focused coping",
                  min = 1,
                  max = 9,
                  value = 5)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("posemo"),
      plotOutput("negemo")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    as.data.frame(input$rele1,input$cong,input$self,input$other,input$futu,input$pfcp,input$afcp)
  })
  output$posemo <- renderPlot({
    
    new <- c(input$rele,input$cong,input$self,input$other,input$futu,input$pfcp,input$afcp)
    maxs=c(9,9,9,9,9,9,9)
    mins=c(1,0,1,1,1,1,1) 
    new <- as.data.frame(scale(t(new), center = mins, scale = maxs - mins))
    #predicted from neural network
    pr.nn1 <- compute(nn,new)
    #regret
    reg1 <- pr.nn1$net.result[,1]*8+1
    #surprise
    sur1 <- pr.nn1$net.result[,2]*8+1
    #guilt
    gui1 <- pr.nn1$net.result[,3]*8+1
    #resignation
    res1 <- pr.nn1$net.result[,4]*8+1
    #calm
    cal1 <- pr.nn1$net.result[,5]*8+1
    #frustration
    fru1 <- pr.nn1$net.result[,6]*8+1
    #anger
    ang1 <- pr.nn1$net.result[,7]*8+1
    #challenge
    cha1 <- pr.nn1$net.result[,8]*8+1
    #sadness
    sad1 <- pr.nn1$net.result[,9]*8+1
    #interest
    int1 <- pr.nn1$net.result[,10]*8+1
    #relief
    rel1 <- pr.nn1$net.result[,11]*8+1
    #bore
    bor1 <- pr.nn1$net.result[,12]*8+1
    #anxiety
    anx1 <- pr.nn1$net.result[,13]*8+1
    #hope
    hop1 <- pr.nn1$net.result[,14]*8+1
    #pride
    pri1 <- pr.nn1$net.result[,15]*8+1
    #fear
    fea1 <- pr.nn1$net.result[,16]*8+1
    #happy
    hap1 <- pr.nn1$net.result[,17]*9
    #gratitude
    gra1 <- pr.nn1$net.result[,18]*8+1
    
    neuralnetwork=as.array(round(c(sur1,cal1,cha1,int1,rel1,hop1,pri1,hap1,gra1),2))
  
    par(mar=c(9,4,1,1))
    barplot(t(neuralnetwork),
            names=c("surprise","calm","challenge","interest","relief","hope","pride","happiness","gratitude"),main='Positive Emotion Predictions',col='gold',
            border='black',cex.axis=1.3,cex.names=1.3,ylim=c(0,9),ylab="Intensity",las=3)
    
  })
  output$negemo <- renderPlot({
    
    new <- c(input$rele,input$cong,input$self,input$other,input$futu,input$pfcp,input$afcp)
    maxs=c(9,9,9,9,9,9,9)
    mins=c(1,0,1,1,1,1,1) 
    new <- as.data.frame(scale(t(new), center = mins, scale = maxs - mins))
    #predicted from neural network
    pr.nn1 <- compute(nn,new)
    #regret
    reg1 <- pr.nn1$net.result[,1]*8+1
    #surprise
    sur1 <- pr.nn1$net.result[,2]*8+1
    #guilt
    gui1 <- pr.nn1$net.result[,3]*8+1
    #resignation
    res1 <- pr.nn1$net.result[,4]*8+1
    #calm
    cal1 <- pr.nn1$net.result[,5]*8+1
    #frustration
    fru1 <- pr.nn1$net.result[,6]*8+1
    #anger
    ang1 <- pr.nn1$net.result[,7]*8+1
    #challenge
    cha1 <- pr.nn1$net.result[,8]*8+1
    #sadness
    sad1 <- pr.nn1$net.result[,9]*8+1
    #interest
    int1 <- pr.nn1$net.result[,10]*8+1
    #relief
    rel1 <- pr.nn1$net.result[,11]*8+1
    #bore
    bor1 <- pr.nn1$net.result[,12]*8+1
    #anxiety
    anx1 <- pr.nn1$net.result[,13]*8+1
    #hope
    hop1 <- pr.nn1$net.result[,14]*8+1
    #pride
    pri1 <- pr.nn1$net.result[,15]*8+1
    #fear
    fea1 <- pr.nn1$net.result[,16]*8+1
    #happy
    hap1 <- pr.nn1$net.result[,17]*9
    #gratitude
    gra1 <- pr.nn1$net.result[,18]*8+1
    
    neuralnetworkneg=as.array(round(c(reg1,gui1,res1,fru1,ang1,sad1,bor1,anx1,fea1),2))
    par(mar=c(9,4,1,1))
   barplot(t(neuralnetworkneg),
            names=c("regret","guilt","resignation","frustration","anger","sadness","boredom","anxiety","fear"),main='Negative Emotion Predictions',col='gold',
            border='black',cex.axis=1.3,cex.names=1.3,ylim=c(0,9),ylab="Intensity",las=3)
    
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

