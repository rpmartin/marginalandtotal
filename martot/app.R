library(shiny)
ui <- fluidPage(
    titlePanel("Tradeoff between pollution damages and abatement costs"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("emissions",
                     "Emissions:",
                     min = 0,
                     max = 200,
                     value = 100)
          ),
        mainPanel(
         plotOutput("plots")
      )
   )
)
server <- function(input, output) {
  output$plots <- renderPlot({
     e <- seq(0,200,.1)
     tac <- function(e) 60000-600*e+3*e^2/2
     mac <- function(e) 600-3*e
     td <- function(e) e^2
     md <- function(e) 2*e
     tsc <- function(e) tac(e)+td(e)
     par(mfrow=c(1,2))
     plot(e,md(e),type="l",col="blue",ylim=c(0,600),ylab="marginal costs",xlab="emissions")
     lines(e,mac(e),col="red")
     leg.txt <- c(paste("marginal damages= ",md(input$emissions)),paste("marginal abatement cost= ",mac(input$emissions)))
     legend("top",leg.txt,col=c("blue","red"),lty=1)
     points(input$emissions,md(input$emissions),col="blue")
     polygon(c(0,input$emissions,input$emissions),c(0,md(input$emissions),0),col=rgb(0,0,1,.5),border=NA)
     polygon(c(200,input$emissions,input$emissions),c(0,mac(input$emissions),0),col=rgb(1,0,0,.5),border=NA)
     points(input$emissions,mac(input$emissions),col="red")
     plot(e,tsc(e),type="l",ylim=c(0,60000),ylab="total costs",xlab="emissions")
     leg.txt <- c(paste("total social cost= ",tsc(input$emissions)),paste("total damages= ",td(input$emissions)),paste("total abatement cost= ",tac(input$emissions)))
     legend("top",leg.txt,col=c("black","blue","red"),lty=1)
     lines(e,tac(e),col="red")
     lines(e,td(e),col="blue")
     points(input$emissions,td(input$emissions),col="blue")
     points(input$emissions,tac(input$emissions),col="red")
     points(input$emissions,tsc(input$emissions),col="black")
     })
}
shinyApp(ui = ui, server = server)

