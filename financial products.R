#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Finanical Productions"),
   
   fluidRow(
     column(4,
       sliderInput("Initial",
                   "Initial Amount:",
                   min = 0,
                   max = 100000,
                   value = 1000,
                   step = 500,
                   pre = "$"),
       sliderInput("Annual",
                   "Annual Contribution",
                   min = 0,
                   max = 50000,
                   value = 2000,
                   step = 500,
                   pre = "$")
     ),
     
     column(4,
       sliderInput("Return",
                   "Return Rate (in %)",
                   min = 0,
                   max = 20,
                   value = 5,
                   step = 0.1),
       sliderInput("Growth",
                   "Growth Rate (in %)",
                   min = 0,
                   max = 20,
                   value = 2)
     ),
     
     column(4,
       sliderInput("Years",
                   "Years",
                   min = 0,
                   max = 50,
                   value = 10,
                   step = 1),
       
       
       selectInput("Facet",
                   "Facet",
                   c("YES","NO"),
                   selected = "NO")
     )
   ),
   
   mainPanel(
     br(),
     h3("Timelines"),
     plotOutput("Timelines"),
     br(),
     h3("Balances"),
     tableOutput("Balances")
   )

)

server <- function(input,output){
  
  dat <- reactive({
    future_value <- function(amount=0, rate=0, years=0) {
      return(amount*(1+rate)**years)
    }
    annuity <- function(contrib=0, rate=0, years=0) {
      return(contrib*((1+rate)**years-1)/rate)
    }
    growing_annuity <- function(contrib=0, rate=0, growth=0,years=0) {
      return(contrib*((1+rate)**years-(1+growth)**years)/(rate-growth))
    }
    
    amount <- input$Initial
    contrib <- input$Annual
    return_rate <- input$Return / 100
    growth_rate <- input$Growth / 100
    year <- input$Years
    
    y <- c(0:year)
    no <- c(amount)
    fixed <- c(amount)
    growing <- c(amount)
    type <- rep(c("no_contrib","fixed_contrib","growing_contrib"),each=year+1)
    
    for(i in 1:year){
      no[i+1] <- future_value(amount=amount,rate=return_rate,years=i)
      fixed[i+1] <- annuity(contrib=contrib,rate=return_rate,years=i)+no[i+1]
      growing[i+1] <- growing_annuity(contrib=contrib,rate=return_rate,growth=growth_rate,years=i)+no[i+1]
    }
    
    dat <- data.frame(y,no,fixed,growing)
    
    return(dat)
    
    #modalities <- data.frame(year=y,money=c(no,fixed,growing),type)
    #colnames(modalities) <- c("year","money","type")
    #
    #return(modalities)
  })
  
  output$Timelines <- renderPlot({
    type <- rep(c("no_contrib","fixed_contrib","growing_contrib"),each=input$Years+1)
    modalities <- data.frame(year=dat()$y,money=c(dat()$no,dat()$fixed,dat()$growing),type)
    colnames(modalities) <- c("year","money","type")
    
    #print(dat()['no'])
    
    gg <- ggplot(modalities)+
      geom_line(aes(x=year,y=money,color=type))+
      geom_point(aes(x=year,y=money,color=type))+
      labs(title = "Three modes of investing")
    
    if(input$Facet == "YES"){
      gg <- gg + 
        facet_wrap(~type) +
        geom_area(aes(x=year,y=money,fill=type,alpha=0.6))
    }
    show(gg)
  })
  
  output$Balances <- renderTable({
  dat()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

