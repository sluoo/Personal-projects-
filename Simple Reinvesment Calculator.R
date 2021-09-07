library(shiny)

# Define UI for application that draws a histogram
# Frontend 
ui <- fluidPage(
    
    # Application title
    titlePanel("Dividend Reinvestment Calculator"),
    h6("Note: the amount specified adds 2 additional stocks to maintain DRIP. 
           Happy investing!", align="left"),
    br(),
    sidebarPanel(
        numericInput("price",h4("What is the current stock price?",align="center"),
                     value=100),
        numericInput("currentHold",h4("How many stocks do you have? If none, enter 0.",
                                      align="center"),value=0),
        numericInput("dividend",h4("What is the current dividend yield?",align="center"),
                     value=1.23),
        selectInput("paymentTime",h4("How frequent is the dividend paid?",align="center"),
                    choices = c("Quarter"=4,"Monthly"=12,"Bi-Yearly"=2,"Yearly"=1),
                    selected = "Quarter")
        
    ),
    mainPanel(
        h2("Given the current number of shares you have, the number of stocks required for DRIP is",align="center"),
        tags$span(h2(textOutput("NewStock"),align="center"),style="color:blue"),
        
        h2("The amount of cash needed to purchase at the current share price is",align="center"),
        tags$span(h2(textOutput("Amount"),align="center"),style="color:blue"),
    
    )
)

# Define server logic required to draw a histogram
# Backend
server <- function(input, output) {
    output$NewStock <-renderText({
        dy <- input$dividend
        p <- 100 
        fq <- as.numeric(input$paymentTime)
        currentPrice <- input$price
        initStock <- input$currentHold 
        amountPerFq <- 1/fq * dy/100 * currentPrice
        Need <- floor((currentPrice/amountPerFq)-initStock + 2)
        paste(Need," stocks") 
    })
    #code 
    output$Amount <-renderText({
        dy <- input$dividend
        p <- 100 
        fq <- as.numeric(input$paymentTime)
        currentPrice <- input$price
        initStock <- input$currentHold 
        amountPerFq <- 1/fq * dy/100 * currentPrice
        Need <- floor((currentPrice/amountPerFq)-initStock + 2)
        total <- Need * currentPrice
        paste0("$",total)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
