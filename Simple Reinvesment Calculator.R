library(shiny)

# Define UI for application that draws a histogram
# Frontend 
ui <- fluidPage(
    
    # Application title
    titlePanel("Dividend Reinvestment Calculator"),
    
    sidebarPanel(
        numericInput("price",h4("What is the current stock price?",align="center"),
                     value=100),
        numericInput("currentHold",h4("How many stocks do you have? If none,enter 0.",
                                      align="center"),value=0),
        numericInput("dividend",h4("What is the current dividend yield?",align="center"),
                     value=1.23),
        selectInput("paymentTime",h4("How frequent is the dividend paid?",align="center"),
                    choices = c("Quarter"=0.25,"Monthly"=0.0833,"Bi-Yearly"=1/2,"Yearly"=1),
                    selected = "Quarter")
    
        ),
    mainPanel(
        h2("The number of stocks required for purchase is",align="center"),
        tags$span(h2(textOutput("NewStock"),align="center"),style="color:blue"),

        h2("The amount of cash needed to purchase is",align="center"),
        tags$span(h2(textOutput("Amount"),align="center"),style="color:blue"),
        
    )
)

# Define server logic required to draw a histogram
# Backend
server <- function(input, output) {
    output$NewStock <-renderText({
        div <- input$dividend/100
        f <- as.numeric(input$paymentTime)
        initial <- input$currentHold 
        Need <- floor((1/div*f)) - initial
        paste(Need," stocks") 
    })
    #code 
    output$Amount <-renderText({
        div <- input$dividend/100
        f <- as.numeric(input$paymentTime)
        initial <- input$currentHold 
        Need <- floor((1/div*f)) - initial
        paste0("$",input$price * Need)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
