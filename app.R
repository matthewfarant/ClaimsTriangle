# WEB APP LINK:
# https://littleknife.shinyapps.io/ClaimsTriangle/

library(shiny)
library(tidyverse)
library(ChainLadder)
library(lattice)
claim<-read_csv('claimdata.csv')
# UI
ui <- fluidPage(
    h1("Loss Development Triangle Generator"),
    h3('MASA Hackathon 2020'),
    h5('A loss development triangle is a unique way of arranging the annual loss evaluations for several past policy periods. By arranging the loss evaluations for past years in a table, we can analyze the change in losses from one evaluation
to the next. In this app, we are using the Mack\'s chain-ladder method, one of the popular ways that insurance companies use to estimate their required claim reserves.'),
    sidebarLayout(
        sidebarPanel(
            selectInput('class','Class',1:14),
            selectInput('devtriangle', 'Development Triangle',
                        c('gpay','npay','Gross Incurred Losses','Net Incurred Losses'))
        ),
        mainPanel(
            tabsetPanel(
                type='tab',
                tabPanel('Dictionary',htmlOutput('text')),
                tabPanel('Data',dataTableOutput('data')),
                tabPanel('Triangle',h3('Incremental'),verbatimTextOutput('triangle'),
                         h3('Cumulative'),verbatimTextOutput('trianglecum')),
                tabPanel('Plot',h3('Incremental'),plotOutput('plotinc'),
                         h3('Cumulative'),plotOutput('plotcum')),
                tabPanel('Chain Ladder',verbatimTextOutput('cd'),plotOutput('cdplot'))
            )
        )
    ),
    hr(),
    print('This app is only used for MASA Hackathon 2020 purposes and not intended for public use.')
)

# Server
server <- function(input, output) {
    
    output$triangle<-renderPrint({as.triangle(filter(claim,classno==input$class),dev='Development_Year',origin='yol',
                                 value=input$devtriangle)})
    output$trianglecum<-renderPrint({incr2cum(as.triangle(filter(claim,classno==input$class),dev='Development_Year',origin='yol',
                                                          value=input$devtriangle))})
    output$data<-renderDataTable(filter(claim,classno==input$class))
    output$text<-renderUI({
        HTML(paste(
        "<b> Class No </b>- Represents the Class No. for the claim under consideration. A class no. could represent
any line of business in general insurance such as Fire, Motor, Marine Hull etc.",
"<b> YOL </b>- stands for the Year of Loss - this is the year in which the loss actually happened. It is also
known as the Accident Year.",
"<b> Acprd </b>- stands for Accounting Period. This is the accounting period in which the entry was made in
the company records. It may or may not be in the same year or month as the year and month of loss
due to reporting delays",
"<b> Policy </b>- Represents the policy number against which a claim is being made",
"<b> Claim </b>- represents the claim number",
"<b> DoL </b>- Date of Loss - the date the incident leading the insured to claim happened",
"<b> Daterep </b>- Date the claim was Reported",
"<b> Gpay </b>- Gross Paid Loss amount",
"<b> Gos </b>- Gross Outstanding Loss amount",
"<b> Npay </b>- Net Paid Loss amount",
"<b> Nos </b>- Net Outstanding Loss amount",sep="<br/>"))
        })
    output$plotinc<-renderPlot({plot(as.triangle(filter(claim,classno==input$class),dev='Development_Year',origin='yol',
                                              value=input$devtriangle),lattice=TRUE)})
    output$plotcum<-renderPlot({plot(incr2cum(as.triangle(filter(claim,classno==input$class),dev='Development_Year',origin='yol',
                                                          value=input$devtriangle)),lattice=TRUE)})
    output$cd<-renderPrint({MackChainLadder(incr2cum(as.triangle(filter(claim,classno==input$class),dev='Development_Year',origin='yol',
                                                                 value=input$devtriangle)))})
    output$cdplot<-renderPlot({plot(MackChainLadder(incr2cum(as.triangle(filter(claim,classno==input$class),dev='Development_Year',origin='yol',
                                                                         value=input$devtriangle))),lattice=TRUE)})
}
# Run the application 
shinyApp(ui = ui, server = server)