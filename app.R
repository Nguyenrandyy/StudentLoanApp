library(shiny)
library(tidyverse)
library(rsconnect)

Data <- read_csv("SCFP2019.csv")

Data %>% 
  mutate(Data, loan_ratio = EDN_INST/INCOME) %>%
  mutate(Data, payment_ratio = (PAYEDU1 + PAYEDU2 +PAYEDU3 +PAYEDU4 +PAYEDU5+PAYEDU6)*12/INCOME)->Data

Data<-select(Data,EDN_INST,PAYEDU1,PAYEDU2,PAYEDU3,PAYEDU4,PAYEDU5,PAYEDU6,PAYEDU7,INCOME,RACE,HHSEX,AGECL,loan_ratio,payment_ratio)

Data <- Data[Data$EDN_INST!=0,]

quant<-function(DF,ResponseV,X1)
{
  if (ResponseV=="Total_Loan_Amount" && X1=='None')
  {quantile(Data$EDN_INST,na.rm=TRUE)}
  else if (ResponseV=="loan_ratio" && X1=='None')
  {quantile(Data$loan_ratio,na.rm=TRUE)}
  else if (ResponseV=="payment_ratio" && X1=='None')
  {quantile(Data$payment_ratio,na.rm=TRUE)}
  else if (ResponseV=="Total_Loan_Amount" && X1=="GENDER")
  {do.call("rbind", tapply(Data$EDN_INST, Data$HHSEX, quantile))} 
  else if (ResponseV=="Total_Loan_Amount" && X1=="RACE")
  {do.call("rbind", tapply(Data$EDN_INST, Data$RACE, quantile))}
  else if (ResponseV=="Total_Loan_Amount" && X1=="AGE")
  {do.call("rbind", tapply(Data$EDN_INST, Data$AGECL, quantile))}
  else if (ResponseV=="loan_ratio" && X1=="GENDER")
  {do.call("rbind", tapply(Data$loan_ratio, Data$HHSEX, quantile))}
  else if (ResponseV=="loan_ratio" && X1=="RACE")
  {do.call("rbind", tapply(Data$loan_ratio, Data$RACE, quantile))}
  else if (ResponseV=="loan_ratio" && X1=="AGE")
  {do.call("rbind", tapply(Data$loan_ratio, Data$AGECL, quantile))}
  else if (ResponseV=="payment_ratio" && X1=="GENDER")
  {do.call("rbind", tapply(Data$payment_ratio, Data$HHSEX, quantile,na.rm=TRUE))}
  else if (ResponseV=="payment_ratio" && X1=="RACE")
  {do.call("rbind", tapply(Data$payment_ratio, Data$RACE, quantile,na.rm=TRUE))}
  else if (ResponseV=="payment_ratio" && X1=="AGE")
  {do.call("rbind", tapply(Data$payment_ratio, Data$AGECL, quantile,na.rm=TRUE))}
}

#import Histogram Function

source("student-loan-app-functions.R")

#App.R

ui <- fluidPage(
  titlePanel("Student Loan Information App"),
  sidebarLayout(
    sidebarPanel(
      selectInput('plot1', h4('Choose the Variable to Plot'), choices=c("Total_Loan_Amount","loan_ratio","payment_ratio"),selected="Total_Loan_Amount"),
      selectInput('add1', h4('Choose an additional variable'),choices=c("None","GENDER","RACE","AGE"),selected="None"),
      selectInput('add2', h4('Choose another additional variable'),choices=c("None","GENDER","RACE","AGE"),selected="None"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot',plotOutput(outputId = "LoanPlot")),
        tabPanel("Quantiles",tableOutput(outputId = "Qtable"))
      )
    )
  )
)


server <- function(input,output){
  output$LoanPlot <- renderPlot({
    x1=input$plot1
    x2=input$add1
    x3=input$add2
    
    hist_student_loans(DF=Data,x1,x2,x3)
  })
  output$Qtable<-renderTable({
    x1=input$plot1
    x2=input$add1
    
    quant(DF=Data,x1,x2)
    
  })
}

shinyApp(ui=ui, server=server)