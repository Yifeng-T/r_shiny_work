
library(ggplot2)
library(plyr)
library(shiny)
library(dplyr)
library(tidyr)
library(glue)
library(plotly)

table_q1 = read.csv("./Data//que1.csv")

themes = colnames(table_q1)

theme_list = list(themes[1],themes[2],themes[3],themes[4],themes[5],themes[6],themes[7],themes[8], themes[9],
                  themes[10], themes[11], themes[12], themes[13], themes[14], themes[15], themes[16], 
                  themes[17], themes[18], themes[19], themes[20], themes[21], themes[22])

avg=c(0.432878788,0.485800866,0.55952381,0.611111111,0.572674419,0.625,0.601470588,0.659090909,0.738095238,
      0.511904762,0.611111111,0.8125,0.84375,0.96875,0.716666667,0.416666667,0.8,0.825,0.742857143,0.861111111,
      0.95,0.45)

theme = c("A","B", "C","D","E","F","G","H","I","J", "K","L","M","N","O","Q","R","S","T","U","V","W")
table_q1_avg = data.frame(theme, avg)

class(themes[1])
#ui
ui <- shinyUI(
  pageWithSidebar(
    headerPanel("Question1 themes data point"),
    sidebarPanel(
      selectInput("select_theme1", "Select the theme1", 
                  choices = theme_list),
      selectInput("select_theme2", "Select the theme2", 
                  choices = theme_list),
      selectInput("select_theme3", "Select the theme3", 
                  choices = theme_list)),
    
    
    mainPanel(
      plotlyOutput("Delays",width="650",height = "450px"),
      plotlyOutput("t2",width="650",height = "450px"),
      plotlyOutput("t3",width="650",height = "450px"),
      plotlyOutput("Dest",width="700",height = "450px")
    )
  ))

#server
server <- function(input, output){
  
  output$Delays <- renderPlotly({
    key = input$select_theme1
    n_col = match(key, themes)
    
    df = data.frame(table_q1[,n_col])
    colnames(df) = c("rank")
    
    
    p <- ggplot(df, aes(x = rank)) + geom_density()+geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FF6666")+ggtitle("Selected theme_1")
    ggplotly(p)
  })
  
  #===========================
  output$t2 <- renderPlotly({
    key = input$select_theme2
    n_col = match(key, themes)
    
    df = data.frame(table_q1[,n_col])
    colnames(df) = c("rank")
    
    
    p <- ggplot(df, aes(x = rank)) + geom_density()+geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FF6666")+ggtitle("Selected theme_2")
    ggplotly(p)
  })
  
  #===========================
  
  output$t3 <- renderPlotly({
    key = input$select_theme3
    n_col = match(key, themes)
    
    df = data.frame(table_q1[,n_col])
    colnames(df) = c("rank")
    
    
    p <- ggplot(df, aes(x = rank)) + geom_density()+geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FF6666")+ggtitle("Selected theme_3")
    ggplotly(p)
  })
  
  
  
  #============================
  
  output$Dest <- renderPlotly({
    df <- table_q1_avg
    p <- ggplot(df, aes(x= theme, y = avg))  + geom_point() + ggtitle("All themes Avg Standardized Ranks")
    theme(axis.text.x = element_text(angle = 60))
    
    ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)