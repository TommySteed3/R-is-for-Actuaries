## app.R ##
library(shiny)
library(shinydashboard)  ## USING shinydashboard - CREATES THIS NICE TEMPLATE OUT OF THE BOX

library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr) # summarise_at()
library(forcats) # helps reorder variables
library(gridExtra) # to use grid arrange - which we have seen


# SEVERAL LESSONS (THINK - PROJECTS) USING SAME DATASET - DON'T WANT A BUNCH OF COPIES - NEXT LEVEL (OF MANY)

data.mortality <- fread('mortality.csv')

#################################################################################################################################################################
#################################################################        FRONT END                   ############################################################
#################################################################################################################################################################
# FRONT END IS HOW THE APP INTERACTS WITH THE USER (UI = USER INTERFACE)

ui <- dashboardPage(
  # NAMETAG
  dashboardHeader(title = "Mortality Analysis"),
 
  ## DEFINE HOME, TABS AND SUB-TABS
  dashboardSidebar(sidebarMenu(id="tabs",
                   menuItem("HOME", tabName="home_tab", icon = icon("home"), selected=TRUE),
                   menuItem("Interactive Experience", tabName = "variabletab1", icon = icon("line-chart"),
                          menuSubItem("Univariate Analysis", tabName = "univariate_tab", icon = icon("angle-right")),
                          menuSubItem("Bivariate Analysis", tabName = "bivariate_tab", icon = icon("angle-right")))  ## Closes menuItem
                                )), #  Closes Dashboard Sidebar
                                          
  ##  LAYOUT OF THE HOME PAGE AND SUB-TABS FROM ABOVE
  dashboardBody(tabItems(
      tabItem(tabName = "home_tab",
      fluidRow(box(width = 12,
                   fluidPage(
                     mainPanel(
                       htmlOutput("selected_HTML")))
                     ))),  # Closes tabItem
            
      tabItem(tabName = "univariate_tab",
              fluidRow(box(width = 12,
                           selectInput("univariate_selection","Category:",colnames(data.mortality),selected = 'Gender'))),
              fluidRow(box(width = 12,
                           plotlyOutput("atoe_plot", height=375))),
              fluidRow(box(width = 12,
                           plotlyOutput("death_plot", height=375)))),  # Closes tabItem
      
      tabItem(tabName = "bivariate_tab",
              fluidRow(box(width = 6,
                           selectInput("bivariate_selection1","Category:",colnames(data.mortality),selected = 'Gender')),
                       box(width = 6,
                           selectInput("bivariate_selection2","Category:",colnames(data.mortality),selected = 'Gender'))),  # Closes fluidRow
              fluidRow(box(width = 12,
                           plotlyOutput("bivariate_atoe", height=375))),
              fluidRow(box(width = 12,
                           plotlyOutput("bivariate_deaths", height=375))))  # Closes tabItem
                              ))  # Closes Dashboard Body

  ) # Closes Dashboard Page = Closes Front End

#################################################################################################################################################################
#################################################################        BACK END                   ############################################################
#################################################################################################################################################################

server <- function(input, output,session) {
  
  data.univariate <- reactive({
  data.mortality %>%
    group_by_(input$univariate_selection) %>%
    summarise_at(vars(Deaths,qx2015VBT.policy),list(sum)) %>%
    mutate(atoe = Deaths/qx2015VBT.policy)
  })
  
  data.bivariate <- reactive({
  data.mortality %>%
    group_by_(input$bivariate_selection1,input$bivariate_selection2) %>%
    summarise_at(vars(Deaths,qx2015VBT.policy),list(sum)) %>%
    mutate(atoe = Deaths/qx2015VBT.policy)
  })
  
  output$atoe_plot <- renderPlotly({
    ggplot(data.univariate(),aes_string(x = input$univariate_selection, y = 'atoe')) + geom_point() + geom_line()
  })
  
  output$death_plot <- renderPlotly({
    ggplot(data.univariate(),aes_string(x = input$univariate_selection, y = 'Deaths')) + geom_bar(stat="identity")
  })
  
  output$bivariate_atoe <- renderPlotly({
    ggplot(data.bivariate(),aes_string(x = input$bivariate_selection1,y = input$bivariate_selection2)) + 
      geom_tile(aes(fill = atoe,colour = "white")) + scale_fill_gradient(low = "white",high = "red") + 
      geom_text(aes(label = round(atoe,2))) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  
  output$bivariate_deaths <- renderPlotly({
    ggplot(data.bivariate(),aes_string(x =input$bivariate_selection1,y = input$bivariate_selection2)) + 
      geom_tile(aes(fill = Deaths,colour = "white")) + scale_fill_gradient(low = "white",high = "red") + 
      geom_text(aes(label = Deaths)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  
  getHTML<-function() {
    return(includeHTML("C:/QCA/Week4/Final_Report.html"))
  }
  
  output$selected_HTML<- renderUI({getHTML()})
  
  
}

#################################################################################################################################################################
#################################################################              DEPLOY                 ############################################################
#################################################################################################################################################################
shinyApp(ui, server)

