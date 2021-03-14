#################################################################################################################################################################
#################################################################           GLOBAL                   ############################################################
#################################################################################################################################################################
library(shiny)
library(shinydashboard)  ## USING shinydashboard - CREATES THIS NICE TEMPLATE OUT OF THE BOX

#################################################################################################################################################################
#################################################################        FRONT END                   ############################################################
#################################################################################################################################################################
# FRONT END IS HOW THE APP INTERACTS WITH THE USER (UI = USER INTERFACE)

ui <- dashboardPage(
  # TITLE
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
      fluidRow(box(width = 12))),  # Closes tabItem
            
      tabItem(tabName = "univariate_tab",
              fluidRow(box(width = 12)),
              fluidRow(box(width = 12)),
              fluidRow(box(width = 12))),  # Closes tabItem
      
      tabItem(tabName = "bivariate_tab",
              fluidRow(box(width = 6),
                       box(width = 6)),  # Closes fluidRow
              fluidRow(box(width = 12)),
              fluidRow(box(width = 12)))  # Closes tabItem
                              ))  # Closes Dashboard Body

  ) # Closes Dashboard Page = Closes Front End

#################################################################################################################################################################
#################################################################        BACK END                   ############################################################
#################################################################################################################################################################

server <- function(input, output,session) {
  
}

#################################################################################################################################################################
#################################################################        DEPLOY                   ############################################################
#################################################################################################################################################################
shinyApp(ui, server)

