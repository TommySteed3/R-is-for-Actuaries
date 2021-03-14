
#### GLOBAL
library(shiny)
library(shinydashboard)  


## FRONT END
ui <- dashboardPage(
  dashboardHeader(),
 
  dashboardSidebar(), 
                                          
  dashboardBody()
  ) 

## BACK END
server <- function(input, output,session) {
  
}

## DEPLOY
shinyApp(ui, server)
