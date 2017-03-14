library(shiny)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Visualization of Lee-Carter Model"), 
  # theme = shinytheme("paper"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("yearslider"), 
      uiOutput("agebox"),
      fileInput("file", 
                label = h5("Change nMx file"), 
                accept = "text/plain"), 
      helpText("Default data is Sweden Mx 5x1.",
               "Download additional data sets from", 
               a("http://www.mortality.org/", 
                 href = "http://www.mortality.org/"))
    ), 
    
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("A: nmx by Age", plotOutput("ageplot")), 
        tabPanel("B: nmx by Year", plotOutput("yearplot"))
      )
    )
  )
)