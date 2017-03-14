library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr) 
library(forcats)
library(magrittr)
library(stringr)
library(ggplot2)

source("lc_functions.R")

swe_mort <- read_table(file.path("data", "Mx_5x1.txt"), skip = 2, na = ".")

d <- wrap_lc_total(swe_mort)

server <- function(input, output) {
  
  data <- reactive({ 
    if (is.null(input$file)) return(NULL)
    
    wrap_lc_total(read_table(input$file$datapath, skip = 2, na = "."))
  })
  
  output$yearslider <- renderUI({
    d <- if (is.null(data())) d else data()
    sliderInput(inputId = "Year", 
                label = h3("A: Year"), 
                value = median(d$Year), 
                min = min(d$Year), max = max(d$Year), 
                sep = "",
                step = 1)
  })
  
  output$ageplot <- renderPlot({
    d <- if (is.null(data())) d else data()
    if (!is.null(input$Year)) {
      plot_nmx_by_age_for_year(d, input$Year)
    } else {
      ggplot() + geom_blank() + theme_minimal()
    }
  })
  
  output$agebox <- renderUI({
    d <- if (is.null(data())) d else data()
    selectInput(inputId = "Age", 
                label = h3("B: Age group"), 
                choices = levels(d$Age), 
                selected = levels(d$Age)[ceiling(.75*length(levels(d$Age)))])
  })
  
  output$yearplot <- renderPlot({ 
    d <- if (is.null(data())) d else data()
    if (!is.null(input$Age)) {
      plot_nmx_by_year_for_age(d, input$Age)
    } else {
      ggplot() + geom_blank() + theme_minimal()
    }
  })
}