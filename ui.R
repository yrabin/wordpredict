
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


textareaInput <- function(inputId, label, value="", placeholder="", rows=2) {
  tagList(
    div(strong(label), style="margin-top: 5px;"),
    tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
    tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
}


shinyUI(fluidPage(
  
  fluidRow( class = 'titleRow',
            column(width = 12, align='center',
                   # Application title
                   headerPanel("Word Prediction")
            )
  ),
  
  fluidRow( class = 'resultRow',
            column(width = 12,  align='center',
                   uiOutput("prediction")
            )
  ),
  
  # Multi-text input box
  fluidRow( class = 'inputRow',
            column(width = 12,  align='center',
                   textareaInput("multitxt", "Type your text:", rows=5)
            )
  ),
  
  tags$head(tags$style("
      .titleRow  { height:80px; }
      .resultRow { height:90px;  background-color:rgb(233,233,233); }
      .inputRow  { height:150px; }
      .debugRow  { height:350px; background-color:pink; }
  "))

))
