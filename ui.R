#Author: Jaymart G. Latigay
#Student #: 2020 - 46260
#Date: December 16, 2022
#CMSC 150 (WX-2L): Exercise 10



source("server.R")
library("shiny")
library("shinyMatrix")
library("shinythemes")

simplex_DataPoints <- matrix(c(0,0),1,2,dimnames = list(NULL, c("x", "y")))
initial_tableau <- matrix(c(0,0),1,2)

#Define UI
ui <- fluidPage(theme = shinytheme("flatly"),   #applies built in themes via the library shinythemes
        navbarPage(  #creates navigation bar in the top part of the window. Allows user to change tabls
          "CMSC 150: Exercise 10",
                  
          tabPanel("About",
              h5("Created by: Jaymart G. Latigay"),
              h5("Date made: December 16, 2022"),
              h5("What is this for: This web application is the integrat of the 3-part exercise for CMSC 150: NUMERICAL AND SYMBOLIC COMPUTATION."),
              h5("What does this porgram do: solves problems using Quadratic Spline Interpolation and Simplex Method(Maximization and Minimization)")
          ), #about and instructions tab
                  
          tabPanel("Quadratic Spline Interpolation",
            sidebarPanel(
              width = 6,  #size of the sidebar panel
              tags$h5(strong("Data inputs:")), 
              matrixInput("simplexData",
                value = simplex_DataPoints,
                rows = list(names = FALSE,extend = TRUE),
                cols = list(names = TRUE),   #allows colnames
                class = "numeric"
              ),
              textInput("yInput", "Enter the value to evaluate: "),
              actionButton(inputId = "submitBtn",label = "submit"), 
          ),
          
                           
            mainPanel(
            width = 6,
            h5(strong("Result:")),
            verbatimTextOutput("final")   #outputs the finalOutput
            
            ),
        ), #QSI tab
                  
            
        tabPanel("Simplex Method",   #created tab for QSI
          sidebarPanel(
           
            
            selectInput("minMax", "Are you doing Maximization or Minimization?:",    #provides user choices
              list(`Maximization` = TRUE,
                  `Minimization` = FALSE)
            ),
            
            selectInput("solveProblem", "Are you going to answer given problem?:",
              list(`Will answer problem` = TRUE,
                  `Will not answer problem` = FALSE)
                             
            ),
            
            width = 6,  #size of the sidebar panel
            tags$h5(strong("Initial Tableau:")),
            
            matrixInput("initialTableau",
              value = initial_tableau,
              rows = list(names = FALSE,extend = TRUE,editableNames = TRUE),
              cols = list(names = TRUE, extend = TRUE, editableNames = TRUE),   #editableNames allows colnames, extend increases the number of column when user inputs at the edge of matrix
              class = "numeric"
            ),
            actionButton(inputId ="simplex_submitBtn",label = "submit")
          ),
                           
        mainPanel(
          width = 6,
          h5(strong("Result:")),
          verbatimTextOutput("final_Simplex")
        )
      ) #Simplex tab
  ) #navbarPage
)





shinyApp(ui, server)



#Resources
#https://stackoverflow.com/questions/67599203/how-to-save-updated-shiny-matrix-input-values-into-a-data-frame-or-working-memo
    #there's a bug with the library shinyMatrix where when you input in col x first, for the second or third row, the cell for col x will be missing
    #To avoid this from happening, input values first in the y column before x column
    #According to the link, this issue has been resolved in the version 0.6.1 of shinyMatrix


