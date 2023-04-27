
server <- function(input, output) {
  #QSI
  data_qsi <- eventReactive(input$submitBtn,{input$simplexData}, ignoreNULL = FALSE)   #only stores values in data_qsi and occurs when user clicks the submit button
  observeEvent(data_qsi(), {simplex_DataPoints.update <<- unique(data_qsi())})  #updates the matrix when button is clicked and inputs within simplex_DataPoints.update
  
  XYvalues <- eventReactive(input$submitBtn, {simplex_DataPoints.update},ignoreNULL = TRUE)  #only stores the values of data points within x and y columns when user clicks submit
  yVal <- eventReactive(input$submitBtn, {input$yInput},ignoreNULL = TRUE)   #only stores the value the user inputted to be evaluated when user clicks submit
  
  output$final  <- renderPrint({   #outputs the functions and the resulting evaluated value
    xy = list(c(XYvalues()[,1]),c(XYvalues()[,2]))   #stores the values in x and y column within a list
    print(poly.qsi(xy,as.numeric(yVal())))   #calls the function poly.qsi in exer 8
  
  })  
  
  #SIMPLEX
  intTableau <- eventReactive(input$simplex_submitBtn,{input$initialTableau}, ignoreNULL = FALSE) #only stores values in intTableau and occurs when user clicks the submit button
  observeEvent(intTableau(), {initial_tableau.update <<- unique(intTableau())})                    #updates the matrix when button is clicked and inputs within initial_tableau.update 
  
  intTableauValues <- eventReactive(input$simplex_submitBtn, {initial_tableau.update},ignoreNULL = TRUE)   #only stores the values inputted by user within the matrix
  checkMinOrMax <- eventReactive(input$simplex_submitBtn, {input$minMax},ignoreNULL = TRUE)                #stores the choice of user in selectInput
  solveGivenProblem <- eventReactive(input$simplex_submitBtn, {input$solveProblem},ignoreNULL = TRUE)       #stores the choice of user in selectInput
  
  output$final_Simplex  <- renderPrint({  #prints the final tableau, basic solution, optimal value, and shipping num
    minOrMax = checkMinOrMax()    #stores value from the eventReactive   
    solveProb = solveGivenProblem()
    
    if(minOrMax == TRUE && solveProb == FALSE) {  
      print("MAXIMIZATION")
      print(simplex(intTableauValues(),TRUE,FALSE))
    }
    
    if(minOrMax == "FALSE" && solveProb == "FALSE") {
      print("MINIMIZATION")
      print(simplex(intTableauValues(),FALSE,FALSE))
      
    }
    
    if(minOrMax == "FALSE" && solveProb == "TRUE") {
      print("GIVEN PROBLEM")
      print(simplex(intTableauValues(),FALSE,TRUE))
    }
    
    if(minOrMax == "FALSE" && solveProb == "TRUE") {
      print("Not an Option. Choose Again!")
    }
    
  }) 
  
}


