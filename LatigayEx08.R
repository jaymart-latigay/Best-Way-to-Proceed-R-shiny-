#Author: Jaymart G. Latigay
#student #: 2020-46260
#Course: CMSC 150
#Exercise #8
#Date: December 05, 2022

source("server.R")

#TO EVALUATE A VALUE X: RUN THE PROGRAM, THEN TYPE IN CONSOLE: poly.qsi(<list>,<data_to_evaluate>) 

#TEST CASES
#x = c(3.0,4.5,7.0,9.0)
#y = c(2.5,1.0,2.5,0.5)

#x = c(3.0,4.5,7.0,9.0,11.0,13.0)
#y = c(2.5,1.0,2.5,0.5,4.0,4.6)
#data = list(x,y)



checkXYCount <- function(val) {   #checks if the # of elements of x and y are equal in count
  countOfX = length(val[[1]])
  countOfY = length(val[[2]])
  
  if(countOfX == countOfY) {
    return(TRUE)
  } else return(FALSE)
}

checkXInput <- function(val, x) {  #checks if the assigned x value, is within the the range of elements in col x
  
  if(x <= val[[1]][length(val[[1]])]) {
    return(TRUE)
  } else return(FALSE)
}


getInternalKnots <- function(val, intKnotsIndex) {  #retrieves the corresponding value v
  intKnot = c()
  intKnot[1] = (val[[1]][intKnotsIndex])^2
  intKnot[2] = val[[1]][intKnotsIndex]
  intKnot[3] = 1
  
  return(intKnot)
}

getEndPoints <- function(val, intKnotsIndex) {  #retrieves the corresponding value v
  endPoint = c()
  endPoint[1] = (val[[1]][intKnotsIndex])^2
  endPoint[2] = val[[1]][intKnotsIndex]
  endPoint[3] = 1
  
  return(endPoint)
}

getFirstDeriv <- function(val, intKnotsIndex) {   #gets values per internal knot
  firstDeriv = c()
  firstDeriv[1] = (val[[1]][intKnotsIndex])*2
  firstDeriv[2] = 1
  firstDeriv[3] = -(val[[1]][intKnotsIndex])*2
  firstDeriv[4] = -1
  
  return(firstDeriv)
}

funct <- function(functionString) {
  ex = eval(parse(text = functionString))  #turns the string into an expression
  return(ex)
} 

checkX <- function(val, x) {
  if(x %in% val) return(FALSE)   #checks whether x is in the vector val
  else return(TRUE)
}

checkRange <-function(val, x) {  #checks if x is within the range of values of data[[1]]
  if(x < val[[1]][1] || x >val[[1]][length(val[[1]])]) return(FALSE)
  else return(TRUE)
}
  
  
  

poly.qsi <- function(data, x) {
  if(!checkX(data[[1]], x)) {
    cat(x, " is a given data")
  }

  if(!checkRange(data, x)) {
    print(NA)
  }
  
  
  if(checkXYCount(data) && checkXInput(data,x) && checkX(data[[1]], x) && checkRange(data, x)) {   #checks if the # elements in x and y are equal and if the inputted x is within the range of col x
    
    dataPointsCount = length(data[[1]]) 
    interval = dataPointsCount -1
    rowCount = 3 * interval  

    internalKnotsCount = dataPointsCount -2
    endPointsCount = 2
    
    #Matrices
    augCoeffMatrix = matrix(nrow =  rowCount, ncol =  rowCount, byrow = TRUE)
    internalKnotsMatrix = matrix(nrow = internalKnotsCount*2, ncol = rowCount, byrow = TRUE)
    endPointsMatrix = matrix(nrow = 2, ncol = rowCount, byrow = TRUE)
    firstDerivMatrix = matrix(nrow = internalKnotsCount, ncol = rowCount, byrow = TRUE)
    RHS = matrix(nrow = rowCount-1, ncol = 1, byrow = TRUE)
    
    lastPastColIndex = 0
    checkIntKnots = 0
    intKnotsIndex = 2
   
    values = c()  #stores the values that getInternalKnots get
    for(i in 1:(internalKnotsCount*2)) {  #row 
      values[1:3] = getInternalKnots(data,intKnotsIndex) # gets the a^2,b,c values 
      
      for(j in 1:3) {  #col    ;since there are 3 variables/values to consider: a,b and c
        internalKnotsMatrix[i,(lastPastColIndex+j)] = values[j]  #inputs value to internalKnotsMatrix
      }
      
     
      lastPastColIndex = lastPastColIndex + 3  #so that the 2nd iteration of the internal knot values, they will be in the succeeding columns
      
      if(i%%2 == 0) {  #since every internal knots gets two iterations of itself
        lastPastColIndex = lastPastColIndex - 3   #for the next internal knot equation values to be within the same col with its predecessor
        intKnotsIndex = intKnotsIndex + 1    #increments the index of knots to be evaluated within the given data
      }
    }
    
    subtr = 2
    endPointVal <- c()
    for(ind in 1:2) {
      if(ind == 1) endPointVal[1:3] = getEndPoints(data,ind) # gets the a^2,b,c values of first end point
      if(ind == 2) endPointVal[1:3] = getEndPoints(data,dataPointsCount) # gets the a^2,b,c values of the last end point
      
      for(j2 in 1:3) {
        if(ind == 1) endPointsMatrix[ind,j2] =  endPointVal[j2] #stores the values of first end point on the first 3 cells in matrix
        if(ind == 2) {
          endPointsMatrix[ind,rowCount-subtr] =  endPointVal[j2]  #stores the values of last end point on the last 3 cells in matrix
          subtr = subtr- 1
          }
      }
    }
    
    intKsIndex = 2   #provides the index of the inner knots in x col in data
    firstDeriv = c()   #vector that stores the values generated 
    addToIndex = 0    #initial value of value to add to the index of a value in the matrix
    
    for(indx in 1:internalKnotsCount) {
      firstDeriv[1:4] = getFirstDeriv(data,intKsIndex)   #stores the generated value from getFirstDeriv function
      for(j3 in 1:4) {  #since there are 4 values to be considered
        if(j3 <= 2) firstDerivMatrix[indx,j3+addToIndex] = firstDeriv[j3]  #stores the left hand side values in the array
        if(j3 > 2) firstDerivMatrix[indx,(j3+addToIndex+1)] = firstDeriv[j3] #stores the right hand side value in the array
      }
      addToIndex = addToIndex + 3 #increments addToIndex by 3 per iteration of indx
      intKsIndex =  intKsIndex + 1 #increments intKsIndex  by 1 per iteration of indx
    }
   
   
   augCoeffMatrix =  rbind(internalKnotsMatrix,endPointsMatrix,firstDerivMatrix)  #combines all submatrices together to form augCoeffMatrix 
   augCoeffMatrix = augCoeffMatrix[,-1]  #removes the 1st column from matrix, since a1 = 0, hence it is omitted
   
   for(row in 1:(rowCount-1)) {
     for(col in 1:(rowCount-1)) {   #locates cells that have NA values and replaces them w/ 0
       if(is.na(augCoeffMatrix[row,col])) {
         augCoeffMatrix[row,col] = 0
       }
     }
   }
   
   #gets the values for RHS
   v = 2
   for(index in 1: (rowCount-1)) {   
     if(index <= internalKnotsCount*2) {  #gets the RHS of ax2 + bx +c 
       RHS[index,1] = data[[2]][v]   #stores the values of y in RHS per column
       if(index%%2 == 0) {  #checks whether there is already 2 copies of the same RHS from the same equation
         v = v + 1  #increments, to allow to move to another value in y
       }
     }
     if(index > internalKnotsCount*2 && index <= (internalKnotsCount*2) + 2) {  #gets the RHS of the 2 endpoints
       if(index == (internalKnotsCount*2) + 1) RHS[index,1] = data[[2]][1]  #smallest in value end point
       else RHS[index,1] = data[[2]][dataPointsCount]   #largest in value endpoint
     }
     
     if(index > (internalKnotsCount*2) + 2) {  #gets the RHS of the first derivative of each internal knot
       RHS[index,1] = 0
     }
   }
   
   augCoeffMatrix = cbind(augCoeffMatrix, RHS)   #merges the 2 matrix by column, respectively
   #print(augCoeffMatrix)  #prints the matrix
   
   eqtnValues = c()   #stores the values returned by Gauss Jordan elim
   eqtnValues[1:rowCount-1] = GaussJordanElimination(augCoeffMatrix)  #implements Gauss Jordan Elim to the finished Augmented Coeff matrix
   
   
   #transforms the values returned by Gauss Jordan Elim into STRING polynomial equations
   locateVal = 0
   poly = c()
   for(val in 1:interval) {
     if(val == 1) {   #for the first 2 column in first row, as a1 is omitted
       poly[val] = paste("function(x) ", eqtnValues[val], "* x + ", eqtnValues[val+1])
       locateVal = locateVal + 2  #increments this, to locate the next values in  eqtnValues
      }
     else { #for the rest of the rows
       poly[val] = paste("function(x) ", eqtnValues[locateVal+1]," * x^2 + ", eqtnValues[locateVal+2], "* x + ", eqtnValues[locateVal+3])
       locateVal = locateVal + 3 #increments by 3
    }
     
   }
   #checks the x vector for the range in which x is within
   range = 0
   for(q in 1:interval) {
     if(x > data[[1]][q]) {
       if(x < data[[1]][q+1]) {
         range = q   
         }
      }
   }
   
   #allows multiple input from the user
   funct = eval(parse(text=poly[range]))
   finalList = list(qsi.fxns = poly, y = funct(x))
   
   return(finalList)
  }
}



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

pivot<-function(mat,x){
  
  mat=mat[order(abs(mat[,x]), decreasing = TRUE),]
  return(mat)
}

GaussJordanElimination <- function(augCoefMatrix){
  y=matrix()
  x=augCoefMatrix
  mat=augCoefMatrix
  n=nrow(mat)
  asMat=mat 
  for(i in 1:n){                             
    if (i!= n){
      tempMat=matrix()
      asMat[i:n,]=pivot(asMat[i:n,],i)          
      pivotRow=asMat[i,]                                    
      if(asMat[i,i]==0){                                    
        return(NA)
      }
    }
    asMat[i,]=asMat[i,]/asMat[i,i]                                   
    for(j in 1:n){                  
      if(i==j){next}                 
      normRow=asMat[j,i] * asMat[i,]                      
      asMat[j,] = asMat[j,]-normRow               
    }
    
  } 
  y[ncol(asMat)-1]=asMat[nrow(asMat),ncol(asMat)]/asMat[nrow(asMat),ncol(asMat)-1]  #same backwards substitution
  for (i in (n - 1): 1){
    y[i] = (asMat[i,n+1] - sum(asMat[i, (i+1):n] * y[(i+1):n])) / asMat[i,i]
  }
  finalList=list(matrix=(asMat),Variables=(c(colnames(asMat)[1:length(colnames(asMat))-1])),solutionSet=y)
  return(y)  
}




