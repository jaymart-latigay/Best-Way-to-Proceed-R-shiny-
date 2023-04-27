#Name: Jaymart G. Latigay
#Student #: 2020-46260
#Date: December 8, 2022
#CMSC 150 (WX-2L): Exercise 9

source("server.R")



 
getPivotColIndex <- function(mat) {  #gets the pivot column
  smallestNegVal = 0
  smallestNegValIndex = 0
  for(i in 1: ncol(mat)) { 
    if(mat[nrow(mat),i] < 0) { #checks if current value is negative
      if(mat[nrow(mat),i] < smallestNegVal) {  #checks if current value is less than smallestNegVal
        smallestNegValIndex = i   #copies the index of the smallest negative value
      }
    }
  }
  return(smallestNegValIndex) #returns the index of smallest negative value
}

getPivotRowIndex <- function(mat, pivotColIndex) { #gets the Pivot row index
  smallestValue = c()
  for(i in 1:nrow(mat)-1) {
    value[i] = mat[i,ncol(mat)] / mat[i,pivotColIndex]   #solution value / values in pivot column
  }
  smallestValueIndex = which(value == min(value[value > 0]))  #gets the index of the smallest produced that is not negative
  
  return(smallestValueIndex[1])
}

gaussJordanElim <- function(mat,pivotRowIndex,pivotColIndex) {
  inColWithPE = 0  
  
  
  pivotElement = mat[pivotRowIndex,pivotColIndex]   
  for(i in 1:ncol(mat)) { 
    mat[pivotRowIndex,i] = mat[pivotRowIndex,i]/pivotElement   #normalized row
  }
  
  positiveValOfBot = mat[nrow(mat),pivotColIndex] * -1
  for(j in 1:nrow(mat)) {
    if(j == pivotRowIndex) next  #skips the pivot row
    inColWithPE = -mat[j,pivotColIndex]  #to make the values in the same column with Pivot Element equal to 0
    
    for(k in 1:ncol(mat)) {
      if(j != nrow(mat)) mat[j,k] = (inColWithPE * mat[pivotRowIndex,k]) + mat[j,k]  #updates all the values in the rows, except the last row
      
      if(j == nrow(mat)) {
        mat[j,k] = (positiveValOfBot * mat[pivotRowIndex,k]) + mat[j,k]  #updates the values in the last row
        
      }
      
    }
  }
  
  return(mat)
}

checkIfNegativeExist <- function(mat) {  #checks if negative value/s still exist in last column
  for(i in 1:ncol(mat)) {
    if(mat[nrow(mat),i] < 0) { #if there is still a negative value return TRUE
      return(TRUE)
    }
  }
  return(FALSE) #no more negative values
}


simplex <- function(tableau,isMax,problem) {
  
  negValInLastRow = TRUE
  rowLength = nrow(tableau)
  colLength = ncol(tableau)
  initialTableau = tableau
  
  
  #print(tableau)  #UNCOMMENT IF YOU WANT TO SEE THE INITIAL TABLEAU
  
  while(negValInLastRow == TRUE) {
  
    pivotColIndex = getPivotColIndex(tableau) 
    pivotRowIndex = getPivotRowIndex(tableau,pivotColIndex)

    tableau = gaussJordanElim(tableau,pivotRowIndex,pivotColIndex)  #applies gauss Jordan Elim to table
    #print(tableau) #UNCOMMENT IF YOU WANT TO SEE TABLEAU PER ITERATION
    negValInLastRow = checkIfNegativeExist(tableau)
  }
  
  basicSOl = matrix(nrow = 1, ncol = ncol(tableau),byrow = TRUE) #Final Basic Solution matrix
  shipping = NA  #will only be a matrix if problem == TRUE
   
  if(isMax && !problem) {  #maximization, not the problem
    for(i in 1:ncol(tableau)-1) {
      matColumn = tableau[,i] #stores specific column from tableau in matColumn
      
      if(length(matColumn[matColumn > 0]) == 1) { #checks if there is only 1 postive value in the column
        rowIndex = which(matColumn == matColumn[matColumn > 0])  #locates the row of the positive value in the column
        basicSOl[1,i] = tableau[rowIndex,ncol(tableau)]  #gets the corressponding value from Solution column based on the row of positive value
      } else basicSOl[1,i] = 0  #if there are more or less than 1 positive val
    }
  }
  
  if(!isMax) { #minimization, not the problem
    for(i in 1:colLength-1) {
      if(i < colLength-1) {
        basicSOl[1,i] = tableau[rowLength,i]
      } else basicSOl[1,i] = tableau[rowLength,i+1]
    }
  
    
    if(problem) { #if problem == TRUE
     shipping = matrix(c(basicSOl[9:23]),nrow = 3, ncol = 5, byrow = TRUE,dimname = list(c("DEN", "PHOE","DAL"), c("SAC","SL","ALB","CHI","NYC"))) #matrix that shows the number of plants ordered by each warehouse and where from 
    }
  }
  
  colnames(basicSOl) = colnames(tableau)  #names the columns
  basicSOl = basicSOl[,-colLength]  #removes the last column
  finalList = list("final.tableau" = tableau, "basic.solution" = basicSOl, "opt.val" =  basicSOl[length(basicSOl)]  ,"shipping.num" = shipping)
  

  return(finalList) #prints final list
}


