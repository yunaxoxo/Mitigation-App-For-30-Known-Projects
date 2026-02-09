Simplex <- function(tableau){
  
  lastRow <- nrow(tableau)
  negRow <- any(tableau[lastRow, ] < 0)
  lastCol <- ncol(tableau) # solution column
  idx <- 1 # for iterations counter
  iterations <- list() # empty list for iterations
  basicSol <- list() # empty list for basic solution
  
  while(negRow){
    
    # find pivot column (most negative number in last row)
    pivotColumn <- which.min(tableau[lastRow, 1:(lastCol - 1)])
  
    colValues <- tableau[1:(lastRow-1), pivotColumn]
    
    # No valid pivot row -> infeasible or unbound
    if (all(colValues <= 0)) {
      return(list(
        status = "Infeasible",
        finalTableau = tableau,
        finalSolution = NULL,
        Z = NA,
        history = iterations,
        basicSolutionIterations = basicSol
      ))
    }

    # test ratios in the pivot column (only for positive)
    testRatios <- rep(NA, lastRow - 1) # vector of NA values
    for (i in 1:(lastRow-1)){
      if(tableau[i,pivotColumn] > 0){
        testRatios[i] <- tableau[i,lastCol] / tableau[i,pivotColumn]
      }
    }
    
    # pivot row index
    pivotRowIndex <- which.min(testRatios)
    
    # extra check
    if (length(pivotRowIndex) == 0 || is.na(testRatios[pivotRowIndex])) {
      return(list(
        status = "Infeasible",
        finalTableau = tableau,
        finalSolution = NULL,
        Z = NA,
        history = iterations,
        basicSolutionIterations = basicSol
      ))
    }
    
    # --- proceed with normal pivot ---
    pivotRow <- tableau[pivotRowIndex, ]
    pivotElement <- tableau[pivotRowIndex, pivotColumn]
    
    # normalize pivot row
    tableau[pivotRowIndex, ] <- tableau[pivotRowIndex, ] / pivotElement
    
    # eliminate pivot column entries in other rows
    for (j in 1:lastRow) {
      if (j != pivotRowIndex) {
        tableau[j, ] <- tableau[j, ] - tableau[j, pivotColumn] * tableau[pivotRowIndex, ]
      }
    }
    
    # ----- Log basic solution each iteration ----- #
    
    solution <- matrix(0, 1, lastCol-1)
    slacks <- sum((substr(colnames(tableau[, -lastCol]), 1, 1)) == "S") # determine the number of slacks
    variables <- lastCol - 2 - slacks # remaining are the variables
    
    # maximization way of extracting a solution
    for(i in 1:(lastCol-1)){
      col <- tableau[,i]
      # only one 1 and rest are zeroes means there is a solution
      if(sum(col == 1) == 1 && sum(col == 0) == (lastRow - 1)){
        rowIndex <- which(col == 1)
        solution[1, i] <- tableau[rowIndex, lastCol]
      }
    }
    
    colnames(solution) <- c(paste0("S", 1:slacks), paste0("X", 1:variables), "Z")
    
    
    # ------ Log each iteration -------- #
    
    hist <- tableau * 1 # for safety, copy is used instead
    solution <- solution * 1
    
    # add each iteration (stores as element so double brackets)
    iterations[[idx]] <- hist
    basicSol[[idx]] <- solution 
    idx <- idx + 1
    
    # update condition
    negRow <- any(tableau[lastRow, ] < 0)
    
  }
  
    # end of the loop
    # minimization extracting solution
    finalSol <- basicSol[[idx - 1]] * 1 # copy the last then change the content
    for (i in 1:(lastCol-1)){
      if(i == (lastCol - 1)){
        finalSol[1,i] <- tableau[lastRow, lastCol] # z variable
      }
      else{
        finalSol[1,i] <- tableau[lastRow, i]
      }
    }
  
  # last iteration should be the same with the final solution
  basicSol[[idx - 1]] <- finalSol
  
  
  return(list(
    status = "Feasible",
    finalTableau = tableau,
    finalSolution = finalSol,
    Z = tableau[[lastRow, lastCol]],
    history = iterations,
    basicSolutionIterations = basicSol
  ))
  
  
}
