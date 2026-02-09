library(here)
source(here("R","datasets.r"))

# Build initial matrix for dual problem
build_initial_matrix <- function(filtered_data){
  
  # Determine how many were selected
  n_rows = nrow(filtered_data)
  
  # Keep only numeric columns (drop non-numeric like "Option")
  filtered_data <- filtered_data[, sapply(filtered_data, is.numeric), drop = FALSE] # will drop non-numeric column for example, 'options' column
  
  # Cost vector
  C <- as.numeric(filtered_data$Cost)
  
  # Remove Cost column for constraints matrix
  A <- t(as.matrix(filtered_data[, names(filtered_data) != "Cost", drop = FALSE]))
  
  # RHS vector
  RHS <- as.numeric(pollutant_targets$Target)

  # Combine constraints + RHS
  tableau <- cbind(A, RHS)
  
  # Constraints (for x <= 20) negate for dual problem
  x_bounds <- -diag(n_rows)
  x_bounds <- cbind(x_bounds, rep(-20,n_rows))
  
  # Objective function row: costs + 1 for RHS
  obj_row <- c(C, 1)
  tableau <- rbind(tableau, x_bounds)
  tableau <- rbind(tableau, obj_row)

  # Remove row/column names
  rownames(tableau) <- NULL
  colnames(tableau) <- NULL

  return(tableau)
}

# Setup initial tableau for simplex
set_up_initial_tableau <- function(primal_tableau) {
  
  # Force matrix to avoid issues with single project
  T <- t(primal_tableau)
  
  n_rows <- nrow(T)     # number of constraints + 1
  n_cols <- ncol(T)     # number of decision variables + 1
  
  # RHS column (numeric vector)
  rhs <- T[, n_cols]
  
  # Remove RHS from T
  T <- T[, 1:(n_cols - 1), drop = FALSE]
  
  # Negate last row (objective function)
  T[n_rows, ] <- -T[n_rows, ]
  
  n_slacks <- n_rows - 1
  slack <- diag(n_slacks)
  
  # Split constraints and objective row
  constraint_rows <- T[1:n_slacks, , drop = FALSE]
  z_row <- as.numeric(T[n_rows,]) 
            
  # Add slack columns
  constraint_rows <- cbind(constraint_rows, slack)
  
  # Add Z column to constraints
  constraint_rows <- cbind(constraint_rows, rep(0, n_slacks))
  
  # add RHS column
  constraint_rows <- cbind(constraint_rows, rhs[1:n_slacks])
  
  # Build final objective row
  z_row_final <- c(z_row, rep(0, n_slacks), rhs[n_rows], 0)
  final <- rbind(constraint_rows, z_row_final)
  
  # Column names: x = decision vars, S = slacks, Z, Solution
  num_decision_vars <- ncol(final) - n_slacks - 2
  colnames(final) <- c(paste0("S", 1:num_decision_vars),
                       paste0("x", 1:n_slacks),
                       "Z", "Solution")
  
  rownames(final) <- NULL
  return(final)
}

 