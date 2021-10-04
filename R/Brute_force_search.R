#' Brute Force Knapsack Algorithm
#' The function returns maximum value within all the possible solutions
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W weight threshold for the knapsack
#' @param ... arguments
#' @param parallel boolean, if 'T' then implements parallelization
#' @return Total and Maximum value within the threshold of W
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack problem}
#' @seealso
#' \code{\link{slow_brute_force}}
#' \code{\link{greedy_knapsack}}
#' \code{\link{knapsack_dynamic}}
#' @export
#' @examples
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = FALSE)

brute_force_knapsack <- function(x, W){
  if (!is.data.frame(x))
    stop("x is not a dataframe!")
  weights = as.matrix(x[, 1])
  values  = as.matrix(x[, 2])
  if(any(weights <= 0 | values <= 0))
    stop("input must be positive")
  
  n_items = nrow(x)
  all_combinations = 0:(2^n_items - 1)
  all_combinations_matrix = 
    sapply(all_combinations, function(x){
      result = intToBits(x)
      result = as.numeric(result)
      return(result[1:n_items])
    })
  
  matrix1 = all_combinations_matrix
  for (i in 1:ncol(matrix1)){
    matrix1[,i] = weights * matrix1[,i]
  }
  sum_w = as.matrix(colSums(matrix1))
  matrix_weights = as.matrix(which(sum_w < W))
  matrix_weights_update = all_combinations_matrix[, c(matrix_weights)]
  
  matrix2 = all_combinations_matrix[, c(matrix_weights)]
  for (i in 1:ncol(matrix2)){
    matrix2[,i] = values * matrix2[,i]
  }
  sum_v = as.matrix(colSums(matrix2))
  opt_value = which.max(sum_v)
  value = sum_v[opt_value, ]
  
  opt_weights = as.matrix(matrix_weights_update[, opt_value])
  #convert to row index:
  elements = which(!opt_weights[,1] == 0) #row elements that doesn't start with 0
  
  list1 = list(value = as.integer(value), elements = elements)
  return(list1)
}
