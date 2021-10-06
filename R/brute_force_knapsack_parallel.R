#' Brute Force Knapsack Algorithm
#' The function returns maximum value within all the possible solutions
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W weight threshold for the knapsack
#' @param parallel Boolean, if 'TRUE' then implements palatalization
#' @return Total and Maximum value within the threshold of W
#' \url{https://en.wikipedia.org/wiki/Knapsack problem}
#' @seealso
#' \code{\link{greedy_knapsack}}
#' \code{\link{knapsack_dynamic}}
#' @export
#' @importFrom utils combn

brute_force_knapsack_parallel <- function(x, W , parallel = FALSE){
  stopifnot( is.data.frame(x),
             is.numeric(W),
             W>0,
             is.numeric(x$w),
             is.numeric(x$v),
             x$w >=0,
             x$v >= 0)
  # get no. of rows
  value <- NULL
  rows <- nrow(x) # Rows of Dataframe
  updated_index <- c()
  row_value <- c()
  cores <- parallel::detectCores() - 1

  # Define Function to Calculate Weight Sum
  sum_weight <- function(column,W,df){
    if (sum(column)<= W & !is.null(sum(column))){
      index <- c(column)
      return(match(index, df[,1]))
    }

  }
  # Function definition ends

  if (parallel ==  TRUE){

    # Start Cluster
    cl <- parallel::makeCluster(cores,type = "PSOCK")

    for (i in 1:rows){

      comb_matrix <- combn(x[,1],m = i)


      true_weight <- parallel::parApply(cl = cl,X = comb_matrix, MARGIN = 2,
                                        FUN = sum_weight,W = W,df=x)


      # Remove Nulls
      true_weight <- true_weight[lengths(true_weight)!=0]

      updated_index <- c(updated_index,true_weight)

    }
    # Stop Cluster
    parallel::stopCluster(cl)
  }
  else{
    for (i in 1:rows){
      comb_matrix <- combn(x[,1],m = i)
      true_weight <- apply(comb_matrix, MARGIN = 2,FUN = sum_weight,W = W,df=x)
      # Remove Nulls
      true_weight <- true_weight[lengths(true_weight)!=0]

      updated_index <- c(updated_index,true_weight)

    }
  }
  # Get Value

  for (i in seq(updated_index)){

    updated_value <-sum(x[updated_index[[i]],2])

    value <- max(updated_value,value)

    if (value == updated_value){
      row_value <- updated_index[[i]]
    }
  }

  result <- list(
    value = value,
    elements = row_value
  )

  return(result)

}
