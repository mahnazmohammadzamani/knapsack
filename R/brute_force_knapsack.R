#'Brute force algorithm for the knapsack problem
#'
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details This algorithm gives all possible values with good accuracy , and also gives the maximum value for the knapsack
#'@author Mahnaz Mohammadzamani
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@seealso
#'\code{\link{knapsack_dynamic}}
#'\code{\link{greedy_knapsack}}
#' @export
#' @importFrom utils combn

brute_force_knapsack <- function(x, W){
  stopifnot( is.data.frame(x),
             is.numeric(W),
             W>0,
             is.numeric(x$w),
             is.numeric(x$v),
             x$w >=0,
             x$v >= 0)
  i=2
  optimum_value = 0     
  selected_items = c()
  weights<-c()
  values<-c()
  while(i<=nrow(x))
  {
    w<-as.data.frame(combn(x[,1], i))
    v<-as.data.frame(combn(x[,2], i))
    sumw<-colSums(w)
    sumv<-colSums(v)
    weights<-which(sumw<=W)
    if(length(weights) != 0){ 
      values<-sumv[weights]
      optimum_value<-max(values)
      temp<-which((values)==optimum_value)
      maxValWghtIdx<-weights[temp]
      maxValWght<-w[, maxValWghtIdx]
      j<-1
      while (j<=i){
        selected_items[j]<-which(x[,1]==maxValWght[j])
        j=j+1
      }
    }
    i=i+1
    
  }
  
  return(list(value=round(optimum_value),elements=selected_items))
}

