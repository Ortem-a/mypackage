# Считает среднюю квадратическую величину входящей таблицы
MeanSqr <- function(df) {
  #' Calculate square mean
  #'
  #' @description This function calculate the square mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - square mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanSqr(data.frame)
  #' MeanSqr(matrix)
  #' MeanSqr(vector)
  #' MeanSqr(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  sum <- 0 # сумма квадратов всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] * df[i,j]
    }
  }
  # посчитать кол-во эл-тов
  if (nrow(df) == 0){
    n <- ncol(df)
  } else if (ncol(df) == 0) {
    n <- nrow(df)
  } else {
    n <- nrow(df) * ncol(df)
  }
  result <- sum / n
  return(result)
}
