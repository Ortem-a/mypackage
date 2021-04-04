# Считает среднюю арифметическую всей входящей таблицы
Mean <- function(df) {
  #' Calculate arithmetical mean
  #'
  #' @description This function calculate the arithmetical mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - arithmetical mean of whole table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Mean(data.frame)
  #' Mean(matrix)
  #' Mean(vector)
  #' Mean(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  sum <- 0 # сумма всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] #df[[i]][j]
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
