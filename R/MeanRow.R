# Считает среднюю арифметическую каждой строки входящей таблицы
MeanRow <- function(df) {
  #' Calculate arithmetical mean
  #'
  #' @description This function calculate the arithmetical mean
  #' of every row input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - arithmetical means of every
  #' row input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanRow(data.frame)
  #' MeanRow(matrix)
  #' MeanRow(vector)
  #' MeanRow(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  result <- numeric()
  sum <- 0 # сумма всех элементов строки
  n = ncol(df) # кол-во элементов каждой строки
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j]
    }
    result = append(result, sum/n)
    sum <- 0
  }
  
  return(result)
}
