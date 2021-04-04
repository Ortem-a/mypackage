# Считает среднюю арифметическую каждого столбца входящей таблицы
MeanCol <- function(df) {
  #' Calculate arithmetical mean
  #'
  #' @description This function calculate the arithmetical mean
  #' of every column input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - arithmetical means of every
  #' column input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanCol(data.frame)
  #' MeanCol(matrix)
  #' MeanCol(vector)
  #' MeanCol(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  result <- numeric()
  sum <- 0 # сумма всех элементов столбца
  n = nrow(df) # кол-во элементов каждого столбца
  for (i in 1:ncol(df)){
    for (j in 1:nrow(df)){
      sum <- sum + df[j,i]
    }
    result = append(result, sum/n)
    sum <- 0
  }
  
  return(result)
}
