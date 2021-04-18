# Считает дисперсию входящей таблицы
Dispersion <- function(df) {
  #' Calculate dispersion
  #'
  #' @description This function calculate the dispersion
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - dispersion of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Dispersion(data.frame)
  #' Dispersion(matrix)
  #' Dispersion(vector)
  #' Dispersion(List)
  #' @export
  
  df = as.data.frame(df)
  
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
  mean <- sum / n
  # считать сумму квадратов разности каждого элемента с средним арифметическим
  sum <- 0
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + (df[i,j] - mean)^2
    }
  }
  result <- sum / n
  return(result)
}