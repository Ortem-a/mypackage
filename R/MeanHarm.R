# Считает среднюю гармоническою входящей таблицы
MeanHarm <- function(df) {
  #' Calculate harmonic mean
  #'
  #' @description This function calculate the harmonic mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - harmonic mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanHarm(data.frame)
  #' MeanHarm(matrix)
  #' MeanHarm(vector)
  #' MeanHarm(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  # подсчитать сумму
  sum <- 0
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){ # игнорировать нули
        sum <- sum + 1/df[i,j]
      }
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
  result <- n/sum
  return(result)
}
