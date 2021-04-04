# Считает среднюю геометрическую входящей таблицы
MeanGeom <- function(df) {
  #' Calculate geometric mean
  #'
  #' @description This function calculate the geometric mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - geometric mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanGeom(data.frame)
  #' MeanGeom(matrix)
  #' MeanGeom(vector)
  #' MeanGeom(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  # подсчитать произведение
  prod <- 1
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){ # игнорировать нули
        prod <- prod * df[i,j]
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
  result <- prod^(1/n)
  return(result)
}
