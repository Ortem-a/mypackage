# Считает стандартную ошибку среднего входящей таблицы
SEM <- function(df) {
  #' Calculate standard error mean
  #'
  #' @description This function calculate the standard error mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - standard error mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' SEM(data.frame)
  #' SEM(matrix)
  #' SEM(vector)
  #' SEM(List)
  #' @export
  
  # посчитать кол-во эл-тов
  if (nrow(df) == 0){
    n <- ncol(df)
  } else if (ncol(df) == 0) {
    n <- nrow(df)
  } else {
    n <- nrow(df) * ncol(df)
  }
  if (n == 0){
    return(-1)
  }
  # найти стандартное отклонение
  # найти сумму квадратов разности элемента и средней арифметической
  # найти среднее арифметическое
  sum <- 0 # сумма всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j]
    }
  }
  m <- sum / n # средняя арифметическая
  sum <- 0 # сумма
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + (df[i,j] - m)^2
    }
  }
  sd <- sqrt(sum / n) # стандартное отклонение
  result <- sd / sqrt(n)
  return(result)
}
