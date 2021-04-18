# Считает доверительный интервал для среднего
ConfidenceInterval <- function(df) {
  #' Calculate confidence interval for mean
  #'
  #' @description This function calculate the confidence interval for mean
  #' of whole input table.
  #' @param df Numeric 2 dimension vector
  #' @return Good way: one number - confidence interval for mean of whole
  #' input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' ConfidenceInterval(data.frame)
  #' ConfidenceInterval(matrix)
  #' ConfidenceInterval(vector)
  #' ConfidenceInterval(List)
  #' @export

  # предобработка данных
  df = as.data.frame(df)
  temp = vector()
  for (i in 1:nrow(df)){
    temp <- append(temp, df[i,])
  }
  df <- unlist(temp)

  n <- length(df)
  if (n == 0){
    return(-1)
  }
  # найти стандартное отклонение
  # найти сумму квадратов разности элемента и средней арифметической
  # найти среднее арифметическое
  sum <- 0 # сумма всех элементов
  for (i in 1:n){
    sum <- sum + df[i]
  }
  m <- sum / n # средняя арифметическая
  sum <- 0 # сумма
  for (i in 1:n){
    sum <- sum + (df[i] - m)^2
  }
  sd <- sqrt(sum / n) # стандартное отклонение
  sem <- sd / sqrt(n)

  result <- c('Left border' = m - 1.96*sem,
              'Right border' = m + 1.96*sem)
  return(result)
}
