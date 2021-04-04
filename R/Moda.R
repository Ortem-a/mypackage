# Считает моду входящей таблицы
Moda <- function(df) {
  #' Calculate moda
  #'
  #' @description This function calculate the moda
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - moda of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Moda(data.frame)
  #' Moda(matrix)
  #' Moda(vector)
  #' Moda(List)
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
  # считать частоты встречаемости каждого элемента
  freq <- c(rep(0, max(df)))
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      freq[df[i,j]] <- freq[df[i,j]] + 1
    }
  }
  # вычислить моду
  result <- as.numeric()
  maxFreq <- max(freq)
  for (i in 1:length(freq)){
    if (freq[i] == maxFreq){
      result <- append(result, i)
    }
  }
  return(result)
}
