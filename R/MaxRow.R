# Считает максимальное значение каждой строки входящей таблицы
MaxRow <- function(df) {
  #' Calculate maximum
  #'
  #' @description This function calculate the maximum of every row
  #' of input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - maximum of every row of input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MaxRow(data.frame)
  #' MaxRow(matrix)
  #' MaxRow(vector)
  #' MaxRow(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  result <- as.numeric()
  for (i in 1:nrow(df)){
    max <- df[i,1] # принять первый эл-т таблицы за максимум
    for (j in 1:ncol(df)){
      if (df[i,j] > max){
        max <- df[i,j]
      }
    }
    result <- append(result, max)
  }
  return(result)
}
