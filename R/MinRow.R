# Считает минимальное значение каждой строки входящей таблицы
MinRow <- function(df) {
  #' Calculate minimum
  #'
  #' @description This function calculate the minimum of every row
  #' of input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - minimum of every row of input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MinRow(data.frame)
  #' MinRow(matrix)
  #' MinRow(vector)
  #' MinRow(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  result <- as.numeric()
  for (i in 1:nrow(df)){
    min <- df[i,1] # принять первый эл-т таблицы за минимум
    for (j in 1:ncol(df)){
      if (df[i,j] < min){
        min <- df[i,j]
      }
    }
    result <- append(result, min)
  }
  return(result)
}
