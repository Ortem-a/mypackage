# Считает минимальное значение каждого столбца входящей таблицы
MinCol <- function(df) {
  #' Calculate minimum
  #'
  #' @description This function calculate the minimum of every column
  #' of input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - minimum of every column of input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MinCol(data.frame)
  #' MinCol(matrix)
  #' MinCol(vector)
  #' MinCol(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  result <- as.numeric()
  for (i in 1:ncol(df)){
    min <- df[1, i] # принять первый эл-т таблицы за минимум
    for (j in 1:nrow(df)){
      if (df[j,i] < min){
        min <- df[j,i]
      }
    }
    result <- append(result, min)
  }
  return(result)
}
