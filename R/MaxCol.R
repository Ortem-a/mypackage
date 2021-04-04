# Считает максимальное значение каждого столбца входящей таблицы
MaxCol <- function(df) {
  #' Calculate maximum
  #'
  #' @description This function calculate the maximum of every column
  #' of input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - maximum of every column of input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MaxCol(data.frame)
  #' MaxCol(matrix)
  #' MaxCol(vector)
  #' MaxCol(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  result <- as.numeric()
  for (i in 1:ncol(df)){
    max <- df[1, i] # принять первый эл-т таблицы за максимум
    for (j in 1:nrow(df)){
      if (df[j,i] > max){
        max <- df[j,i]
      }
    }
    result <- append(result, max)
  }
  return(result)
}
