# Считает максимальное значение входящей таблицы
Max <- function(df) {
  #' Calculate maximum
  #'
  #' @description This function calculate the maximum
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - maximum of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Max(data.frame)
  #' Max(matrix)
  #' Max(vector)
  #' Max(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  max <- df[1,1] # принять первый эл-т таблицы за максимум
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] > max){
        max <- df[i,j]
      }
    }
  }
  return(max)
}
