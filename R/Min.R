# Считает минимальное значение входящей таблицы
Min <- function(df) {
  #' Calculate minimum
  #'
  #' @description This function calculate the minimum
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - minimum of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Min(data.frame)
  #' Min(matrix)
  #' Min(vector)
  #' Min(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  min <- df[1,1] # принять первый эл-т таблицы за минимум
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] < min){
        min <- df[i,j]
      }
    }
  }
  return(min)
}
