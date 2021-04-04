# Считает максимальное и минимальное значения входящей таблицы
Range <- function(df) {
  #' Calculate minimum and maximum
  #'
  #' @description This function calculate minimum and maximum of whole
  #' input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: named numeric vector - minimum and maximum of whole
  #' input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Range(data.frame)
  #' Range(matrix)
  #' Range(vector)
  #' Range(List)
  #' @export
  
  if (length(df) < 1){
    return(-1)
  }
  min <- df[1,1] # принять первый эл-т таблицы за минимум
  max <- df[1,1] # принять первый эл-т таблицы за максимум
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] < min){
        min <- df[i,j]
      }
      if (df[i,j] > max){
        max <- df[i,j]
      }
    }
  }
  result <- c('min' = min, 'max' = max)
  return(result)
}
