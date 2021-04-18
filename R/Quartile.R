# считает квартили входящего вектора
Quartile <- function(df) {
  #' Calculate median
  #'
  #' @description This function calculate the median
  #' of whole input table.
  #' @param df Numeric 2 dimension vector
  #' @return Good way: one number - median of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Quartile(data.frame)
  #' Quartile(matrix)
  #' Quartile(vector)
  #' Quartile(List)
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
  
  # сортировать по неубыванию
  df <- sort(df)
  
  if (n %% 2 == 1){ # в выборке нечетное число эл-тов
    f <- df[(n+1)/4]
    m <- df[(n+1)/2]
    l <- df[n+1-(n+1)/4]
  } else { # в выборке четное число эл-тов
    f <- df[as.integer(n/4) + 1]
    m <- (df[n/2] + df[n/2 + 1]) / 2
    l <- df[n-as.integer(n/4)]
  }
  # удалить имена векторов
  f <- unname(f)
  m <- unname(m)
  l <- unname(l)
  result <- c('first' = f, 'second' = m, 'third' = l)
  return(result)
}