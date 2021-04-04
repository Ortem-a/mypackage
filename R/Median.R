# Считает медиану входящей таблицы
Median <- function(df) {
  #' Calculate median
  #'
  #' @description This function calculate the median
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - median of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Median(data.frame)
  #' Median(matrix)
  #' Median(vector)
  #' Median(List)
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
  if (n %% 2 == 1){ # в выборке нечетное число эл-тов
    result <- df[(n+1)/2]
  } else { # в выборке четное число эл-тов
    if (ncol(df) %% 2 == 0){ # число столбов четное
      midCol = ncol(df) / 2
    } else { # нечетное
      midCol = (ncol(df) + 1) / 2
    }
    if (nrow(df) %% 2 == 0){ # число строк четное
      midRow = nrow(df) / 2
    } else { # нечетное
      midRow = (nrow(df) + 1) / 2
    }
    result <- (df[midRow, midCol] + df[midRow, midCol + 1]) / 2
  }
  return(result)
}
