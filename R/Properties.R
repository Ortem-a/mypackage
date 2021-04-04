# Считает основные статистические величины входящей таблицы
Properties <- function(df) {
  #' Calculate main statistic values
  #'
  #' @description This function calculate main statistic values of whole
  #' input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: named numeric vector - main statistic values of whole
  #' input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Properties(data.frame)
  #' Properties(matrix)
  #' Properties(vector)
  #' Properties(List)
  #' @export
  
  if(length(df) < 1){
    return(-1)
  }
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
  # сумма всех элементов
  kSum <- 0
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      kSum <- kSum + df[i,j]
    }
  }
  
  # MEAN
  mean <- kSum / n
  
  result <- as.numeric()
  
  # MAD
  sum <- 0
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + abs(df[i,j] - mean)
    }
  }
  mad <- sum / n
  
  # SD
  sum <- 0 # сумма
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + (df[i,j] - mean)^2
    }
  }
  sd <- sqrt(sum / n)
  
  # SEM
  sem = sd / sqrt(n)
  
  # MeanGarm
  # подсчитать сумму
  sum <- 0
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){ # игнорировать нули
        sum <- sum + 1/df[i,j]
      }
    }
  }
  meanGarm <- n/sum
  
  # MeanGeom
  prod <- 1 # подсчитать произведение
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){ # игнорировать нули
        prod <- prod * df[i,j]
      }
    }
  }
  meanGeom <- prod^(1/n)
  
  # MeanSqr
  sum <- 0 # сумма квадратов всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] * df[i,j]
    }
  }
  meanSqr <- sum / n
  
  # Median
  if (n %% 2 == 1){ # в выборке нечетное число эл-тов
    median <- df[(n+1)/2]
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
    median <- (df[midRow, midCol] + df[midRow, midCol + 1]) / 2
  }
  
  # Moda
  # считать частоты встречаемости каждого элемента
  freq <- c(rep(0, max(df)))
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      freq[df[i,j]] <- freq[df[i,j]] + 1
    }
  }
  # вычислить моду
  moda <- as.numeric()
  maxFreq <- max(freq)
  for (i in 1:length(freq)){
    if (freq[i] == maxFreq){
      moda <- append(moda, i)
    }
  }
  
  # Range
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
  range <- c('min' = min, 'max' = max)
  
  # Подготовить ответ
  result <- c('Mean' = mean,
              'MAD' = mad,
              'SD' = sd,
              'SEM' = sem,
              'Mean Garmony' = meanGarm,
              'Mean Geometry' = meanGeom,
              'Mean Square' = meanSqr,
              'Median' = median,
              'Moda' = moda,
              'Range' = range)
  
  return(result)
}
