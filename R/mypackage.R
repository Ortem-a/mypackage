#' mypackage
#'
#' @name mypackage
#' @docType package
#' @author Anikin_Artem
#' @import devtools roxygen2 testthat covr lintr docstring
NULL

# Считает среднюю арифметическую всей входящей таблицы
Mean <- function(df) {
  #' Calculate arithmetical mean
  #'
  #' @description This function calculate the arithmetical mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - arithmetical mean of whole table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Mean(data.frame)
  #' Mean(matrix)
  #' Mean(vector)
  #' Mean(List)
  #' @export

  if (length(df) < 1){
    return(-1)
  }
  sum <- 0 # сумма всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] #df[[i]][j]
    }
  }
  # посчитать кол-во эл-тов
  if (nrow(df) == 0){
    n <- ncol(df)
  } else if (ncol(df) == 0) {
    n <- nrow(df)
  } else {
    n <- nrow(df) * ncol(df)
  }
  result <- sum / n

  return(result)
}

# Считает среднюю арифметическую каждой строки входящей таблицы
MeanRow <- function(df) {
  #' Calculate arithmetical mean
  #'
  #' @description This function calculate the arithmetical mean
  #' of every row input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - arithmetical means of every
  #' row input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanRow(data.frame)
  #' MeanRow(matrix)
  #' MeanRow(vector)
  #' MeanRow(List)
  #' @export

  if (length(df) < 1){
    return(-1)
  }
  result <- numeric()
  sum <- 0 # сумма всех элементов строки
  n = ncol(df) # кол-во элементов каждой строки
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j]
    }
    result = append(result, sum/n)
    sum <- 0
  }

  return(result)
}

# Считает среднюю арифметическую каждого столбца входящей таблицы
MeanCol <- function(df) {
  #' Calculate arithmetical mean
  #'
  #' @description This function calculate the arithmetical mean
  #' of every column input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: numeric vector - arithmetical means of every
  #' column input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanCol(data.frame)
  #' MeanCol(matrix)
  #' MeanCol(vector)
  #' MeanCol(List)
  #' @export

  if (length(df) < 1){
    return(-1)
  }
  result <- numeric()
  sum <- 0 # сумма всех элементов столбца
  n = nrow(df) # кол-во элементов каждого столбца
  for (i in 1:ncol(df)){
    for (j in 1:nrow(df)){
      sum <- sum + df[j,i]
    }
    result = append(result, sum/n)
    sum <- 0
  }

  return(result)
}

# Считает среднюю гармоническою входящей таблицы
MeanHarm <- function(df) {
  #' Calculate harmonic mean
  #'
  #' @description This function calculate the harmonic mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - harmonic mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanHarm(data.frame)
  #' MeanHarm(matrix)
  #' MeanHarm(vector)
  #' MeanHarm(List)
  #' @export

  if (length(df) < 1){
    return(-1)
  }
  # подсчитать сумму
  sum <- 0
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){ # игнорировать нули
        sum <- sum + 1/df[i,j]
      }
    }
  }
  # посчитать кол-во эл-тов
  if (nrow(df) == 0){
    n <- ncol(df)
  } else if (ncol(df) == 0) {
    n <- nrow(df)
  } else {
    n <- nrow(df) * ncol(df)
  }
  result <- n/sum
  return(result)
}

# Считает среднюю геометрическую входящей таблицы
MeanGeom <- function(df) {
  #' Calculate geometric mean
  #'
  #' @description This function calculate the geometric mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - geometric mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanGeom(data.frame)
  #' MeanGeom(matrix)
  #' MeanGeom(vector)
  #' MeanGeom(List)
  #' @export

  if (length(df) < 1){
    return(-1)
  }
  # подсчитать произведение
  prod <- 1
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (df[i,j] != 0){ # игнорировать нули
        prod <- prod * df[i,j]
      }
    }
  }
  # посчитать кол-во эл-тов
  if (nrow(df) == 0){
    n <- ncol(df)
  } else if (ncol(df) == 0) {
    n <- nrow(df)
  } else {
    n <- nrow(df) * ncol(df)
  }
  result <- prod^(1/n)
  return(result)
}

# Считает среднюю квадратическую величину входящей таблицы
MeanSqr <- function(df) {
  #' Calculate square mean
  #'
  #' @description This function calculate the square mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - square mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MeanSqr(data.frame)
  #' MeanSqr(matrix)
  #' MeanSqr(vector)
  #' MeanSqr(List)
  #' @export

  if (length(df) < 1){
    return(-1)
  }
  sum <- 0 # сумма квадратов всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] * df[i,j]
    }
  }
  # посчитать кол-во эл-тов
  if (nrow(df) == 0){
    n <- ncol(df)
  } else if (ncol(df) == 0) {
    n <- nrow(df)
  } else {
    n <- nrow(df) * ncol(df)
  }
  result <- sum / n
  return(result)
}

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

# Считает моду входящей таблицы
Moda <- function(df) {
  #' Calculate moda
  #'
  #' @description This function calculate the moda
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - moda of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' Moda(data.frame)
  #' Moda(matrix)
  #' Moda(vector)
  #' Moda(List)
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
  # считать частоты встречаемости каждого элемента
  freq <- c(rep(0, max(df)))
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      freq[df[i,j]] <- freq[df[i,j]] + 1
    }
  }
  # вычислить моду
  result <- as.numeric()
  maxFreq <- max(freq)
  for (i in 1:length(freq)){
    if (freq[i] == maxFreq){
      result <- append(result, i)
    }
  }
  return(result)
}

# Считает стандратное отклонение входящей таблицы
SD <- function(df) {
  #' Calculate standard deviation
  #'
  #' @description This function calculate the standard deviation
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - standard deviation of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' SD(data.frame)
  #' SD(matrix)
  #' SD(vector)
  #' SD(List)
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
  # найти сумму квадратов разности элемента и средней арифметической
  # найти среднее арифметическое
  sum <- 0 # сумма всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] #df[[i]][j]
    }
  }
  m <- sum / n # средняя арифметическая
  sum <- 0 # сумма
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + (df[i,j] - m)^2
    }
  }
  result <- sqrt(sum / n)
  return(result)
}

# Считает среднее абсолютное отклонение входящей таблицы
MAD <- function(df) {
  #' Calculate mean absolute deviation
  #'
  #' @description This function calculate the mean absolute deviation
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - mean absolute deviation of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' MAD(data.frame)
  #' MAD(matrix)
  #' MAD(vector)
  #' MAD(List)
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
  # найти сумму квадратов разности элемента и средней арифметической
  # найти среднее арифметическое
  sum <- 0 # сумма всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j] #df[[i]][j]
    }
  }
  m <- sum / n # средняя арифметическая
  sum <- 0 # сумма
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + abs(df[i,j] - m)
    }
  }
  result <- sum / n
  return(result)
}

# Считает стандартную ошибку среднего входящей таблицы
SEM <- function(df) {
  #' Calculate standard error mean
  #'
  #' @description This function calculate the standard error mean
  #' of whole input table.
  #' @param df Numeric 2 dimension table
  #' @return Good way: one number - standard error mean of whole input table
  #'
  #' Bad way: -1 if input data are invalid
  #' @details The inputs can be also numeric vector.
  #' @examples
  #' SEM(data.frame)
  #' SEM(matrix)
  #' SEM(vector)
  #' SEM(List)
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
  # найти стандартное отклонение
  # найти сумму квадратов разности элемента и средней арифметической
  # найти среднее арифметическое
  sum <- 0 # сумма всех элементов
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + df[i,j]
    }
  }
  m <- sum / n # средняя арифметическая
  sum <- 0 # сумма
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      sum <- sum + (df[i,j] - m)^2
    }
  }
  sd <- sqrt(sum / n) # стандартное отклонение
  result <- sd / sqrt(n)
  return(result)
}

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
