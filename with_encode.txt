#' mypackage
#'
#' @name mypackage
#' @docType package
#' @author Anikin_Artem
#' @import devtools roxygen2 testthat covr lintr
NULL

#' Считает среднюю арифметическую всей входящей таблицы
#'
#' @param численная двумерная таблица
#' @return одно число - среднее арифметическое таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Mean(df)
Mean <- function(df) {
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

#' Считает среднюю арифметическую каждой строки входящей таблицы
#'
#' @param численная двумерная таблица
#' @return вектор чисел - среднее арифметическое каждой строки таблицы в случае
#' успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MeanRow(df)
MeanRow <- function(df) {
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

#' Считает среднюю арифметическую каждого столбца входящей таблицы
#'
#' @param численная двумерная таблица
#' @return вектор чисел - среднее арифметическое каждого столбца таблицы в случае
#' успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MeanCol(df)
MeanCol <- function(df) {
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

#' Считает среднюю гармоническою входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - среднее гармоническое всей таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MeanGarm(df)
MeanGarm <- function(df) {
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

#' Считает среднюю геометрическую входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - среднее геометрическое всей таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MeanGeom(df)
MeanGeom <- function(df) {
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

#' Считает среднюю квадратическую величину входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - средняя квадратическая всей таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MeanSqr(df)
MeanSqr <- function(df) {
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

#' Считает медиану входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - медиана всей таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Median(df)
Median <- function(df) {
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

#' Считает моду входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число или вектор - мод(а) всей таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Moda(df)
Moda <- function(df) {
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

#' Считает стандратное отклонение входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - стандартное отклонение таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' SD(df)
SD <- function(df) {
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

#' Считает среднее абсолютное отклонение входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - среднее абсолютное отклонение таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MAD(df)
MAD <- function(df) {
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

#' Считает стандартную ошибку среднего входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - стандартная ошбика среднего таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' SEM(df)
SEM <- function(df) {
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

#' Считает минимальное значение входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - минимальное значение таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Min(df)
Min <- function(df) {
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

#' Считает минимальное значение каждой строки входящей таблицы
#'
#' @param численная двумерная таблица
#' @return вектор чисел - минимальное значение каждой строки таблицы в случае
#' успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MinRow(df)
MinRow <- function(df) {
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

#' Считает минимальное значение каждого столбца входящей таблицы
#'
#' @param численная двумерная таблица
#' @return вектор чисел - минимальное значение каждого столбца таблицы в случае
#' успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MinCol(df)
MinCol <- function(df) {
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

#' Считает максимальное значение входящей таблицы
#'
#' @param численная двумерная таблица
#' @return число - максимальное значение таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Max(df)
Max <- function(df) {
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

#' Считает максимальное значение каждой строки входящей таблицы
#'
#' @param численная двумерная таблица
#' @return вектор чисел - максимальное значение каждой строки таблицы в случае
#' успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MaxRow(df)
MaxRow <- function(df) {
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

#' Считает максимальное значение каждого столбца входящей таблицы
#'
#' @param численная двумерная таблица
#' @return вектор чисел - максимальное значение каждого столбца таблицы в случае
#' успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' MaxCol(df)
MaxCol <- function(df) {
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

#' Считает максимальное и минимальное значения входящей таблицы
#'
#' @param численная двумерная таблица
#' @return именованный вектор чисел - максимальное и минимальное значения
#' таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Range(df)
Range <- function(df) {
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

#' Считает основные статистические величины входящей таблицы
#'
#' @param численная двумерная таблица
#' @return именованный вектор чисел - основные статистические величины входящей
#' таблицы в случае успеха
#' @return -1 если поданы некорректные данные
#' @export
#' @examples
#' Properties(df)
Properties <- function(df) {
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
