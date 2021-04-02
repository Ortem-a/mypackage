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
  sum <- 0 # сумма всех элементов столбца
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
SEM <- function(variables) {
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
