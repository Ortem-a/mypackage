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



