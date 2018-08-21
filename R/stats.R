#' @title Formated mean and standard deviation
#'
#' @description Format the mean and sd of a vector
#' @param x a numerical vector
#' @return formated string
#' @examples m_sd(1:3)
#' @export m_sd
m_sd <- function(x, ...) sprintf("%.2f(%.2f)", mean(x), sd(x))
