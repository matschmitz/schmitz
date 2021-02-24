#' @title APA plot theme
#' @description Formatted APA plot
#' @return Format for ggplot2
#' @examples
#' df <- data.frame(x = 1:5, y = 1:5)
#' ggplot2::ggplot(df, aes(x, y)) + apatheme
#' @export apatheme
apatheme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border     = ggplot2::element_blank(),
                   axis.line        = ggplot2::element_line(),
                   text             = ggplot2::element_text(size = 17, family = "serif"))