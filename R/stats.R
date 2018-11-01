#' @import data.table
#' @import magrittr
#' @importFrom lmSupport modelEffectSizes

#' @title Formated mean and standard deviation
#'
#' @description Format the mean and sd of a vector
#' @param x a numerical vector
#' @return formated string
#' @examples m_sd(1:3)
#' @export m_sd
m_sd <- function(x, ...) sprintf("%.2f(%.2f)", mean(x, ...), sd(x, ...))

#' @title Effect sizes for lm summary
#'
#' @description Add effect sizes to the lm summary
#' @param mdl an lm model obtained from lm
#' @param digits number of decimal digits
#' @param printOut prints the output
#' @return data table with summary statistics
#' @examples ss(mdl)
#' @export ss
ss <- function(mdl, digits = 3, printOut = TRUE) {
    mdl.coefficients <- summary(mdl)$coefficients
    mdl.effects <- lmSupport::modelEffectSizes(mdl, Print = FALSE)$Effects[, 2:3]
    if(is.matrix(mdl.effects)) {
        mdl.s <- cbind(mdl.coefficients, mdl.effects)
    } else {
        mdl.s <- cbind(mdl.coefficients %>% as.matrix, mdl.effects %>% as.matrix() %>% t)
    }
    coef.names <- row.names(mdl.s)
    mdl.s <- data.table(mdl.s)
    mdl.s[, ` ` := coef.names]
    mdl.s[, `F value` := `t value`**2]
    mdl.s[`Pr(>|t|)` >= .1,    `  ` := " "  ]
    mdl.s[`Pr(>|t|)` <  .1,    `  ` := "."  ]
    mdl.s[`Pr(>|t|)` <  .05,   `  ` := "*"  ]
    mdl.s[`Pr(>|t|)` <  .01,   `  ` := "**" ]
    mdl.s[`Pr(>|t|)` <  .001,  `  ` := "***"]

    setcolorder(mdl.s, c(" ", "Estimate", "Std. Error", "df", "t value",
                         "F value", "Pr(>|t|)", "pEta-sqr", "  "))
    if(printOut) {
        as.character(mdl$call)[2] %>% cat(., "\n")
        schmitz::roundify(mdl.s, digits)
    }
    return(invisible(mdl.s))
}
