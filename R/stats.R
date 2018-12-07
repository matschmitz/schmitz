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
ss <- function(mdl, digits = 3) {
    mdl.type <- ifelse(is.null(names(mdl)), "lmer", "lm")
    mdl.coefficients <- summary(mdl)$coefficients
    if (mdl.type == "lm") {
        mdl.effects <- lmSupport::modelEffectSizes(mdl, Print = FALSE)$Effects[, 2:3]
        if(is.matrix(mdl.effects)) {
            mdl.s <- cbind(mdl.coefficients, mdl.effects)
        } else {
            mdl.s <- cbind(mdl.coefficients %>% as.matrix, mdl.effects %>% as.matrix() %>% t)
        }
    } else { # lmer
        mdl.s <- mdl.coefficients
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

    if (mdl.type == "lm") {
        mdl.s <- mdl.s[, .(` `, `Estimate`, `Std. Error`, `df`, `t value`,
                           `F value`, `Pr(>|t|)`, `pEta-sqr`, `  `)]

        as.character(mdl$call)[2] %>% cat("\n")
    } else { # lmer
        mdl.s <- mdl.s[, .(` `, `Estimate`, `Std. Error`, `df`, `t value`,
                           `F value`, `Pr(>|t|)`, `  `)]

        as.character(mdl@call)[2] %>% cat("\n")
    }

    print(schmitz::roundify(mdl.s, digits = 3))
}

#' @title Standard error of the mean
#'
#' @description Compute the stardard error of the mean (SE)
#' @param x a numerical vector
#' @return scalar
#' @examples sem(1:10)
#' @export sem
sem <- function(x) sd(x)/sqrt(length(x))

#' @title 95% confidence interval
#'
#' @description Compute the 95% confidence interval of the mean
#' @inheritParams genMask
#' @return vector of length 2 with the first value being the low 95%CI and the second value the high 95%ci
#' @examples ci95(1:10)
#' @export ci95
ci95 <- function(x) c(mean(x) - 1.96 * sem(x), mean(x) + 1.96 * sem(x))

#' @title 95% low confidence interval
#'
#' @description Compute the 95% low confidence interval of the mean
#' @inheritParams genMask
#' @return scalar with low 95%CI
#' @examples ci95l(1:10)
#' @export ci95l
ci95l <- function(x) ci95(x)[1]

#' @title 95% high confidence interval
#'
#' @description Compute the 95% high confidence interval of the mean
#' @param x a numerical vector
#' @return scalar with high 95%CI
#' @examples ci95h(1:10)
#' @export ci95h
ci95h <- function(x) ci95(x)[2]
