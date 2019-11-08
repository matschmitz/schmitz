#' @title Formated mean and standard deviation
#' @description Format the mean and sd of a vector
#' @param x a numerical vector
#' @return formated string
#' @examples m_sd(rnorm(100))
#' @export m_sd
m_sd <- function(x, ...) sprintf("%.2f(%.2f)", mean(x, ...), sd(x, ...))

#' @title Effect sizes for lm summary
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
    mdl.s[, coefs := coef.names]
    mdl.s[, Fval := `t value`**2]
    mdl.s[`Pr(>|t|)` >= .1,    `  ` := " "  ]
    mdl.s[`Pr(>|t|)` <  .1,    `  ` := "."  ]
    mdl.s[`Pr(>|t|)` <  .05,   `  ` := "*"  ]
    mdl.s[`Pr(>|t|)` <  .01,   `  ` := "**" ]
    mdl.s[`Pr(>|t|)` <  .001,  `  ` := "***"]
    
    # Bayes factor
    
    mdl.s[, BF10 := {
        .coefs <- ifelse(coefs == "(Intercept)", 1, coefs)
        mdl_null <- update(mdl, paste("~ . -", .coefs))
        exp((BIC(mdl_null) - BIC(mdl))/2)
    }, 1:nrow(mdl.s)]
    
    mdl.s[, BF01 := 1 / BF10]
    
    if (mdl.type == "lm") {
        mdl.s <- mdl.s[, .(` ` = coefs, b = round(`Estimate`, 2), SE = round(`Std. Error`, 2), `df`,
                           Fval = round(Fval, 2), p = round(`Pr(>|t|)`, 3), 
                           BF10 = round(BF10, 2), BF01 = round(BF01, 2),
                           peta2 = round(`pEta-sqr`, 3), `  `)]
        as.character(mdl$call)[2] %>% cat("\n")
    } else { # lmer
        mdl.s <- mdl.s[, .(` ` = coefs, b = round(`Estimate`, 2), SE = round(`Std. Error`, 2), `df`,
                           Fval = round(Fval, 2),
                           BF10 = round(BF10, 2), BF01 = round(BF01, 2),
                           p = round(`Pr(>|t|)`, 3), `  `)]
        as.character(mdl@call)[2] %>% cat("\n")
    }
    
    print(mdl.s)
}

#' @title Report Bayes factor
#' @description Report the Bayes factor (BF10, BF01 and error)
#' @param BFobj a `BFBayesFactor` object as returned from `lmBF(mdlH1)/lmBF(mdlH0)`. Warning, 
#' the `BFobj` must be in favor of the alternative hypothesis (BF10)
#' @param digits number of decimal digits
#' @return String with the BF10, BF01 and error
#' @export ssBF
ssBF <- function(BFobj, digits = 2) {
    # Temporary turn off warning
    oldw <- getOption("warn")
    options(warn = -1)
    
    # Extract factor(s) being tested
    numerator <- as.character(names(BFobj@numerator))
    numerator <- strsplit(numerator, " \\+ ")[[1]]
    
    denomiator <- BFobj@denominator@shortName
    denomiator <- strsplit(denomiator, " \\+ ")[[1]]
    
    factorDiff <- setdiff(numerator, denomiator)
    factorDiff <- paste(factorDiff, collapse = ", ")
    
    # Extract bayes factor and error
    BF <- extractBF(BFobj)
    BF10 <- BF$bf
    BF01 <- 1/BF10
    BFerr <- BF$error * 100
    
    if (BF10 > 1) {
        interpretation <- dplyr::case_when(
            data.table::between(BF10, 1, 3) ~ "anectdotal",
            data.table::between(BF10, 3, 10) ~ "substantial",
            data.table::between(BF10, 10, 30) ~ "strong",
            data.table::between(BF10, 30, 150) ~ "very strong",
            BF10 > 150 ~ "decisive"
        )
        interpretation <- paste(interpretation, "evidence in favor of H1")
    } else {
        interpretation <- dplyr::case_when(
            data.table::between(BF01, 1, 3) ~ "anectdotal",
            data.table::between(BF01, 3, 10) ~ "substantial",
            data.table::between(BF01, 10, 30) ~ "strong",
            data.table::between(BF01, 30, 150) ~ "very strong",
            BF10 > 150 ~ "decisive"
        )
        interpretation <- paste(interpretation, "evidence in favor of H0")
    }
    
    
    return(
        cat(factorDiff, " BF10: ", round(BF10, 2), " BF01: ", round(BF01, 2),
            " ± ", round(BFerr, 2), "% ", interpretation, sep = "")
    )
    
    # Reset warnings to current
    options(warn = oldw)
}

#' @title Standard error of the mean
#' @description Compute the stardard error of the mean (SE)
#' @param x a numerical vector
#' @return scalar
#' @examples sem(1:10)
#' @export sem
sem <- function(x) sd(x)/sqrt(length(x))

#' @title 95 percent confidence interval
#' @description Compute the 95 percent confidence interval of the mean
#' @param x a numerical vector
#' @return vector of length 2: the first value is the low 95 percent confidence interval
#'  and the second the high 95 percent confidence interval
#' @examples ci95(1:10)
#' @export ci95
ci95 <- function(x) c(mean(x) - 1.96 * sem(x), mean(x) + 1.96 * sem(x))
