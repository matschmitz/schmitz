#' @title Formated mean and standard deviation
#' @description Format the mean and sd of a vector
#' @param x a numerical vector
#' @return formated string
#' @examples
#' m_sd(rnorm(100))
#' @export m_sd
m_sd <- function(x, ...) sprintf("%.2f(%.2f)", mean(x, ...), sd(x, ...))


#' @title LMM and LM results summary
#' @description Formats the results from LMM and LM. Provides effect sizes.
#' @param mdl an lm model obtained from lm
#' @return data table with summary statistics
#' @examples
#' ss(mdl)
#' @export ss
ss <- function(mdl) {
  mdl.type <- ifelse(is.null(names(mdl)), "lmer", "lm")
  mdl.coefficients <- summary(mdl)$coefficients
  if (mdl.type == "lm") {
    mdl.effects <- lmSupport::modelEffectSizes(mdl, Print = FALSE)$Effects[, 2:3]
    if (is.matrix(mdl.effects)) {
      mdl.s <- cbind(mdl.coefficients, mdl.effects)
    } else {
      mdl.s <- cbind(mdl.coefficients %>% as.matrix(), mdl.effects %>% as.matrix() %>% t())
    }
  } else { # lmer
    mdl.s <- mdl.coefficients
  }
  coef.names <- row.names(mdl.s)
  mdl.s <- data.table(mdl.s)
  mdl.s[, coefs := coef.names]
  mdl.s[, Fval := `t value`**2]
  mdl.s[`Pr(>|t|)` >= .1, `  ` := " "  ]
  mdl.s[`Pr(>|t|)` < .1, `  ` := "."  ]
  mdl.s[`Pr(>|t|)` < .05, `  ` := "*"  ]
  mdl.s[`Pr(>|t|)` < .01, `  ` := "**" ]
  mdl.s[`Pr(>|t|)` < .001, `  ` := "***"]

  if (mdl.type == "lm") {
    mdl.s <- mdl.s[, .(
      ` ` = coefs, b = round(`Estimate`, 2), SE = round(`Std. Error`, 2),
      df = round(df, 2), Fval = round(Fval, 2),
      peta2 = round(`pEta-sqr`, 3),
      p = ifelse(`Pr(>|t|)` < .001, "<.001", as.character(round(`Pr(>|t|)`, 3))),
      `  `
    )]
    as.character(mdl$call)[2] %>% cat("\n")
  } else { # lmer
    mdl.s <- mdl.s[, .(
      ` ` = coefs, b = round(`Estimate`, 2), SE = round(`Std. Error`, 2),
      df = round(df, 2), Fval = round(Fval, 2),
      p = ifelse(`Pr(>|t|)` < .001, "<.001", as.character(round(`Pr(>|t|)`, 3))),
      `  `
    )]
    as.character(mdl@call)[2] %>% cat("\n")
  }
  mdl.s
}


#' @title LMM and LM results summary with Bayes factor
#' @description Formats the results from LMM and LM. Provides effect sizes and Bayes factor.
#' @param mdl an lm model obtained from lm
#' @return data table with summary statistics
#' @examples
#' ssb(mdl)
#' @export ssBF
ssBF <- function(mdl) {
  mdl.s <- ss(mdl)

  mdl.s[, BF10 := {
    .coefs <- ifelse(` ` == "(Intercept)", 1, ` `)
    mdl_null <- update(mdl, paste("~ . -", .coefs))
    exp((BIC(mdl_null) - BIC(mdl))/2)
  }, 1:nrow(mdl.s)]
  
  mdl.s[, BF01 := 1 / BF10]
  mdl.s[, BF10 := dplyr::case_when(BF10 > 1e5 ~ ">1e5",
                                   BF10 < .001 ~ "<.001",
                                   TRUE ~ as.character(round(BF10, 2)))]
  mdl.s[, BF01 := dplyr::case_when(BF01 > 1e5 ~ ">1e5",
                                   BF01 < .001 ~ "<.001",
                                   TRUE ~ as.character(round(BF01, 2)))]
  
  mdl.s <- mdl.s[, .(` `, b, SE, df, Fval, peta2, BF10, BF01, p, `  `)]
  
  print(mdl.s)
}

#' @title Standard error of the mean
#' @description Compute the stardard error of the mean (SE)
#' @param x a numerical vector
#' @return scalar
#' @examples
#' sem(1:10)
#' @export sem
sem <- function(x) sd(x) / sqrt(length(x))

#' @title 95 percent confidence interval
#' @description Compute the 95 percent confidence interval of the mean
#' @param x a numerical vector
#' @return vector of length 2: the first value is the low 95 percent confidence interval
#'  and the second the high 95 percent confidence interval
#' @examples
#' ci95(1:10)
#' @export ci95
ci95 <- function(x) c(mean(x) - 1.96 * sem(x), mean(x) + 1.96 * sem(x))
