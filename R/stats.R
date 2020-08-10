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
  print(mdl.s)
}


#' @title LMM and LM results summary with Bayes factor
#' @description Formats the results from LMM and LM. Provides effect sizes and Bayes factor.
#' @param mdl an lm model obtained from lm
#' @return data table with summary statistics
#' @examples
#' ssb(mdl)
#' @export ssBF
ssBF <- function(mdl) {
  mdl.s <- quiet(ss(mdl))

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
  
  mdl.type <- ifelse(is.null(names(mdl)), "lmer", "lm")
  if (mdl.type == "lm") {
    mdl.s <- mdl.s[, .(` `, b, SE, df, Fval, peta2, BF10, BF01, p, `  `)]
  } else {
    mdl.s <- mdl.s[, .(` `, b, SE, df, Fval, BF10, BF01, p, `  `)]
  }
  
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

#' @title Correlation Matrix APA
#' @description Correlation matrix in APA format
#' @param X a matrix, data.frame, or data table
#' @param vars (optional; Default = `NULL`) Character vector on which compute the function.
#' @param digits (optional; Default = 2) 
#' @param kableFormat (optional; Default = `TRUE`) Should the table be formated with kableExtra
#' @param kableColor (optional; Default = `TRUE`) Color formatble table (only if `kableFormat = TRUE`)
#' @return Correlation matrix in APA format (in HTML if `kableExtra = TRUE`).
#' @examples
#' apacorr(mtcars)
#' @export apacorr
apacorr <- function(X, vars = NULL, digits = 2, kableFormat = TRUE, kableColor = TRUE) {
  X <- data.table::data.table(X)
  if(!is.null(vars)) X <- X[, .SD, .SDcols = vars]
  
  corrRes <- Hmisc::rcorr(as.matrix(X))
  
  C <- corrRes$r
  P <- corrRes$P
  vars <- rownames(C)
  
  COR <- weights::rd(C, digits = 2)
  
  P[is.na(P)] <- 99
  
  # Add sig stars
  COR[as.vector(P < .001)]            <- paste0(COR[as.vector(P < .001)], "***")
  COR[as.vector(P < .01 & P >= .001)] <- paste0(COR[as.vector(P < .01 & P >= .001)], "** ")
  COR[as.vector(P < .05 & P >= .01)]  <- paste0(COR[as.vector(P < .05 & P >= .01)], "*  ")
  COR[as.vector(P >= .05)]            <- paste0(COR[as.vector(P >= .05)], "   ")
  COR[as.vector(COR %like% "^\\.")]   <- paste0(" ", COR[as.vector(COR %like% "^\\.")]) # fail
  
  # Add colors
  if (kableFormat & kableColor) {
    COR[] <- paste0("<span style='color:xxx'>", COR, "</span>")
    
    COR[as.vector(P < .05 & C < 0     & C > -.10)] %<>% gsub("xxx", "#E6B0AA", .)
    COR[as.vector(P < .05 & C <= -.10 & C > -.30)] %<>% gsub("xxx", "#CD6155", .)
    COR[as.vector(P < .05 & C <= -.30 & C > -.50)] %<>% gsub("xxx", "#A93226", .)
    COR[as.vector(P < .05 & C <= -.50)]            %<>% gsub("xxx", "#7B241C", .)
    
    COR[as.vector(P < .05 & C > 0    & C < .10)] %<>% gsub("xxx", "#A9DFBF", .)
    COR[as.vector(P < .05 & C >= .10 & C < .30)] %<>% gsub("xxx", "#52BE80", .)
    COR[as.vector(P < .05 & C >= .30 & C < .50)] %<>% gsub("xxx", "#229954", .)
    COR[as.vector(P < .05 & C >= .50)]           %<>% gsub("xxx", "#196F3D", .)
    
    COR[as.vector(P >= .05)] %<>% gsub("xxx", "#ABB2B9", .)
  }
  
  diag(COR) <- "—  "
  gdata::upperTriangle(COR) <- ""
  
  COR <- data.table::data.table(COR)
  data.table::setnames(COR, names(COR), as.character(1:ncol(COR)))
  
  COR[, Variable := paste0(1:length(vars), ". ", vars)]
  COR[, M := round(colMeans(X), 2) %>% format(nsmall = 2)]
  COR[, SD := round(apply(X, 2, sd), 2) %>% format(nsmall = 2)]
  
  data.table::setcolorder(COR, c("Variable", "M", "SD"))
  
  if(kableFormat) {
    COR %>% 
    kableExtra::kable(escape = FALSE) %>% 
      kableExtra::kable_styling(full_width = FALSE, position = "left") %>% 
      kableExtra::footnote(general = "\\**p*<.05, \\*\\**p*<.01, \\*\\*\\**p*<.001",
                           footnote_as_chunk = TRUE)
  } else {
    return(COR)
  }
}
