#' @title Formatted mean and standard deviation
#' @description Format the mean and sd of a vector
#' @param x a numerical vector
#' @return formatted string
#' @examples
#' m_sd(rnorm(100))
#' @export m_sd
m_sd <- function(x, ...) sprintf("%.2f(%.2f)", mean(x, ...), sd(x, ...))


#' @title Print p-values
#' @description Nicely print p-values (*p<.05, **p<.01, ***p<.001)
#' @param mdl an lm model obtained from lm
#' @return data table with summary statistics
#' @examples
#' pprint(.003)
#' @export pprint
pprint <- function(pval) {
  formated_pval <- data.table::fcase(pval < .001, ".001***",
                                     pval < .01, sprintf("%.3f**", pval),
                                     pval < .05, sprintf("%.3f*", pval),
                                     pval >= .05, sprintf("%.3f", pval))
  gsub("0\\.", ".", formated_pval)
}


#' @title LMM and LM results summary
#' @description Formats the results from LMM and LM. Provides effect sizes.
#' @param mdl an lm model obtained from lm
#' @return data table with summary statistics
#' @examples
#' ss(mdl)
#' @export ss
ss <- function(mdl) {
  mdl.type <- ifelse(is.null(names(mdl)), "lmer", "lm")
  mdl.summary <- summary(mdl)
  mdl.coefficients <- mdl.summary$coefficients
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
    confints <- confint(mdl)
    mdl.s <- mdl.s[, .(
      ` `     = coefs,
      b       = sprintf("%.2f", `Estimate`),
      `95%CI` = sprintf("(%.2f, %.2f)", confints[, 1], confints[, 2]),
      # SE      = sprintf("%.2f", `Std. Error`),
      df      = sprintf("%.f", df),
      Fval    = sprintf("%.2f", Fval),
      peta2   = schmitz::round3(`pEta-sqr`),
      p       = schmitz::round3(`Pr(>|t|)`),
      `  `
    )]
    as.character(mdl$call)[2] %>% cat("\n")
    rsq <- mdl.summary$r.squared
    f <- mdl.summary$fstatistic[1]
    df1 <- mdl.summary$fstatistic[2]
    df2 <- mdl.summary$fstatistic[3]
    p <- pf(f, df1, df2, lower.tail = F)
    attr(mdl.s, "rsq") <- rsq
    attr(mdl.s, "n") <- nrow(mdl$model)
    attr(mdl.s, "f") <- f
    attr(mdl.s, "df1") <- df1
    attr(mdl.s, "df2") <- df2
    attr(mdl.s, "p") <- p
    
    mdl.fit.print <- ifelse(p < .001, 
                            sprintf("R2=%s, F(%.f,%.f)=%.2f,  p<%s",
                                    schmitz::round3(rsq), df1, df2, f, pprint(p)),
                            sprintf("R2=%s, F(%.f,%.f)=%.2f,  p=%s",
                                    schmitz::round3(rsq), df1, df2, f, pprint(p)))
    
  } else { # lmer
    mdl.s <- mdl.s[, .(
      ` ` = coefs, b = sprintf("%.2f", `Estimate`), SE = sprintf("%.2f", `Std. Error`),
      df = sprintf("%.2f", df), Fval = sprintf("%.2f", Fval),
      # See https://doi.org/10.5334/joc.10 for the computation COMPUTE D CONVERT TO ETA
      peta2 = {
        rsq <- data.table(r2glmm::r2beta(mdl, method = 'nsj', partial = TRUE))
        rsq <- rsq[, .(Effect, Rsq)]
        rsq[Effect == "Model", `:=`(Effect = "(Intercept)", Rsq = NA)]
        rsq <- rsq[match(mdl.s$coefs, Effect)][, Rsq] # order coefficients correctly
        schmitz::round3(rsq)
      },
      p = schmitz::round3(`Pr(>|t|)`),
      `  `
    )]
    mdl.fit.print <- as.character(mdl@call)[2]
  }
  cat(mdl.fit.print, "\n\n")
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
                                   TRUE ~ sprintf("%.2f", BF10))]
  mdl.s[, BF01 := dplyr::case_when(BF01 > 1e5 ~ ">1e5",
                                   BF01 < .001 ~ "<.001",
                                   TRUE ~ sprintf("%.2f", BF01))]
  
  mdl.type <- ifelse(is.null(names(mdl)), "lmer", "lm")
  if (mdl.type == "lm") {
    mdl.s <- mdl.s[, .(` `, b, SE, df, Fval, peta2, BF10, BF01, p, `  `)]
    as.character(mdl$call)[2] %>% cat("\n")
  } else {
    mdl.s <- mdl.s[, .(` `, b, SE, df, Fval, peta2, BF10, BF01, p, `  `)]
    as.character(mdl@call)[2] %>% cat("\n")
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
#' @param w (optional; Default = `NULL`) Weight vector to be applied to the analyses.
#' @param digits (optional; Default = 2) 
#' @param corType (optional; Default = "pearson") Correlation type.
#' @param kableFormat (optional; Default = `TRUE`) Should the table be formated with kableExtra
#' @param kableColor (optional; Default = `TRUE`) Color formatble table (only if `kableFormat = TRUE`)
#' @return Correlation matrix in APA format (in HTML if `kableExtra = TRUE`).
#' @examples
#' apacorr(mtcars)
#' @export apacorr
apacorr <- function(X, vars = NULL, w = NULL, digits = 2, corType = "pearson",
                    kableFormat = TRUE, kableColor = TRUE) {
  X <- data.table::data.table(X)
  if(!is.null(vars)) X <- X[, .SD, .SDcols = vars]
  
  if(is.null(w)) {
    corrRes <- Hmisc::rcorr(as.matrix(X), type = corType)
    C <- corrRes$r
    P <- corrRes$P
    vars <- rownames(C)
  }
  
  if(!is.null(w)) {
    if (!corType == "pearson") stop("corType can only be 'pearson' for wieghted correlations")
    corrRes <- weights::wtd.cor(X, weight = w, mean1 = FALSE)
    C <- corrRes$correlation
    P <- corrRes$p.value
    vars <- rownames(C)
  }
  
  COR <- weights::rd(C, digits = digits)
  
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
  if (is.null(w)) {
    COR[, M := sprintf("%.2f", colMeans(X, na.rm = TRUE))]
    COR[, SD := sprintf("%.2f", apply(X, 2, sd, na.rm = TRUE))]
  }
  
  if (!is.null(w)) {
    COR[, M := sprintf("%.2f", apply(X, 2, Hmisc::wtd.mean, weights = w, na.rm = TRUE))]
    COR[, SD := sprintf("%.2f", apply(X, 2, function(x) {
      sqrt(Hmisc::wtd.var(x, weights = w, na.rm = TRUE))
    }))]
  }
  
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
