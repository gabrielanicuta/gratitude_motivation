# Installing and loading required libraries
# remotes::install_version("lavaan", '0.6.16')
if(!require(lavaan)) install.packages("lavaan")
if(!require(dplyr)) install.packages("dplyr")
if(!require(expss)) install.packages("expss")
library(lavaan); library(dplyr); library(expss)
p <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}
getFit <- function(sem.rez) {
  tmp <- c(round(as.numeric(rez$header['optim.iterations'])),
           round(as.numeric(rez$fit['npar'])),
           round(as.numeric(rez$data['nobs'])),
           round(as.numeric(rez$fit['chisq.scaled']), 3),
           round(as.numeric(rez$fit['df.scaled'])),
           as.numeric(rez$fit['pvalue.scaled']),
           round(as.numeric(rez$fit['cfi.scaled']), 3),
           round(as.numeric(rez$fit['srmr']), 3),
           round(as.numeric(rez$fit['rmsea.scaled']), 3),
           round(as.numeric(rez$fit['rmsea.ci.lower.scaled']), 3),
           round(as.numeric(rez$fit['rmsea.ci.upper.scaled']), 3),
           as.numeric(rez$fit['rmsea.pvalue.scaled']))
  tmp <- as.vector(data.frame(tmp)); tmp <- unlist(tmp);  tmp <- as.numeric(tmp)
  tmp[6] <- p(as.numeric(tmp[6]), digits = 3); tmp[12] <- p(as.numeric(tmp[12]), digits = 3)
  tmp[c(1:5, 7:11)] <- round(as.numeric(tmp[c(1:5, 7:11)]), 3)
  return(tmp)}

# Loading dataset
load(file = "Totals.RData"); names(ds)

# SEM model analysis ####
## Setting-up the model ####
m <- '
  # First regression paths
  AMOT.T2 ~ B1*AMOT.T1 + B10*GRAT.T1
  EXRE.T2 ~ B2*EXRE.T1 + B11*GRAT.T1
  INRE.T2 ~ B7*INRE.T1 + B12*GRAT.T1
  IDRE.T2 ~ B8*IDRE.T1 + B13*GRAT.T1
  INTR.T2 ~ B9*INTR.T1 + B14*GRAT.T1

  # Second regression paths
  COPE.T2 ~ B3*COPE.T1 + B4*GRAT.T1 + B15*AMOT.T2 + B17*EXRE.T2 + B19*INRE.T2 + B21*IDRE.T2 + B23*INTR.T2
  TAPE.T2 ~ B6*TAPE.T1 + B5*GRAT.T1 + B16*AMOT.T2 + B18*EXRE.T2 + B20*INRE.T2 + B22*IDRE.T2 + B24*INTR.T2

  # Covariances on mediators residuals
  AMOT.T2 ~~ CR1*EXRE.T2 + CR2*INRE.T2 + CR3*IDRE.T2 + CR4*INTR.T2
  EXRE.T2 ~~ CR5*INRE.T2 + CR6*IDRE.T2 + CR7*INTR.T2
  INRE.T2 ~~ CR8*IDRE.T2 + CR9*INTR.T2
  IDRE.T2 ~~ CR10*INTR.T2

  # Indirect effects
  CP.AM.CP := B10 * B15 # Amotivation
  CP.ER.CP := B11 * B17 # Extrinsic regulation
  CP.IR.CP := B12 * B19 # Introjected regulation
  CP.ID.CP := B13 * B21 # Identified regulation
  CP.IM.CP := B14 * B23 # Intrinsic motivation

  TP.AM.TP := B10 * B16 # Amotivation
  TP.ER.TP := B11 * B18 # Extrinsic regulation
  TP.IR.TP := B12 * B20 # Introjected regulation
  TP.ID.TP := B13 * B22 # Identified regulation
  TP.IM.TP := B14 * B24 # Intrinsic motivation

  # Total Effects
  TOT.CP := B4 * CP.AM.CP * CP.ER.CP * CP.IR.CP * CP.ID.CP * CP.IM.CP
  TOT.TP := B5 * TP.AM.TP * TP.ER.TP * TP.IR.TP * TP.ID.TP * TP.IM.TP
'

## Assessing model ####
m.sem <- sem(model = m, data = ds, fixed.x = F, meanstructure = F,
             missing = "listwise", estimator = "MLM", auto.var = F,  fixed.x = F, check.start = T)
cat("\14")
rez <- summary(m.sem, fit.measures = T, standardized = T, rsquare = T); rez

## Returning values - Fit indexes ####
fit.rez <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(fit.rez) <- c("Iterations", "Parameters", "N", "Chi sqr", "df", "p Chi sqr", "CFI", "SRMR",
                       "RMSEA", "95% CI Lower", "95% CI Upper", "p RMSEA")
fit.rez <- rbind(fit.rez, getFit(rez)); fit.rez <- fit.rez[-1, ]
## Returning values - Coefficients ####
effects <- rez$pe %>%
  filter(op == "~" | op == ":=") %>%
  select(-label, -exo, -std.lv, -std.nox)
effects[, 4:8] <- round(effects[, 4:8], 4)
effects$op[effects$op == "~"] <- "<-"; effects$op[effects$op == ":="] <- " <- "
effects
effects$lhs <- c(rep("Amotivation (T2)", 2), rep("Extrinsic regulation (T2)", 2), rep("Introjected regulation (T2)", 2),
                 rep("Identified regulation (T2)", 2), rep("Intrinsic motivation (T2)", 2),
                 rep("Contextual performance (T2)", 7), rep("Task performance (T2)", 7),
                 rep("Contextual performance (T2)", 5), rep("Task performance (T2)", 5),
                 "TOTAL Contextual performance (T2)", "TOTAL Task performance (T2)")
effects$rhs <- c("Amotivation (T1)", "Gratitude (T1)", "Extrinsic regulation (T1)", "Gratitude (T1)", "Introjected regulation (T1)",
                 "Gratitude (T1)", "Identified regulation (T1)", "Gratitude (T1)", "Intrinsic motivation (T1)", "Gratitude (T1)",
                 "Contextual performance (T1)", "Gratitude (T1)", "Amotivation (T2)", "Extrinsic regulation (T2)", "Introjected regulation (T2)",
                 "Identified regulation (T2)", "Intrinsic motivation (T2)", "Task performance (T1)", "Gratitude (T1)", "Amotivation (T2)",
                 "Extrinsic regulation (T2)", "Introjected regulation (T2)", "Identified regulation (T2)", "Intrinsic motivation (T2)",
                 rep(c("Amotivation (T2) <- Gratitude (T1)", "Extrinsic regulation (T2) <- Gratitude (T1)" ,
                 "Introjected regulation (T2) <- Gratitude (T1)", "Identified regulation (T2) <- Gratitude (T1)",
                 "Intrinsic motivation (T2) <- Gratitude (T1)"), 2),
                 "", "")
colnames(effects) <- c("Outcomes", " ", "Predictors", "Estimator", "SE", "z", "p", "Beta")

## Show tables in Viewer and removing objects ####
rm(ds, m.sem, rez, m, getFit)
rownames(fit.rez) <- NULL; fit.rez
expss_output_viewer(); as.etable(fit.rez); expss_output_default()
effects; expss_output_viewer(); as.etable(effects); expss_output_default()
