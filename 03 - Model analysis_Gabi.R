# Installing and loading required libraries
# remotes::install_version("lavaan", '0.6.16')
if(!require(lavaan)) install.packages("lavaan")
if(!require(dplyr)) install.packages("dplyr")
if(!require(expss)) install.packages("expss")
library(lavaan); library(dplyr); library(expss)
getFit <- function(sem.rez) {
  tmp <- c(round(as.numeric(rez$header['optim.iterations'])),
           round(as.numeric(rez$fit['npar'])),
           round(as.numeric(rez$data['nobs'])),
           round(as.numeric(rez$fit['chisq.scaled']), 3),
           round(as.numeric(rez$fit['df.scaled'])),
           as.numeric(rez$fit['pvalue.scaled']),
           round(as.numeric(rez$fit['cfi.robust']), 3),
           round(as.numeric(rez$fit['srmr']), 3),
           round(as.numeric(rez$fit['rmsea.robust']), 3),
           round(as.numeric(rez$fit['rmsea.ci.lower.robust']), 3),
           round(as.numeric(rez$fit['rmsea.ci.upper.robust']), 3),
           as.numeric(rez$fit['rmsea.pvalue.robust']))
  tmp <- as.vector(data.frame(tmp)); tmp <- unlist(tmp); tmp <- as.numeric(tmp)
  tmp[6] <- p(as.numeric(tmp[6]), digits = 3); tmp[12] <- p(as.numeric(tmp[12]))
  tmp[c(1:5, 7:11)] <- round(as.numeric(tmp[c(1:5, 7:11)]), 3)
  return(tmp)
}
p <- function(x, digits = 3){
  if (is.na(x)) return(NA)
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}
addSignificance <- function(number, p = 1){
  number <- sub("^(-?)0.", "\\1.", sprintf("%.2f", as.numeric(number)))
  if (p != 1) {
    if (p <= .001) {
      number = paste(number, "***", sep = "")
    } else if (p <= .01) {
      number = paste(number, "**", sep = "")
    } else if (p <= .05) {
      number = paste(number, "*", sep = "")
    } else if (p <= .08) {
      number = paste(number, "^", sep = "")
    }
  }
  return(number)
}


# Loading dataset
load(file = "Totals.RData"); names(ds)

# SEM model analysis ####
## Setting-up the model ####
m <- '
  # First regression paths
  AMOT.T2 ~ B7*AMOT.T1 + B1*GRAT.T1
  EXRE.T2 ~ B13*EXRE.T1 + B10*GRAT.T1
  INRE.T2 ~ B17*INRE.T1 + B14*GRAT.T1
  IDRE.T2 ~ B21*IDRE.T1 + B18*GRAT.T1
  INTR.T2 ~ B25*INTR.T1 + B22*GRAT.T1
  GRAT.T2 ~ B6*GRAT.T1

  # Second regression paths
  COPE.T2 ~ B8*COPE.T1 + B3*GRAT.T1 + B2*AMOT.T1 + B11*EXRE.T1 + B15*INRE.T1 + B19*IDRE.T1 + B23*INTR.T1
  TAPE.T2 ~ B9*TAPE.T1 + B4*GRAT.T1 + B5*AMOT.T1 + B12*EXRE.T1 + B16*INRE.T1 + B20*IDRE.T1 + B24*INTR.T1

  # Indirect effects assessment
  Gr1.Am2.Cp2 := B1 * B2 # Gratitude (T1) -> Amotivation (T2) -> Contextual performance (T2)
  Gr1.Am2.Tp2 := B1 * B5 # Gratitude (T1) -> Amotivation (T2) -> Task performance (T2)
  Gr1.Ex2.Cp2 := B10 * B11 # Gratitude (T1) -> Extrinsic regulation (T2) -> Contextual performance (T2)
  Gr1.Ex2.Tp2 := B10 * B12 # Gratitude (T1) -> Extrinsic regulation (T2) -> Task performance (T2)
  Gr1.In2.Cp2 := B14 * B15 # Gratitude (T1) -> Introjected regulation (T2) -> Contextual performance (T2)
  Gr1.In2.Tp2 := B14 * B16 # Gratitude (T1) -> Introjected regulation (T2) -> Task performance (T2)
  Gr1.Id2.Cp2 := B18 * B19 # Gratitude (T1) -> Identified regulation (T2) -> Contextual performance (T2)
  Gr1.Id2.Tp2 := B18 * B20 # Gratitude (T1) -> Identified regulation (T2) -> Task performance (T2)
  Gr1.Im2.Cp2 := B22 * B23 # Gratitude (T1) -> Intrinsic motivation (T2) -> Contextual performance (T2)
  Gr1.Im2.Tp2 := B22 * B24 # Gratitude (T1) -> Intrinsic motivation (T2) -> Task performance (T2)


  # Covariances
  # GRAT.T1 ~~ CV1*AMOT.T1 + CV8*EXRE.T1 + CV13*INRE.T1 + CV18*INRE.T1 + CV23*INTR.T1 + CV2*COPE.T1 + CV4*TAPE.T1
  # AMOT.T1 ~~ CV3*COPE.T1 + CV5*TAPE.T1
  # EXRE.T1 ~~ CV9*COPE.T1 + CV10*TAPE.T1
  # INRE.T1 ~~ CV14*COPE.T1 + CV15*TAPE.T1
  # IDRE.T1 ~~ CV19*COPE.T1 + CV20*TAPE.T1
  # INTR.T1 ~~ CV24*COPE.T1 + CV25*TAPE.T1

  AMOT.T2 ~~ COPE.T2 + TAPE.T2
  EXRE.T2 ~~ COPE.T2 + TAPE.T2
  INRE.T2 ~~ COPE.T2 + TAPE.T2
  IDRE.T2 ~~ COPE.T2 + TAPE.T2
  INTR.T2 ~~ COPE.T2 + TAPE.T2

'

## Assessing model ####
m.sem <- sem(model = m, data = ds, fixed.x = F, meanstructure = F, #se = "boot", bootstrap = 5000, iseed = 34534,
             missing = "listwise", estimator = "MLMV", auto.var = F,  fixed.x = F, check.start = T,
             se = "robust.sem")
rez <- summary(m.sem, fit.measures = T, standardized = T, rsquare = T, ci = T); cat("\14"); rez

## Returning values - Fit indexes ####
fit.rez <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(fit.rez) <- c("Iterations", "Parameters", "N", "Chi sqr", "df", "p Chi sqr", "CFI", "SRMR",
                       "RMSEA", "90% CI Lower", "90% CI Upper", "p RMSEA")
fit.rez <- rbind(fit.rez, getFit(rez)); fit.rez <- fit.rez[-1,]; rownames(fit.rez) <- NULL


## Returning values - Coefficients ####
effects <- rez$pe %>%
  filter(op == "~" | op == ":=") %>%
  select(-label, -exo, -std.lv, -std.nox)
effects[, 4:10] <- round(effects[, 4:10], 4)
effects$op[effects$op == "~"] <- "<-"; effects$op[effects$op == ":="] <- " <- "
effects
effects$lhs <- c(rep("Amotivation (T2)", 2), rep("Extrinsic regulation (T2)", 2), rep("Introjected regulation (T2)", 2),
                 rep("Identified regulation (T2)", 2), rep("Intrinsic motivation (T2)", 2),
                 "Gratitude (T2)",
                 rep("Contextual performance (T2)", 7), rep("Task performance (T2)", 7),
                 rep(c("Contextual performance (T2)", "Task performance (T2)"), 5))
effects$rhs <- c("Amotivation (T1)", "Gratitude (T1)", "Extrinsic regulation (T1)",
                 "Gratitude (T1)", "Introjected regulation (T1)", "Gratitude (T1)",
                 "Identified regulation (T1)", "Gratitude (T1)", "Intrinsic motivation (T1)",
                 "Gratitude (T1)", "Gratitude (T1)", "Contextual performance (T1)",
                 "Gratitude (T1)", "Amotivation (T1)", "Extrinsic regulation (T1)",
                 "Introjected regulation (T1)", "Identified regulation (T1)", "Intrinsic motivation (T1)",
                 "Task performance (T1)", "Gratitude (T1)", "Amotivation (T1)",
                 "Extrinsic regulation (T1)", "Introjected regulation (T1)", "Identified regulation (T1)",
                 "Intrinsic motivation (T1)",
                 rep("Amotivation (T2) <- Gratitude (T1)", 2),
                 rep("Extrinsic regulation (T2) <- Gratitude (T1)", 2),
                 rep("Introjected regulation (T2) <- Gratitude (T1)", 2),
                 rep("Identified regulation (T2) <- Gratitude (T1)", 2),
                 rep("Intrinsic motivation (T2) <- Gratitude (T1)", 2))
colnames(effects) <- c("Outcomes", " ", "Predictors", "Estimator", "SE", "z", "p","95% CI Lo.", "95% CI Up.", "Beta")




