# Installing and loading required libraries
# remotes::install_version("lavaan", '0.6.16')
if(!require(lavaan)) install.packages("lavaan")
library(lavaan)
p <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}
getFit <- function(sem.rez) {
  tmp <- c(round(as.numeric(rez$header['optim.iterations'])),
           round(as.numeric(rez$fit['npar'])),
           round(as.numeric(rez$data['nobs'])),
           round(as.numeric(rez$fit['chisq']), 3),
           round(as.numeric(rez$fit['df'])),
           as.numeric(rez$fit['pvalue']),
           round(as.numeric(rez$fit['cfi']), 3),
           round(as.numeric(rez$fit['srmr']), 3),
           round(as.numeric(rez$fit['rmsea']), 3),
           round(as.numeric(rez$fit['rmsea.ci.lower']), 3),
           round(as.numeric(rez$fit['rmsea.ci.upper']), 3),
           as.numeric(rez$fit['rmsea.pvalue']))
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
  AMOT.T2 ~ AMOT.T1 + GRAT.T1
  EXRE.T2 ~ EXRE.T1 + GRAT.T1
  INRE.T2 ~ INRE.T1 + GRAT.T1
  IDRE.T2 ~ IDRE.T1 + GRAT.T1
  INTR.T2 ~ INTR.T1 + GRAT.T1

  # Second regression paths
  COPE.T2 ~ COPE.T1 + GRAT.T1 + AMOT.T2 + EXRE.T2 + INRE.T2 + IDRE.T2 + INTR.T2
  TAPE.T2 ~ TAPE.T1 + GRAT.T1 + AMOT.T2 + EXRE.T2 + INRE.T2 + IDRE.T2 + INTR.T2

  # Covariances on mediators residuals
  AMOT.T2 ~~ EXRE.T2 + INRE.T2 + IDRE.T2 + INTR.T2
  EXRE.T2 ~~ INRE.T2 + IDRE.T2 + INTR.T2
  INRE.T2 ~~ IDRE.T2 + INTR.T2
  IDRE.T2 ~~ INTR.T2
'

## Assessing model ####
m.sem <- sem(model = m, data = ds, fixed.x = F, meanstructure = F,
             missing = "listwise", estimator = "MLM", auto.var = F,  fixed.x = F, check.start = T)
cat("\14")
rez <- summary(m.sem, fit.measures = T, standardized = T, rsquare = T); rez

## Returning values - Fit indexes ####
