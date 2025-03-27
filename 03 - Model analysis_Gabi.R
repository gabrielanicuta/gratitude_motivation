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
  AMOT.T2 ~ AMOT.T1 + GRAT.T1
  EXRE.T2 ~ EXRE.T1 + GRAT.T1
  INRE.T2 ~ INRE.T1 + GRAT.T1
  IDRE.T2 ~ IDRE.T1 + GRAT.T1
  INTR.T2 ~ INTR.T1 + GRAT.T1
  GRAT.T2 ~ GRAT.T1

  # Second regression paths
  COPE.T2 ~ COPE.T1 + GRAT.T1 + AMOT.T1 + EXRE.T1 + INRE.T1 + IDRE.T1 + INTR.T1
  TAPE.T2 ~ TAPE.T1 + GRAT.T1 + AMOT.T1 + EXRE.T1 + INRE.T1 + IDRE.T1 + INTR.T1

  # Covariances on T2 variables
  AMOT.T2 ~~ EXRE.T2 + INRE.T2 + IDRE.T2 + INTR.T2 + GRAT.T2
  EXRE.T2 ~~ INRE.T2 + IDRE.T2 + INTR.T2 + GRAT.T2
  INRE.T2 ~~ IDRE.T2 + INTR.T2 + GRAT.T2
  IDRE.T2 ~~ INTR.T2 + GRAT.T2
  INTR.T2 ~~ GRAT.T2

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
