# Installing and loading libraries
if(!require(rstatix)) install.packages("rstatix")
if(!require(psych)) install.packages("psych")
if(!require(sasLM)) install.packages("sasLM")
if(!require(expss)) install.packages("expss")
if(!require(Hmisc)) install.packages("Hmisc")
library(rstatix); library(psych); library(sasLM); library(expss); library(Hmisc)
p <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}
getUnivar <- function(name, variable) {
  tmp <- c(as.numeric(psych::describe(variable)),
           round(as.numeric(SkewnessSE(variable)), 3),
           round(as.numeric(KurtosisSE(variable)), 3))
  tst <- shapiro.test(variable)
  tmp <- c(tmp, round(as.numeric(tst$statistic), 3), as.numeric(tst$p.value))
  tmp <- as.vector(data.frame(tmp)); names(tmp) <- NULL; tmp <- unlist(tmp)
  tmp <- as.numeric(tmp); tmp[17] <- p(tmp[17], digits = 3)
  tmp <- c(name, round(as.numeric(tmp[2:16]), 2), tmp[17])
  return(tmp)}

# Loading dataset
load(file = "Totals.RData"); names(ds)

# Outliers assessment ####
## Trait Gratitude ####
out <- ds %>% rstatix::identify_outliers(GRAT.T1); out <- out[, c('GRAT.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$GRAT.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$GRAT.T1[which(ds$GRAT.T1 >= 9)] <- NA

## Amotivation ####
out <- ds %>% rstatix::identify_outliers(AMOT.T1); out <- out[, c('AMOT.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$AMOT.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$AMOT.T1[which(ds$AMOT.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(AMOT.T2); out <- out[, c('AMOT.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$AMOT.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$AMOT.T2[which(ds$AMOT.T2 >= 9)] <- NA

## Extrinsic regulation ####
out <- ds %>% rstatix::identify_outliers(EXRE.T1); out <- out[, c('EXRE.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$EXRE.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$EXRE.T1[which(ds$EXRE.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(EXRE.T2); out <- out[, c('EXRE.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$EXRE.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$EXRE.T2[which(ds$EXRE.T2 >= 9)] <- NA

## Introjected regulation ####
out <- ds %>% rstatix::identify_outliers(INRE.T1); out <- out[, c('INRE.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$INRE.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$INRE.T1[which(ds$INRE.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(INRE.T2); out <- out[, c('INRE.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$INRE.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$INRE.T2[which(ds$INRE.T2 >= 9)] <- NA

## Identified regulation ####
out <- ds %>% rstatix::identify_outliers(IDRE.T1); out <- out[, c('IDRE.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$IDRE.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$IDRE.T1[which(ds$IDRE.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(IDRE.T2); out <- out[, c('IDRE.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$IDRE.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$IDRE.T2[which(ds$IDRE.T2 >= 9)] <- NA

## Intrinsic motivation ####
out <- ds %>% rstatix::identify_outliers(INTR.T1); out <- out[, c('INTR.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$INTR.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$INTR.T1[which(ds$INTR.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(INTR.T2); out <- out[, c('INTR.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$INTR.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$INTR.T2[which(ds$INTR.T2 >= 9)] <- NA

## Task performance ####
out <- ds %>% rstatix::identify_outliers(TAPE.T1); out <- out[, c('TAPE.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$TAPE.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$TAPE.T1[which(ds$TAPE.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(TAPE.T2); out <- out[, c('TAPE.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$TAPE.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$TAPE.T2[which(ds$TAPE.T2 >= 9)] <- NA

## Contextual performance ####
out <- ds %>% rstatix::identify_outliers(COPE.T1); out <- out[, c('COPE.T1', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$COPE.T1, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$COPE.T1[which(ds$COPE.T1 >= 9)] <- NA

out <- ds %>% rstatix::identify_outliers(COPE.T2); out <- out[, c('COPE.T2', 'is.extreme')]
colnames(out) <- c("Values", "Extreme"); out
#boxplot(ds$COPE.T2, horizontal = T, col = "dark green", outline = T)
#mtext(paste("Outliers: ", paste(out$Values, collapse=", ")), cex=0.6)
#ds$COPE.T2[which(ds$COPE.T2 >= 9)] <- NA

# Univariate statistics and normality ####
univar <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
colnames(univar) <- c(" ", "n", "mean", "sd", "median", "trimmed", "mad", "min", "max", "range",
                      "skew", "kurtosis", "se", "skew.se", "kurtosis.se", "W", "p")

univar <- rbind(univar, getUnivar("Gratitude (T1)", ds$GRAT.T1)); univar <- univar[-1,]
univar <- rbind(univar, getUnivar("Amotivation (T1)", ds$AMOT.T1))
univar <- rbind(univar, getUnivar("Amotivation (T2)", ds$AMOT.T2))
univar <- rbind(univar, getUnivar("Extrinsic regulation (T1)", ds$EXRE.T1))
univar <- rbind(univar, getUnivar("Extrinsic regulation (T2)", ds$EXRE.T2))
univar <- rbind(univar, getUnivar("Introjected regulation (T1)", ds$INRE.T1))
univar <- rbind(univar, getUnivar("Introjected regulation (T2)", ds$INRE.T2))
univar <- rbind(univar, getUnivar("Identified regulation (T1)", ds$IDRE.T1))
univar <- rbind(univar, getUnivar("Identified regulation (T2)", ds$IDRE.T2))
univar <- rbind(univar, getUnivar("Intrinsic motivation (T1)", ds$INTR.T1))
univar <- rbind(univar, getUnivar("Intrinsic motivation (T2)", ds$INTR.T2))
univar <- rbind(univar, getUnivar("Task performance (T1)", ds$TAPE.T1))
univar <- rbind(univar, getUnivar("Task performance (T2)", ds$TAPE.T2))
univar <- rbind(univar, getUnivar("Contextual performance (T1)", ds$COPE.T1))
univar <- rbind(univar, getUnivar("Contextual performance (T2)", ds$COPE.T2))
## Build initial table ####
univar$trimmed <- NULL; univar$mad <- NULL; univar$se <- NULL
rownames(univar) <- NULL; univar <- data.frame(univar); univar <- univar[-1,]
colnames(univar) <- c(" ", "N", "Mean", "SD", "Median", "Min", "Max", "R",
                      "Skew", "Kurt", "SE Skew", "SE Kurt", "Shapiro", "p")

## Prepare table for print ####
print.univar <- univar; rec <- 1
while (rec <= nrow(print.univar)) {
  print.univar$Skew[rec] <- paste(print.univar$Skew[rec], " (", print.univar$`SE Skew`[rec], ")", sep = "")
  print.univar$Kurt[rec] <- paste(print.univar$Kurt[rec], " (", print.univar$`SE Kurt`[rec], ")", sep = "")
  rec <- rec + 1}
print.univar$Shapiro <- NULL; print.univar$p <- NULL;print.univar$R <- NULL
print.univar$`SE Skew` <- NULL; print.univar$`SE Kurt` <- NULL
colnames(print.univar) <- c("Variables", "N", "Mean", "SD", "Median", "Min", "Max","Skew (SE)", "Kurt (SE)")
rownames(print.univar) <- NULL

# Extracting participants' characteristics ####
n <- nrow(ds); N <- 305; n.p <- round(n / N * 100, 2)

## Gender analysis ####
gen <- table(ds$gender); gen
gen.m <- as.numeric(gen[1]); gen.f <- as.numeric(gen[2])
gen.m.p <- round(gen.m / n * 100, 2); gen.f.p <- round(gen.f / n * 100, 2)

## Age analysis ####
age.min <- min(ds$age, na.rm = T); age.max <- max(ds$age, na.rm = T)
age.m <- round(mean(ds$age, na.rm = T), 2); age.sd <- round(sd(ds$age, na.rm = T), 2)

## Tenure analysis ####
ten.min <- min(ds$seniority.job, na.rm = T); ten.max <- max(ds$seniority.job, na.rm = T)
ten.m <- round(mean(ds$seniority.job, na.rm = T), 2); ten.sd <- round(sd(ds$seniority.job, na.rm = T), 2)

# Multivariate normality assumption ####
ds <- ds %>% select(GRAT.T1, AMOT.T1, AMOT.T2, EXRE.T1, EXRE.T2, INRE.T1, INRE.T2, IDRE.T1, IDRE.T2, INTR.T1, INTR.T2,
                    TAPE.T1, TAPE.T2, COPE.T1, COPE.T2)
multivar <- mardia(ds, plot = F); multivar
mahal.min <- round(min(multivar$d), 2)
mahal.max <- round(max(multivar$d), 2)
M.Sk <- round(multivar$b1p, 2)
Sk <- round(multivar$skew, 2); p.Sk <- p(round(multivar$p.skew, 2), digits = 3)
M.K <- round(multivar$b2p, 2)
K <- round(multivar$kurtosis, 2); p.K <- p(round(multivar$p.kurt, 2), digits = 3)

# Correlation matrix ####
colnames(ds) <- c("(1) Gratitude (T1)",
                  "(2) Amotivation (T1)", "(3) Amotivation (T2)",
                  "(4) Extrinsic regulation (T1)", "(5) Extrinsic regulation (T2)",
                  "(6) Introjected regulation (T1)", "(7) Introjected regulation (T2)",
                  "(8) Identified regulation (T1)", "(9) Identified regulation (T2)",
                  "(10) Intrinsic motivation (T1)", "(11) Intrinsic motivation (T2)",
                  "(12) Task performance (T1)", "(13) Task performance (T2)",
                  "(14) Contextual performance (T1)", "(15) Contextual performance (T2)")
head(ds); cor.mat <- rcorr(as.matrix(ds), type = "spearman"); cor.mat
cor.mat$r <- round(cor.mat$r, 3); cor.mat$P <- round(cor.mat$P, 3)
#chart.Correlation(cor.data, histogram = F, method = "spearman")
cor.min <- round(min(cor.mat$r), 2); cor.max <- round(max(cor.mat$r[cor.mat$r != 1]), 2)
cor.det <- det(cor.mat$r); cor.mat$r[upper.tri(cor.mat$r)] <- NA

## Adding alpha on main diagonal
cor.mat$r[1,1] <- crnb[1,2]   # Gratitude (T1)
cor.mat$r[2,2] <- crnb[2,2]   # Amotivation (T1)
cor.mat$r[3,3] <- crnb[2,2]   # Amotivation (T2)
cor.mat$r[4,4] <- crnb[3,2]   # Extrinsic regulation (T1)
cor.mat$r[5,5] <- crnb[3,2]   # Extrinsic regulation (T2)
cor.mat$r[6,6] <- crnb[4,2]   # Introjected regulation (T1)
cor.mat$r[7,7] <- crnb[4,2]   # Introjected regulation (T2)
cor.mat$r[8,8] <- crnb[5,2]   # Identified regulation (T1)
cor.mat$r[9,9] <- crnb[5,2]   # Identified regulation (T2)
cor.mat$r[10,10] <- crnb[6,2] # Intrinsic motivation (T1)
cor.mat$r[11,11] <- crnb[6,2] # Intrinsic motivation (T2)
cor.mat$r[12,12] <- crnb[7,2] # Task performance (T1)
cor.mat$r[13,13] <- crnb[7,2] # Task performance (T2)
cor.mat$r[14,14] <- crnb[8,2] # Contextual performance (T1)
cor.mat$r[15,15] <- crnb[8,2] # Contextual performance (T1)
## Means and Standard deviations
M <- round(psych::describe(ds)$mean, 3); SD <- round(psych::describe(ds)$sd, 3)
cor.mat$r <- rbind(cor.mat$r, M, SD)
rownames(cor.mat$r) <- c(colnames(cor.mat$r), "Means", "Standard deviations"); colnames(cor.mat$r) <- c(1:ncol(ds))

## Show tables in Viewer and removing objects ####
rm(ds, out, rec, getUnivar, multivar)
rownames(univar) <- NULL; univar
expss_output_viewer(); as.etable(univar); expss_output_default()
expss_output_viewer(); as.etable(print.univar); expss_output_default()
expss_output_viewer(); as.etable(cor.mat$r); expss_output_default()
