# Installing and loading libraries
if(!require(dplyr)) install.packages("dplyr")
if(!require(dplyr)) install.packages("dplyr")
library(dplyr); library(psych)
p <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', round(x, 3))}
getRezAlpha <- function(a) {
  tmp <- c(a$title,
           round(as.numeric(a$total[1]), 2),
           round(as.numeric(a$feldt$lower.ci), 2),
           round(as.numeric(a$feldt$upper.ci), 2))
  return(tmp)}

# Loading dataset and computing scoring key ####
load(file = "Raw data.RData")
key <- list(
  # Trait gratitude ####
  GRAT.T1 = c('gq1', 'gq2', 'gq3', 'gq4', 'gq5', 'gq6'),

  # Work motivation ####
  ## Amotivation ####
  AMOT.T1 = c('amotiv1', 'amotiv2', 'amotiv3'),
  AMOT.T2 = c('t2_amotiv1', 't2_amotiv2', 't2_amotiv3'),

  ## Extrinsic regulation ####
  EXRE.T1 = c('re1', 're2', 're3', 're4', 're5', 're6'),
  EXRE.T2 = c('t2_re1', 't2_re2', 't2_re3', 't2_re4', 't2_re5', 't2_re6'),

  ## Introjected regulation ####
  INRE.T1 = c('introiectie1', 'introiectie2', 'introiectie3', 'introiectie4'),
  INRE.T2 = c('t2_introiectie1', 't2_introiectie2', 't2_introiectie3', 't2_introiectie4'),

  ## Identified regulation ####
  IDRE.T1 = c('identif1', 'identif2', 'identif3'),
  IDRE.T2 = c('t2_identif1', 't2_identif2', 't2_identif3'),

  ## Intrinsic motivation ####
  INTR.T1 = c('intrinsec1', 'intrinsec2', 'intrinsec3'),
  INTR.T2 = c('t2_intrinsec1', 't2_intrinsec2', 't2_intrinsec3'),

  # Task performance ####
  TAPE.T1= c('TP2', 'TP4', 'TP6', 'TP8', 'TP10', 'TP12', 'TP14',
           'TP15', 'TP16'),
  TAPE.T2= c('t2_TP2', 't2_TP4', 't2_TP6', 't2_TP8', 't2_TP10', 't2_TP12', 't2_TP14',
           't2_TP15', 't2_TP16'),

  # Contextual performance ####
  COPE.T1= c('CP1', 'CP3', 'CP5', 'CP7', 'CP9', 'CP11', 'CP13'),
  COPE.T2= c('t2_CP1', 't2_CP3', 't2_CP5', 't2_CP7', 't2_CP9', 't2_CP11', 't2_CP13')
)

# Internal consistency ####
crnb <- data.frame(NA, NA, NA, NA)
colnames(crnb) <- c(" ", "Cronbac's alpha", "95% CI Lower", "95% CI Upper")
## Gratitude ####
a <- psych::alpha(x = ds %>% dplyr::select(key$GRAT.T1),
                  cumulative = T, title = "Gratitude", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Amotivation ####
a <- psych::alpha(x = ds %>% dplyr::select(key$AMOT.T1),
                  cumulative = T, title = "Amotivation", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Extrinsic regulatioN ####
a <- psych::alpha(x = ds %>% dplyr::select(key$EXRE.T1),
                  cumulative = T, title = "Extrinsic regulation", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Introjected regulation ####
a <- psych::alpha(x = ds %>% dplyr::select(key$INRE.T1),
                  cumulative = T, title = "Introjected regulation", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Identified regulation ####
a <- psych::alpha(x = ds %>% dplyr::select(key$IDRE.T1),
                  cumulative = T, title = "Identified regulation", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Intrinsic motivation ####
a <- psych::alpha(x = ds %>% dplyr::select(key$INTR.T1),
                  cumulative = T, title = "Intrinsic motivation", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Task performance ####
a <- psych::alpha(x = ds %>% dplyr::select(key$TAPE.T1),
                  cumulative = T, title = "Task performance", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Contextual performance ####
a <- psych::alpha(x = ds %>% dplyr::select(key$COPE.T1),
                  cumulative = T, title = "Contextual performance", check.keys = T); a
crnb <- rbind(crnb, getRezAlpha(a))

## Removing first row and set row names to null ####
crnb <- crnb[-1,]; rownames(crnb) <- NULL


# Compute total scores and save final dataset ####
scores <- psych::scoreItems(keys = key, items = ds, totals = T, missing = T, impute = "median")
ds <- cbind(ds, scores$scores); head(ds)
save(ds, file = "Totals.RData")
