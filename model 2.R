
library(haven); library(dplyr); library(psych)
t1 <- read_sav("C:/Users/user/Desktop/t1 - t2.sav")
View(t1)

# Computing scoring key ####
key <- list(
  #Trait gratitude
  gqT1 = c('gq1', 'gq2', 'gq3', 'gq4', 'gq5', 'gq6'),
  
  #Work motivation 
  #Amotivation
  amotiv_T1 = c('amotiv1', 'amotiv2', 'amotiv3'),
  amotiv_T2 = c('t2_amotiv1', 't2_amotiv2', 't2_amotiv3'),
  
  # Extrinsic regulation
  reglare_ext_T1 = c('re1', 're2', 're3', 're4', 're5', 're6'),
  reglare_ext_T2 = c('t2_re1', 't2_re2', 't2_re3', 't2_re4', 't2_re5', 't2_re6'),
  
  #Introjected regulation
  introiectie_T1 = c('introiectie1', 'introiectie2', 'introiectie3', 'introiectie4'),
  introiectie_T2 = c('t2_introiectie1', 't2_introiectie2', 't2_introiectie3', 't2_introiectie4'),
  
  #Identified regulation
  identif_T1 = c('identif1', 'identif2', 'identif3'),
  identif_T2 = c('t2_identif1', 't2_identif2', 't2_identif3'),
  
  #Intrinsic motivation
  intrinsec_T1 = c('intrinsec1', 'intrinsec2', 'intrinsec3'),
  intrinsec_T2 = c('t2_intrinsec1', 't2_intrinsec2', 't2_intrinsec3'),
  
  #Task performance
  TP_T1= c('TP2', 'TP4', 'TP6', 'TP8', 'TP10', 'TP12', 'TP14',
           'TP15', 'TP16'),
  TP_T2= c('t2_TP2', 't2_TP4', 't2_TP6', 't2_TP8', 't2_TP10', 't2_TP12', 't2_TP14',
           't2_TP15', 't2_TP16'),
  
  #Contextual performance
  CP_T1= c('CP1', 'CP3', 'CP5', 'CP7', 'CP9', 'CP11', 'CP13'),
  CP_T2= c('t2_CP1', 't2_CP3', 't2_CP5', 't2_CP7', 't2_CP9', 't2_CP11', 't2_CP13')
)


# Internal consistency
# Gratitude
a.GRATIT <- psych::alpha(x = t1 %>% dplyr::select(key$gqT1),
                         cumulative = T, title = "Gratitude"); a.GRATIT 
GRATIT.raw <- round(a.GRATIT$total[2], 2)
GRATIT.CI.lo <- round(a.GRATIT$feldt$lower.ci, 2)
GRATIT.CI.up <- round(a.GRATIT$feldt$upper.ci, 2)

# Amotivation
a.AMOTIV <- psych::alpha(x = t1 %>% dplyr::select(key$amotiv_T1),
                         cumulative = T, title = "Amotivation"); a.AMOTIV 
AMOTIV.raw <- round(a.AMOTIV$total[2], 2)
AMOTIV.CI.lo <- round(a.AMOTIV$feldt$lower.ci, 2)
AMOTIV.CI.up <- round(a.AMOTIV$feldt$upper.ci, 2)

# Extrinsic regulation
a.EXT <- psych::alpha(x = t1 %>% dplyr::select(key$reglare_ext_T1),
                         cumulative = T, title = "Extrinsic regulation"); a.EXT 
EXT.raw <- round(a.EXT$total[2], 2)
EXT.CI.lo <- round(a.EXT$feldt$lower.ci, 2)
EXT.CI.up <- round(a.EXT$feldt$upper.ci, 2)

# Introjected regulation
a.INTRO <- psych::alpha(x = t1 %>% dplyr::select(key$introiectie_T1),
                      cumulative = T, title = "Introjected regulation"); a.INTRO 
INTRO.raw <- round(a.INTRO$total[2], 2)
INTRO.CI.lo <- round(a.INTRO$feldt$lower.ci, 2)
INTRO.CI.up <- round(a.INTRO$feldt$upper.ci, 2)

# Introjected regulation
a.INTRO <- psych::alpha(x = t1 %>% dplyr::select(key$introiectie_T1),
                        cumulative = T, title = "Introjected regulation"); a.INTRO 
INTRO.raw <- round(a.INTRO$total[2], 2)
INTRO.CI.lo <- round(a.INTRO$feldt$lower.ci, 2)
INTRO.CI.up <- round(a.INTRO$feldt$upper.ci, 2)

# Identified regulation
a.IDENT <- psych::alpha(x = t1 %>% dplyr::select(key$identif_T1),
                        cumulative = T, title = "Identified regulation"); a.IDENT 
IDENT.raw <- round(a.IDENT$total[2], 2)
IDENT.CI.lo <- round(a.IDENT$feldt$lower.ci, 2)
IDENT.CI.up <- round(a.IDENT$feldt$upper.ci, 2)

# Intrinsic motivation
a.INTRI <- psych::alpha(x = t1 %>% dplyr::select(key$intrinsec_T1),
                        cumulative = T, title = "Intrinsic motivation"); a.INTRI 
INTRI.raw <- round(a.INTRI$total[2], 2)
INTRI.CI.lo <- round(a.INTRI$feldt$lower.ci, 2)
INTRI.CI.up <- round(a.INTRI$feldt$upper.ci, 2)

# Task performance
a.TASK <- psych::alpha(x = t1 %>% dplyr::select(key$TP_T1),
                        cumulative = T, title = "Task performance"); a.TASK
TASK.raw <- round(a.TASK$total[2], 2)
TASK.CI.lo <- round(a.TASK$feldt$lower.ci, 2)
TASK.CI.up <- round(a.TASK$feldt$upper.ci, 2)

# Contextual performance
a.CONT <- psych::alpha(x = t1 %>% dplyr::select(key$CP_T1),
                       cumulative = T, title = "Contextual performance"); a.CONT
CONT.raw <- round(a.CONT$total[2], 2)
CONT.CI.lo <- round(a.CONT$feldt$lower.ci, 2)
CONT.CI.up <- round(a.CONT$feldt$upper.ci, 2)


# Compute total scores

scores <- scoreItems(keys = key, items = t1, totals = T, delete=FALSE)
t1 <- cbind(t1, scores$scores); head(t1); rm(scores)
View(t1)

library(lavaan)
model.1 <- 'TP_T2 ~ gqT1 + TP_T1 + amotiv_T2 + reglare_ext_T2 + introiectie_T2 
                      + identif_T2 + intrinsec_T2
            CP_T2 ~ gqT1 + CP_T1 + amotiv_T2 + reglare_ext_T2 + introiectie_T2 
                      + identif_T2 + intrinsec_T2
            amotiv_T2 ~ gqT1 + amotiv_T1
            reglare_ext_T2 ~ gqT1 + reglare_ext_T1
            introiectie_T2 ~ gqT1 + introiectie_T1
            identif_T2 ~ gqT1 + identif_T1
            intrinsec_T2 ~ gqT1 + intrinsec_T1
            
       # corr of res
            amotiv_T2 ~~ reglare_ext_T2 + introiectie_T2 + identif_T2 + intrinsec_T2
            reglare_ext_T2 ~~ introiectie_T2 + identif_T2 + intrinsec_T2       
            introiectie_T2 ~~  + identif_T2 + intrinsec_T2  
            identif_T2 ~~ intrinsec_T2  
            
'

M.sem <- sem(model = model.1, data = t1, fixed.x = F, meanstructure = F,
             missing = "listwise", estimator = 'MLM', 
             auto.var = F,  fixed.x = F, check.start = T)
M.1.rez <- summary(M.sem, fit.measures = T, standardized = T, rsquare = T); M.1.rez
