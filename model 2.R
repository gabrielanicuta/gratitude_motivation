


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
