if (!require(semTools)) {install.packages("semTools")}
library(semTools)

findRMSEApower(rmsea0 = 0.05, rmseaA = 0.01, df = 39, n = 246, alpha = 0.05)
100 - findRMSEApower(rmsea0 = 0.08, rmseaA = 0.01, df = 42, n = 246, alpha = 0.05) * 100
# La un prag al semnificației statistice mai mic de 0,05, există 67,75% șanse pentru a găsi, în populație,
# un model cu 42 de grade de libertate care să se potrivească la fel de bine cu cel analizat sub ipoteza de saturare
# RMSEA = .05 și 0,08% șanse ca  în condiții ideale de replicare, modelul să nu se potrivească la nivelul populației
# (RMSEA > .08)

findRMSEAsamplesize(rmsea0 = 0.05, rmseaA = 0.01, df = 42, power = 0.80, alpha = 0.05)
# Pentru ca o replicare, în absolut aceleași condiții, să conducă la 80% șanse de a se găsi același model în populație,
# va fi nevoie de un eșantion format din circa 298 de persoane (RMSEA = .05)

