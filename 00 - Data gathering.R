# Installing and loading libraries
if(!require(foreign)) install.packages("foreign")
library(foreign)

# Importing dataset ####
ds <- read.spss(file = "t1 - t2.sav", to.data.frame = T, use.value.labels = T); head(ds)


# Saving dataset
save(ds, file = "Raw data.RData")
