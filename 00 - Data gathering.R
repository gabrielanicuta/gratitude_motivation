# Installing and loading libraries
if(!require(foreign)) install.packages("foreign")
library(foreign)

# Importing dataset ####
ds <- read.spss(file = "t1 - t2.sav", to.data.frame = T, use.value.labels = T); ds <- ds[, 1:113]
colnames(ds) <- c("name", names(ds[2:52]),
  "age", "gender", "education", "seniority.tot", "seniority.job", "profession",
  "employer.type", "job.type", "management", "job.form", names(ds[63:113]))
# Setting-up factors
ds$gender <- factor(ds$gender, labels = c("Male", "Female"))
ds$education <- ordered(ds$education, labels = c("Gymnasium or less", "Arts and crafts", "High school", "Post-secondary", "Bachelor", "Master", "PhD"))
ds$seniority.tot <- as.numeric(gsub("([0-9]+).*$", "\\1", ds$seniority.tot))
ds$seniority.job <- as.numeric(gsub("([0-9]+).*$", "\\1", ds$seniority.job))
ds$employer.type <- factor(ds$employer.type, labels = c("Budgetary", "Private"))
ds$job.type <- factor(ds$job.type, labels = c("Full time", "Part time"))
ds$management <- factor(ds$management, labels = c("Yes", "No"))
ds$job.form <- factor(ds$job.form, labels = c("From home (Remote)", "Hybrid", "At the office"))

# Saving dataset
save(ds, file = "Raw data.RData")

