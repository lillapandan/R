# Install packages "xlsx" and "drgee"

# Set working directory
setwd("~/")

# Read dataset.xlsx into R
library(xlsx)
dat1 <- read.xlsx2("dataset.xlsx", 1, colClass = c(rep("numeric", 34)))

str(dat1)
dim(dat1)

dat2 <- dat1[dat1$Concordance_Risi_ADOS2 > 0,]
dim(dat2)

# Gee model with robust standard error
# Covert categorical variable into factor, if any

library(drgee)
fit1 <- gee(outcome ~ exposure, data=dat2, clusterid="clusterID")
summary(fit1)