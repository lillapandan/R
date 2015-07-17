library(drgee)

# Set working directory
setwd("/Users/~/")

# Read dataset.csv into R
dat1 <- read.table("ratss.csv", 
                    header = T, 
                    sep = ";", 
                    dec = ",", 
                    na.strings = c("NA", "N/A", "N/AN/A"), 
                    colClass = c(rep("numeric", 38)))

varNames <- c("Pair_nb",
              "Concordance_Risi_ADOS2",
              "RISI_class_ADOS2",
              "SRS.tot.score",
              "IQ_GAI.score")

dat2 <- dat1[dat1$Concordance_Risi_ADOS2 > 0, varNames]

table(dat2$Concordance_Risi_ADOS2)
# 1 = Discordant; 2 = Concordant; 3 = TD (concordant unaffected)

dat2 <- dat2[complete.cases(dat2[,3:4]),]

# Visulize data
aff.con <- dat2[dat2$Concordance_Risi_ADOS2 == 2 & dat2$RISI_class_ADOS2 ==1, ]
aff.dis <- dat2[dat2$Concordance_Risi_ADOS2 == 1 & dat2$RISI_class_ADOS2 ==1, ]
unaff.con <- dat2[dat2$Concordance_Risi_ADOS2 == 3 & dat2$RISI_class_ADOS2 ==0, ]
unaff.dis <- dat2[dat2$Concordance_Risi_ADOS2 == 1 & dat2$RISI_class_ADOS2 ==0, ]

aff.con$group <- "orange2"
aff.dis$group <- "steelblue2"
unaff.con$group <- "orange2"
unaff.dis$group <- "steelblue2"

aff.con$pat <- 19
aff.dis$pat <- 19
unaff.con$pat <- 1
unaff.dis$pat <- 1



fit1 <- lm(SRS.tot.score ~ IQ_GAI.score, data = aff.con)
fit2 <- lm(SRS.tot.score ~ IQ_GAI.score, data = aff.dis)
fit3 <- lm(SRS.tot.score ~ IQ_GAI.score, data = unaff.con)
fit4 <- lm(SRS.tot.score ~ IQ_GAI.score, data = unaff.dis)

loess(SRS.tot.score ~ IQ_GAI.score, aff.con)$fitted

par(mfrow = c(2,2), pch = 1)
plot(aff.con$IQ_GAI.score, aff.con$SRS.tot.score, xlab = "IQ", ylab = "Autism")
abline(fit1, col = "orange2")
lines(lowess(aff.con$IQ_GAI.score, aff.con$SRS.tot.score), col = "steelblue2")
title("Aff.con")

plot(aff.dis$IQ_GAI.score, aff.dis$SRS.tot.score, xlab = "IQ", ylab = "Autism")
abline(fit2,col = "orange2")
lines(lowess(aff.dis$IQ_GAI.score, aff.dis$SRS.tot.score), col = "steelblue2")
title("Aff.dis")

plot(unaff.con$IQ_GAI.score, unaff.con$SRS.tot.score, xlab = "IQ", ylab = "Autism")
abline(fit3, col = "orange2")
lines(lowess(unaff.con$IQ_GAI.score, unaff.con$SRS.tot.score), col = "steelblue2")
title("Unaff.con")

plot(unaff.dis$IQ_GAI.score, unaff.dis$SRS.tot.score, xlab = "IQ", ylab = "Autism")
abline(fit4, col = "orange2")
lines(lowess(unaff.dis$IQ_GAI.score, unaff.dis$SRS.tot.score), col = "steelblue2")
title("Unaff.dis")

par(mfrow = c(1,1))

dat <- rbind(aff.con, aff.dis, unaff.con, unaff.dis)
plot(dat$IQ_GAI.score, dat$SRS.tot.score, col = dat$group, pch = dat$pat, xlab = "IQ", ylab = "Autism")
abline(fit1, col = "orange2")
abline(fit2, col = "steelblue2")
abline(fit3, col = "orange2", lty = 2)
abline(fit4, col = "steelblue2", lty = 2)



# GEE
# Linear regression with robust standard error
fit.con <- gee(SRS.tot.score ~ IQ_GAI.score, data = dat2[dat2$Concordance_Risi_ADOS2 == 2, ], clusterid="Pair_nb")
fit.dis <- gee(SRS.tot.score ~ IQ_GAI.score, data = dat2[dat2$Concordance_Risi_ADOS2 == 1, ], clusterid="Pair_nb")
fit.td <- gee(SRS.tot.score ~ IQ_GAI.score, data = dat2[dat2$Concordance_Risi_ADOS2 == 3, ], clusterid="Pair_nb")


# Conditional linear regression
cond.fit.con <- gee(SRS.tot.score ~ IQ_GAI.score, data = dat2[dat2$Concordance_Risi_ADOS2 == 2, ], clusterid="Pair_nb", cond = TRUE)
cond.fit.dis <- gee(SRS.tot.score ~ IQ_GAI.score, data = dat2[dat2$Concordance_Risi_ADOS2 == 1, ], clusterid="Pair_nb", cond = TRUE)
cond.fit.td <- gee(SRS.tot.score ~ IQ_GAI.score, data = dat2[dat2$Concordance_Risi_ADOS2 == 3, ], clusterid="Pair_nb", cond = TRUE)



CI <- function(x){
  result <- cbind(coef(x)[2], confint(x)[2,1], confint(x)[2,2], summary(x)$coef[2,4])
  colnames(result) <- c("Estimate", "Lower", "Upper", "p-value")
  round(result, 3)
}

cond.CI <- function(x){
  result <- cbind(coef(x), confint(x)[1], confint(x)[2], summary(x)$coef[4])
  colnames(result) <- c("Estimate", "Lower", "Upper", "p-value")
  round(result, 3)
}

# Example for how to use CI and cond.CI 
CI(fit.con)
cond.CI(cond.fit.con)

fit.list <- list(fit.con, fit.dis, fit.td)
cond.fit.list <- list(cond.fit.con, cond.fit.dis, cond.fit.td)


results <- lapply(fit.list, CI)	
cond.results <- lapply(cond.fit.list, cond.CI)	

output <- rbind(matrix(unlist(results), nrow = 3, byrow = T),
                matrix(unlist(cond.results), nrow = 3, byrow = T))







