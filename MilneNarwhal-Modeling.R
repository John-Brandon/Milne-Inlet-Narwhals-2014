###=== === +++ === === +++ === === +++ === ===
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Fit models to relative abundance and distribution (RAD) data 
#           Milne Inlet LGL / Baffinland shore based (Bruce Head) narwhal counts
#
#  Notes : 
#  1) Your main directory will differ. 
#  2) 
#====== +++ === === +++ === === +++ === ===
library(vcd)
library(AER)
library(pscl)
library("glmmADMB")
# load data
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") 

head(counts.by.stratum.keepers) # data matrix
dat = counts.by.stratum.keepers # re-assign to "dat" (shorter and easier name to type)
nrow(dat) # number of counts

sum(dat$TotalCount.with.na - dat$TotalCount.without.na, na.rm = TRUE)
length(which(is.na(dat$TotalCount.with.na))) # check
length(which(is.na(dat$TotalCount.without.na)))


?goodfit
fit1 <- goodfit(dat$value) 
summary(fit1) 
rootogram(fit1) # apparently the same as plot(fit)

fit2 <- goodfit(dat$value, type = "nbinomial", method = "MinChisq") 
str(fit2)
summary(fit2) 
rootogram(fit2) # apparently the same as plot(fit)

fit3 <- goodfit(dat$value, type = "nbinomial", method = "ML") 
summary(fit3) 
rootogram(fit3) # apparently the same as plot(fit)

# try real world data
data("HorseKicks")
str(HorseKicks)
head(HorseKicks)
?HorseKicks

HK.fit <- goodfit(HorseKicks)
summary(HK.fit)
plot(HK.fit)

data("Federalist")
## try geometric and full negative binomial distribution
F.fit <- goodfit(Federalist, type = "nbinomial", par = list(size = 1))
F.fit2 <- goodfit(Federalist, type = "nbinomial")
summary(F.fit)
summary(F.fit2)
plot(F.fit)
plot(F.fit2)

dummy <- rnbinom(200, size = 1.5, prob = 0.8)
gf <- goodfit(dummy, type = "nbinomial", method = "MinChisq")
summary(gf)
plot(gf)
dummy <- rbinom(100, size = 6, prob = 0.5)
gf1 <- goodfit(dummy, type = "binomial", par = list(size = 6))
gf2 <- goodfit(dummy, type = "binomial", par = list(prob = 0.6, size = 6))
summary(gf1)
plot(gf1)
summary(gf2)
plot(gf2)

library(vcd)
data(quine) 
nrow(quine)
?goodfit
fit <- goodfit(quine$Days) 
summary(fit) 
rootogram(fit) # apparently the same as plot(fit)
hist(quine$Days)
Ord_plot(quine$Days) # here the slope is positive and intercept is positive which speaks to neg.binom
distplot(quine$Days, type="poisson")
distplot(quine$Days, type="nbinom")

mod1 <- glm(Days~Age+Sex, data=quine, family="poisson")
summary(mod1)
anova(mod1, test="Chisq")

library(AER)
deviance(mod1)/mod1$df.residual
dispersiontest(mod1)

library(pscl)
mod2 <- zeroinfl(Days~Age+Sex, data=quine, dist="poisson")
AIC(mod1, mod2)

res <- residuals(mod1, type="deviance")
plot(log(predict(mod1)), res)
abline(h=0, lty=2)
?abline
qqnorm(res)
qqline(res)

install.packages("faraway");library(faraway)
halfnorm(residuals(mod1))

plot(Days~Age, data=quine) 
prs  <- predict(mod1, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ quine$Age, col="red")
points(pris$lwr  ~ quine$Age, col="pink", pch=19)
points(pris$upr  ~ quine$Age, col="pink", pch=19)
