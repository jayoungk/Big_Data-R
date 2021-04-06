rm(list=ls())
setwd("C:/Users/13124/Desktop/Harris/2020-2 Spring/Big Data/week 8/")
fx <- read.csv("FXmonthly.csv")
fx <- (fx[2:120,]-fx[1:119,])/(fx[1:119,])


## [1] Discuss correlation amongst dimensions of fx.
## How does this relate to the applicability of factor modelling?
# since there is correlation, factor analysis could be worthwhile
dim(fx)  
par(mfrow=c(2,2))
pairs(fx[1:6])
pairs(fx[7:12])
pairs(fx[13:18])
pairs(fx[19:23])


## [2] Fit, plot, and interpret principal components.
par(mfrow=c(1,1))
(pc <- prcomp(fx, scale=TRUE)) #rotations aka what "defines" the PCs?
plot(pc, main="")
(pc2 <- prcomp(fx, scale=TRUE, rank = 2))
(z <- predict(pc))
round(pc$rotation[,1:3],1) 
summary(pc)


# t( round(pc$rotation[,1:2],2) )
# t( round(pc2$rotation[,1:2],2) )

## do some k-means, for comparison
# grp <- kmeans(scale(fx), centers=7, nstart=20)
# par(mfrow=c(1,2))
# plot(z[,1:2], type="n", xlim=c(-4,5))
# text(x=z[,1], y=z[,2],labels=rownames(fx), col=rainbow(7)[grp$cluster])
# plot(z[,3:4], type="n", xlim=c(-3,3))
# text(x=z[,3], y=z[,4], labels=rownames(fx), col=rainbow(7)[grp$cluster])

z <- predict(pc)


plot(z[,1:2], col=0, # col=0 to get an empy plot
     ylim=c(-3,3), xlim=c(-3,3), # hides "monarch cove",living with ed", and "next" but these are all tiny 
     ) 

text(z[,1:2], labels=rownames(z))




## [3] Regress SP500 returns onto currency movement factors, 
## using both 'glm on first K' and lasso techniques.
## Use the results to add to your factor interpretation.

sp<-read.csv("sp500.csv")
library(gamlr)

sp500 <- sp$sp500

zfx <- predict(pc)

zdf <- as.data.frame(zfx)

kfits <- lapply(1:23, #add one Pc each time
                function(K) glm(sp500~., data=zdf[,1:K,drop=FALSE]))

(aicc <- sapply(kfits, AICc)) # apply AICc to each fit
which.min(aicc) ## it likes 3 factors best
(bic <- sapply(kfits, BIC)) # apply BIC to each fit
which.min(bic) ## it likes 3 factors best


lassoPCR <- cv.gamlr(x=zfx, y=sp500, nfold=23)

## lasso.1se agrees with IC on first 3, then grabs a couple extra
coef(lassoPCR) 

## plot
par(mfrow=c(1,2))
plot(aicc, pch=21, bg="maroon", xlab="K", ylab="AICc")
plot(bic, pch=21, bg="maroon", xlab="K", ylab="BIC")
plot(lassoPCR) 

round(pc$rotation[,15],1) 
round(pc$rotation[,17],1)
round(pc$rotation[,20],1) 
round(pc$rotation[,21],1) 
round(pc$rotation[,23],1) 


## [4] Fit lasso to the original covariates and describe 
## how it differs from PCR here.
lasso <- cv.gamlr(x=as.matrix(fx), y=sp500, nfold=23)
par(mfrow=c(1,2))
plot(lasso, main="Regression on Original Covariates")
plot(lassoPCR, main="PCR")
## since you haven't simplified into linear factors 
## the estimation variance overwhelms any signal


