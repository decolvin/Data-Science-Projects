#Clear memory and load libraries
rm(list = ls())
memory.size(max=T)
library(doParallel)
library(foreach)
library(psych)
library(forecast)
library(Hmisc)
library(MASS)
library(pROC)
library(class)
library(randomForest)
library(gbm)
library(e1071)

#Set up computer to run fast
c1=makeCluster(4) #my CPU has 5 cores, only dedicating 4 to RStudio
registerDoParallel(c1)

#test
foreach(i=1:4) %dopar% i^100

setwd("E:/School/Northwestern/PREDICT 422/Group Project") #change working directory to where data saved

# load the data
gp.data <- read.csv(file="E:/School/Northwestern/PREDICT 422/Group Project/charity.csv", header=TRUE, sep=",")

#########################
#########################
########## EDA ##########
#########################
#########################
describe(gp.data)       #look at data
str(gp.data)            #look at data

####################
### Correlations ###
####################
library(corrplot)
IndVars <- gp.data[,2:21]
nums <- sapply(IndVars, is.numeric)
res <- cor(IndVars[,nums])
corrplot(res, type = "upper", tl.col = "black", tl.cex = 1)

cor(gp.data$avhv, gp.data$incm) # AVHV and INCM = 0.7261524
cor(gp.data$avhv, gp.data$inca) # AVHV and INCA = 0.8401095
cor(gp.data$avhv, gp.data$plow) # AVHV and PLOW = -0.6308818
cor(gp.data$incm, gp.data$inca) # INCM and INCA = 0.8700841
cor(gp.data$incm, gp.data$plow) # INCM and PLOW = -0.6552348
cor(gp.data$inca, gp.data$plow) # INCA and PLOW = -0.6379051
cor(gp.data$npro, gp.data$tgif) # NPRO and TGIF = 0.7058844
cor(gp.data$lgif, gp.data$rgif) # LGIF and RGIF = 0.6972845
cor(gp.data$lgif, gp.data$agif) # LGIF and AGIF = 0.6246859
cor(gp.data$rgif, gp.data$agif) # RGIF and AGIF = 0.7107389

################################
### Histograms for variables ### 
################################
numericVars <- IndVars[,nums]
str(numericVars)
par(mfrow=c(5, 4))                                        #Set number of panels
par(mar=c(2,2,2,2))                                       #Make margins smaller
colnames <- dimnames(numericVars)[[2]]
for (i in 1:20) {
  hist(numericVars[,i], main=colnames[i], col="blue", border="white")
}

par(mfrow=c(1, 1))

###################################
########## Data Cleaning ##########
###################################
#gp.data[,c("ID")] <- NULL #Remove unecessary variable

##HOME
hist(gp.data$home)                          #one inflated, definition implies categorical
#gp.data$home <- as.factor(gp.data$home)
##CHLD
hist(gp.data$chld)                          #zero inflated, definition implies categorical
#gp.data$chld <- as.factor(gp.data$chld)
##HINC
hist(gp.data$hinc)                          #no obvious issues, definition states categorical
#gp.data$hinc <- as.factor(gp.data$hinc)
##GENF
hist(gp.data$genf)                          #no obvious issues, definition implies categorical
#gp.data$genf <- as.factor(gp.data$genf)
##WRAT
hist(gp.data$wrat)                          #left skewed, definition implies categorical
#gp.data$wrat <- as.factor(gp.data$wrat)


##AVHV
hist(gp.data$avhv)                          #right skewed
gp.data$avhv.trans <- log(gp.data$avhv)     #log transform appears better than sqrt and BoxCox
#gp.data[,c("avhv")] <- NULL                 #Remove unecessary variable
##INCM
hist(gp.data$incm)                          #right skewed
gp.data$incm.trans <- log(gp.data$incm)     #log transform appears better than sqrt and BoxCox
#gp.data[,c("incm")] <- NULL                 #Remove unecessary variable
##INCA
hist(gp.data$inca)                          #right skewed
gp.data$inca.trans <- log(gp.data$inca)     #log transform appears better than sqrt and BoxCox
#gp.data[,c("inca")] <- NULL                 #Remove unecessary variable
##PLOW
hist(gp.data$plow)                          #zero inflated, right skewed
gp.data$plow.trans <- sqrt(gp.data$plow)    #sqrt appears better than log and BoxCox
#gp.data[,c("plow")] <- NULL                 #Remove unecessary variable
##NPRO
hist(gp.data$npro)                          #slightly right skewed
gp.data$npro.trans <- sqrt(gp.data$npro)    #sqrt appears better than log and BoxCox
#gp.data[,c("npro")] <- NULL                 #Remove unecessary variable
##tgif
hist(gp.data$tgif)                                         #right skewed
lambda.tgif <- BoxCox.lambda(gp.data$tgif)
gp.data$tgif.trans <- (BoxCox(gp.data$tgif, lambda.tgif))  #BoxCox power appears better than log or sqrt
#gp.data[,c("tgif")] <- NULL                                #Remove old variable
##lgif
hist(gp.data$lgif)                          #right skewed  
gp.data$lgif.trans <- log(gp.data$lgif)     #log transform appears better than sqrt or BoxCox
#gp.data[,c("lgif")] <- NULL                 #Remove unecessary variable
##rgif
hist(gp.data$rgif)                          #right skewed     
gp.data$rgif.trans <- log(gp.data$rgif)     #log transform appears better than sqrt or BoxCox
#gp.data[,c("rgif")] <- NULL                 #Remove unecessary variable
##tdon
hist(gp.data$tdon)                          #slightly right skewed  
gp.data$tdon.trans <- sqrt(gp.data$tdon)    #sqrt appears better than log and BoxCox
#gp.data[,c("tdon")] <- NULL                 #Remove unecessary variable
##tlag
hist(gp.data$tlag)                          #right skewed, no obvious transform  
##agif
hist(gp.data$agif)                          #right skewed
gp.data$agif.trans <- log(gp.data$agif)     #log transform appears better than sqrt or BoxCox
#gp.data[,c("agif")] <- NULL                 #Remove unecessary variable

### Transform Histogram code ###
#lambda <- BoxCox.lambda(y.valid)
#hist(BoxCox(y.valid, lambda))
#hist(log(y.valid))
#hist(sqrt(y.valid))
#hist(y.valid, breaks=18, main="Histogram of DAMT")
#describe(y.valid)

#############################################
####### Data Preparetion for Analysis #######
#############################################

data.train <- gp.data[gp.data$part=="train",]
x.train <- data.train[,-c(1,22,23,24)]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- gp.data[gp.data$part=="valid",]
x.valid <- data.valid[,-c(1,22,23,24)]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- gp.data[gp.data$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,-c(1,22,23,24)]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

data.train.c <- data.frame(x.train, donr=c.train) # to classify donr
data.train.y <- data.frame(x.train[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

#############################################################
#############################################################
################### CLASSIFICATION MODELING #################
#############################################################
#############################################################

#############################################################
################## Logistic Regression ######################
#############################################################

###Logistic Regression, no predictor transforms & no nonlinear terms
glm.logistic.simple <- glm(donr ~ reg1 + reg2 + home + chld + wrat + incm + plow + npro + tgif + tdon + tlag, data=data.train.c, family=binomial)
summary(glm.logistic.simple)
#Non-significant variables : inca, reg4, lgif, rgif, avhv, genf, reg3, hinc, agif
glm.log.simple.prob <- predict(glm.logistic.simple, data.valid, type="response")
roc(data.valid$donr, glm.log.simple.prob)$auc # AUC=0.9137 on validation set
glm.pred1=rep(0,2018)
glm.pred1[glm.log.simple.prob>0.5]=1
table(glm.pred1,data.valid$donr)   #confusion matrix
mean(glm.pred1==data.valid$donr)   #accuracy rate=0.8379584
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log1 <- cumsum(14.5*c.valid[order(glm.log.simple.prob, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid1 <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid1, max(profit.log1))      # report number of mailings, 1387
                                        # report maximum profit, $11407

###Logistic Regression, with predictor transforms & no nonlinear terms
glm.logistic.simple.trans <- glm(donr ~ reg1 + reg2 + home + chld + wrat + incm.trans + tgif.trans + tdon.trans + tlag, data=data.train.c, family=binomial)
summary(glm.logistic.simple.trans)
#Non-significant variables : reg4, reg3, plow.trans, rgif.trans, inca.trans, npro.trans, genf, agif.trans, avhv.trans, lgif.trans, hinc
glm.log.simple.trans.prob <- predict(glm.logistic.simple.trans, data.valid, type="response")
roc(data.valid$donr, glm.log.simple.trans.prob)$auc # AUC=0.9126 on validation set
glm.pred2=rep(0,2018)
glm.pred2[glm.log.simple.trans.prob>0.5]=1
table(glm.pred2,data.valid$donr)   #confusion matrix
mean(glm.pred2==data.valid$donr)   #accuracy rate=0.8389495
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log2 <- cumsum(14.5*c.valid[order(glm.log.simple.trans.prob, decreasing=T)]-2)
plot(profit.log2) # see how profits change as more mailings are made
n.mail.valid2 <- which.max(profit.log2) # number of mailings that maximizes profits
c(n.mail.valid2, max(profit.log2))      # report number of mailings, 1372
                                        # report maximum profit, $11364.5

###Logistic Regression, no predictor transforms & with nonlinear terms
glm.logistic.poly <- glm(donr ~ reg1 + reg2 + home + chld + wrat + incm + plow + npro + tgif + I(tgif^2) + tdon + I(tdon^2) + tlag, data=data.train.c, family=binomial)
summary(glm.logistic.poly)
#Non-significant variables : reg4, reg3, I(lgif^2), genf, I(avhv^2), avhv, I(npro^2), I(plow^2), lgif, I(rgif^2), rgif, I(inca^2), inca, I(incm^2), hinc, agif, I(agif^2), I(tlag^2)
glm.log.poly.prob <- predict(glm.logistic.poly, data.valid, type="response")
roc(data.valid$donr, glm.log.poly.prob)$auc # AUC=0.9212 on validation set
glm.pred3=rep(0,2018)
glm.pred3[glm.log.poly.prob>0.5]=1
table(glm.pred3,data.valid$donr)   #confusion matrix
mean(glm.pred3==data.valid$donr)   #accuracy rate=0.8434093
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log3 <- cumsum(14.5*c.valid[order(glm.log.poly.prob, decreasing=T)]-2)
plot(profit.log3) # see how profits change as more mailings are made
n.mail.valid3 <- which.max(profit.log3) # number of mailings that maximizes profits
c(n.mail.valid3, max(profit.log3))      # report number of mailings, 1349
                                        # report maximum profit, $11425

###Logistic Regression, with predictor transforms & with nonlinear terms
glm.logistic.poly.trans <- glm(donr ~ reg1 + reg2 + home + chld + wrat + incm.trans + tgif.trans + tdon.trans + I(tdon.trans^2) + tlag, data=data.train.c, family=binomial)
summary(glm.logistic.poly.trans)
#Non-significant variables : I(agif.trans ^2), I(lgif.trans^2), I(npro.trans^2), I(plow.trans^2), agif.trans, plow.trans, I(incm.trans^2), I(rgif.trans^2), I(tgif.trans^2), rgif.trans, lgif.trans, reg3, reg4, genf, I(inca.trans^2), npro.trans, I(avhv.trans^2), inca.trans , avhv.trans, hinc, I(tlag^2)
glm.log.poly.trans.prob <- predict(glm.logistic.poly.trans, data.valid, type="response")
roc(data.valid$donr, glm.log.poly.trans.prob)$auc #AUC=0.9223 on validation set
glm.pred4=rep(0,2018)
glm.pred4[glm.log.poly.trans.prob>0.5]=1
table(glm.pred4,data.valid$donr)   #confusion matrix
mean(glm.pred4==data.valid$donr)   #accuracy rate=0.8439049
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log4 <- cumsum(14.5*c.valid[order(glm.log.poly.trans.prob, decreasing=T)]-2)
plot(profit.log4) # see how profits change as more mailings are made
n.mail.valid4 <- which.max(profit.log4) # number of mailings that maximizes profits
c(n.mail.valid4, max(profit.log4))      # report number of mailings, 1385.0
                                        # report maximum profit, $11425.5

#############################################################
############# Linear Discriminant Analysis ##################
#############################################################

###LDA, no predictor transforms & no nonlinear terms
lda.fit1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c)
lda.pred1 <- predict(lda.fit1,data.valid)
lda.class1 <- lda.pred1$class
roc(data.valid$donr, lda.pred1$posterior[,2])$auc #AUC=0.9138 on validation set
table(lda.class1,data.valid$donr)                 #confusion matrix
mean(lda.class1==data.valid$donr)                 #accuracy rate=0.8399405
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda1 <- cumsum(14.5*c.valid[order(lda.pred1$posterior[,2], decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid.lda1 <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid.lda1, max(profit.lda1))      # report number of mailings, 1472.0
                                            # report maximum profit, $11367.5

###LDA, with predictor transforms & no nonlinear terms
lda.fit2 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv.trans + incm.trans + inca.trans + plow.trans + npro.trans + tgif.trans + lgif.trans + rgif.trans + tdon.trans + tlag + agif.trans, data=data.train.c)
lda.pred2 <- predict(lda.fit2,data.valid)
lda.class2 <- lda.pred2$class
roc(data.valid$donr, lda.pred2$posterior[,1])$auc #AUC=0.9135 on validation set
table(lda.class2,data.valid$donr)                 #confusion matrix
mean(lda.class2==data.valid$donr)                 #accuracy rate=0.8354807
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda2 <- cumsum(14.5*c.valid[order(lda.pred2$posterior[,2], decreasing=T)]-2)
plot(profit.lda2) # see how profits change as more mailings are made
n.mail.valid.lda2 <- which.max(profit.lda2) # number of mailings that maximizes profits
c(n.mail.valid.lda2, max(profit.lda2))      # report number of mailings, 1383
                                            # report maximum profit, $11357

###LDA, no predictor transforms & with nonlinear terms
lda.fit3 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + I(avhv^2) + incm + I(incm^2) + inca + I(inca^2) + plow + I(plow^2) + npro + I(npro^2) + tgif + I(tgif^2) + lgif + I(lgif^2) + rgif + I(rgif^2) + tdon + I(tdon^2) + tlag + I(tlag ^2) + agif + I(agif ^2), data=data.train.c)
lda.pred3 <- predict(lda.fit3,data.valid)
lda.class3 <- lda.pred3$class
roc(data.valid$donr, lda.pred3$posterior[,1])$auc #AUC=0.9215 on validation set
table(lda.class3,data.valid$donr)                 #confusion matrix
mean(lda.class3==data.valid$donr)                 #accuracy rate=0.8419227
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda3 <- cumsum(14.5*c.valid[order(lda.pred3$posterior[,2], decreasing=T)]-2)
plot(profit.lda3) # see how profits change as more mailings are made
n.mail.valid.lda3 <- which.max(profit.lda3) # number of mailings that maximizes profits
c(n.mail.valid.lda3, max(profit.lda3))      # report number of mailings, 1326.0
                                            # report maximum profit, $11369.5

###LDA, with predictor transforms & with nonlinear terms
lda.fit4 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv.trans + I(avhv.trans^2) + incm.trans + I(incm.trans^2) + inca.trans + I(inca.trans^2) + plow.trans + I(plow.trans^2) + npro.trans + I(npro.trans^2) + tgif.trans + I(tgif.trans^2) + lgif.trans + I(lgif.trans^2) + rgif.trans + I(rgif.trans^2) + tdon.trans + I(tdon.trans^2) + tlag + I(tlag ^2) + agif.trans + I(agif.trans ^2), data=data.train.c)
lda.pred4 <- predict(lda.fit4,data.valid)
lda.class4 <- lda.pred4$class
roc(data.valid$donr, lda.pred4$posterior[,1])$auc #AUC=0.923 on validation set
table(lda.class4,data.valid$donr)                 #confusion matrix
mean(lda.class4==data.valid$donr)                 #accuracy rate=0.8448959
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda4 <- cumsum(14.5*c.valid[order(lda.pred4$posterior[,2], decreasing=T)]-2)
plot(profit.lda4) # see how profits change as more mailings are made
n.mail.valid.lda4 <- which.max(profit.lda4) # number of mailings that maximizes profits
c(n.mail.valid.lda4, max(profit.lda4))      # report number of mailings, 1368.0
                                            # report maximum profit, $11401.5

#############################################################
############ Quadratic Discriminant Analysis ################
#############################################################

###QDA, no predictor transforms & no nonlinear terms
qda.fit1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c)
qda.pred1 <- predict(qda.fit1,data.valid)
qda.class1 <- qda.pred1$class
roc(data.valid$donr, qda.pred1$posterior[,1])$auc #AUC=0.9048 on validation set
table(qda.class1,data.valid$donr)                 #confusion matrix
mean(qda.class1==data.valid$donr)                 #accuracy rate=0.8349851
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.qda1 <- cumsum(14.5*c.valid[order(qda.pred1$posterior[,2], decreasing=T)]-2)
plot(profit.qda1) # see how profits change as more mailings are made
n.mail.valid.qda1 <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid.qda1, max(profit.qda1))      # report number of mailings, 1399
                                            # report maximum profit, $11238

###QDA, with predictor transforms & no nonlinear terms
qda.fit2 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv.trans + incm.trans + inca.trans + plow.trans + npro.trans + tgif.trans + lgif.trans + rgif.trans + tdon.trans + tlag + agif.trans, data=data.train.c)
qda.pred2 <- predict(qda.fit2,data.valid)
qda.class2 <- qda.pred2$class
roc(data.valid$donr, qda.pred2$posterior[,1])$auc #AUC=0.9128 on validation set
table(qda.class2,data.valid$donr)                 #confusion matrix
mean(qda.class2==data.valid$donr)                 #accuracy rate=0.8325074
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.qda2 <- cumsum(14.5*c.valid[order(qda.pred2$posterior[,2], decreasing=T)]-2)
plot(profit.qda2) # see how profits change as more mailings are made
n.mail.valid.qda2 <- which.max(profit.qda2) # number of mailings that maximizes profits
c(n.mail.valid.qda2, max(profit.qda2))      # report number of mailings, 1369
                                            # report maximum profit, $11269

###QDA, no predictor transforms & with nonlinear terms
qda.fit3 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + I(avhv^2) + incm + I(incm^2) + inca + I(inca^2) + plow + I(plow^2) + npro + I(npro^2) + tgif + I(tgif^2) + lgif + I(lgif^2) + rgif + I(rgif^2) + tdon + I(tdon^2) + tlag + I(tlag ^2) + agif + I(agif ^2), data=data.train.c)
qda.pred3 <- predict(qda.fit3,data.valid)
qda.class3 <- qda.pred3$class
roc(data.valid$donr, qda.pred3$posterior[,1])$auc #AUC=0.8839 on validation set
table(qda.class3,data.valid$donr)                 #confusion matrix
mean(qda.class3==data.valid$donr)                 #accuracy rate=0.8166501
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.qda3 <- cumsum(14.5*c.valid[order(qda.pred3$posterior[,2], decreasing=T)]-2)
plot(profit.qda3) # see how profits change as more mailings are made
n.mail.valid.qda3 <- which.max(profit.qda3) # number of mailings that maximizes profits
c(n.mail.valid.qda3, max(profit.qda3))      # report number of mailings, 1424
                                            # report maximum profit, $10999.5

###QDA, with predictor transforms & with nonlinear terms
qda.fit4 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv.trans + I(avhv.trans^2) + incm.trans + I(incm.trans^2) + inca.trans + I(inca.trans^2) + plow.trans + I(plow.trans^2) + npro.trans + I(npro.trans^2) + tgif.trans + I(tgif.trans^2) + lgif.trans + I(lgif.trans^2) + rgif.trans + I(rgif.trans^2) + tdon.trans + I(tdon.trans^2) + tlag + I(tlag ^2) + agif.trans + I(agif.trans ^2), data=data.train.c)
qda.pred4 <- predict(qda.fit4,data.valid)
qda.class4 <- qda.pred4$class
roc(data.valid$donr, qda.pred4$posterior[,1])$auc #AUC=0.8901 on validation set
table(qda.class4,data.valid$donr)                 #confusion matrix
mean(qda.class4==data.valid$donr)                 #accuracy rate=0.8052527
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.qda4 <- cumsum(14.5*c.valid[order(qda.pred4$posterior[,2], decreasing=T)]-2)
plot(profit.qda4) # see how profits change as more mailings are made
n.mail.valid.qda4 <- which.max(profit.qda4) # number of mailings that maximizes profits
c(n.mail.valid.qda4, max(profit.qda4))      # report number of mailings, 1329
                                            # report maximum profit, $10885

#############################################################
################## K-Nearest Neighbors ######################
#############################################################
set.seed(1)
knn.pred7 <- knn(x.train.std, x.valid.std, c.train, k=7) #7 appeared to be best k on validation set, considered 1-10
knn.pred7 <- as.numeric(as.character(knn.pred7))
table(knn.pred7,data.valid$donr) #confusion matrix
mean(knn.pred7==data.valid$donr) #accuracy rate=0.796333
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.knn <- cumsum(14.5*c.valid[order(knn.pred7, decreasing=T)]-2)
plot(profit.knn) # see how profits change as more mailings are made
n.mail.valid.knn <- which.max(profit.knn) # number of mailings that maximizes profits
c(n.mail.valid.knn, max(profit.knn))      # report number of mailings, 1250
                                          # report maximum profit, $10811

#################################################
################## Bagging ######################
#################################################
set.seed(1)
bag <- randomForest(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.c, mtry=20, importance=TRUE, ntree=500) #mtry=20 means consider all 20 predictors at each split aka bagging
bag.pred <- predict(bag,newdata=data.valid) 
roc(data.valid$donr, bag.pred)$auc # AUC=0.9525 on validation set
bag.pred1=rep(0,2018)
bag.pred1[bag.pred>0.5]=1
table(bag.pred1,data.valid$donr)   #confusion matrix
mean(bag.pred1==data.valid$donr)   #accuracy rate=0.8904856
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.bag <- cumsum(14.5*c.valid[order(bag.pred, decreasing=T)]-2)
plot(profit.bag) # see how profits change as more mailings are made
n.mail.valid.bag <- which.max(profit.bag) # number of mailings that maximizes profits
c(n.mail.valid.bag, max(profit.bag))      # report number of mailings, 1262
                                          # report maximum profit, $11715

#######################################################
################## Random Forest ######################
#######################################################
set.seed(1)
RF <- randomForest(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.c, mtry=5, importance=TRUE, ntree=500) #mtry=5 means consider only 5 predictors at each split aka RF
RF.pred <- predict(RF,newdata=data.valid) 
roc(data.valid$donr, RF.pred)$auc # AUC=0.9587 on validation set
RF.pred1=rep(0,2018)
RF.pred1[RF.pred>0.5]=1
table(RF.pred1,data.valid$donr)   #confusion matrix
mean(RF.pred1==data.valid$donr)   #accuracy rate=0.8909812
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.RF <- cumsum(14.5*c.valid[order(RF.pred, decreasing=T)]-2)
plot(profit.RF) # see how profits change as more mailings are made
n.mail.valid.RF <- which.max(profit.RF) # number of mailings that maximizes profits
c(n.mail.valid.RF, max(profit.RF))      # report number of mailings, 1266
                                        # report maximum profit, $11736

#######################################################
###################### Boosting #######################
#######################################################
####5000 trees, depth 4
set.seed(1)
boost1<- gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c, distribution = "bernoulli", n.trees=5000, interaction.depth = 4)
#regression use distribution = "gaussian", classification use distribution = "bernoulli"
#interaction.dept limits the depth of each tree
boost1.pred <- predict(boost1,newdata=data.valid, n.trees = 5000, type="response") 
roc(data.valid$donr, boost1.pred)$auc # AUC=0.9612 on validation set
boost1.pred.actual=rep(0,2018)
boost1.pred.actual[boost1.pred>0.5]=1
table(boost1.pred.actual,data.valid$donr)   #confusion matrix
mean(boost1.pred.actual==data.valid$donr)   #accuracy rate=0.8919722
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.boost1 <- cumsum(14.5*c.valid[order(boost1.pred, decreasing=T)]-2)
plot(profit.boost1) # see how profits change as more mailings are made
n.mail.valid.boost1 <- which.max(profit.boost1) # number of mailings that maximizes profits
c(n.mail.valid.boost1, max(profit.boost1))      # report number of mailings, 1274
                                                # report maximum profit, $11836

###5000 trees, depth 2
set.seed(1)
boost2<- gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c, distribution = "bernoulli", n.trees=5000, interaction.depth = 2)
#regression use distribution = "gaussian", classification use distribution = "bernoulli"
#interaction.dept limits the depth of each tree
boost2.pred <- predict(boost2,newdata=data.valid, n.trees = 5000, type="response") 
roc(data.valid$donr, boost2.pred)$auc # AUC=0.9493 on validation set
boost2.pred.actual=rep(0,2018)
boost2.pred.actual[boost2.pred>0.5]=1
table(boost2.pred.actual,data.valid$donr)   #confusion matrix
mean(boost2.pred.actual==data.valid$donr)   #accuracy rate=0.8716551
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.boost2 <- cumsum(14.5*c.valid[order(boost2.pred, decreasing=T)]-2)
plot(profit.boost2) # see how profits change as more mailings are made
n.mail.valid.boost2 <- which.max(profit.boost2) # number of mailings that maximizes profits
c(n.mail.valid.boost2, max(profit.boost2))      # report number of mailings, 1267
                                                # report maximum profit, $11763

###5000 trees, depth 6
set.seed(1)
boost3<- gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c, distribution = "bernoulli", n.trees=5000, interaction.depth = 6)
#regression use distribution = "gaussian", classification use distribution = "bernoulli"
#interaction.dept limits the depth of each tree
boost3.pred <- predict(boost3,newdata=data.valid, n.trees = 5000, type="response") 
roc(data.valid$donr, boost3.pred)$auc # AUC=0.9645 on validation set
boost3.pred.actual=rep(0,2018)
boost3.pred.actual[boost3.pred>0.5]=1
table(boost3.pred.actual,data.valid$donr)   #confusion matrix
mean(boost3.pred.actual==data.valid$donr)   #accuracy rate=0.9038652
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.boost3 <- cumsum(14.5*c.valid[order(boost3.pred, decreasing=T)]-2)
plot(profit.boost3) # see how profits change as more mailings are made
n.mail.valid.boost3 <- which.max(profit.boost3) # number of mailings that maximizes profits
c(n.mail.valid.boost3, max(profit.boost3))      # report number of mailings, 1239
                                                # report maximum profit, $11877
###5000 trees, depth 8
set.seed(1)
boost4<- gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c, distribution = "bernoulli", n.trees=5000, interaction.depth = 8)
#regression use distribution = "gaussian", classification use distribution = "bernoulli"
#interaction.dept limits the depth of each tree
boost4.pred <- predict(boost4,newdata=data.valid, n.trees = 5000, type="response") 
roc(data.valid$donr, boost4.pred)$auc # AUC=0.966 on validation set
boost4.pred.actual=rep(0,2018)
boost4.pred.actual[boost4.pred>0.5]=1
table(boost4.pred.actual,data.valid$donr)   #confusion matrix
mean(boost4.pred.actual==data.valid$donr)   #accuracy rate=0.9063429
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.boost4 <- cumsum(14.5*c.valid[order(boost4.pred, decreasing=T)]-2)
plot(profit.boost4) # see how profits change as more mailings are made
n.mail.valid.boost4 <- which.max(profit.boost4) # number of mailings that maximizes profits
c(n.mail.valid.boost4, max(profit.boost4))      # report number of mailings, 1235.0
                                                # report maximum profit, $11870.5
###8000 trees, depth 6
set.seed(1)
boost5<- gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data=data.train.c, distribution = "bernoulli", n.trees=10000, interaction.depth = 6)
#regression use distribution = "gaussian", classification use distribution = "bernoulli"
#interaction.dept limits the depth of each tree
boost5.pred <- predict(boost5,newdata=data.valid, n.trees = 10000, type="response") 
roc(data.valid$donr, boost5.pred)$auc # AUC=0.97 on validation set
boost5.pred.actual=rep(0,2018)
boost5.pred.actual[boost5.pred>0.5]=1
table(boost5.pred.actual,data.valid$donr)   #confusion matrix
mean(boost5.pred.actual==data.valid$donr)   #accuracy rate=0.9088206
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.boost5 <- cumsum(14.5*c.valid[order(boost5.pred, decreasing=T)]-2)
plot(profit.boost5) # see how profits change as more mailings are made
n.mail.valid.boost5 <- which.max(profit.boost5) # number of mailings that maximizes profits
c(n.mail.valid.boost5, max(profit.boost5))      # report number of mailings, 1260
                                                # report maximum profit, $11922

#######################################################
############## Support Vector Classifier ##############
#######################################################
SVM.data <- data.frame(x=x.train[,-c(10,11,12,13,14,15,16,17,18,20)], y=as.factor(data.train[,22]))  #to get SVM() to perform classification (not regression) y needs to be a factor
SVM.data.test <- data.frame(x=data.valid[,-c(1,11,12,13,14,15,16,17,18,19,21,22,23,24)], y=as.factor(data.valid[,22]))            #to get SVM() to perform classification (not regression) y needs to be a factor

### Predicting with SVM
###linear kernel
set.seed(1)
tune.SVM1 <- tune(svm, y~., data=SVM.data, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.SVM1)                 #best cost is 0.01
best.SVM1 <- tune.SVM1$best.model    #tune function stores bestmodel
SVM1.pred <- predict(best.SVM1,SVM.data.test)
table(SVM1.pred,SVM.data.test$y)   #confusion matrix
mean(SVM1.pred==SVM.data.test$y)   #accuracy rate=0.8349851
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.SVM1 <- cumsum(14.5*c.valid[order(SVM1.pred, decreasing=T)]-2)
plot(profit.SVM1) # see how profits change as more mailings are made
n.mail.valid.SVM1 <- which.max(profit.SVM1) # number of mailings that maximizes profits
c(n.mail.valid.SVM1, max(profit.SVM1))      # report number of mailings, 1825.0
                                            # report maximum profit, $10545.5


###Polynomial kernel
set.seed(1)
tune.SVM2 <- tune(svm, y~., data=SVM.data, kernel="polynomial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.SVM2)                 #best cost is 1
best.SVM2 <- tune.SVM2$best.model    #tune function stores bestmodel
SVM2.pred <- predict(best.SVM2,SVM.data.test)
table(SVM2.pred,SVM.data.test$y)   #confusion matrix
mean(SVM2.pred==SVM.data.test$y)   #accuracy rate=0.8349851
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.SVM2 <- cumsum(14.5*c.valid[order(SVM2.pred, decreasing=T)]-2)
plot(profit.SVM2) # see how profits change as more mailings are made
n.mail.valid.SVM2 <- which.max(profit.SVM2) # number of mailings that maximizes profits
c(n.mail.valid.SVM2, max(profit.SVM2))      # report number of mailings, 1142
                                            # report maximum profit, $10795

###radial kernel
set.seed(1)
tune.SVM3 <- tune(svm, y~., data=SVM.data, kernel="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.SVM3)                 #best cost is 5
best.SVM3 <- tune.SVM3$best.model    #tune function stores bestmodel
SVM3.pred <- predict(best.SVM3,SVM.data.test)
table(SVM3.pred,SVM.data.test$y)   #confusion matrix
mean(SVM3.pred==SVM.data.test$y)   #accuracy rate=0.8800793
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.SVM3 <- cumsum(14.5*c.valid[order(SVM3.pred, decreasing=T)]-2)
plot(profit.SVM3) # see how profits change as more mailings are made
n.mail.valid.SVM3 <- which.max(profit.SVM3) # number of mailings that maximizes profits
c(n.mail.valid.SVM3, max(profit.SVM3))      # report number of mailings, 1075
# report maximum profit, $11132

# load the data
charity <- read.csv("/Users/decolvin/Box Sync/1 Northwestern MSPA/PREDICT 422_DL/Group Project/charity.csv") # load the "charity.csv" file
str(charity)
summary(charity)

# predictor transformations

charity.t <- charity
#charity.t[,c(2:6,9)] <- lapply(charity.t[,c(2:5,9)], factor)
#charity.t$chld0 <- factor(ifelse(charity.t$chld==0,1,0))
#charity.t$chld1 <- factor(ifelse(charity.t$chld==1,1,0))
#charity.t$chld2 <- factor(ifelse(charity.t$chld==2,1,0))
#charity.t$chld3 <- factor(ifelse(charity.t$chld==3,1,0))
#charity.t$chld4 <- factor(ifelse(charity.t$chld==4,1,0))
#charity.t$donr <- factor(charity.t$donr)
#charity.t <- charity.t[,-c(1,7)]

#histogram of numeric variables
nums <- sapply(charity.t, is.numeric)
par(mfrow=c(4,4))
for (i in colnames(charity.t[,nums])) {
  hist(charity.t[,i], main=i)
}
par(mfrow=c(1,1))

#make some variables more normally distributed
charity.t$avhv <- log(charity.t$avhv)
charity.t$incm <- log(charity.t$incm)
charity.t$inca <- log(charity.t$inca)
charity.t$lgif <- log(charity.t$lgif)

# set up data for analysis

data.train <- charity.t[charity$part=="train",]

library(corrplot)
corrplot(cor(data.train[,2:23]), method="circle")

x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)


##### CLASSIFICATION MODELING ######

# linear discriminant analysis
library(MASS)
model.lda1 <- lda(donr ~ .+ I(hinc^2), data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

lda.summary <- data.frame("Num Mailed"=n.mail.valid,
                          "Response Rate"=((table(chat.valid.lda1, c.valid)[2,1]+table(chat.valid.lda1, c.valid)[2,2])/2018)*100, 
                          "Profit"=max(profit.lda1))

library(ROCR)
pred = prediction(post.valid.lda1, c.valid)
perf = performance(pred,"tpr","fpr")
auc.lda1 <- performance(pred, measure = "auc")@y.values

######################################################################

# quadratic discriminant analysis
library(MASS)
model.qda1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, qda should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.qda1 <- predict(model.qda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
plot(profit.qda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda1)) # report number of mailings and maximum profit
# 1348.0 11209.5

cutoff.qda1 <- sort(post.valid.qda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.qda1 <- ifelse(post.valid.qda1>cutoff.qda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda1, c.valid) # classification table
#               c.valid
#chat.valid.qda1   0   1
#              0 630  40
#              1 389 959
# check n.mail.valid = 389+959 = 1348
# check profit = 14.5*959-2*1348 = 11209.5

qda.summary <- data.frame("Num Mailed"=n.mail.valid,
                          "Response Rate"=((table(chat.valid.qda1, c.valid)[2,1]+table(chat.valid.qda1, c.valid)[2,2])/2018)*100, 
                          "Profit"=max(profit.qda1))

library(ROCR)
pred = prediction(post.valid.qda1, c.valid)
perf = performance(pred,"tpr","fpr")
auc.qda1 <- performance(pred, measure = "auc")@y.values

######################################################################

# logistic regression
model.log1 <- glm(donr ~ . + I(hinc^2), 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1287.0 11650.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 713  18
#              1 306 981
# check n.mail.valid = 306+981 = 1287
# check profit = 14.5*981-2*1287 = 11650.5

log.summary <- data.frame("Num Mailed"=n.mail.valid,
                          "Response Rate"=((table(chat.valid.log1, c.valid)[2,1]+table(chat.valid.log1, c.valid)[2,2])/2018)*100, 
                          "Profit"=max(profit.log1))

library(ROCR)
pred = prediction(post.valid.log1, c.valid)
perf = performance(pred,"tpr","fpr")
auc.log1 <- performance(pred, measure = "auc")@y.values

######################################################################

# knn
library(class)
set.seed(1)
best <- rep(NA,10)
for (i in 1:10) {
  post.valid.knn1 <- knn(x.train.std, x.valid.std, c.train, k=i)
  post.valid.knn1 <- as.numeric(as.character(post.valid.knn1))
  
  profit.knn1 <- cumsum(14.5*c.valid[order(post.valid.knn1, decreasing=T)]-2)
  n.mail.valid <- which.max(profit.knn1) # number of mailings that maximizes profits
  c(n.mail.valid, max(profit.knn1)) # report number of mailings and maximum profit
  # 2017.0 10451.5
  
  cutoff.knn1 <- sort(post.valid.knn1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
  chat.valid.knn1 <- ifelse(post.valid.knn1>cutoff.knn1, 1, 0) # mail to everyone above the cutoff
  t <- table(chat.valid.knn1, c.valid) # classification table
  best[i] <- 14.5 * t[2,2] - 2 * (t[2,1]+t[2,2])
}
plot(best)
points(which.max(best), max(best), col='red')

set.seed(1)
post.valid.knn1 <- knn(x.train.std, x.valid.std, c.train, k=which.max(best))
post.valid.knn1 <- as.numeric(as.character(post.valid.knn1))

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.knn1 <- cumsum(14.5*c.valid[order(post.valid.knn1, decreasing=T)]-2)
plot(profit.knn1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.knn1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.knn1)) # report number of mailings and maximum profit
# 1242.0 11088

cutoff.knn1 <- sort(post.valid.knn1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.knn1 <- ifelse(post.valid.knn1>cutoff.knn1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.knn1, c.valid) # classification table
#               c.valid
#chat.valid.knn1   0   1
#              0  716 64
#              1  303 935
# check n.mail.valid = 303+935 = 1238
# check profit = 14.5*935-2*1238 = 11081.5

knn.summary <- data.frame("Num Mailed"=n.mail.valid,
                          "Response Rate"=((table(chat.valid.knn1, c.valid)[2,1]+table(chat.valid.knn1, c.valid)[2,2])/2018)*100, 
                          "Profit"=max(profit.knn1))

library(ROCR)
pred = prediction(post.valid.knn1, c.valid)
perf = performance(pred,"tpr","fpr")
auc.knn1 <- performance(pred, measure = "auc")@y.values
#0.8192927962

######################################################################

# random forest
library(randomForest)
set.seed(1)
model.rf <- randomForest(donr ~ ., data.train.std.c, mtry=20, importance=T)
model.rf
post.valid.rf <- predict(model.rf, data.valid.std.c)

profit.rf <- cumsum(14.5*c.valid[order(post.valid.rf, decreasing=T)]-2)
plot(profit.rf) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf)) # report number of mailings and maximum profit
# 1276.0 11716

cutoff.rf <- sort(post.valid.rf, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.rf <- ifelse(post.valid.rf>cutoff.rf, 1, 0) # mail to everyone above the cutoff
table(chat.valid.rf, c.valid) # classification table
#               c.valid
#chat.valid.rf   0   1
#              0  727 15
#              1  292 984
# check n.mail.valid = 292+984 = 1276
# check profit = 14.5*984-2*1276 = 11716

rf.summary <- data.frame("Num Mailed"=n.mail.valid,
                         "Response Rate"=((table(chat.valid.rf, c.valid)[2,1]+table(chat.valid.rf, c.valid)[2,2])/2018)*100, 
                         "Profit"=max(profit.rf))

library(ROCR)
pred = prediction(post.valid.rf, c.valid)
perf = performance(pred,"tpr","fpr")
auc.rf <- performance(pred, measure = "auc")@y.values

######################################################################

# Boosting
library(gbm)
best.depth <- rep(NA,10)
for (i in 1:10) {
  set.seed(1)
  model.boost <- gbm(donr ~ ., data.train.std.c, distribution = "gaussian", n.trees=5000, interaction.depth = i)
  
  post.valid.boost <- predict(model.boost, data.valid.std.c, n.trees=5000)
  
  profit.boost <- cumsum(14.5*c.valid[order(post.valid.boost, decreasing=T)]-2)
  n.mail.valid <- which.max(profit.boost) # number of mailings that maximizes profits
  best.depth[i] <- max(profit.boost)
}
best.depth <- which.max(best.depth)

set.seed(1)
model.boost <- gbm(donr ~ ., data.train.std.c, distribution = "gaussian", n.trees=5000, interaction.depth = best.depth)
summary(model.boost)

post.valid.boost <- predict(model.boost, data.valid.std.c, n.trees=5000)

profit.boost <- cumsum(14.5*c.valid[order(post.valid.boost, decreasing=T)]-2)
plot(profit.boost) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.boost) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.boost)) # report number of mailings and maximum profit
# 1279.0 11869.5

cutoff.boost <- sort(post.valid.boost, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.boost <- ifelse(post.valid.boost>cutoff.boost, 1, 0) # mail to everyone above the cutoff
table(chat.valid.boost, c.valid) # classification table
#               c.valid
#chat.valid.boost   0   1
#              0  735 4
#              1  284 995
# check n.mail.valid = 284+995 = 1279
# check profit = 14.5*995-2*1279 = 11869.5

boost.summary <- data.frame("Num Mailed"=n.mail.valid,
                            "Response Rate"=((table(chat.valid.boost, c.valid)[2,1]+table(chat.valid.boost, c.valid)[2,2])/2018)*100, 
                            "Profit"=max(profit.boost))

library(ROCR)
pred = prediction(post.valid.boost, c.valid)
perf = performance(pred,"tpr","fpr")
auc.boost <- performance(pred, measure = "auc")@y.values

######################################################################

# SVM
library(e1071)
best.cost <- rep(NA,10)
for (i in 1:10) {
  set.seed(1)
  model.svm <- svm(as.factor(donr) ~ ., data=data.train.std.c, kernel="radial", cost=i)
  table(model.svm$fitted , data.train.std.c$donr)
  
  post.valid.svm <- predict(model.svm, newdata=data.valid.std.c)
  post.valid.svm <- as.numeric(as.character(post.valid.svm))
  
  profit.svm <- cumsum(14.5*c.valid[order(post.valid.svm, decreasing=T)]-2)
  n.mail.valid <- which.max(profit.svm)
  best.cost[i] <- max(profit.svm)
}
plot(best.cost)
best.cost <- which.max(best.cost)

set.seed(1)
model.svm <- svm(as.factor(donr) ~ ., data=data.train.std.c, kernel="radial", cost=best.cost)

post.valid.svm <- predict(model.svm, newdata=data.valid.std.c)
post.valid.svm <- as.numeric(as.character(post.valid.svm))

profit.svm <- cumsum(14.5*c.valid[order(post.valid.svm, decreasing=T)]-2)
plot(profit.svm) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.svm) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm)) # report number of mailings and maximum profit
# 1069 11100.5

cutoff.svm <- sort(post.valid.svm, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.svm <- ifelse(post.valid.svm>cutoff.svm, 1, 0) # mail to everyone above the cutoff
table(chat.valid.svm, c.valid) # classification table
#               c.valid
#chat.valid.svm   0   1
#              0  863  86
#              1  156 913
# check n.mail.valid = 156+913 = 1069
# check profit = 14.5*913-2*1069 = 11100.5

svm.summary <- data.frame("Num Mailed"=n.mail.valid,
                          "Response Rate"=((table(chat.valid.svm, c.valid)[2,1]+table(chat.valid.svm, c.valid)[2,2])/2018)*100, 
                          "Profit"=max(profit.svm))

library(ROCR)
pred = prediction(post.valid.svm, c.valid)
perf = performance(pred,"tpr","fpr")
auc.svm <- performance(pred, measure = "auc")@y.values
#0.880411324

######################################################################
######################################################################
######################################################################
# Results

results.donr.mpe <- rbind("lda"=lda.summary,"qda"=qda.summary,"log"=log.summary,
                          "knn"=knn.summary,"rf"=rf.summary,"boost"=boost.summary,
                          "svm"=svm.summary)
results.donr.mpe
#           n.mail  response  profit
# lda        1388   68.78097  11622.5
#qda         1348   66.79881  11209.5
#log         1287   63.77602  11650.5
#knn         1242   61.34787  11088.0
#rf          1276   63.23092  11716.0
#boost       1279   63.37958  11869.5
#svm         1069   52.97324  11100.5

results.donr.auc <- rbind("lda"=auc.lda1,"qda"=auc.qda1,"log"=auc.log1,
                          "knn"=auc.knn1,"rf"=auc.rf,"boost"=auc.boost,
                          "svm"=auc.svm)
results.donr.auc

# select model.boost since it has maximum profit in the validation sample

post.test <- predict(model.boost, data.test.std, n.trees = 5000) 

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.boost)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .6 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1779  228
# based on this model we'll mail to the 228 highest posterior probabilities

# See below for saving chat.test into a file for submission



##### PREDICTION MODELING ######

# Least squares regression

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mae.ls1 <- mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615

#####################################################################

# subset selection
library(leaps)

predict.regsubsets <- function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k=5
set.seed (1)
folds=sample(1:k,nrow(data.train.std.y),replace=TRUE)
cv.errors=matrix(NA,k,20, dimnames=list(NULL, paste(1:20)))
for(j in 1:k){
  best.fit <- regsubsets(damt ~ .,data=data.train.std.y[folds!=j, ], nvmax =20, method="exhaustive") 
  for(i in 1:19){
    pred <- predict.regsubsets(best.fit, data.train.std.y[folds==j,], id=i) 
    cv.errors[j,i] <- mean( (data.train.std.y$damt[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors ,2,mean)
plot(mean.cv.errors, type='b')
smallest <- which.min(mean.cv.errors)
smallest

model.ls2 <- regsubsets(damt ~ ., data.train.std.y, nvmax=20, method = "exhaustive")

pred.valid.ls2 <- predict.regsubsets(model.ls2, data.valid.std.y, id=14)  # validation predictions
mae.ls2 <- mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.624626
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.163837

#####################################################################

# lasso
library(glmnet)
grid <- 10^seq(10,-2,length=100)
x=model.matrix(damt ~., data.train.std.y)[,-1]
y=data.train.std.y$damt

model.lasso <- glmnet(x, y, alpha=1, lambda=grid)
plot(lasso.mod)

set.seed (1)
cv.out <- cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

pred.valid.lasso <- predict(lasso.mod,s=bestlam ,newx=model.matrix(damt ~., data.valid.std.y)[,-1])
mae.lasso <- mean((pred.valid.lasso-y.valid)^2)
#1.62662
sd((y.valid - pred.valid.lasso)^2)/sqrt(n.valid.y) # std error
# 0.1645279

#####################################################################

# rf
library(randomForest)
set.seed(1)
model.rf <- randomForest(damt ~ ., data.train.std.y, mtry=20, importance=T)
model.rf
pred.valid.rf <- predict(model.rf, data.valid.std.y)
mae.rf <- mean((pred.valid.rf-y.valid)^2)
#1.70413
sd((y.valid - pred.valid.rf)^2)/sqrt(n.valid.y) # std error
# 1.70413

#####################################################################

# boost
library(gbm)
best.depth <- rep(NA,10)
for (i in 1:10) {
  set.seed(1)
  model.boost <- gbm(damt ~ ., data.train.std.y, distribution = "gaussian", n.trees=5000, interaction.depth = i)
  
  pred.valid.boost <- predict(model.boost, data.valid.std.y, n.trees=5000)
  
  best.depth[i] <- mean((pred.valid.boost - y.valid)^2)
}
best.depth <- which.min(best.depth)

set.seed(1)
model.boost <- gbm(damt ~ ., data.train.std.y, distribution = "gaussian", n.trees=5000, interaction.depth = best.depth)
#summary(model.boost)

pred.valid.boost <- predict(model.boost, data.valid.std.y, n.trees=5000)
mae.boost <- mean((pred.valid.boost-y.valid)^2)
#1.436761
sd((y.valid - pred.valid.boost)^2)/sqrt(n.valid.y) # std error
#0.1647273

######################################################################

# SVM
library(e1071)
best.cost <- rep(NA,10)
for (i in 1:10) {
  set.seed(1)
  model.svm <- svm(damt ~ ., data=data.train.std.y, kernel="radial", cost=i)
  
  pred.valid.svm <- predict(model.svm, newdata=data.valid.std.y)
  
  best.cost[i] <- mean((pred.valid.svm - y.valid)^2)
}
best.cost <- which.min(best.cost)

set.seed(1)
model.svm <- svm(damt ~ ., data=data.train.std.y, kernel="radial", cost=best.cost)

pred.valid.svm <- predict(model.svm, newdata=data.valid.std.y)
mae.svm <- mean((pred.valid.svm-y.valid)^2)
#1.436761
sd((y.valid - pred.valid.svm)^2)/sqrt(n.valid.y) # std error
#0.1647273

######################################################################
# Results
results.damt <- rbind("ls1"=mae.ls1,"ls2"=mae.ls2,"lasso"=mae.lasso,
                      "rf"=mae.rf,"boost"=mae.boost,"svm"=mae.svm)
results.damt

#model MPE
#ls1   1.623029
#ls2   1.624626
#lasso 1.626620
#rf    1.704130
#boost 1.436761
#svm   1.590360

# select model.boost since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.boost, data.test.std, n.trees=5000) # test predictions


######################################################################
######################################################################
######################################################################
# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="ABC.csv", row.names=FALSE) # use your initials for the file name

for(package in c("scales",
                 "randomForest", "caret",
                 "ROCR",
                 "ggplot2", "gridExtra")) {
  if(!require(package, character.only=TRUE)) {
    install.packages(package)
    library(package, character.only=TRUE)
  }
}

rm(package)

###########################################################################
# Preliminaries - Raw Data
###########################################################################

#-------------------------------------------------------------------------
# Data load
#-------------------------------------------------------------------------

# Load the dataset
df_charity.raw <- read.csv("charity.csv")

# Data structure/stats
#summary(df_charity.raw)
#str(df_charity.raw)
#dim(df_charity.raw)
#temp <- sapply(df_charity.raw, typeof)
#table(temp)
#double integer 
#1      23

# Convert blank to NA
df_charity.raw[df_charity.raw == ""] <- NA


#-------------------------------------------------------------------------
# Variable type conversions
#-------------------------------------------------------------------------

# Identify and convert character/numeric variables where appropriate
#str(df_charity.raw, list.len=nrow(df_charity.raw))
df_charity.clean <- df_charity.raw

cols <- colnames(df_charity.clean)
cols <- cols[cols != "ID"]

for (c in cols) {
  if (is.numeric(df_charity.clean[, c]) == TRUE) {
    if ((is.character(na.omit(df_charity.clean[, c])) == TRUE) | (length(unique(na.omit(df_charity.clean[, c]))) <= 2)) {
      df_charity.clean[, c] <- as.factor(df_charity.clean[, c])
    }
  }
}

rm(cols)

#str(df_charity.clean, list.len=nrow(df_charity.clean))


#-------------------------------------------------------------------------
# Data exploration
#-------------------------------------------------------------------------

df_charity.expl <- df_charity.clean

# Summary statistics
cols <- colnames(df_charity.expl[, sapply(df_charity.expl, is.numeric)])
cols <- cols[cols != "ID"]
df_temp <- df_charity.expl[, cols]
stats <- lapply(df_temp , function(x) rbind(mean=mean(x),
                                            median=median(x),
                                            s.d.=sd(x),
                                            min=min(x),
                                            max=max(x),
                                            miss=length(which(is.na(x))),
                                            n=length(x)))
df_temp <- t(data.frame(stats))
round(df_temp, digits=4)
#write.table(df_temp, "temp.csv", sep="\t") 
rm(cols, df_temp, stats)

# Correlations
cols <- colnames(df_charity.expl[, sapply(df_charity.expl, is.numeric)])
df_temp <- t(cor(df_charity.expl["damt"], df_charity.expl[cols], use="complete"))
colnames(df_temp) <- "Corr"
round(df_temp, digits=4)
#write.table(df_temp, "temp.csv", sep="\t") 
rm(cols, df_temp)


cols <- colnames(df_charity.expl[, sapply(df_charity.expl, is.numeric)])
cols <- cols[cols != "ID"]

for (i in cols) {
  
  df_temp <- df_charity.expl[i]
  colnames(df_temp) <- i
  
  plot1 <- ggplot(data=df_temp, aes(x=df_temp[, i])) +
    geom_histogram(color="darkblue", fill="lightblue") +
    ggtitle(paste("Histogram:", i)) +
    labs(x="", y="count")
  
  plot2 <- ggplot(data=df_temp, aes(x=factor(""), y=df_temp[, i])) +
    geom_boxplot(color="darkblue", fill="lightblue") +
    ggtitle(paste("Boxplot:", i)) +
    labs(x="", y="value")
  
  png(filename=paste0("expl_num_", i, ".png"), 
      width=1000, height=600, res=150)
  
  grid.arrange(plot1, plot2, ncol=2)
  
  dev.off()
  
}

rm(df_temp)


df_temp <- df_charity.expl[c("npro", "plow", "donr")]
df_temp <- na.omit(df_temp)
#colnames(df_temp) <- c("npro", "plow", "donr")

plot1 <- ggplot(data=df_temp, aes(x=df_temp[, "donr"], y=df_temp[, "npro"])) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  ggtitle(paste("Boxplot: NPRO by DONR")) +
  labs(x="DONR", y="value")

plot2 <- ggplot(data=df_temp, aes(x=df_temp[, "donr"], y=df_temp[, "plow"])) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  ggtitle(paste("Boxplot: PLOW by DONR")) +
  labs(x="DONR", y="value")

png(filename=paste0("expl_boxcomp_1.png"), 
    width=1200, height=800, res=150)

grid.arrange(plot1, plot2, ncol=2)

dev.off()

rm(df_temp)


###########################################################################
# Preliminaries - Campaign Response
###########################################################################

#-------------------------------------------------------------------------
# Prep for train/validation observations
#-------------------------------------------------------------------------

# Remove response/ID variables
resp_DONR.orig <- df_charity.clean[, "donr"]
resp_DAMT.orig <- df_charity.clean[, "damt"]
flag_PART.orig <- df_charity.clean[, "part"]
flag_ID.orig <- df_charity.clean[, "ID"]

df_charity.trnval <- df_charity.clean[(df_charity.clean[, "part"] == "train") | (df_charity.clean[, "part"] == "valid"), ]

resp_DONR.trnval <- df_charity.trnval[, "donr"]
resp_DAMT.trnval <- df_charity.trnval[, "damt"]
flag_PART.trnval <- df_charity.trnval[, "part"]
flag_ID.trnval <- df_charity.trnval[, "ID"]

df_charity.trnval[, "donr"] <- NULL
df_charity.trnval[, "damt"] <- NULL
df_charity.trnval[, "part"] <- NULL
df_charity.trnval[, "ID"] <- NULL


#-------------------------------------------------------------------------
# Data imputation
#-------------------------------------------------------------------------

df_charity.imp <- df_charity.trnval

# Check for NA's, zero's and blanks
sum(is.na(df_charity.imp)) # 0
sum(df_charity.imp == 0) # 24963
sum(df_charity.imp == "") # 0

for (c in 1:ncol(df_charity.imp)) {
  nas <- sum(is.na(df_charity.imp[, c]))
  zero <- sum(df_charity.imp[, c] == 0)
  blank <- sum(df_charity.imp[, c] == "")
  print(paste(colnames(df_charity.imp)[c], nas, zero, blank))
}

#str(df_charity.imp, list.len=nrow(df_charity.imp))
rm(nas, zero, blank)

# Impute numeric
cols <- colnames(df_charity.imp[, sapply(df_charity.imp, is.numeric)])

for (c in cols) {
  if (sum(is.na(df_charity.imp[, c])) > 0) {
    nm <- paste(c, "IMP", sep="_")
    df_charity.imp[, nm] <- df_charity.imp[, c]
    med <- median(df_charity.imp[, nm], na.rm=TRUE)
    df_charity.imp[, nm][is.na(df_charity.imp[, nm])] <- med
    df_charity.imp[, c] <- NULL
  }
}

rm(cols, nm, med)

# Impute factor
cols <- colnames(df_charity.imp[, sapply(df_charity.imp, is.factor)])

for (c in cols) {
  if (sum(is.na(df_charity.imp[, c])) > 0) {
    nm <- paste(c, "IMP", sep="_")
    df_charity.imp[, nm] <- as.numeric(df_charity.imp[, c]) - 1
    mod <- which.max(df_charity.imp[, nm]) - 1
    df_charity.imp[, nm][is.na(df_charity.imp[, nm])] <- mod
    df_charity.imp[, nm] <- as.factor(df_charity.imp[, nm])
    df_charity.imp[, c] <- NULL
  }
}

rm(cols, nm, mod)

# Check for NA's, zero's and blanks
sum(is.na(df_charity.imp)) # 0
sum(df_charity.imp == 0) # 24963
sum(df_charity.imp == "") # 0

for (c in 1:ncol(df_charity.imp)) {
  nas <- sum(is.na(df_charity.imp[, c]))
  zero <- sum(df_charity.imp[, c] == 0)
  blank <- sum(df_charity.imp[, c] == "")
  print(paste(colnames(df_charity.imp)[c], nas, zero, blank))
}

#str(df_charity.imp, list.len=nrow(df_charity.imp))
rm(nas, zero, blank)

#-------------------------------------------------------------------------
# Data trimming
#-------------------------------------------------------------------------

df_charity.trim <- df_charity.imp

# Create trimmed variables
cols <- colnames(df_charity.trim[, sapply(df_charity.trim, is.numeric)])

for (c in cols) {
  min <- min(df_charity.trim[, c])
  max <- min(df_charity.trim[, c])
  p01 <- quantile(df_charity.trim[, c], c(0.01)) 
  p99 <- quantile(df_charity.trim[, c], c(0.99))
  if (p01 > min | p99 < max) {
    nm <- paste(c, "T99", sep="_")
    df_charity.trim[, nm] <- df_charity.trim[, c]
    t99 <- quantile(df_charity.trim[, c], c(0.01, 0.99))
    df_charity.trim[, nm] <- squish(df_charity.trim[, nm], t99)
    #df_charity.trim[, c] <- NULL
  }
}

rm(cols, min, max, p01, p99, nm, t99)


#-------------------------------------------------------------------------
# Data transformations
#-------------------------------------------------------------------------

df_charity.trans <- df_charity.trim

# Create variable transformations
cols <- colnames(df_charity.trans[, sapply(df_charity.trans, is.numeric)])

for (c in cols) {
  nm <- paste(c, "LN", sep="_")
  df_charity.trans[, nm] <- df_charity.trans[, c]
  df_charity.trans[, nm] <- (sign(df_charity.trans[, nm]) * log(abs(df_charity.trans[, nm])+1))
  #df_charity.trans[, c] <- NULL
}

rm(cols, nm)


#-------------------------------------------------------------------------
# Create dummy variables
#-------------------------------------------------------------------------

df_charity.dum <- df_charity.imp

cols <- colnames(df_charity.dum[, sapply(df_charity.dum, is.factor)])

for (c in cols) {
  if (length(unique(df_charity.dum[, c])) <= 10) {
    for(level in unique(df_charity.dum[, c])[1:length(unique(df_charity.dum[, c]))-1]) {
      nm <- paste("DUM", c, level, sep="_")
      df_charity.dum[, nm] <- ifelse(df_charity.dum[, c] == level, 1, 0)
    }
  }
}

rm(cols, nm, level)

#length(df_charity.imp[, grepl("DUM_", names(df_charity.imp))]) #0
df_charity.dum <- df_charity.dum[, grepl("DUM_", names(df_charity.dum))]
df_charity.dum[, "ID"] <- flag_ID.trnval


#-------------------------------------------------------------------------
# Final prep for train/validation observations
#-------------------------------------------------------------------------

# Final transformed numeric data set
df_charity.trans <- df_charity.trans[, sapply(df_charity.trans, is.numeric)]
df_charity.trans[, "ID"] <- flag_ID.trnval

# Merge transformed data with dummies
df_charity.trnval <- merge(df_charity.trans, df_charity.dum, all=FALSE)

# Add back response/ID variables
df_charity.trnval[, "donr"] <- resp_DONR.trnval
df_charity.trnval[, "damt"] <- resp_DAMT.trnval
df_charity.trnval[, "part"] <- flag_PART.trnval

# Subset data frame for train set
df_charity.train <- df_charity.trnval[df_charity.trnval[, "part"] == "train", ]
nrow(df_charity.train) # 3984

# Remove response/ID variables
resp_DONR.train <- df_charity.train[, "donr"]
resp_DAMT.train <- df_charity.train[, "damt"]
flag_PART.train <- df_charity.train[, "part"]
flag_ID.train <- df_charity.train[, "ID"]

df_charity.train[, "donr"] <- NULL
df_charity.train[, "damt"] <- NULL
df_charity.train[, "part"] <- NULL
df_charity.train[, "ID"] <- NULL

# Subset data frame for validation set
df_charity.valid <- df_charity.trnval[df_charity.trnval[, "part"] == "valid", ]
nrow(df_charity.valid) # 2018

# Remove response/ID variables
resp_DONR.valid <- df_charity.valid[, "donr"]
resp_DAMT.valid <- df_charity.valid[, "damt"]
flag_PART.valid <- df_charity.valid[, "part"]
flag_ID.valid <- df_charity.valid[, "ID"]

df_charity.valid[, "donr"] <- NULL
df_charity.valid[, "damt"] <- NULL
df_charity.valid[, "part"] <- NULL
df_charity.valid[, "ID"] <- NULL


###########################################################################
# Estimate Model - Campaign Response
###########################################################################

#-------------------------------------------------------------------------
# Variable importance
#-------------------------------------------------------------------------

# Fit randomForest model
set.seed(1)
fit_rf <- randomForest(as.factor(resp_DONR.train) ~ ., data=df_charity.train, 
                       ntree=60, do.trace=TRUE)

# Varible importance
imp_rf <- varImp(fit_rf, scale=FALSE)
#plot(imp_rf, top=20)
imp_rf[, "Variable"] <- rownames(imp_rf)
imp_rf <- imp_rf[with(imp_rf, order(-Overall)), ]

df_temp <- imp_rf
df_temp <- transform(df_temp, 
                     Variable=reorder(Variable, -Overall))

png(filename="varimp_amt.png", 
    width=1000, height=600, res=150)

ggplot(df_temp[1:20, ], aes(Variable, Overall)) +
  geom_bar(stat="identity", fill="steelblue") +
  scale_fill_manual() +
  labs(x="",
       y="Importance") +
  #title="RESPONSE16 - Variable Importance") + 
  theme(text=element_text(size=10), axis.text.x=element_text(angle=270, hjust=0))

dev.off()

rm(df_temp)

# Subset dataset
#cols_DONR <- imp_rf[1:10, "Variable"]
cols_DONR <- imp_rf[1:nrow(imp_rf), "Variable"]
df_charity.trainsub <- df_charity.train[, cols_DONR]
df_charity.validsub <- df_charity.valid[, cols_DONR]


#-------------------------------------------------------------------------
# Model fitting
#-------------------------------------------------------------------------

df_charity.trainsub[, "resp_DONR.train"] <- resp_DONR.train # some versions of caret require pred be incl. in d.frame

objControl <- trainControl(method="cv", number=3, 
                           returnResamp="none", allowParallel=TRUE, verboseIter=TRUE)

# Naive Bayes: nb, Naive Bayes Classifier: nbDiscrete
set.seed(1)
fit_nb <- train(as.factor(resp_DONR.train) ~ .,
                data=df_charity.trainsub,
                method="nb", trControl=objControl)

# Random Forest: ranger, Parallel Random Forest: parRF
set.seed(1)
fit_rfc <- train(as.factor(resp_DONR.train) ~ .,
                 data=df_charity.trainsub,
                 method="parRF", trControl=objControl)

# glmnet: glmnet
set.seed(1)
fit_glmnet <- train(as.factor(resp_DONR.train) ~ .,
                    data=df_charity.trainsub,
                    method="glmnet", trControl=objControl)

# Logistic Regression: LogitBoost
set.seed(1)
fit_logboost <- train(as.factor(resp_DONR.train) ~ .,
                      data=df_charity.trainsub,
                      method="LogitBoost", trControl=objControl)

# Linear Discriminant Analysis: lda
set.seed(1)
fit_lda <- train(as.factor(resp_DONR.train) ~ .,
                 data=df_charity.trainsub,
                 method="lda", trControl=objControl)

# k-Nearest Neighbors: knn, k-Nearest Neighbors: kknn
set.seed(1)
fit_knn <- train(as.factor(resp_DONR.train) ~ .,
                 data=df_charity.trainsub,
                 method="kknn", trControl=objControl)


df_charity.trainsub[, "resp_DONR.train"] <- NULL

ls_fitnm <- list("nb", "rfc", "glm", "lboost", "lda", "knn")
ls_fit <- list(fit_nb, fit_rfc, fit_glmnet, fit_logboost, fit_lda, fit_knn)

for (i in 1:length(ls_fit)) {
  # ROC Curve - In-Sample
  pred_resp <- predict(object=ls_fit[[i]], newdata=df_charity.trainsub)
  pred_prob <- predict(object=ls_fit[[i]], newdata=df_charity.trainsub, type="prob")
  
  rocr_pred <- prediction(pred_prob[,2], resp_DONR.train)
  rocr_perf <- performance(rocr_pred, measure="tpr", x.measure="fpr")
  
  auc <- performance(rocr_pred, measure="auc")
  auc <- auc@y.values[[1]]
  print(paste0(ls_fitnm[[i]], ": in-sample: ", auc))
  
  rocr.data <- data.frame(fpr=unlist(rocr_perf@x.values),
                          tpr=unlist(rocr_perf@y.values))
  
  plot1 <- ggplot(rocr.data, aes(x=fpr, ymin=0, ymax=tpr)) + 
    geom_ribbon(alpha=0.2) + 
    geom_line(aes(y=tpr)) + 
    geom_abline(linetype="dashed") + 
    #ggtitle(expression(atop("xxx", atop("In-Sample ROC Curve")))) +
    ggtitle("In-Sample ROC Curve") +
    labs(x="False Positive Rate", y="True Positive Rate") + 
    annotate("text", x=0.75, y=0.25, label=paste("AUC:", round(auc,4)))
  
  rm(rocr.data, auc)
  
  png(filename=paste0("resp_", ls_fitnm[[i]], "_insample_roc.png"), 
      width=800, height=800, res=150)
  
  print(plot1)
  
  dev.off()
  
  # ROC Curve - Out-of-Sample
  pred_resp <- predict(object=ls_fit[[i]], newdata=df_charity.validsub)
  pred_prob <- predict(object=ls_fit[[i]], newdata=df_charity.validsub, type="prob")
  
  rocr_pred <- prediction(pred_prob[,2], resp_DONR.valid)
  rocr_perf <- performance(rocr_pred, measure="tpr", x.measure="fpr")
  
  auc <- performance(rocr_pred, measure="auc")
  auc <- auc@y.values[[1]]
  print(paste0(ls_fitnm[[i]], ": out-sample: ", auc))
  
  rocr.data <- data.frame(fpr=unlist(rocr_perf@x.values),
                          tpr=unlist(rocr_perf@y.values))
  
  plot2 <- ggplot(rocr.data, aes(x=fpr, ymin=0, ymax=tpr)) + 
    geom_ribbon(alpha=0.2) + 
    geom_line(aes(y=tpr)) + 
    geom_abline(linetype="dashed") + 
    #ggtitle(expression(atop("xxx", atop("Out-of-Sample ROC Curve")))) +
    ggtitle("Out-of-Sample ROC Curve") +
    labs(x="False Positive Rate", y="True Positive Rate") + 
    annotate("text", x=0.75, y=0.25, label=paste("AUC:", round(auc,4)))
  
  rm(rocr.data, auc)
  
  png(filename=paste0("resp_", ls_fitnm[[i]], "_outsample_roc.png"), 
      width=800, height=800, res=150)
  
  print(plot2)
  
  dev.off()
  
  png(filename=paste0("resp_", ls_fitnm[[i]], "_sample_roc.png"), 
      width=1000, height=600, res=150)
  
  grid.arrange(plot1, plot2, ncol=2)
  
  dev.off()
  
  # Confusion matrix
  confmat <- table(round(as.numeric(pred_resp, digits=0)),
                   resp_DONR.valid)
  row.names(confmat) = c(0,1)
  print(confusionMatrix(confmat, positive="1"))
  rm(confmat)
}

# validation set using using Naive Bayes and maximum profit function
# average donation = $14.50 and mailing cost = $2
temp <- as.numeric(resp_DONR.valid) - 1
pred_resp <- predict(object=fit_nb, newdata=df_charity.validsub, type="prob")[, 2]
profitfunc_nb <- cumsum(14.5 * temp[order(pred_resp, decreasing=TRUE)] - 2)
#maximum profit
max(profitfunc_nb)

# validation set using using Random Forest and maximum profit function
# average donation = $14.50 and mailing cost = $2
temp <- as.numeric(resp_DONR.valid) - 1
pred_resp <- predict(object=fit_rfc, newdata=df_charity.validsub, type="prob")[, 2]
profitfunc_rfc <- cumsum(14.5 * temp[order(pred_resp, decreasing=TRUE)] - 2)
#maximum profit
max(profitfunc_rfc)

# validation set using using GLMNET and maximum profit function
# average donation = $14.50 and mailing cost = $2
temp <- as.numeric(resp_DONR.valid) - 1
pred_resp <- predict(object=fit_glmnet, newdata=df_charity.validsub, type="prob")[, 2]
profitfunc_glmnet <- cumsum(14.5 * temp[order(pred_resp, decreasing=TRUE)] - 2)
#maximum profit
max(profitfunc_glmnet)

# validation set using using LogitBoost and maximum profit function
# average donation = $14.50 and mailing cost = $2
temp <- as.numeric(resp_DONR.valid) - 1
pred_resp <- predict(object=fit_logboost, newdata=df_charity.validsub, type="prob")[, 2]
profitfunc_logboost <- cumsum(14.5 * temp[order(pred_resp, decreasing=TRUE)] - 2)
#maximum profit
max(profitfunc_logboost)

# validation set using using LDA and maximum profit function
# average donation = $14.50 and mailing cost = $2
temp <- as.numeric(resp_DONR.valid) - 1
pred_resp <- predict(object=fit_lda, newdata=df_charity.validsub, type="prob")[, 2]
profitfunc_lda <- cumsum(14.5 * temp[order(pred_resp, decreasing=TRUE)] - 2)
#maximum profit
max(profitfunc_lda)

# validation set using using KNN and maximum profit function
# average donation = $14.50 and mailing cost = $2
temp <- as.numeric(resp_DONR.valid) - 1
pred_resp <- predict(object=fit_knn, newdata=df_charity.validsub, type="prob")[, 2]
profitfunc_knn <- cumsum(14.5 * temp[order(pred_resp, decreasing=TRUE)] - 2)
#maximum profit
max(profitfunc_knn)


###########################################################################
# Estimate Model - Donation Amount
###########################################################################

#-------------------------------------------------------------------------
# Subset for true responses
#-------------------------------------------------------------------------

# Can either use the full train set, or further subset based on true responses
# Subset data frame for train set
df_charity.train <- df_charity.trnval[df_charity.trnval[, "part"] == "train", ]
nrow(df_charity.train) # 3984

# Subset data frame for train set based on true resp
df_charity.train.true <- df_charity.trnval[((df_charity.trnval[, "part"] == "train") & 
                                              (df_charity.trnval[, "donr"] == 1)), ]
nrow(df_charity.train.true) # 1995

# Comment out if want to use full train set
df_charity.train <- df_charity.train.true

# Remove response/ID variables
resp_DONR.train <- df_charity.train[, "donr"]
resp_DAMT.train <- df_charity.train[, "damt"]
flag_PART.train <- df_charity.train[, "part"]
flag_ID.train <- df_charity.train[, "ID"]

df_charity.train[, "donr"] <- NULL
df_charity.train[, "damt"] <- NULL
df_charity.train[, "part"] <- NULL
df_charity.train[, "ID"] <- NULL

# Can either use the full validation set, or further subset based on true responses
# Subset data frame for validation set
df_charity.valid <- df_charity.trnval[df_charity.trnval[, "part"] == "valid", ]
#nrow(df_charity.valid) # 2018

# Subset data frame for validation set based on true resp
df_charity.valid.true <- df_charity.trnval[((df_charity.trnval[, "part"] == "valid") & 
                                              (df_charity.trnval[, "donr"] == 1)), ]
#nrow(df_charity.valid.true) # 999

# Comment out if want to use full validation set
df_charity.valid <- df_charity.valid.true

# Remove response variables
resp_DONR.valid <- df_charity.valid[, "donr"]
resp_DAMT.valid <- df_charity.valid[, "damt"]
flag_PART.valid <- df_charity.valid[, "part"]
flag_ID.valid <- df_charity.valid[, "ID"]

df_charity.valid[, "donr"] <- NULL
df_charity.valid[, "damt"] <- NULL
df_charity.valid[, "part"] <- NULL
df_charity.valid[, "ID"] <- NULL

#-------------------------------------------------------------------------
# Variable importance
#-------------------------------------------------------------------------

# Fit randomForest model
set.seed(1)
fit_rf <- randomForest(resp_DAMT.train ~ ., data=df_charity.train, 
                       ntree=20, importance=TRUE, do.trace=TRUE)

# Varible importance
imp_rf <- varImp(fit_rf, scale=FALSE)
#plot(imp_rf, top=20)
imp_rf[, "Variable"] <- rownames(imp_rf)
imp_rf <- imp_rf[with(imp_rf, order(-Overall)), ]

df_temp <- imp_rf
df_temp <- transform(df_temp, 
                     Variable=reorder(Variable, -Overall))

png(filename="varimp_resp.png", 
    width=1000, height=600, res=150)

ggplot(df_temp[1:20, ], aes(Variable, Overall)) +
  geom_bar(stat="identity", fill="steelblue") +
  scale_fill_manual() +
  labs(x="",
       y="Importance") +
  #title="RESPONSE16 - Variable Importance") + 
  theme(text=element_text(size=10), axis.text.x=element_text(angle=270, hjust=0))

dev.off()

rm(df_temp)

# Subset dataset
#cols_DAMT <- imp_rf[1:10, "Variable"]
cols_DAMT <- imp_rf[1:nrow(imp_rf), "Variable"]
df_charity.trainsub <- df_charity.train[, cols_DAMT]
df_charity.validsub <- df_charity.valid[, cols_DAMT]


#-------------------------------------------------------------------------
# Model fitting
#-------------------------------------------------------------------------

df_charity.trainsub[, "resp_DAMT.train"] <- resp_DAMT.train # some versions of caret require pred be incl. in d.frame

objControl <- trainControl(method="cv", number=3, 
                           returnResamp="none", allowParallel=TRUE, verboseIter=TRUE)

# Linear Regression: lm
set.seed(1)
fit_lms <- train(resp_DAMT.train ~ .,
                 data=df_charity.trainsub,
                 method="lmStepAIC", trControl=objControl)


# Random Forest: ranger, Parallel Random Forest: parRF
set.seed(1)
fit_rfr <- train(resp_DAMT.train ~ .,
                 data=df_charity.trainsub,
                 method="parRF", trControl=objControl)


# eXtreme Gradient Boosting: xgbLinear
set.seed(1)
fit_xgb <- train(resp_DAMT.train ~ .,
                 data=df_charity.trainsub,
                 method="xgbLinear", trControl=objControl)


# Partial Least Squares: kernelpls
set.seed(1)
fit_pls <- train(resp_DAMT.train ~ .,
                 data=df_charity.trainsub,
                 method="kernelpls", trControl=objControl)


# Ridge Regression with Variable Selection: foba
set.seed(1)
fit_rr <- train(resp_DAMT.train ~ .,
                data=df_charity.trainsub,
                method="foba", trControl=objControl)


# The lasso: lasso
set.seed(1)
fit_lasso <- train(resp_DAMT.train ~ .,
                   data=df_charity.trainsub,
                   method="lasso", trControl=objControl)

df_charity.trainsub[, "resp_DAMT.train"] <- NULL


ls_fitnm <- list("lm", "rfr", "xgb", "pls", "rr", "lasso")
ls_fit <- list(fit_lms, fit_rfr, fit_xgb, fit_pls, fit_rr, fit_lasso)

for (i in 1:length(ls_fit)) {
  summary(ls_fit[[i]])
  
  pred <- predict(ls_fit[[i]], df_charity.trainsub)
  pred[pred <= 0] <- 0
  
  # GOF
  k <- 54
  n <- length(resp_DAMT.train)
  resid <- resp_DAMT.train - pred
  sst <- sum((resp_DAMT.train - mean(resp_DAMT.train))^2)
  sse <- sum(resid^2)
  ssr <- sst - sse
  mae <- mean(abs(resid))
  mse <- mean((resid)^2)
  rmse <- sqrt(mse)
  r2 <- ssr / sst
  adjr2 <- 1 - (sse/(n-k-1)) / (sst/(n-1))
  print(paste0(cat(ls_fitnm[[i]], ": in-sample: ", "\n",
                   "sst: ", round(sst, 2), "\n",
                   "sse: ", round(sse, 2), "\n",
                   "ssr: ", round(ssr, 2), "\n", 
                   "mae: ", round(mae, 2), "\n", 
                   "mse: ", round(mse, 2), "\n",
                   "rmse: ", round(rmse, 2), "\n",
                   "r2: ", round(r2, 4), "\n",
                   "adjr2 ", round(adjr2, 4), "\n\n", sep="")))
  rm(k, n, resid, sst, sse, ssr, mae, mse, r2, adjr2)
  
  # Plot of actual vs. predicted
  plot1 <- ggplot(aes(x=actual,y=pred), data=data.frame(actual=resp_DAMT.train, pred=pred)) + 
    geom_point() + 
    geom_abline(color='red') + 
    #ggtitle(expression(atop("MLR (Stepwise Sel.)", atop("In-Sample Predict vs. Actuals")))) +
    ggtitle("In-Sample Predict vs. Actuals") +
    labs(x="Actual", y="Prediction") 
  
  png(filename=paste0("images/amt_", ls_fitnm[[i]], "_insample_pred.png"), 
      width=800, height=800, res=150)
  
  print(plot1)
  
  dev.off()
  
  pred <- predict(ls_fit[[i]], df_charity.validsub)
  pred[pred <= 0] <- 0
  
  # GOF
  k <- 54
  n <- length(resp_DAMT.valid)
  resid <- resp_DAMT.valid - pred
  sst <- sum((resp_DAMT.valid - mean(resp_DAMT.valid))^2)
  sse <- sum(resid^2)
  ssr <- sst - sse
  mae <- mean(abs(resid))
  mse <- mean((resid)^2)
  rmse <- sqrt(mse)
  r2 <- ssr / sst
  adjr2 <- 1 - (sse/(n-k-1)) / (sst/(n-1))
  print(paste0(cat(ls_fitnm[[i]], ": out-sample: ", "\n",
                   "sst: ", round(sst, 2), "\n",
                   "sse: ", round(sse, 2), "\n",
                   "ssr: ", round(ssr, 2), "\n", 
                   "mae: ", round(mae, 2), "\n", 
                   "mse: ", round(mse, 2), "\n",
                   "rmse: ", round(rmse, 2), "\n",
                   "r2: ", round(r2, 4), "\n",
                   "adjr2 ", round(adjr2, 4), "\n\n", sep="")))
  rm(k, n, resid, sst, sse, ssr, mae, mse, r2, adjr2)
  
  # Plot of actual vs. predicted
  plot2 <- ggplot(aes(x=actual,y=pred), data=data.frame(actual=resp_DAMT.valid, pred=pred)) + 
    geom_point() + 
    geom_abline(color='red') + 
    #ggtitle(expression(atop("MLR (Stepwise Sel.)", atop("Out-of-Sample Predict vs. Actuals")))) +
    ggtitle("Out-of-Sample Predict vs. Actuals") +
    labs(x="Actual", y="Prediction")
  
  png(filename=paste0("images/amt_", ls_fitnm[[i]], "_outsample_pred.png"), 
      width=800, height=800, res=150)
  
  print(plot2)
  
  dev.off()
  
  png(filename=paste0("images/amt_", ls_fitnm[[i]], "_sample_pred.png"), 
      width=1000, height=600, res=150)
  
  grid.arrange(plot1, plot2, ncol=2)
  
  dev.off()
}


###########################################################################
# Create Score
###########################################################################

#-------------------------------------------------------------------------
# Prep for test observations
#-------------------------------------------------------------------------

df_charity.test <- df_charity.clean[df_charity.clean[, "part"] == "test", ]

resp_DONR.test <- df_charity.test[, "donr"]
resp_DAMT.test <- df_charity.test[, "damt"]
flag_PART.test <- df_charity.test[, "part"]
flag_ID.test <- df_charity.test[, "ID"]

df_charity.test[, "donr"] <- NULL
df_charity.test[, "damt"] <- NULL
df_charity.test[, "part"] <- NULL
df_charity.test[, "ID"] <- NULL


#-------------------------------------------------------------------------
# Data imputation
#-------------------------------------------------------------------------

df_charity.imp <- df_charity.test

# Check for NA's, zero's and blanks
sum(is.na(df_charity.imp)) # 0
sum(df_charity.imp == 0) # 8214
sum(df_charity.imp == "") # 0

for (c in 1:ncol(df_charity.imp)) {
  nas <- sum(is.na(df_charity.imp[, c]))
  zero <- sum(df_charity.imp[, c] == 0)
  blank <- sum(df_charity.imp[, c] == "")
  print(paste(colnames(df_charity.imp)[c], nas, zero, blank))
}

#str(df_charity.imp, list.len=nrow(df_charity.imp))
rm(nas, zero, blank)

# Impute numeric
cols <- colnames(df_charity.imp[, sapply(df_charity.imp, is.numeric)])

for (c in cols) {
  if (sum(is.na(df_charity.imp[, c])) > 0) {
    nm <- paste(c, "IMP", sep="_")
    df_charity.imp[, nm] <- df_charity.imp[, c]
    med <- median(df_charity.imp[, nm], na.rm=TRUE)
    df_charity.imp[, nm][is.na(df_charity.imp[, nm])] <- med
    df_charity.imp[, c] <- NULL
  }
}

rm(cols, nm, med)

# Impute factor
cols <- colnames(df_charity.imp[, sapply(df_charity.imp, is.factor)])

for (c in cols) {
  if (sum(is.na(df_charity.imp[, c])) > 0) {
    nm <- paste(c, "IMP", sep="_")
    df_charity.imp[, nm] <- as.numeric(df_charity.imp[, c]) - 1
    mod <- which.max(df_charity.imp[, nm]) - 1
    df_charity.imp[, nm][is.na(df_charity.imp[, nm])] <- mod
    df_charity.imp[, nm] <- as.factor(df_charity.imp[, nm])
    df_charity.imp[, c] <- NULL
  }
}

rm(cols, nm, mod)

# Check for NA's, zero's and blanks
sum(is.na(df_charity.imp)) # 0
sum(df_charity.imp == 0) # 8214
sum(df_charity.imp == "") # 0

for (c in 1:ncol(df_charity.imp)) {
  nas <- sum(is.na(df_charity.imp[, c]))
  zero <- sum(df_charity.imp[, c] == 0)
  blank <- sum(df_charity.imp[, c] == "")
  print(paste(colnames(df_charity.imp)[c], nas, zero, blank))
}

#str(df_charity.imp, list.len=nrow(df_charity.imp))
rm(nas, zero, blank)

#-------------------------------------------------------------------------
# Data trimming
#-------------------------------------------------------------------------

df_charity.trim <- df_charity.imp

# Create trimmed variables
cols <- colnames(df_charity.trim[, sapply(df_charity.trim, is.numeric)])

for (c in cols) {
  min <- min(df_charity.trim[, c])
  max <- min(df_charity.trim[, c])
  p01 <- quantile(df_charity.trim[, c], c(0.01)) 
  p99 <- quantile(df_charity.trim[, c], c(0.99))
  if (p01 > min | p99 < max) {
    nm <- paste(c, "T99", sep="_")
    df_charity.trim[, nm] <- df_charity.trim[, c]
    t99 <- quantile(df_charity.trim[, c], c(0.01, 0.99))
    df_charity.trim[, nm] <- squish(df_charity.trim[, nm], t99)
    #df_charity.trim[, c] <- NULL
  }
}

rm(cols, min, max, p01, p99, nm, t99)


#-------------------------------------------------------------------------
# Data transformations
#-------------------------------------------------------------------------

df_charity.trans <- df_charity.trim

# Create variable transformations
cols <- colnames(df_charity.trans[, sapply(df_charity.trans, is.numeric)])

for (c in cols) {
  nm <- paste(c, "LN", sep="_")
  df_charity.trans[, nm] <- df_charity.trans[, c]
  df_charity.trans[, nm] <- (sign(df_charity.trans[, nm]) * log(abs(df_charity.trans[, nm])+1))
  #df_charity.trans[, c] <- NULL
}

rm(cols, nm)


#-------------------------------------------------------------------------
# Create dummy variables
#-------------------------------------------------------------------------

df_charity.dum <- df_charity.imp

cols <- colnames(df_charity.dum[, sapply(df_charity.dum, is.factor)])

for (c in cols) {
  if (length(unique(df_charity.dum[, c])) <= 10) {
    for(level in unique(df_charity.dum[, c])[1:length(unique(df_charity.dum[, c]))-1]) {
      nm <- paste("DUM", c, level, sep="_")
      df_charity.dum[, nm] <- ifelse(df_charity.dum[, c] == level, 1, 0)
    }
  }
}

rm(cols, nm, level)

#length(df_charity.imp[, grepl("DUM_", names(df_charity.imp))]) #0
df_charity.dum <- df_charity.dum[, grepl("DUM_", names(df_charity.dum))]
df_charity.dum[, "ID"] <- flag_ID.test


#-------------------------------------------------------------------------
# Final prep for train/validation observations
#-------------------------------------------------------------------------

df_charity.trans <- df_charity.trans[, sapply(df_charity.trans, is.numeric)]
df_charity.trans[, "ID"] <- flag_ID.test

df_charity.test <- merge(df_charity.trans, df_charity.dum, all=FALSE)
df_charity.test[, "ID"] <- NULL


#-------------------------------------------------------------------------
# Score Modelling
#-------------------------------------------------------------------------

# Response modelling
# Correct for any missing dummy variables in test set
for (c in cols_DONR) {
  if (!(c %in% colnames(df_charity.test))) { 
    print(c)
    df_charity.test[, c] <- 0
  }
}

# Predict campaign response using optimal model
pred_DONR <- 1 - predict(object=fit_glmnet, newdata=df_charity.test[, cols_DONR], type="prob")[, 1]

# check response rate prediction
sum(pred_DONR >= 0.5) # 400
sum(pred_DONR >= 0.5) / length(pred_DONR) # 0.1993024

# test response is lower than train or validation response, but still higher than typical response
# therefore, we will adjust mail rate provided over test set
trate <- 0.1 # typical response rate is 0.1
vrate <- 0.5 # validation response rate is 0.5
orate <- 0.635778 # optimal model validation rate (see above)
adjtest1 <- orate/(vrate/trate) # adjustment for mail yes
adjtest0 <- (1-orate)/((1-vrate)/(1-trate)) # adjustment for mail no
adjtest <- adjtest1/(adjtest1+adjtest0) # scale into a proportion
nmailtest <- round(nrow(df_charity.test) * adjtest, 0) # calculate number of mailings for test set 

cutofftest <- sort(pred_DONR, decreasing=TRUE)[nmailtest + 1] # set cutoff based on n.mail.test
cutofftest # 0.6552621

temp <- ifelse(pred_DONR > cutofftest, 1, 0) # mail to everyone above the cutoff
sum(temp) / length(temp) # 0.1624315

rm(trate, vrate, orate, adjtest1, adjtest0, adjtest, nmailtest)


# Donation amount modelling
# Correct for any missing dummy variables in test set
for (c in cols_DAMT) {
  if (!(c %in% colnames(df_charity.test))) { 
    print(c)
    df_charity.test[, c] <- 0
  }
}


# Subset data frame for test set based on true resp
#df_charity.test.true <- df_charity.test[pred_DONR >= 0.6552621, ]
#nrow(df_charity.test.true) # 327

#pred_DAMT <- predict(fit_xgb, df_charity.test[, cols_DAMT]) # xgb is superior in dealing with zero inf (incl. non-resp)
pred_DAMT <- predict(fit_rr, df_charity.test[, cols_DAMT]) # rr is superior in dealing with non-zero inf (excl. non-resp)
mean(pred_DAMT) # 13.57757

df_score <- data.frame(ID=flag_ID.test, 
                       donr.prob=pred_DONR, 
                       damt=pred_DAMT)
df_score <- df_score[order(-df_score[, "donr.prob"]),]

df_score[, "donr.bin"] <- ifelse(df_score[, "donr.prob"] >= 0.6552621, 1, 0)

df_score[, "score_all"] <- df_score[, "donr.prob"] * df_score[, "damt"] - 2

df_score[, "score_allscore"] <- 0
df_score[, "score_predprob"] <- 0
df_score[, "score_highprob"] <- 0
df_score[, "score_highval"] <- 0
for (r in 1:nrow(df_score)) {
  if (df_score[r, "score_all"] >= 0) {
    df_score[r, "score_allscore"] <- df_score[r, "score_all"]
  }
  if (df_score[r, "donr.bin"] == 1) {
    df_score[r, "score_predprob"] <- df_score[r, "donr.prob"] * df_score[r, "damt"] - 1
  }
  if (df_score[r, "donr.prob"] >= 0.99) {
    df_score[r, "score_highprob"] <- df_score[r, "donr.prob"] * df_score[r, "damt"] - 1
  }
  if (df_score[r, "damt"] >= 17.50) {
    df_score[r, "score_highval"] <- df_score[r, "donr.prob"] * df_score[r, "damt"] - 1
  }
}

sum(df_score[, "score_all"]) # 2604.805
sum(df_score[, "score_allscore"]) # 4696.197
sum(df_score[, "score_predprob"]) # 3785.715
sum(df_score[, "score_highprob"]) # 678.1682
sum(df_score[, "score_highval"]) # 205.176

nrow(df_score) # 2007
length(df_score[, "ID"][df_score[, "score_all"] >= 0]) # 756
length(df_score[, "ID"][df_score[, "donr.prob"] >= 0.6552621]) # 327
length(df_score[, "ID"][df_score[, "donr.prob"] >= 0.99]) # 50
length(df_score[, "ID"][df_score[, "damt"] >= 17.50]) # 42

# Write score
write.csv(df_score, "score.csv")

df_submit <- df_score[, c("donr.prob", "donr.bin", "damt")]
colnames(df_submit) <- c("chat.prob", "chat.bin", "yhat")

# Write submission
write.csv(df_submit, "submission.csv")


