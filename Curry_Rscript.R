curry<-read.csv("C:/Users/sony/Desktop/2016spring/stat3080/prelim data/Curry.csv",header = TRUE,check.names = FALSE)
#appendix 
#basic graphs
head(curry)
par(mfcol=c(1,2))
hist(curry$FGA)
qqnorm(curry$FGA, main = "Curry$FGA")
hist(curry$`3P%`)
qqnorm(curry$`3P%`, main = "Curry$`3P%`")
hist(curry$`FG%`)
qqnorm(curry$`FG%`, main = "Curry$`FG%`")
hist(curry$PTS)
qqnorm(curry$`PTS`, main = "Curry$`PTS`")
hist(curry$FG)
qqnorm(curry$`FG`, main = "Curry$`FG`")
hist(curry$FTA)
qqnorm(curry$`FTA`, main = "Curry$`FTA`")
hist(curry$Winning)
qqnorm(curry$`Winning`, main = "Curry$`winning")
hist(curry$`+/-`)
qqnorm(curry$`+/-`, main = "Curry$`Efficiency`")
hist(curry$`AST`)
qqnorm(curry$`AST`, main = "Curry$`AST`")
hist(curry$`TOV`)
qqnorm(curry$`TOV`, main = "Curry$`TOV`")

#Results & Analysis
#Hypothesis Testing
#1.One-sample test: One-Sample t-test
t.test(curry$FGA,mu=15,alt="greater")
#2.Two-sample dependent test: Paired t-test
t.test(curry$`3P%`,curry$`FG%`,paired=TRUE,alt="greater")
#3.Two-sample independent tests: 
# F-test for variances
pts1 <- curry$PTS[1:40]
pts2 <- curry$PTS[41:80]
var.test(pts1,pts2,alt="less")
#4.wo-sample independent tests: 
# Wilcox Rank-Sum test:
FTA1 <- curry$FTA[1:40]
FTA2 <- curry$FTA[41:80]
wilcox.test(FTA1,FTA2, correct = FALSE, alternative = "less")
#5.categorical test
#1-sample proportion z-test
prop.test(sum(curry$FG),sum(curry$FGA),p=0.45,alternative = "greater",correct = FALSE)
#6.categorical test
# two sample proportion z test
win <- sum (curry$Winning > 0)
eff <-sum(curry$`+/-` > 0)
p1 <- win/80
p2 <- eff/80
phat <- c(p1,p2)
phat
n<-c(80,80)
prop.test(n*phat,n,alternative = "two.sided",correct = FALSE)



#MLR
smp<-floor(0.8*nrow(curry))
set.seed(7)
split<-sample(seq_len(nrow(curry)),size=smp)
train<-curry[split, ]
test<-curry[-split, ] 
#model selection
model1<-lm(train$`+/-`~train$`FG%`+train$PTS+train$AST+train$TOV+train$`FG%`:train$PTS
             +train$`FG%`:train$AST+train$`FG%`:train$TOV+train$PTS:train$AST+train$PTS:train$TOV
             +train$AST:train$TOV+train$`FG%`:train$PTS:train$AST+train$`FG%`:train$PTS:train$TOV+
             train$AST:train$PTS:train$TOV+train$`FG%`:train$AST:train$TOV+train$`FG%`:train$PTS:train$AST:train$TOV)
summary(model1)
model2 <-update(model1,~.- train$`FG%`:train$PTS:train$AST:train$TOV)
summary(model2)
model3 <-update(model2,~.- train$`FG%`:train$PTS:train$AST)
summary(model3)
model4 <-update(model3,~.- train$`FG%`:train$PTS:train$TOV)
summary(model4)
model5 <-update(model4,~.- train$PTS:train$AST:train$TOV)
summary(model5)
model6 <-update(model5,~.- train$`FG%`:train$AST:train$TOV)
summary(model6)
model7 <-update(model6,~.- train$AST:train$TOV)
summary(model7)
model8 <-update(model7,~.- train$PTS:train$TOV)
summary(model8)
model9<-update(model8,~.- train$PTS:train$AST)
summary(model9)
model10<-update(model9,~.-train$`FG%`:train$AST)
summary(model10)
model11<-update(model10,~.-train$`FG%`:train$PTS)
summary(model11)
model12<-update(model11,~.-train$`FG%`:train$TOV)
summary(model12)
model13<-update(model12,~.- train$PTS)
summary(model13)

#check assumptions of MLR:
fit<-model13
qqnorm(fit$residuals, main = "Check Normality")
qqline(fit$residuals)

plot(1:length(fit$residuals),fit$residuals,main="residual vs order of observations")
abline(h=0)

ts<-ts(fit$residuals)
plot(ts,main="time series of residu")
abline(0,0)

#prediction:
#change FG% to FGP because the symbol '%' will cause string matching problem 
colnames(train)[6]<-"FGP"
mod<-lm(`+/-` ~  FGP+ AST + TOV, data = train)
mod
new<-data.frame(FGP=test$`FG%`,AST=test$AST,TOV=test$TOV)
pre<-predict(mod,newdata=new,interval = "predict")
plot(test$`+/-`,ylim = c(-20,50), main = "testing dataset prediction ")
lines(1:16,pre[,2])
lines(1:16,pre[,3])

#other statistics
#R^2
summary(model13)$r.squared
#SSE
anova(model13)[4,2]



#time series
first<-curry[1:72,18]
last<-curry[73:80,18]

curry.ts<-ts(first, start=0)
plot(curry.ts,main="Steph Curry's Efficiency during 2014-2015 Season", ylab="Efficiency", xlab="Matches over time")
acf(curry.ts,main="ACF Curry Efficiency")
pacf(curry.ts, main="PACF Curry Efficiency")


#kernel smoothing
ks1<-ksmooth(1:length(curry.ts),curry.ts,"normal",bandwidth = 3)
plot(curry.ts,main="Kernel smoothing of bandwidth 3", ylab = "Curry's Efficiency")
lines(ks1,col="red",lwd=2)

ks2<-ksmooth(1:length(curry.ts),curry.ts,"normal",bandwidth = 6)
plot(curry.ts,main="Kernel smoothing of bandwidth 6", ylab = "Curry's Efficiency")
lines(ks2,col="Green",lwd=2)

ks3<-ksmooth(1:length(curry.ts),curry.ts,"normal",bandwidth = 12)
plot(curry.ts,main="Kernel smoothing of bandwidth 12", ylab = "Curry's Efficiency")
lines(ks3,col="Blue",lwd=2)


#differencing and model building
diff<-diff(curry.ts)
plot(diff, main=" Differencing Efficiency Time Series")
acf(diff,main="ACF Diff Efficiency")
#q=1
pacf(diff,main="PACF Diff Efficiency")
#p=3
source("C:/Users/sony/Desktop/2016spring/stat3080/sarima.R")
ar<-sarima(diff,3,0,0)
ma<-sarima(diff,0,0,1)
arma<-sarima(diff,3,0,1)
c(ar$AIC,ma$AIC,arma$AIC)
c(ar$BIC,ma$BIC,arma$BIC)
#MA(1) is the possible model for Curry's efficiency  time series because it has 
#the smallest AIC and BIC;p-values for Ljung-Box statistic are significant in MA(1)

acf(ma$fit$residuals,main="acf of MA Residuals")
Box.test(ma$fit$residuals,lag=1)
plot.ts(ma$fit$residuals, main="Time Series of MA residuals")
ma$fit

acf(ar$fit$residuals,main="acf of AR Residuals")
Box.test(ar$fit$residuals,lag=1)
plot.ts(ar$fit$residuals, main="Time Series of AR residuals" )
ar$fit

acf(arma$fit$residuals,main="acf of ARMA Residuals")
Box.test(arma$fit$residuals,lag=1)

plot.ts(arma$fit$residuals, main="Time Series of ARMA residuals")
arma$fit

#prediction:
source("C:/Users/sony/Desktop/2016spring/stat3080/sarima.for.R")
par(mfcol=c(1,1))
sarima.for(first,8,0,0,1,0,0,0,8)
values <- c(rep(NA,71),last)
lines(values,col="red")







