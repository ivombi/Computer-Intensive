R 3.6.1
#######################
### CIM Homework I ####
#######################
###### KUBAM, Ivo #####
##### SAHLI, Ilyas ####
### DAM, Nsoh Tanih ###
## ABUAZOUM, Mahmoud ##
#######################
setwd("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Computer Intensive\\External Datasets")
rm(list = ls())
# Q01
data(mtcars) #import the data
x <- mtcars$mpg #select the interesting variable
## (a) Calsical Confidence Interval
xhat <- mean(x) #mean of variable x
se.xhat <- sqrt(var(x)/length(x)) #standard error for xhat
ci.ub <- xhat + qnorm(0.975,lower.tail=TRUE)*se.xhat #upper bound
ci.lb <- xhat - qnorm(0.975,lower.tail=TRUE)*se.xhat #lower bound
ci_class <- c(ci.lb, ci.ub) #calsical confidence interval
## (b-1) Parametric bootstrap
set.seed(100)
n <- length(x)
B <- 1000
mx <- mean(x)
vx <-var(x)
x.bpar <-c(1:B)
for(i in 1:B)
  {
    boot.i<-rnorm(n,mx,sqrt(vx))
    x.bpar[i]<-mean(boot.i)
  }
se.bpar <- sqrt(var(x.bpar))
ci.bpar<-quantile(x.bpar,probs=c(0.025,0.975));ci.bpar
par(mfrow=c(1,2))
hist(x.bpar,probability=T,nclass=50,main="Parametric Bootstrap", xlab = expression(bar(theta)))
abline(v=ci.bpar[1], col = "blue")
abline(v=ci.bpar[2], col = "blue")
text(ci.bpar[1]-0.5,0.3, paste('lower'), col = "gray40")
text(ci.bpar[1]-0.5,0.27, format(round(ci.bpar[1], 3), nsmall = 3), col = "gray40")
text(ci.bpar[2]+0.5,0.3, paste('upper'), col = "gray40")
text(ci.bpar[2]+0.5,0.27, format(round(ci.bpar[2], 3), nsmall = 3), col = "gray40")
## (b-2) Nonparametric bootstrap
set.seed(100)
n <-length(x)
B <-1000
x.bnon<-c(1:B)
for (i in 1:B)
{
  boot.i<-sample(x,n,replace=TRUE)
  x.bnon[i]<-mean(boot.i)
}
se.bnon <- sqrt(var(x.bnon))
ci.bnon <- quantile(x.bnon, probs=c(0.025,0.975));ci.bnon
hist(x.bnon,probability=T,nclass=50, main="Nonparametric Bootstrap",
     xlab = expression(bar(theta)))
abline(v=ci.bnon[1], col = "red", lty = 2,lwd=2)
abline(v=ci.bnon[2], col = "red", lty = 2,lwd=2)
text(ci.bnon[1]-0.5,0.3, paste('lower'), col = "gray40")
text(ci.bnon[1]-0.5,0.27, format(round(ci.bnon[1], 3), nsmall = 3), col = "gray40")
text(ci.bnon[2]+0.5,0.3, paste('upper'), col = "gray40")
text(ci.bnon[2]+0.5,0.27, format(round(ci.bnon[2], 3), nsmall = 3), col = "gray40")
##########
setwd("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Computer Intensive\\External Datasets")
rm(list = ls())
##########
#Q02
gender<-as.factor(c(rep("Male",89),rep("Female",89)))
length(gender)
vote<-c(rep("Clinton",45),rep("Trump",44),rep("Clinton",54),rep("Trump",35))
length(vote)
tvg <- table(vote,gender);tvg
## (a)
RR <- (tvg[1,1]/sum(tvg[1,]))/(tvg[2,1]/sum(tvg[2,]));RR
se.logRR <- sqrt(1/tvg[1,1]+1/tvg[2,1]-1/sum(tvg[1,])-1/sum(tvg[2,]));se.logRR
lb <- exp(log(RR)-1.96*se.logRR);lb
ub <- exp(log(RR)+1.96*se.logRR);ub
ci.RR <- c(lb,ub);ci.RR
## (b)
set.seed(100)
n <- length(gender)
index <-c(1:n)
B<-1000
for(i in 1:B)
{
  boot.i<-sample(index ,n,replace=T)
  x.b<-vote[boot.i]
  y.b<-gender[boot.i]
  tvg <- table(x.b,y.b)
  RR[i] <- (tvg[1,1]/sum(tvg[1,]))/(tvg[2,1]/sum(tvg[2,]))
}
log.RR <- log(RR)
se.log.RR <- sqrt(var(log.RR))
se.log.RR
ci.RR <- exp(quantile(log.RR , probs = c(0.025,0.975)));ci.RR
##########
setwd("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Computer Intensive\\External Datasets")

rm(list = ls())
par(mfrow=c(1,1))
##########
#Q03
## (a) Spearman Correlation coefficient
sp.cor <- cor(mtcars$mpg,mtcars$hp,method="spearman");sp.cor
## (b-1) Parametric Bootstrap-Spearman
n<-length(mtcars$mpg)
rho <- cor(mtcars$mpg, mtcars$hp, method = c("spearman"));rho
mu1 <- mean(mtcars$mpg); s1 <- sqrt(var(mtcars$mpg)/n) ;mu1;s1
mu2 <- mean(mtcars$hp); s2 <- sqrt(var(mtcars$hp)/n) ;mu2;s2
mu <- c(mu1,mu2) # Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix
library(MASS)
bvn1 <- mvrnorm(n, mu = mu, Sigma = sigma )
y<-bvn1[,1]
x<-bvn1[,2]
index<-c(1:n)
B<-1000
corr.par<-y_boot<-x_boot<-c(1:B)
for(i in 1:B){
  index.i<-sample(index,n,replace = T)
  y_boot<-y[index.i]
  x_boot<-x[index.i]
  corr.par[i]<-cor(y_boot,x_boot,method = "spearman")
}
corr.parm<-mean(corr.par);corr.parm
corr.sd<-sd(corr.par);corr.sd
ci.spc <-quantile(corr.par,probs=c(0.025,0.975));ci.spc
hist(corr.par,probability=T,nclass=50,main="Spearman-parametric", xlab = expression(hat(rho)[S]))
abline(v=ci.spc[1], col = "blue", lty = 2,lwd=2)
abline(v=ci.spc[2], col = "blue", lty = 2,lwd=2)
abline(v=corr.parm[1], col = "red", lty = 1,lwd=2)
text(ci.spc[1]-0.02,4, paste('lower'), col = "gray40")
text(ci.spc[1]-0.02,4.4, format(round(ci.spc[1], 3), nsmall = 3), col = "gray40")
text(ci.spc[2]+0.02,4, paste('upper'), col = "gray40")
text(ci.spc[2]+0.02,4.4, format(round(ci.spc[2], 3), nsmall = 3), col = "gray40")
par(mfrow=c(1,2))
## (b-2) Non Parametric Bootstrap-Spearman
mpg.hp <- data.frame(mtcars[,c("mpg","hp")])
set.seed(100)
B <- 1000
n <- nrow(mpg.hp)
index <-c(1:n)
mhp.index <-NULL
spc.np<-c(1:B) #Spearman Correlation coefficient
for(i in 1:B) {
  index.b<-sample(index,n,replace=TRUE)
  mhp.index <-mpg.hp[index.b,]
  spc.np[i]<-cor(mhp.index$mpg,mhp.index$hp,method = "spearman")
}
se.spc <-sd(spc.np) #standard error for Spearman correlation-nonparametric bootstrap
ci.spc <-quantile(spc.np,probs=c(0.025,0.975))
hist(spc.np,probability=T,nclass=50,main="Spearman", xlab = expression(hat(rho)[S]))
abline(v=ci.spc[1], col = "blue", lty = 1,lwd=2)
abline(v=ci.spc[2], col = "blue", lty = 1,lwd=2)
text(ci.spc[1],10, paste('lower'), col = "gray40")
text(ci.spc[1],9, format(round(ci.spc[1], 3), nsmall = 3), col = "gray40")
text(ci.spc[2],10, paste('upper'), col = "gray40")
text(ci.spc[2],9, format(round(ci.spc[2], 3), nsmall = 3), col = "gray40")
## (c) Nonparametric Bootstrap-Pearson
mpg.hp <- data.frame(mtcars[,c("mpg","hp")])
set.seed(100)
B <- 1000
n <- nrow(mpg.hp)
index <-c(1:n)
mhp.index <-NULL
pc.np<-c(1:B) #Pearson Correlation coefficient
for(i in 1:B) {
  index.b<-sample(index,n,replace=TRUE)
  mhp.index <-mpg.hp[index.b,]
  pc.np[i]<-cor(mhp.index$mpg,mhp.index$hp,method = "pearson")
}
se.pc <-sd(pc.np) #standard error for Pearson correlation-nonparametric bootstrap
ci.pc <-quantile(pc.np,probs=c(0.025,0.975))
hist(pc.np,probability=T,nclass=50,main="Pearson", xlab = expression(hat(rho)[P]))
abline(v=ci.pc[1], col = "red", lty = 2,lwd=2)
abline(v=ci.pc[2], col = "red", lty = 2,lwd=2)
text(ci.pc[1],8, paste('lower'), col = "gray40")
text(ci.pc[1],7, format(round(ci.pc[1], 3), nsmall = 3), col = "gray40")
text(ci.pc[2],8, paste('upper'), col = "gray40")
text(ci.pc[2],7, format(round(ci.pc[2], 3), nsmall = 3), col = "gray40")
##########
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW01")
rm(list = ls())
par(mfrow=c(1,1))
##########
#Q4
library(ggplot2)
## (a) sleep data and difference
data(sleep)
Y <- sleep$extra[sleep$group==1]#the first measurement
X <- sleep$extra[sleep$group==2]#the second measurement
D = Y - X
gr <- NULL
gr[sleep$group==1] <- 'y'
gr[sleep$group==2] <- 'X'
sleep$gr <- gr
ggplot(sleep, aes(x=sleep$gr, y=sleep$extra, color=sleep$group))+
  geom_boxplot()+
  theme(legend.position="none")+
  labs(x = "Measurements", y = "Extra")
## (b)
mD <- mean(D)
lo <- mD+qt(p=0.025,df=9,lower.tail = T)*(sd(D)/sqrt(length(D)))
up <- mD+qt(p=0.025,df=9,lower.tail = F)*(sd(D)/sqrt(length(D)))
ci.mD <- cbind(lo,up)
## (c) Non parametric bootstrap
m1<-sleep$extra[1:10]
m2<-sleep$extra[11:20]
n<-10
index<-c(1:n)
B<-1000
diff.boot<-c(1:B)
for(i in 1:B)
{
  index.i<-sample(index,n,replace=T)
  m1.boot<-m1[index.i]
  m2.boot<-m2[index.i]
  diff.boot[i]<-t.test(m1.boot,m2.boot,paired = TRUE,conf.level = 0.95)$estimate
}
diff_mean <- mean(diff.boot)
sd_diff <-sd(diff.boot) #standard error for D-nonparametric bootstrap
##Quantile confidence interval
ci.Quan <-quantile(diff.boot,probs=c(0.025,0.975));ci.Quan # confidence interval for D-nonparametric boo
## t confidence interval
ci.t<-cbind(diff_mean-qt(.95,9)*sd_diff,diff_mean+qt(.95,9)*sd_diff);ci.t
## Standard normal confidence interval
ci.std<-cbind(diff_mean+qnorm(0.95,0.389)*sd_diff,diff_mean-qnorm(0.95,0.389)*sd_diff)*sd_diff;ci.std
ci.std <- c(-3.042,-0.909)
par(mfrow=c(1,1))
hist(diff.boot,probability=T,nclass=50,main=expression(D[i]), xlab = expression(bar(D)))
abline(v=diff_mean[1], col = "red", lty = 1,lwd=2)
abline(v=ci.Quan[1], col = 3, lty = 2,lwd=2)
abline(v=ci.Quan[2], col = 3, lty = 2,lwd=2)
abline(v=ci.t[1], col = 4, lty = 2,lwd=2)
abline(v=ci.t[2], col = 4, lty = 2,lwd=2)
abline(v=ci.std[1], col = 6, lty = 2,lwd=2)
abline(v=ci.std[2], col = 6, lty = 2,lwd=2)
text(ci.Quan[1]-0.1,0.9, format(round(ci.Quan[1], 3), nsmall = 3), col = 3)
text(ci.Quan[2]-0.1,0.9, format(round(ci.Quan[2], 3), nsmall = 3), col = 3)
text(ci.t[1]+0.12,0.8, format(round(ci.t[1], 3), nsmall = 3), col = 4)
text(ci.t[2]+0.12,0.8, format(round(ci.t[2], 3), nsmall = 3), col = 4)
text(ci.std[1]+0.12,1.1, format(round(ci.std[1], 3), nsmall = 3), col = 6)
text(ci.std[2]-0.12,1.1, format(round(ci.std[2], 3), nsmall = 3), col = 6)
legend('top', legend=c(expression(bar(D)),'t CI','Quantile CI', 'Normal CI'),
        col=c("red", 3,4,6), lty=c(1,2,2,2), cex=1, bty = "n")


R 3.6.1
#######################
### CIM Homework II ###
#######################
###### KUBAM, Ivo #####
##### SAHLI, Ilyas ####
### DAM, Nsoh Tanih ###
## ABUAZOUM, Mahmoud ##
#######################
############################################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW02")
rm(list = ls())
par(mfrow=c(1,1))
11############################################
# Q01
####HOMEWORK TWO#######
###Question 01###
help(chickwts)
str(chickwts)
levels(chickwts$feed)
require(stats); require(graphics)
boxplot(weight ~ feed, data = chickwts, col = c(2:7), main = "Chickwts dataset",
        ylab = "Weight at six weeks (gm)")
casein <- chickwts$weight[chickwts$feed=="casein"]
horsebean <- chickwts$weight[chickwts$feed=="horsebean"]
linseed <- chickwts$weight[chickwts$feed=="linseed"]
meatmeal <- chickwts$weight[chickwts$feed=="meatmeal"]
soybean <- chickwts$weight[chickwts$feed=="soybean"]
sunflower <- chickwts$weight[chickwts$feed=="sunflower"]
##Summary table
install.packages("fastR")
library(fastR)
library(xtable)
stats=favstats(weight~feed, data=chickwts)
stats[,-1]
xtable(stats)
summary(chickwts);sd(chickwts[,1]);length(chickwts[,1])
chickTest<-chickwts
#Question 1
#a)
r=5
n=65
anova <- anova(fm1 <- lm(weight ~ feed, data = chickwts))
model <- aov(weight ~ feed, data = chickwts)
Fobs=anova$'F value'[1] ##obs
Fobs
crpoin <- qf(0.95,r,n)
#b)
#Non parametric bootstrap Ftest using SSD AND SSTD
h <- length(horsebean)
s <- length(soybean)
m.horse <- mean(horsebean)
m.casein <- mean(casein)
m.linseed <- mean(linseed)
m.meatmeal <- mean(meatmeal)
m.sunflower <- mean(sunflower)
m.soybean <- mean(soybean)
avg.m <- mean(chickTest$weight)
m.horse.tild <- horsebean-m.horse+avg.m
m.horse.tild <- horsebean-m.horse+avg.m
m.casein.tild <- casein-m.casein+avg.m
m.linseed.tild <- linseed-m.linseed+avg.m
m.meatmeal.tild <- meatmeal-m.meatmeal+avg.m
m.sunflower.tild <- sunflower-m.sunflower+avg.m
m.soybean.tild <- soybean-m.soybean+avg.m
set.seed(10)
B <- 1000
anova.Fstat <- anova.msr <- anova.mse <- anova.ssr <- anova.sse <- c(1:B)
for (b in 1:B){
  horse.b <- sample(m.horse.tild,length(horsebean),replace = TRUE)
  casein.b <- sample(m.casein.tild,length(casein),replace = TRUE)
  linseed.b <- sample(m.linseed.tild,length(linseed),replace = TRUE)
  meatmeal.b <- sample(m.meatmeal.tild,length(meatmeal),replace = TRUE)
  sunflower.b <- sample(m.sunflower.tild,length(sunflower),replace = TRUE)
  soybean.b <- sample(m.soybean.tild,length(soybean),replace = TRUE)
  #creating the dataset
  chick2 <- as.data.frame(rbind(cbind(horse.b,replicate(length(horsebean),"horsebean")),
                                cbind(casein.b,replicate(length(casein),"casein")),
                                cbind(linseed.b,replicate(length(linseed),"linseed")),
                                cbind(meatmeal.b,replicate(length(meatmeal),"meatmeal")),
                                cbind(sunflower.b,replicate(length(sunflower),"sunflower")),
                                cbind(soybean.b,replicate(length(soybean),"soybean"))))
  colnames(chick2) <- c("weight","feed")
  anova1 <- anova(lm(as.numeric(chick2$weight)~as.factor(chick2$feed)))
  anova.Fstat[b] <- anova1$'F value'[1]
  anova.msr[b] <- anova1$'Mean Sq'[1]
  anova.ssr[b] <- anova1$'Sum Sq'[1]
  anova.mse[b] <- anova1$'Mean Sq'[2]
  anova.sse[b] <- anova1$'Sum Sq'[2]
}
F.m <- mean(anova.Fstat)
msr.m <- mean(anova.msr)
mse.m <- mean(anova.mse)
ssr.m <- mean(anova.ssr)
sse.m <- mean(anova.sse)
ssr.m;sse.m;msr.m;mse.m;msr.m/mse.m;F.m
Pmc <- (1+sum(abs(anova.Fstat)> abs(Fobs)))/(B+1);Pmc
#c)Semi parametric
chick2c <- read.csv('xdata.csv');chick2c <- chick2c[,-1]
names(chick2c) <- c('weight.tild', 'feed')
str(chick2c)
fit.anova=aov(weight.tild~feed,data = chick2c)
ei <- fit.anova$residuals
n <- length(chick2c$weight.tild)
set.seed(10)
B <- 1000
anova.Fstat2 <- anova.msr <- anova.mse <- anova.ssr <- anova.sse <- c(1:B)
for (b in 1:B){
  e.boot <- sample(ei,size = n,replace = T)
  y.boot <- fit.anova$coefficients[1]+fit.anova$coefficients[2]+fit.anova$coefficients[3]+
    fit.anova$coefficients[4]+fit.anova$coefficients[5] +fit.anova$coefficients[6] + e.boot
  x.boot <- chick2c$feed
  anova2 <- anova(lm(y.boot~x.boot))
  13length(y.boot)
  anova.Fstat2[b] <- anova2$'F value'[1]
  anova.msr[b] <- anova2$'Mean Sq'[1]
  anova.ssr[b] <- anova2$'Sum Sq'[1]
  anova.mse[b] <- anova2$'Mean Sq'[2]
  anova.sse[b] <- anova2$'Sum Sq'[2]
}
F_mean <- mean(anova.Fstat2)
msr_mean <- mean(anova.msr)
mse_mean <- mean(anova.mse)
ssr_mean <- mean(anova.ssr)
sse_mean <- mean(anova.sse)
ssr_mean;sse_mean;msr_mean;mse_mean;F_mean
pf(F_mean, df1=r-1, df2=n-r, lower.tail=FALSE)
#d)
par(mfrow=c(1,3))
hist(anova.Fstat,main = "NP Bootstrap F stat dist",
     breaks = 'Scott',probability = T,ylim = c(0,1), xlab='F-statistic')
lines(c( crpoin,crpoin), c(0:1), col="red", lwd=3, lty=2)
hist(anova.Fstat2,main = "Semi Bootstrap F stat dist", xlab='F-statistic',
     breaks = 'Scott',probability = T,ylim = c(0,1))
lines(c( F.m,F.m), c(0:1), col="blue", lwd=3, lty=2)
lines(c( crpoin,crpoin), c(0:1), col="red", lwd=3, lty=2)
x <- rf(100000, df1 = 5, df2 = 65)
hist(x,breaks = 'Scott', freq = FALSE, xlim = c(0,3),
     ylim = c(0,1),xlab = '', main = "Classical F dist")
lines(c( crpoin,crpoin), c(0:1), col="red", lwd=3, lty=2)
par(mfrow=c(1,1))
#e) difference between horsebean and soybean
obs_diff<-t.test(soybean,horsebean)$statistic
B<-1000
t.boot<-c(1:B)
for (b in 1:B){
  h.b<-sample(m.horse.tild,h,replace = T)
  s.b<-sample(m.soybean.tild,s,replace = T)
  t.boot[b]<-t.test(h.b,s.b)$statistic
}
t.boot.m <- mean(t.boot)
sd_err<-sqrt(var(t.boot))
hist(t.boot,probability = T,nclass=50, xlab = c('Diff'),
     main = 'Difference between horsebean and soybean')
ci.bpar<-quantile(t.boot,probs=c(0.05,0.95))
abline(v=t.boot.m, col = "red", lty = 1,lwd=2)
abline(v=ci.bpar[1], col = "blue", lty = 2,lwd=2)
abline(v=ci.bpar[2], col = "blue", lty = 2,lwd=2)
text(ci.bpar[1]-0.5,0.3, paste('lower'), col = "gray40")
text(ci.bpar[1]-0.5,0.27, format(round(ci.bpar[1], 3), nsmall = 3), col = "gray40")
text(ci.bpar[2]+0.5,0.3, paste('upper'), col = "gray40")
text(ci.bpar[2]+0.5,0.27, format(round(ci.bpar[2], 3), nsmall = 3), col = "gray40")
############################################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW02")
rm(list = ls())
par(mfrow=c(1,1))
############################################
#####QUESTION 2
resp<-as.factor(c(rep(0,90),rep(1,172),rep(0,346),rep(1 ,173)))
levels(resp)<-c("Smoking-No","Smoking-Yes")
trt<-as.factor(c(rep(1,90),rep(1,172),rep(2,346),rep(2,173)))
Smoking<-table(trt,resp)
row.names(Smoking)=c("MI-case","Control")
Smoking
str(Smoking)
#a
n_mi<-Smoking[1,1]+Smoking[1,2]
prob_mi<-Smoking[1,2]/n_mi
n_ctrl<-Smoking[2,1]+Smoking[2,2]
prob_con<-Smoking[2,2]/n_ctrl
N<-sum(Smoking)
diff=prob_mi-prob_con
overal_probability=(Smoking[1,2]+Smoking[2,2])/(n_mi+n_ctrl)
#From the central limit theorem
sde<-sqrt((prob_con*prob_mi)*(1/n_mi+1/n_ctrl))
#confidence interval
lo<-prob_mi-1.96*sde
up<-prob_mi+1.96*sde
ci.piclas <- c(lo,up);ci.piclas
diff(ci.piclas)
#b)Bootstrap for prob
B<-10000
prob_mi_boot<-c(1:B)
for (i in 1:B){
  smoke1<-rbinom(n_mi,1,prob_mi)
  prob_mi_boot[i]<-length(which(smoke1==1))/length(smoke1)
}
mean(prob_mi_boot)
sd<-sqrt(var(prob_mi_boot))
#CI
lo1<-prob_mi-1.96*sd
up1<-prob_mi+1.96*sd
ci.piZ <- c(lo1,up1);ci.piZ
diff(ci.piZ)
par(mfrow=c(1,2))
hist(prob_mi_boot,probability = T, breaks = 100,
     xlab = expression(hat(pi)[M]), main = "Parametric Bootstrap")
abline(v=ci.piZ[1], col = "blue", lty = 2,lwd=2)
abline(v=ci.piZ[2], col = "blue", lty = 2,lwd=2)
abline(v=prob_mi[1], col = "red")
text(ci.piZ[1]-0.03,15, paste('lower'), col = "gray40")
text(ci.piZ[1]-0.03,19, format(round(ci.piZ[1], 3), nsmall = 3), col = "gray40")
text(ci.piZ[2]+0.03,15, paste('upper'), col = "gray40")
text(ci.piZ[2]+0.03,19, format(round(ci.piZ[2], 3), nsmall = 3), col = "gray40")
#percential CI
ci.piP <-quantile(prob_mi_boot,c(0.05,0.95));ci.piP
diff(ci.piP)
#c)
data_mi<-cbind(resp,trt)
data_mi<-as.data.frame(data_mi)
str(data_mi)
n<-length(data_mi$resp)
index<-c(1:n)
B<-1000
diff.boot<-c(1:B)
for(i in 1:B)
{
  index.i<-sample(index,n,replace=T)
  resp.boot<-resp[index.i]
  treat.boot<-trt[index.i]
  d<-cbind.data.frame(treat.boot,resp.boot)
  Smoking=table(d)
  n_smoke<-Smoking[1,2]+Smoking[2,2]
  prob_mi<-Smoking[1,2]/n_mi
  prob_con<-Smoking[2,2]/n_ctrl
  diff.boot[i]<-prob_mi-prob_con
}
mean(diff.boot)
diff.m<-mean(diff.boot)
ci.piQ <-quantile(diff.boot,c(0.05,0.95));ci.piQ
hist(diff.boot,probability = T, breaks = 100,
     xlab = expression(hat(pi)[M]-hat(pi)[C]), main = "Non-parametric Bootstrap")
abline(v=ci.piQ[1], col = "blue", lty = 2,lwd=2)
abline(v=ci.piQ[2], col = "blue", lty = 2,lwd=2)
abline(v=diff.m[1], col = "red")
text(ci.piQ[1]-0.03,5, paste('lower'), col = "gray40")
text(ci.piQ[1]-0.03,6, format(round(ci.piQ[1], 3), nsmall = 3), col = "gray40")
text(ci.piQ[2]+0.03,5, paste('upper'), col = "gray40")
text(ci.piQ[2]+0.03,6, format(round(ci.piQ[2], 3), nsmall = 3), col = "gray40")
#d)
n<-length(data_mi$resp)
index<-c(1:n)
B<-1000
index<-c(1:n)
B<-1000
diff.boot<-c(1:B)
for(i in 1:B)
{
  index.i<-sample(index,n,replace=T)
  resp.boot<-data_mi$resp[index.i]
  treat.boot<-data_mi$trt[index.i]
  Smoking<-table(treat.boot,resp.boot)
  n_smoke<-Smoking[1,2]+Smoking[2,2]
  prob_mi<-Smoking[1,2]/n_smoke
  prob_con<-Smoking[2,2]/n_smoke
  diff.boot[i]<-prob_mi-prob_con
}
mean(diff.boot)
p_value<-(1+sum(abs(diff.boot==0)))/(B+1);p_value
############################################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW02")
rm(list = ls())
par(mfrow=c(1,1))
############################################
###Question 3#####
require(bootstrap)
help(law)
plot(law$LSAT,law$GPA,xlab = 'Average Score on a National Law Test - LSAT',
     ylab = 'Average GPA')
#a)
x <-cor.test(law$LSAT,law$GPA,method = "pearson")
corr_law <- x$estimate;corr_law
#b)
sd_err<-sqrt((1-corr_law^2)/(length(law$LSAT)-2));sd_err
x$conf.int
#c Non Parametric
n<-length(law$LSAT)
index<-c(1:n)
B<-1000
cor.boot<-c(1:B)
for(i in 1:B)
{
  index.i<-sample(index,n,replace=T)
  x.b<-law$LSAT[index.i]
  y.b<-law$GPA[index.i]
  cor.boot[i]<- cor(x.b, y.b, method = c("pearson"))
}
cor.boot.npm <- mean(cor.boot)
se.corr<-sqrt(var(cor.boot));se.corr
ci.cor.np <-quantile(cor.boot,c(0.05,0.95));ci.cor.np
par(mfrow=c(1,2))
hist(cor.boot,probability = T, breaks = 100, ylim = c(0,3.5), xlim = c(0,1.3),
     xlab = expression(hat(rho)), main = "Non-parametric Bootstrap")
abline(v=ci.cor.np[1], col = "blue", lty = 2,lwd=2)
abline(v=ci.cor.np[2], col = "blue", lty = 2,lwd=2)
abline(v=cor.boot.npm[1], col = "red")
text(ci.cor.np[1]-0.1,3, paste('lower'), col = "gray40")
text(ci.cor.np[1]-0.1,3.3, format(round(ci.cor.np[1], 3), nsmall = 3), col = "gray40")
text(ci.cor.np[2]+0.1,3, paste('upper'), col = "gray40")
text(ci.cor.np[2]+0.1,3.3, format(round(ci.cor.np[2], 3), nsmall = 3), col = "gray40")
#Parametric
# Parameters for bivariate normal distribution

n<-length(law$LSAT);n

rho <- cor(law$LSAT, law$GPA, method = c("spearman"));rho
mu1 <- mean(law$LSAT); s1 <- sqrt(var(law$LSAT)/n) ;mu1;s1
mu2 <- mean(law$GPA); s2 <- sqrt(var(law$GPA)/n) ;mu2;s2

mu <- c(mu1,mu2) # Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix
library(MASS)
bvn1 <- mvrnorm(n, mu = mu, Sigma = sigma )

y<-bvn1[,1]
x<-bvn1[,2]


B<-1000
cor.boot<-c(1:B)
index<-c(1:n)
for(i in 1:B)
{
  bvn1 <- mvrnorm(n, mu = mu, Sigma = sigma )
  
  y<-bvn1[,1]
  x<-bvn1[,2]
  index.b<-sample(index,n,replace = T)
  y1<-y[index.b]
  x1<-x[index.b]
  cor.boot[i]<-cor.test(x1,y1)$estimate
}


cor.boot.pm <- mean(cor.boot)
se.corr<-sqrt(var(cor.boot));se.corr
ci.cor.p <-quantile(cor.boot,c(0.05,0.95));ci.cor.np
hist(cor.boot,probability = T, breaks = 100, ylim = c(0,2),
     xlab = expression(hat(rho)), main = "parametric Bootstrap")
abline(v=ci.cor.p[1], col = "blue", lty = 2,lwd=2)
abline(v=ci.cor.p[2], col = "blue", lty = 2,lwd=2)
abline(v=cor.boot.pm[1], col = "red")
text(ci.cor.p[1]-0.1,1.5, paste('lower'), col = "gray40")
text(ci.cor.p[1]-0.1,1.8, format(round(ci.cor.p[1], 3), nsmall = 3), col = "gray40")
text(ci.cor.p[2]+0.1,1.5, paste('upper'), col = "gray40")
text(ci.cor.p[2]+0.1,1.8, format(round(ci.cor.p[2], 3), nsmall = 3), col = "gray40")

#e)parametric test
n<-length(law$LSAT)
B<-1000
coeff.boot<-c(1:B)
for (b in 1:B){
  lsat<-rnorm(n,m.lsat,sd_lsat) #Check distribution
  gpa<-rnorm(n,m.gpa,sd_gpa)
  cor.boot[b]<-cor.test(lsat,gpa)$estimate
}
mean(cor.boot)
p_value<-(1+sum(abs(cor.boot)>corr_law))/(B+1);p_value
#Non parametric test
n<-length(law$LSAT)
B<-1000
coeff.boot<-c(1:B)
for (b in 1:B){
  x.boot<-sample(law$LSAT,n)
  y.boot<-sample(law$GPA,n)
  coeff.boot[b]<-cor(x.boot,y.boot)
}
mean(coeff.boot)
p_value<-(1+sum(abs(coeff.boot)>corr_law))/(B+1);p_value
############################################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW02")
rm(list = ls())
par(mfrow=c(1,1))
############################################
#Q4
help(cars)
plot(cars$speed,cars$dist, xlab = 'Speed', ylab = 'Distance')
18#A
mod <- lm(cars$dist~cars$speed);mod
mod$coefficients
##B##
(50-mod$coefficients[1])/mod$coefficients[2]
##C##
#c)i non Parametric
set.seed(10)
B<-1000
theta.boot<-c(1:B)
n<-length(cars$speed)
index<-c(1:n)
for (i in 1:B){
  index.i<-sample(index,size=n,replace = T)
  mydata<-cars[index.i,]
  b1<-summary(lm(mydata$dist~mydata$speed))$coeficient[1]
  mydata.new<-subset(mydata,mydata$dist<50)
  theta.boot[i]<-max(mydata)
}
s.np.m <- mean(theta.boot);s.np.m
ci.s.np <- quantile(theta.boot,probs=c(0.05,0.95));ci.s.np
#ci) Semi Parametric
fitlm<-lm(cars$dist~cars$speed)
ei<-fitlm$residuals
n<-length(cars$speed)
B<-1000
theta.boot<-c(1:B)
for (i in 1:B){
  e.boot<-sample(ei,size = n,replace = T)
  y.boot<-fitlm$coefficients[1]+fitlm$coefficients[2]*cars$speed+e.boot
  x.boot<-cars$speed
  mydata<-as.data.frame(cbind(y.boot,x.boot))
  mydata.new<-subset(mydata,mydata$y.boot<50)
  theta.boot[i]<-max(mydata.new$x.boot)
}
ci.s.sp <- quantile(theta.boot,probs = c(0.05,0.95));ci.s.sp
#d)
n<-length(cars$speed)
#Non Parametric single
index<-c(1:n)
index.i<-sample(index,size=n,replace = T)
y.boot<-cars$dist[index.i]
x.boot<-cars$speed[index.i]
fitlmboot<-lm(y.boot~x.boot)
RSS.np<-sum((cars$dist-fitlmboot$fit)^2);RSS.np
#semi parametric single
e.boot<-sample(ei,size = n,replace = T)
y.boot<-fitlm$coefficients[1]+fitlm$coefficients[2]*cars$speed+e.boot
x.boot<-cars$speed
fit.boot<-lm(y.boot~x.boot)
RSS.sp<-sum((cars$dist-fit.boot$fit)^2);RSS.sp
theta.0<-(RSS.np-RSS.sp)/n
#Boostrap to compare residuals for both method and calculate theta
19fitlm<-lm(cars$dist~cars$speed)
ei<-fitlm$residuals
B<-1000
n<-length(cars$speed)
RSS.np<-RSS.sp<-theta<-beta0<-beta1<-c(1:B)
index<-c(1:n)
for(i in 1:B)
{
  index.i<-sample(index,size=n,replace = T)
  y.boot<-cars$dist[index.i]
  x.boot<-cars$speed[index.i]
  fitlmboot<-lm(y.boot~x.boot)
  RSS.np[i]<-sum((cars$dist-fitlmboot$fit)^2)
  e.boot<-sample(ei,size = n,replace = T)
  y.boot<-fitlm$coefficients[1]+fitlm$coefficients[2]*cars$speed+e.boot
  x.boot<-cars$speed
  fit.boot<-lm(y.boot~x.boot)
  beta0[i]<-fit.boot$coefficients[1]
  beta1[i]<-fit.boot$coefficients[2]
  RSS.sp[i]<-sum((cars$dist-fit.boot$fit)^2)
  theta[i]<-(RSS.np[i]-RSS.sp[i])/n
}
RSS.combine<-cbind(RSS.sp,RSS.np)
boxplot(RSS.combine)
mean(RSS.np)
mean(RSS.sp)
hist(theta,nclass=50)
ci<-quantile(theta,probs=c(0.025,0.975))
ci
lines(c(ci[1],ci[1]),c(0,500),col=2,lwd=3)
lines(c(ci[2],ci[2]),c(0,500),col=2,lwd=3)
lines(c(theta.0,theta.0),c(0,500),col=4,lwd=3)
#predicting
beta0<-mean(beta0)
beta1<-mean(beta1)
pred.21<-beta0 + beta1*21



R 3.6.1
#######################
## CIM Homework III ###
#######################
###### KUBAM, Ivo #####
##### SAHLI, Ilyas ####
### DAM, Nsoh Tanih ###
## ABUAZOUM, Mahmoud ##
#######################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW03")
rm(list = ls())
# Q01
#Trimmed means are robust estimators of central tendency.
#To compute a trimmed mean, we remove a predetermined amount of observations on each
#side of a distribution, and average the remaining observations
#Using trimmed means confers two advantages:
#1trimmed means provide a better estimation of the location of the bulk of the
#observations than the mean when sampling from asymmetric distributions;
#2 the standard error of the trimmed mean is less affected by outliers and
#asymmetry than the mean, so that tests using trimmed means can have more power
#than tests using the mean.
help("airquality")
hist(airquality$Wind)
x<-airquality$Wind
summary(x)
x_mean<-mean(airquality$Wind);x_mean #####mean of observed
x_median <- median(airquality$Wind);x_median #######median of observed
x_trim <- mean(airquality$Wind,trim = 0.1);x_trim ######trimmed mean of observed
#a) Bootstrap
n<-length(airquality$Wind)
B<-1000
boot.mean<-c(1:B)
boot.median<-c(1:B)
boot.tri.mean<-c(1:B)
for(i in 1:B){
  boot.mean[i]<-mean(sample(x,n,replace = T))
  boot.median[i]<-median(sample(x,n,replace = T))
  boot.tri.mean[i]<-mean(sample(x,n,replace = T),trim = 0.1)
}
### bias, mse and var for mean ###
mu=mean(boot.mean);mu ########mean bootstraap
bias_mu=mu-x_mean;bias_mu
var_mu=var(boot.mean);var_mu
MSE_mu=var_mu+(bias_mu)^2;MSE_mu ###########MSE
####bias , mse and var for median
me=mean(boot.median);me ########mean bootstraap
bias_me=me-x_median;bias_me
var_me=var(boot.median);var_me
MSE_me=var_me+(bias_me)^2;MSE_me ###########MSE
####bias , mse and var for trimmed mean
mean_tri=mean(boot.tri.mean);mean_tri ########mean bootstraap
bias_tri=mu-x_trim;bias_tri
var_tri=var(boot.tri.mean);var_tri
MSE_tri=var_tri+(bias_tri)^2;MSE_tri ###########MSE
par(mfrow=c(1,3))
hist(boot.mean,col=0,nclass=30,probability=T,main = "Sample mean")
lines(c(x_mean,x_mean),c(-1,5),lwd=2,col="red")
lines(c(mu,mu),c(-1,5), lty = 2,lwd=2, col="blue")
hist(boot.median,col=0,nclass=30,probability=T, main = "Sample median ")
lines(c(x_median,x_median),c(0,5),lwd=2,col="red")
lines(c(me,me),c(0,5), lty = 2,lwd=2,col="blue")
hist(boot.tri.mean,col=0,nclass=30,probability=T, main = "Sample trimed mean")
lines(c(x_trim,x_trim),c(0,5),lwd=2,col="red")
lines(c(mean_tri,mean_tri),c(0,5), lty = 2,lwd=2,col="blue")
#b) Jackknife
x<-airquality$Wind
n<-length(airquality$Wind)
b.mean.jack<-b.median.jack<-b.tri.mean.jack<-c(1:n)
for (i in 1:n){
  x.jack<-x[-c(i)]
  b.mean.jack[i]<-mean(x.jack)
  b.median.jack[i]<-median(x.jack)
  b.tri.mean.jack[i]<-mean(x.jack,trim = 0.1)
}
mean.jack<-mean(b.mean.jack)
median.jack<-mean(b.median.jack)
tri.jack<-mean(b.tri.mean.jack)
###bias, mse and variance of mean
mean.jack<-mean(b.mean.jack);mean.jack ####jacknife means
bias.jack <-(n-1)*(mean.jack-x_mean);bias.jack ### jack bias mean
var.jack <- var(b.mean.jack);var.jack ### jack variance
MSE.jack <- var.jack + (bias.jack)^2 ;MSE.jack ### jack MSE
###bias, mse and variance of median
median.jack<-mean(b.median.jack);median.jack ####jacknife median
bias.jack.median <-(n-1)*(median.jack-x_median);bias.jack.median ### jack bias median
var.jack.median <- var(b.median.jack);var.jack.median ### jack variance median
MSE.jack.median <- var.jack.median + (bias.jack.median)^2;MSE.jack.median ### jack MSE median
###bias, mse and variance of trimmed
median.tri<-mean(b.tri.mean.jack);median.tri ####jacknife trimmed means
bias.jack.tri <-(n-1)*(tri.jack-x_trim);bias.jack.tri ### jack bias trimmed mean
var.jack.tri <- var(b.tri.mean.jack);var.jack.tri ### jack variance trimmed
MSE.jack.tri <- var.jack.tri + (bias.jack.tri)^2;MSE.jack.tri ### jack MSE trimmed
par(mfrow=c(1,2))
hist(b.mean.jack,col=0,nclass=30,probability=T,main = "Jackknife mean")
lines(c(x_mean,x_mean),c(0,50),lwd=2,col="red")
lines(c(mean.jack,mean.jack),c(0,50), lty = 2,lwd=2,col="blue")
hist(b.tri.mean.jack,col=0,nclass=30,probability=T, main = "Jackknife trimed mean")
lines(c(x_trim,x_trim),c(0,50),lwd=2,col="red")
lines(c(tri.jack,tri.jack),c(0,50), lty = 2,lwd=2,col="blue")
#######################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW03")
rm(list = ls())
par(mfrow=c(1,1))

#######################
#Q2
beetle<-read.table("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Computer Intensive\\External Datasets\\beetle.txt", header=FALSE,na.strings=".", dec=".", strip.white=TRUE,col.names=c("dose","y","z","n"))
str(beetle)
beetle
plot(beetle$dose,beetle$y/beetle$n,xlab="DOSE",ylab="prop. of dead beetles")


state<-(c(rep(1,27),rep(0,2),rep(1,23),rep(0,7),rep(1,19),rep(0,9),rep(1,13),rep(0,14),
          rep(1,7),rep(0,23),rep(1,2),rep(0,29),rep(1,1),rep(0,29),rep(1,0),rep(0,29),rep(1,26),rep(0,4),
          rep(1,24),rep(0,6),rep(1,34),rep(0,9),rep(1,15),rep(0,14),
          rep(1,4),rep(0,29),rep(1,4),rep(0,24),rep(1,0),rep(0,32),rep(1,0),rep(0,31)))
dose<-(c(rep(47.06,29),rep(52.99,30),rep(56.91,28),rep(60.84,27),rep(64.76,30),rep(68.69,31),
         rep(72.61,30),rep(76.54,29),rep(47.06,30),rep(52.99,30),rep(56.91,43),rep(60.84,29),
         rep(64.76,33),rep(68.69,28),rep(72.61,32),rep(76.54,31)))
dim(state)

beetle2<-cbind(state,dose)
colnames(beetle2)<-c("state","dose")
beetle2<-as.data.frame(beetle2)
beetle2$state<-as.factor(beetle2$state)
str(beetle2)

######2a
proc nlmixed data=beetle2 corr cov;
parms beta0=14 beta1=-0.5;
eta = beta0 + beta1*dose;
expeta = exp(eta);
p = expeta/(1+expeta);
model reponse ~ binomial(1,p);
*estimate '1/beta1' 1/beta1;
run;

###2c
#c)parametric
B<-1000
p.sample<-sum(beetle$z)/sum(beetle$n);p.sample
p.b<-0.15
n<-length(beetle2$state)
prob.boot<-c(1:B)
beta0.b<-beta1.b<-c(1:B)
ed15.boot<-c(1:B)
for(b in 1:B)
{
pos.boot<-(rbinom(n,1,p.sample))
beetle.boot<-as.data.frame(cbind(pos.boot,beetle2$dose))
fit.boot<-glm(pos.boot~dose,family=binomial(link = "logit"))
beta0.b<-fit.boot$coefficients[1]
beta1.b<-fit.boot$coefficients[2]
ed15.boot[b]<-(-1.734601-beta0.b)/(beta1.b)
}
mean.ed15.boot <- mean(ed15.boot)
quantile(ed15.boot,probs = c(0.025,0.975))
hist(ed15.boot, main = "Distribution of ED15 Non-Parametric bOOtstrap Samples")
lines(c(mean.ed15.boot,mean.ed15.boot),c(0,10000),lwd=2,col="blue")
quantile(ed15.boot,probs = c(0.025,0.975))
?rbinom
#non parametric
B<-1000
n<-length(beetle2$state)
index<-c(1:n)
ed15.boot2<-c(1:B)
for (i in 1:B){
  index.i<-sample(index,size = n,replace = T)
  y<-beetle2$state[index.i]
  x<-beetle2$dose[index.i]
  fit.boot<-glm(y~x,family=binomial(link = "logit"))
  beta0.b<-fit.boot$coefficients[1]
  beta1.b<-fit.boot$coefficients[2]
  ed15.boot2[i]<-(-1.734601-beta0.b)/(beta1.b)
}
quantile(ed15.boot2,probs = c(0.025,0.975))
###2d
#d)Standard error
#i)bootstrap standard error
std_err<-sqrt(var(ed15.boot2));std_err
hist(ed15.boot2)
lines(c(mean.ed15.boot,mean.ed15.boot),c(0,10000),lwd=2,col="blue")

#jackknife
B<-1000
n<-length(beetle2$state)
ed15.jack<-c(1:n)
index<-c(1:n-1)
for (i in 1:n){
x.jack<-beetle2[-c(i),]
index.i<-sample(index,size = n,replace = T)
y<-x.jack$state[index.i]
x<-x.jack$dose[index.i]
fit.boot<-glm(y~x,family=binomial(link = "logit"))
beta0.b<-fit.boot$coefficients[1]
beta1.b<-fit.boot$coefficients[2]
ed15.jack[i]<-(-1.734601-beta0.b)/(beta1.b)
}
ed15.jack.mean<-mean(ed15.jack)
ed15.jack.var <- var(ed15.jack);ed15.jack.var
ed15.jack.stderr<-sqrt((n-1)/n*sum((ed15.jack-ed15.jack.mean)^2));ed15.jack.stderr
ed <- (n-1)*(ed15.jack.mean - (66.659))
bias.jack <-(n-1)*(ed15.jack.mean-x_mean);bias.jack
hist
#####2e
#E Hypothesis
#i)Parametric
t.obs<-summary(glm(state~dose,data = beetle2,family = binomial(link = "logit")))$coefficients[2,3]
b.obs<-summary(glm(state~dose,data = beetle2,family = binomial(link = "logit")))$coefficients[2,1]
beta0<-14.45699
eta<-beta0
prob.i<-exp(eta)/(1+exp(eta))
prob.i<-rep(prob.i,16)
B<-1000
prob.boot<-matrix(0,16,B)
test.stat<-coeff.boot<-matrix(0,2,B)
pos.boot<-neg.boot<-c(1:16)
for(b in 1:B)
{
for(i in 1:16)
{
pos.boot[i]<-sum(rbinom(beetle$n[i],1,prob.i[i]))
}
neg.boot<-beetle$n-pos.boot
fit.boot<-glm(cbind(pos.boot,neg.boot)~dose,family=binomial(link = "logit"),data = beetle)
summary(fit.boot)$coefficients[2,3]
#prob.boot[,b]<-fit.boot$fit
coeff.boot[,b]<-fit.boot$coefficients[2]
test.stat[,b]<-summary(fit.boot)$coefficients[2,3]
}
p.value1<- (1+sum(abs(coeff.boot))>abs(b.obs))/(B+1);p.value1
p.value1<- (1+sum(test.stat)>t.obs)/(B+1);p.value1
#non parametric
t.obs<-summary(glm(state~dose,data = beetle2,family = binomial(link = "logit")))$coefficients[2,3]
b.obs<-summary(glm(state~dose,data = beetle2,family = binomial(link = "logit")))$coefficients[2]
B<-1000
n<-length(beetle2$state)
beta1.b<-t.stats.b<-c(1:B)
index<-c(1:n)
for (i in 1:B){
index.i<-sample(index,size = n,replace = T)
y<-beetle2$state[index.i]
fit.boot<-glm(y~dose,family=binomial(link = "logit"),data = beetle2)
beta1.b[i]<-summary(fit.boot)$coefficients[2]
t.stat.b[i]<-summary(fit.boot)$coefficients[2,3]
}
p.value2<- (1+sum(beta1.b)>b.obs)/(B+1);p.value2
p.value2<- (1+sum(t.stat.b)>t.obs)/(B+1);p.value2
#######################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW03")
rm(list = ls())
par(mfrow=c(1,1))
#######################
#Q3
table1data <- c(80,83,78,88,78,93,88,78,97,103,99,104,103,95,95,97,99)
doses <- as.factor(c(1,1,1,1,1,2,2,2,3,3,2,4,4,4,5,5,5))
data<-as.data.frame(cbind(table1data,doses))
colnames(data)<-c("response","doses")
data$doses<-as.factor(data$doses)
str(data)
fitanova<-anova(lm(data$response~data$doses,data = data));fitanova
write.table(data,file = "data1")
#a)
######SAS######
proc import datafile="D A T A file" dbms=csv
out=data1 replace;
run;
data data1;
set data1;
drop var1;
run;
proc glm data = data1;
class doses;
model response = doses;
13*means b /deponly;
contrast 'Compare 1st & 2nd grp' doses 1 -1 0 0 0;
contrast 'Compare 1st & 3rd grp' doses 1 0 -1 0 0;
contrast 'Compare 1st & 4th grp' doses 1 0 0 -1 0;
contrast 'Compare 1st & 5th grp' doses 1 0 0 0 -1;
run;
quit;
#b)
mean_dose1<-mean(data$response[data$doses=="1"]);mean_dose1
mean_dose2<-mean(data$response[data$doses=="2"]);mean_dose2
mean_dose3<-mean(data$response[data$doses=="3"]);mean_dose3
mean_dose4<-mean(data$response[data$doses=="4"]);mean_dose4
mean_dose5<-mean(data$response[data$doses=="5"]);mean_dose5
mean_avg<-mean(data$response);mean_avg
z_tilde1<-data$response[data$doses=="1"]-mean_dose1+mean_avg
z_tilde2<-data$response[data$doses=="2"]-mean_dose2+mean_avg
z_tilde3<-data$response[data$doses=="3"]-mean_dose3+mean_avg
z_tilde4<-data$response[data$doses=="4"]-mean_dose4+mean_avg
z_tilde5<-data$response[data$doses=="5"]-mean_dose5+mean_avg
#dose 2 ##
ratio=mean_dose2/mean_dose1
m=length(z_tilde1)
n=length(z_tilde2)
B<-1000
t.boot<-c(1:B)
for (i in 1:B) {
x.b<-sample(z_tilde1,m,replace = T)
y.b<-sample(z_tilde2,n,replace = T)
t.boot[i]<-mean(y.b)/mean(x.b)
}
pmc<-(1+sum(abs(t.boot)>abs(ratio)))/(B+1);pmc
#dose 3##
ratio=mean_dose3/mean_dose1
m=length(z_tilde1)
n=length(z_tilde3)
B<-1000
t.boot<-c(1:B)
for (i in 1:B) {
x.b<-sample(z_tilde1,m,replace = T)
y.b<-sample(z_tilde3,n,replace = T)
t.boot[i]<-mean(y.b)/mean(x.b)
}
mean(t.boot)
pmc<-(1+sum(abs(t.boot)>abs(ratio)))/(B+1);pmc
#dose 4 ##
ratio=mean_dose4/mean_dose1
m=length(z_tilde1)
n=length(z_tilde4)
B<-1000
t.boot<-c(1:B)
for (i in 1:B) {
x.b<-sample(z_tilde1,m,replace = T)
y.b<-sample(z_tilde4,n,replace = T)
t.boot[i]<-mean(y.b)/mean(x.b)
}
pmc<-(1+sum(abs(t.boot)>abs(ratio)))/(B+1);pmc
## dose 5##
ratio=mean_dose5/mean_dose1
m=length(z_tilde1)
n=length(z_tilde5)
B<-1000
t.boot<-c(1:B)
for (i in 1:B) {
x.b<-sample(z_tilde1,m,replace = T)
y.b<-sample(z_tilde5,n,replace = T)
t.boot[i]<-mean(y.b)/mean(x.b)
}
pmc<-(1+sum(abs(t.boot)>abs(ratio)))/(B+1);pmc
#######################
setwd("/Volumes/Salma/UHasselt/3rd/CIM/Homework/CIMHW/LaTeX/HW03")
rm(list = ls())
par(mfrow=c(1,1))
#######################
#Q4
x<-c(11,6,14,13,13,6,10,9,17,12)
# H0: u=13
# H1: u not = 13
#classical
t.test(x,mu=13);
#non parametric
t.obs<-t.test(x,mu=13)$statistic
z = mean(x)
n=length(x)
z.tilde = x-z +13
B=1000
t.boot<-c(1:B)
for (b in 1:B){
z.b = sample(z.tilde,x,replace = T)
t.boot[b]<-t.test(z.b,mu=13)$statistic
}
mean(t.boot)
p_value<-(1+sum(abs(t.boot)>abs(t.obs)))/(B+1);p_value
#parametric
mu=13
sdv<-sqrt(var(x))
n=length(x)
t.obs<-t.test(x,mu=13)$statistic
B<-1000
t.boot1<-c(1:B)
for (b in 1:B){
z.b = rnorm(n,mu,sdv)
t.boot1[b]<-t.test(z.b,mu=13)$statistic
}
mean(t.boot1)
p_value<-(1+sum(abs(t.boot1)>abs(t.obs)))/(B+1);p_value
#b upper limit
#parametric
upb_range<-seq(7.6, 13.4, by=.1)
mu<-mean(x)
std<-(sqrt(var(x)/n))
B<-1000
t.intv<-c(1:B)
for (b in 1:B){
z.b = rnorm(n,mu,sdv)
t.intv[b]<-round(t.test(z.b)$conf.int[2],digits = 1)
}
sum=0
for (i in t.intv){
if (i < 13.6 & i> 7.5) {
sum=sum+1
} }
p_value<-(sum)/(B+1);p_value

