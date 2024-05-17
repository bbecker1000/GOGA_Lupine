## set working directory
setwd("C:/Documents and Settings/aforrestel/Desktop/MBB analysis")
nativity<-read.csv("Nativity for R.csv",header=TRUE)
names(nativity)
require(plotrix)
require(ggplot2)
require(nlme)
nativity$plot<-as.factor(nativity$PLOT)

##################################################################
################ANOVA for nativity####################
#########Seeing if plot effect is needed###########################
model<-lme(DIFFERENCE~TREATMENT * SITE, random = ~1 | plot, data=nativity, na.action="na.omit")
model2<-gls(DIFFERENCE~TREATMENT * SITE,data=nativity, na.action="na.omit")
summary(model)
summary(model2)
anova(model2,model)
############# since p = 1 simple model is fine, plot effect not needed
model<-aov(DIFFERENCE~TREATMENT + SITE + TREATMENT:SITE, data=nativity, na.action="na.omit")
summary(model)
######### strong treatment effect, no site effect
########### now do pairwise tukey tests to see which treatments are different
TukeyHSD(model)
## burn and mech both diff from control but not from each other
##################################################################
##################################################################

##################################################################
##### ANOVA assumption testing#####################################
ec1<-resid(model)
boxplot(ec1 ~ nativity$TREATMENT[1:24])
hist(nativity$DIFFERENCE)
qqnorm(nativity$DIFFERENCE)
##################################################################


################SUMMARY TABLE####################
pre<-subset(nativity,YEAR==2010)
post<-subset(nativity,YEAR==2011)
avg_nat_pre <- aggregate(pre$PER_NATIVE,by=list(pre$TREATMENT),mean,na.rm=TRUE)
avg_nat_post <- aggregate(post$PER_NATIVE,by=list(post$TREATMENT),mean,na.rm=TRUE)
stderr_nat_pre <- aggregate(pre$PER_NATIVE,by=list(pre$TREATMENT),std.error,na.rm=TRUE)
stderr_nat_post <- aggregate(post$PER_NATIVE,by=list(post$TREATMENT),std.error,na.rm=TRUE)
final.table<-cbind(avg_nat_pre,avg_nat_post$x,stderr_nat_pre$x,stderr_nat_post$x)
names(final.table)<-c("Treatment","Pre","Post","SE_Pre","SE_Post")
write.csv(final.table,"nativity_summary.csv")

################SLIGHTLY DIFFERENT SUMMARY TABLE####################
native.avg<-ddply(nativity, c("TREATMENT", "YEAR"), function(df)
return(c(native.avg=mean(df$PER_NATIVE), nat.se=std.error(df$PER_NATIVE))))

################BAR PLOT####################
avg.plot<-qplot(TREATMENT,native.avg,fill=factor(YEAR),data=native.avg, geom="bar",position="dodge",ylim=c(0,1),xlab="",ylab="")
dodge <- position_dodge(width=.9) 
avg.plot+scale_fill_grey()+ opts(panel.background = theme_rect(colour = NA))+geom_errorbar(aes(ymax=native.avg+nat.se, ymin=native.avg-nat.se), position=dodge,width=.5)

################ANOVA####################
anova(lm(nativity$DIFFERENCE ~ nativity$TREATMENT)) 



################ASSUMPTION CHECKING####################

################HISTOGRAMS####################

hist(Total,xlab="All Fuels", ylab="Frequency",main="All Fuels")
hist(TEN,xlab="10hr", ylab="Frequency",main="10hr")
hist(ONE.TO.HUND,xlab="1-100hr", ylab="Frequency",main="1-100hr")
hist(THOUS,xlab="1000hr", breaks=seq(0,20,by=2),ylab="Frequency",main="1000hr")
hist(ONE.TO.THOUS,xlab="1-1000hr", ylab="Frequency",main="1-1000hr")
hist(THOUS.ASIN,xlab="1000hr", ylab="Frequency",main="1000hr LOG TRANS")
hist(ONE.TO.THOUS.LOG,xlab="1-1000hr", ylab="Frequency",main="1-1000hr LOG TRANS")
hist(HLC,xlab="HLC", ylab="Frequency",main="HLC")

################FITTED VERSUS RESIDUAL PLOTS#################### 
## fit versus res plot FOR TOTAL FUELS
lmResult <- lm(Total ~ BA.DEAD.LIDE)
summary(lmResult)
fit <- fitted(lmResult)
res <- residuals(lmResult)
plot(res ~ fit);
plot(Total ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Total Fuel Load (kg/m2)",main="Total Fuel Load")
abline(lmResult)

## fit versus res plot FOR 10
lmResult <- lm(TEN ~ BA.DEAD.LIDE)
summary(lmResult)
fit <- fitted(lmResult)
res <- residuals(lmResult)
plot(res ~ fit);
identify(fit,res,labels=PLOT)

## fit versus res plot FOR 1-100
lmResult <- lm(ONE.TO.HUND ~ BA.DEAD.LIDE)
summary(lmResult)
fit <- fitted(lmResult)
res <- residuals(lmResult)
plot(res ~ fit);

## fit versus res plot FOR 1000
lmResult <- lm(THOUS.ASIN ~ BA.DEAD.LIDE)
summary(lmResult)
fit <- fitted(lmResult)
res <- residuals(lmResult)
plot(res ~ fit);
identify(fit,res,labels=PLOT)
hist(res,xlab="1000 res", ylab="Frequency")


## fit versus res plot FOR 1-1000
lmResult <- lm(ONE.TO.THOUS.LOG ~ BA.DEAD.LIDE)
summary(lmResult)
fit <- fitted(lmResult)
res <- residuals(lmResult)
plot(res ~ fit);
identify(fit,res,labels=PLOT)


################ANOVA####################
##TOTAL FUELS
lmResult <- lm(Total ~ BA.DEAD.LIDE)
summary(lmResult)
plot(Total ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Total Fuel Load (kg/m2)",main="Total Fuel Load")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,max(Total)*.99,paste("r-squared =",r2),cex=.7,pos=4)
text(0,max(Total)*.97,paste("slope p-value =",p),cex=.7,pos=4)

## 10 HOUR FUELS
lmResult <- lm(TEN ~ BA.DEAD.LIDE)
summary(lmResult)
plot(TEN ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="10 Hr Fuel Load (kg/m2)",main="10 Hr Fuels")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,1.35,paste("r-squared =",r2),cex=.7,pos=4)
text(0,1.31,paste("slope p-value =",p),cex=.7,pos=4)


## 1-100 HR FUELS
lmResult <- lm(ONE.TO.HUND ~ BA.DEAD.LIDE)
summary(lmResult)
plot(ONE.TO.HUND ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="1 - 100 Hr Fuel Load (kg/m2)",main="1 - 100 Hr Fuels")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,3.1,paste("r-squared =",r2),cex=.7,pos=4)
text(0,3.025,paste("slope p-value =",p),cex=.7,pos=4)

## 1000 hr fuels
lmResult <- lm(THOUS ~ BA.DEAD.LIDE)
summary(lmResult)
plot(THOUS ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="1000 Hr Fuel Load (kg/m2)",main="1000 Hr Fuels")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,15.5,paste("r-squared =",r2),cex=.7,pos=4)
text(0,15,paste("slope p-value =",p),cex=.7,pos=4)

## Litter
lmResult <- lm(Litter ~ BA.DEAD.LIDE)
summary(lmResult)
plot(Litter ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Litter (kg/m2)",main="Litter")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,15.5,paste("r-squared =",r2),cex=.7,pos=4)
text(0,15,paste("slope p-value =",p),cex=.7,pos=4)

## HLC
lmResult <- lm(HLC ~ BA.DEAD.LIDE)
summary(lmResult)
plot(HLC ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Height to Live Crown (m)",main="Height to Live Crown")
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(33,14.4,paste("slope p-value =",p),pos=4)

#####################################################
### SCATTER PLOTS ON ONE PAGE #######################
##TOTAL FUELS
lmResult <- lm(Total ~ BA.DEAD.LIDE)
summary(lmResult)
par(mfrow = c(3,2))
plot(Total ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Total Fuel Load (kg/m2)",main="Total Fuel Load")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,max(Total)*.99,paste("r-squared =",r2),pos=4)
text(0,max(Total)*.95,paste("slope p-value =",p),pos=4)

## 1-100 HR FUELS
lmResult <- lm(ONE.TO.HUND ~ BA.DEAD.LIDE)
summary(lmResult)
plot(ONE.TO.HUND ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="1 - 100 Hr Fuel Load (kg/m2)",main="1 - 100 Hr Fuels")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,3.05,paste("r-squared =",r2),pos=4)
text(0,2.92,paste("slope p-value =",p),pos=4)

## Litter
lmResult <- lm(Litter ~ BA.DEAD.LIDE)
summary(lmResult)
plot(Litter ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Litter (kg/m2)",main="Litter")
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(50,3,paste("r-squared =",r2),pos=2)
text(50,2.85,paste("slope p-value =",p),pos=2)

## 1000 hr fuels
lmResult <- lm(THOUS ~ BA.DEAD.LIDE)
summary(lmResult)
plot(THOUS ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="1000 Hr Fuel Load (kg/m2)",main="1000 Hr Fuels")
abline(lmResult)
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(0,15.3,paste("r-squared =",r2),pos=4)
text(0,14.4,paste("slope p-value =",p),pos=4)


#########################################################
## misc other code

anova(lm(nativity$DIFFERENCE ~ nativity$TREATMENT))

##attempting polynomial regression for litter
## Litter
lide2<-BA.DEAD.LIDE^2
lmResult <- lm(Litter ~ BA.DEAD.LIDE + lide2)
summary(lmResult)
plot(Litter ~ BA.DEAD.LIDE,xlab="Basal Area of Dead Tanoak (m2/ha)", ylab="Litter (kg/m2)",main="Litter")
r2<-round((summary(lmResult)$r.squared),3)
p<-round(summary(lmResult)$coefficients[2,4],3)
text(50,3,paste("r-squared =",r2),pos=2)
text(50,2.85,paste("slope p-value =",p),pos=2)

coefficients(lmResult)
coefficients(lmResult)[["(Intercept)"]]
plot(ONE.TO.THOUS ~ BA.DEAD.LIDE);
abline(lmResult);

#this cycles through diagnostic plots 
plot(lmResult)

ylim = c(0,max(Total))