## set working directory
setwd("C:/Documents and Settings/aforrestel/Desktop/MBB")
lupine<-read.csv("lupine R.csv",header=TRUE)
names(lupine)
require(nlme)
lupine$PLOT<-as.factor(lupine$Plot)

##################################################################
##### ANOVA assumption testing#####################################
par(mfrow = c(3,2))

hist(lupine$IMM_TRANS)
hist(lupine$ADULTS)
hist(lupine$RESP_TRANS)
hist(lupine$TOT_TRANS)

qqnorm(lupine$IMM_TRANS)
qqnorm(lupine$ADULTS)
qqnorm(lupine$RESP_TRANS)
qqnorm(lupine$TOT_TRANS)

 

##################################################################
##################################################################


################ANOVA for immatures####################
#########Seeing if plot effect is needed###########################
model<-lme(IMMATURE~Treatment * Site, random = ~1 | PLOT, data=lupine, na.action="na.omit")
model2<-gls(IMMATURE~Treatment * Site,data=lupine, na.action="na.omit")
summary(model)
summary(model2)
anova(model2,model)
############# since p = 0.5 simple model is fine, plot effect not needed
model<-aov(IMMATURE~Treatment + Site + Treatment:Site, data=lupine, na.action="na.omit")
summary(model)
######### strong treatment effect, marginally significant site effect
########### now do pairwise tukey tests to see which treatments are different
TukeyHSD(model)
## mech diff from burn & control; burn and control not different
##################################################################
##################################################################


################ANOVA for RESPROUTS####################
#########Seeing if plot effect is needed###########################
model<-lme(RESP_TRANS~Treatment * Site, random = ~1 | PLOT, data=lupine, na.action="na.omit")
model2<-gls(RESP_TRANS~Treatment * Site,data=lupine, na.action="na.omit")
summary(model)
summary(model2)
anova(model2,model)
############# since p = 1 simple model is fine, plot effect not needed
model<-aov(RESP_TRANS~Treatment + Site + Treatment:Site, data=lupine, na.action="na.omit")
summary(model)
######### strong treatment effect, NO site effect
########### now do pairwise tukey tests to see which treatments are different
TukeyHSD(model)
## burn and mech both diff from control but not from each other
##################################################################
##################################################################

################ANOVA for TOTAL LUPINE####################
#########Seeing if plot effect is needed###########################
model<-lme(TOT_TRANS~Treatment * Site, random = ~1 | PLOT, data=lupine, na.action="na.omit")
model2<-gls(TOT_TRANS~Treatment * Site,data=lupine, na.action="na.omit")
summary(model)
summary(model2)
anova(model2,model)
############# since p = 0.5 simple model is fine, plot effect not needed
model<-aov(TOT_TRANS~Treatment + Site + Treatment:Site, data=lupine, na.action="na.omit")
summary(model)
######### strong SITE effect, NO TREATMENT effect

################ANOVA for immatures - LUAL only####################
#########Seeing if plot effect is needed###########################
model<-lme(LUAL_I~Treatment * Site, random = ~1 | PLOT, data=lupine, na.action="na.omit")
model2<-gls(LUAL_I~Treatment * Site,data=lupine, na.action="na.omit")
summary(model)
summary(model2)
anova(model2,model)
############# since p = 0.99 simple model is fine, plot effect not needed
model<-aov(LUAL_I~Treatment + Site + Treatment:Site, data=lupine, na.action="na.omit")
summary(model)
######### strong treatment effect, marginally significant site effect
########### now do pairwise tukey tests to see which treatments are different
TukeyHSD(model)
## mech diff from burn & control; burn and control not different
##################################################################
##################################################################

