####Read in the Data##########
setwd("C:/Users/sidvy/Desktop/Stat4123Project")
data=read.table("cereal.txt", header=T)
data <- data[-c(5, 21, 58), ]
mfr=factor(data$mfr)
type=factor(data$type)
name=factor(data$name)
shelf=factor(data$shelf)
calories=data$calories
protein=data$protein
fat=data$fat
sodium=data$sodium
fiber=data$fiber
carbo=data$carbo
sugars=data$sugars
potass=data$potass
vitamins=factor(data$vitamins)
weight=data$weight
cups=data$cups
rating=data$rating

#######Exploratory Analysis#########
plot(shelf,sugars)


######Construct Correlation Matrix########
X=cbind(sugars,protein,fat,sodium,fiber,carbo,calories,weight,cups,potass,mfr,type,shelf,vitamins)
hat=X%*%solve(t(X)%*%X)%*%t(X)
diag(hat)
cor(X)
pairs(data)
#drop potass because of high correlation with fiber
ls2=lm(rating~mfr+shelf+calories+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+cups)
library(car)
vif(ls)

######fitting initial model############
ls=lm(rating~mfr+type+shelf+calories+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+cups)
ls2=lm(rating~type+shelf+calories+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+cups)
anova(ls,ls2)
#we decided to drop mfr using the anova test
ls=lm(rating~type+shelf+calories+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+cups)
ls2=lm(rating~type+shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+cups)
anova(ls,ls2)
# we decide to drop calories because there is evidence of multicollinearity between weight...we see this because of a drastic change
# in the weight coefficient between the 2 models

ls=lm(rating~type+shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+cups)
ls2=lm(rating~type+shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight)
anova(ls,ls2)
# we decide to drop cups from our anova test above

ls=lm(rating~type+shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight)
ls2=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight)
anova(ls,ls2)
# we decide to drop type from our model using the anova test above

ls=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight)
ls2=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+shelf*sugars)
anova(ls,ls2)
# we see that the interaction term between shelf and sugars is significant and decide to keep it

ls=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+weight+shelf*sugars)
ls2=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+shelf*sugars)
anova(ls,ls2)

# we decide to drop weight from the model using the anova test from above

ls=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+shelf*sugars)
ls2=lm(rating~protein+fat+sodium+fiber+carbo+sugars+vitamins)
anova(ls,ls2)
#we see that shelf is signficant and decide to keep it as evident in the anova test above

####further analysis
par(mfrow=c(3,2))
plot(shelf,sugars)
plot(shelf,carbo)
plot(shelf,fat)
plot(shelf,protein)
plot(shelf,sodium)
plot(shelf,fiber)

#####final model########
ls=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+shelf*sugars)
vif(ls)


#######Residual Analysis#######

###residual analysis of model1
res=ls$residuals
fits=ls$fitted.values
par(mfrow=c(2,2))
plot(fits, res)
abline(h=0)
qqnorm(res)
qqline(res)
shapiro.test(res)
result=boxcox(ls)
result$x[result$y==max(result$y)]
### outlier detection
#standardized residual
MSres=(summary(ls)$sigma)^2
stand=res/sqrt(MSres)
which(abs(stand)>3)
#leverage
#hat matrix
X=cbind(1,shelf,protein,fat,sodium,fiber,carbo,sugars,vitamins,shelf:sugars)
hat=X%*%solve(t(X)%*%X)%*%t(X)
lev=diag(hat)
lev
# cut-off point
p=dim(X)[2]
n=dim(X)[1]
2*p/n # leverage
which(lev>2*p/n)
#2,4,11,54,65
#cook's distance
cooks=cooks.distance(ls)
which(cooks>1) #none
# DEFIT
dffits=dffits(ls)
# cut-off point
2*sqrt(p/n) # for DFFITS
which(abs(dffits)>2*sqrt(p/n)) #4,7,11,29,43,44,54,65,68
# COVRATIO
# command
covratio=covratio(ls)
# cut-off point
1+3*p/n; 1-3*p/n # COVRATIO
which(covratio>1+3*p/n) #1,2,37,38,43,53,62,65,67,69
which(covratio<1-3*p/n) #8,32

####Model Done#######
ls=lm(rating~shelf+protein+fat+sodium+fiber+carbo+sugars+vitamins+shelf*sugars)
summary(ls)
anova(ls)
vif(ls)
