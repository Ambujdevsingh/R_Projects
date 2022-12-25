getwd()
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
E=read.csv("C:\\ambuj\\IIM Indore\\R _lectures\\R_repository\\Healthcare.csv")
E=data.frame(E)
class(E)
summary(E$AGE)
E=E[order(E$AGE),]
E
library(ggplot2)
par("mar")
par(mar=c(4,4,2,2))
plot(E$AGE,E$Cost.of.Treatment,xlab="AGE",ylab="Expenditure",main ="Age vs Expenditure",col=E$AGE)

#corelation bw age and Expenditure 
cor(E$AGE,E$Cost.of.Treatment)

#fitting a linear regression model=[ cost_of_treatment=βo+β1*Age] 

lrm=lm(Cost.of.Treatment~AGE,E)
lrm
summary(lrm)
#crosschecking the value of intercept and β1
cov_xy=cov(E$AGE,E$Cost.of.Treatment)
var_x=var(E$AGE)
b1=cov_xy/var_x
b1
b0=mean(E$Cost.of.Treatment)-b1*mean(E$AGE)
b0

Predicted_Exp=predict(lrm)
N=cbind(E,Predicted_Exp)
N

#plot a line bw these predicted data points
n=nrow(E)
n
segments(E$AGE[1],Predicted_Exp[1],E$AGE[n],Predicted_Exp[n])


for(i in 1:n)
{
  segments(E$AGE[i],E$Cost.of.Treatment[i],E$AGE[i],Predicted_Exp[i])
  
}


# new patients with different age to predict expenditure
new_p=data.frame(AGE=c(51:57))
new_p
#predicting the Expenditure of new patients 
predict(lrm,new_p)
#predicting the Expenditure of new patients as per the confidence interval
predict(lrm,new_p,interval = "prediction",level=0.95)

# for getting confidence interval
confint(lrm,level=0.95)
