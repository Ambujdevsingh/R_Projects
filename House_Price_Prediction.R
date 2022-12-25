library(dplyr)
library(ggplot2)
library(summarytools)
getwd()
house=read.csv("C:\\ambuj\\IIM Indore\\R _lectures\\R_repository\\House_Price.csv")
head(house,5)
str(house)
attributes(house)
summary(house$age)
summary(house,c(1:10))
summary(house)
ind=which(is.na(house$n_hos_beds))
house<-house[-ind,]
summary(house)
par("mar")
par(mar=c(4,4,2,3))
hist(house$crime_rate)   
pairs(~price+crime_rate+n_hot_rooms+rainfall, house)
barplot(table(house$airport))
barplot(table(house$waterbody))
boxplot(house$air_qual)
boxplot(house$price)
#observations 
hist(house$rainfall)
plot(house$rainfall,house$price)
#null values 
sum(is.na(house))
house$Meandist<-(house$dist1+house$dist2+house$dist3+house$dist4)/4
house$Meandist
#replacing individual columns to aggregate columns
house<-house[,-c(7,8,9,10)]
summary(house)
str(house)
#removing unnecessary columns 
barplot(table(house$bus_ter))
#this var $bus_ter has only one value "yes" this has no significance on the model 
#removing column bus_ter
house<-house[,-14]
summary(house)
# treating outliers by quantile range 

quantile(house$n_hot_rooms,0.99)
uv<-3*quantile(house$n_hot_rooms,0.99)
house$n_hot_rooms[house$n_hot_rooms>uv]<-uv
summary(house$n_hot_rooms)

#crime rate 
plot(house$price,house$crime_rate)
pairs(~price+crime_rate,house)
#crime rate having log relationship converting it to linear 
house$crime_rate<-log(1+house$crime_rate)

plot(house$price,house$crime_rate)
### converting Categorical variable to numerical variable to generate regression model 
house$airport<-ifelse(house$airport=="YES",1,0)
str(house)
# generating multi linear model 
lm=lm(price~.,house)
summary(lm)
str(house)
dim(house)
#                      Coefficients Std_Error  t_value Pr(>|t|)    
#(Intercept)              -7.216880   5.326241  -1.355 0.176064    
#crime_rate                0.115019   0.352433   0.326 0.744296    
#resid_area               -0.040063   0.057978  -0.691 0.489896    
#air_qual                -21.494264   5.927222  -3.626 0.000318 
#room_num                  4.051451   0.430822   9.404  < 2e-16 
#age                      -0.005038   0.013673  -0.368 0.712663    
#teachers                  1.024767   0.122998   8.332 8.40e-16 
#poor_prop                -0.572978   0.053341 -10.742  < 2e-16 
#airport                   1.243745   0.458556   2.712 0.006921  
#n_hos_beds                0.341900   0.152726   2.239 0.025635 
#n_hot_rooms               0.085927   0.082620   1.040 0.298852    
#waterbodyLake and River  -0.885415   0.785009  -1.128 0.259923    
#waterbodyNone            -0.219899   0.649244  -0.339 0.734984    
#waterbodyRiver           -0.419696   0.637817  -0.658 0.510841    
#rainfall                  0.015514   0.017949   0.864 0.387834    
#parks                    58.037776  51.933004   1.118 0.264317    
#Meandist                 -1.217834   0.190047  -6.408 3.52e-10 

# fitting bet model with subset selection method 
install.packages("leaps")
library(leaps)
#best subset method 
lm_best=regsubsets(price~.,data=house,nvmax=15)
summary(lm_best)
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
# 9th model having best adjusted R^2 among all these models 
# then considering 9th model from above  
# then coefficients  
coefficients(lm_best,9)
#{(Intercept)     air_qual     room_num     teachers    poor_prop 
#-9.19315745 -22.04274872   4.05461688   1.03999647  -0.57627826 
#airport   n_hos_beds  n_hot_rooms        parks     Meandist 
#1.28099394   0.34723545   0.09388324  56.27518238  -1.14688737 


# forward selection method 
Lm_forward= regsubsets(price~.,data=house,nvmax=15,method = "forward")
summary(Lm_forward)$adjr2
which.max(summary(Lm_forward)$adjr2) # 9th model 
coefficients(Lm_forward,9)

# backward selection method 
Lm_backward= regsubsets(price~.,data=house,nvmax=15,method = "backward")
summary(Lm_backward)$adjr2
which.max(summary(Lm_backward)$adjr2) # 9th model 
coefficients(Lm_backward,9)