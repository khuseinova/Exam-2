#Kseniia Huseinova

#1a.Answer: 0.6914625
pnorm(q=-0.45, mean=4, sd=8.9,lower.tail=F)
#1b Answer: 0.08913093
pnorm(q=2.56, mean=8, sd=3.2)*2
#1c Answer: -5.247636 and  11.24764
qnorm(p=0.126,mean=3,sd=7.2,lower.tail = T)
qnorm(p=0.126,mean=3,sd=7.2,lower.tail = F)
#1d Answer: 0.01119319
pt(19.68/8.2,32, lower.tail = F)
#1e. Answer: 0.6515927
pt(-2.64/6.6,11, lower.tail = F)

#2I want to compare two categories: people who have some college degree at all and people who have a master degree. 
#H0 = 0 
#H1 > 1
#Hypothesis: people with some college degree spend more time on socializing, relaxing and leisure activities than people with a master degree. 
# We can see that people having High school degree spend more time relaxing than people with higher degree. Its 353.8632 while for people with masterdegree its 257.6610. With higher degree numbers decreasing. That seems reasonable, as people pursuing higher degree spend more time for studying, but also most of them work at this time, so they do not have that much time for leisure activities.

#3
attach(dat_ATUS)
summary(dat_ATUS)
use_varb <- (AGE >= 21 | AGE <= 35) & (AGE >= 35)
summary(use_varb)
dat_use <- subset(dat_ATUS,use_varb) 
detach()
attach(dat_use)
summary(dat_use)

use_varb <- (AGE >= 21 | AGE <= 35) 
summary(use_varb)
dat_use <- subset(dat_ATUS,use_varb) 
detach()
attach(dat_use)
summary(dat_use)

# I tried to look on a group of people from 21 to 35 years old.
# I want to see how number of kids, sex, the total number of hours worked per week, full time or part time, and overall activity of working influence the activity of taking care of the children.

model_temp1 <- lm(ACT_CAREHH ~ HH_NUMKIDS + SEX + UHRSWORKT + FULLPART + ACT_WORK)
summary(model_temp1)

# Obviously, sex is exogenous, because we cant change it and it cannot be affected by other things. In these situation, all my variables are exogenous, as they influence how many hours the person spends for taking care of the children, which means these variables are independent. 
# Polynomials in Age are important, because people in different ages can decide whether to have kids, to take care of them of or not to. My estimate shows that age has estimate of -0.45 and statistically significantly. 

model_temp1 <- lm(ACT_CAREHH ~ HH_NUMKIDS + SEX + UHRSWORKT + FULLPART + ACT_WORK + age)
summary(model_temp1)

# It is interesting that hours worked, full time or part time job don't have a huge influence on time spend for children. So I want to remove it.

model_temp1 <- lm(ACT_CAREHH ~ HH_NUMKIDS + SEX + ACT_WORK + AGE)
summary(model_temp1)
#Now all my variables statistically significant, because my p-value is <2e-16, which is less than the lefel of significance of 0.001 it means that the independent variables are statistically significant.
# I want to see what  are the predicted probabilities for a few particular groups, I chose
to_be_predicted <- dat_ATUS(AGE >= 21 | AGE <= 35)
to_be_predicted$yhat <- predict(model_temp1, newdata = to_be_predicted)
summary(to_be_predicted$yhat)

# Now I want to estimate simple logit model. I chose as variables age, number of kids, sex, activity work, and social activity

detach()
attach(dat_ATUS)
use_varb <- (ACT_CAREHH >=0)
summary(use_varb)
dat_use <- subset(dat_ATUS,use_varb) 
detach()
attach(dat_use)
summary(dat_use)

model_temp1 <- glm(ACT_CAREHH ~ AGE + HH_NUMKIDS + SEX + ACT_WORK + ACT_SOCIAL)
summary(model_temp1)

# My variables are independent, which means they have a huge effect on care activity, all of them are statistically significant. All of them seems plausible. As we can see, the number of kids has the biggest effect, and it seems reasonable, as usually people having more chilfren have to spend more time taking care of them, as well as SEXFemale which has estimate 8.81, and its also plausible as more often women take care of children.
# Lets look how many types error I and II are made by the model.
set.seed(1)
index<-sample(x=2,size=nrow(dat_use),replace=TRUE,prob=c(0.7,0.3))
train<-dat_use[index==1,]
test<-dat_use[index==2,]
dim(dat_use)
dim(train)
dim(test)
trainmodel<-glm(ACT_CAREHH ~ AGE + HH_NUMKIDS + SEX + ACT_WORK + ACT_SOCIAL)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$ACT_CAREHH,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum
## An 23% accuracy of predicting correctly. 


8.# i want to use another model to predict how much time is spent caring. I will use tideverse model
detach()
attach(dat_ATUS)
require("standardize")

require(tidyverse)
ACT_CAREHH <- (dat_ATUS$ACT_CAREHH > 0)

dat_ATUS$ACT_CAREHH <- recode_factor(dat_ATUS$ACT_CAREHH, "\"AGE\"" = "ACT_FOOD", "\"HH_NUMKIDS\"" = "SEX", "\"ACT_WORK\""  = "ACT_SOCIAL",
                                .default = "D")
summary(dat_ATUS$ACT_CAREHH)

