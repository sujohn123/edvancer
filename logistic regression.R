memory.limit()
data(cars)
data(audit)
cars<-cars
str(cars)
plot(density(cars$dist))

qqnorm(cars$speed)
qqline(cars$speed)



binom.test(10,15,0.2)

?t.test()
?summary()

?pnorm()

setwd("f:/R")

x<-read.csv("final.csv")
names(x)
str(x)
ww<-read.csv("winequality-white.csv",header=T,sep=';')
?read.csv
str(ww)
head(ww)
fix(ww)
?t.test
t.test(ww$fixed.acidity,mu=6.10,conf.level = 0.99)
t.test(ww$fixed.acidity,ww$volatile.acidity, mu=0,conf.level = 0.95,paired=T)

t.test(ww$alcohol,ww$quality, mu=0,conf.level = 0.95,paired=T)

same<-data.frame(alchohol=ww$alcohol,quality=ww$quality)
str(same)
mytable<-table(same)
mytable
str(mytable)
fix(mytable)
newtab<-margin.table(mytable,1)
newtab
audit<-read.csv("audit.csv")
data(audit)




---------Practise Assignment 4--Hypothesis testing and anova 



1)      Read the dataset [5]

benefits=read.csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Benefits.csv")
benefits<-read.csv("benifits.csv")

getwd()
setwd("F:/R")
write.csv(benefits,"benefits.csv")
str(benefits)
names(benefits)
benefits$joblost
benefits$nwhite
attach(benefits)
fix(benefits)
detach(benefits)


2)      Treat the missing values existing in the dataset [5]
benefits<-na.omit(benefits)



3)      Perform a t-test to find out if there is any statistically significant
difference in the state unemployment rate between the white and non-white group [8]

?t.test()
t.test(state~nwhite,data=benefits)

This is when both are categorical variables
chisq.test(joblost,nwhite)



4)      Perform a t-test to find out if there is any statistically significant 
difference in the state unemployment rate between the male and female [8]

benefits$state
names(benefits)
stateur
t.test(state~sex,data=benefits)

This is when both are categorical variables and we are checking whether the distribution
of categories of one variable is same or not over the categories of another variable

chisq.test(joblost,sex)

5)Test out the distribution of the variable state unemployment rate [8]

hist(stateur,breaks=20)
qqnorm(benefits$stateur)
?qqnorm()

6)      Compute the basic statistics for the continuous variables 

summary(state)

7)      Prepare the cross tabulation results for at least 
4 categorical variables, taking 2 variables at a time [6]

names(benefits)
bluecol
tabletotal<-table(sex,state,married,bluecol)
tabletotal
tablefreq<-ftable(sex,state,married,bluecol)
tablefreq
tabvalue<-xtabs(sex+state+married+bluecol,data=benefits)

8)      Conduct chi-square test of independence between any two categorical
variables and interpret the result [6]
chisq.test(sex,married)
chisq.test(married,sex)


#9)What kind of variable binning techniques you are going to apply to 
#transform the continuous variable age into categorical variables.
#Create 6 categories as age group. [8]

#I am using cut functiona and seq function to split the age group to 8 categories
#if we will start from 20 to 61 because that is the range and add by 5( you can 
#use any number here)

#But this is apparently not the efficient way to do this. The efficient way will 
#follow in the code given by the solution of this problem. 

range(age)
newbin<-cut(age,breaks=seq(20,61,5))
newbin[1:10]
as.data.frame(table(newbin))

########This is the solution given in the LMS. I have added few points so i myself
#####in the future can understand this shit

#we can bin in two ways. We can either break the range in equal intervals. Or we cant make bins
#in sorted order of age, maintaing that each bin has equal numbers.

#equal range bins:
#It means the diffence between the range that we are creating should be same
#for exampole 20-26 and 27-33 which has a gap of 6 

benefits$bin1=cut(benefits$age,breaks=6,label=c(1:6))
#or you can put sequence as i have done previously
bin1
fix(benefits)
table(benefits$bin1)#It will show how the bins are divided..Here every bin has different
#total number of items but the range is equal For eg: 1 represent 1st bin and 
its range is from
m<-split(benefits$age,benefits$bin1)
class(m)
range(m[[1]])



#equal size bins:
# Here the ranges will not have equal gaps but the number of items in each category
#will be the same
library(dplyr)

?mutate()
benefits= benefits %>%
  arrange(age) %>%
  mutate(bin2=cut(1:nrow(benefits),breaks=6,label=c(1:6)))

table(benefits$bin2) # all the bins has equal number of items....813


#we can examine these for ranges and memebr counts
table(benefits$bin1)
table(benefits$bin2)

tapply(benefits$age,benefits$bin1,range)
tapply(benefits$age,benefits$bin2,range)


10)Perform one way ANOVA on the age group and the state unemployment rate, 
also conduct a post hoc test to verify which age groups are statistically 
significantly different from other groups. [10]

attach(benefits)
names(benefits)
an<-aov(stateur~bin1,data=benefits)
summary(an)
TukeyHSD(an)
#Here we are trying to see if the various bins 1:6 has same state unemployment or 
#not
t.test(stateur,bin1,paired=T)






#####################Simple Linear regression########################################
data.reg<-read.csv("dataset.csv")
names(data.reg)
cor(data.reg[,18:20])
cor(data.reg[,19:20])

pairs(data.reg[,19:20])
Model1<-lm(Satisfaction~Delivry_Speed,data=data.reg)
summary(Model1)

summary.aov(Model1)
par(mfrow=c(1,4))# This will help show all four graphs in a single screen
plot(Model1)
dev.off()




#################Multiple Linear regression practise#######################################
#using the same dataset "dataset" from above 

cor(data.reg[,7:19])
#scatterplot martrx /graph
pairs(data.reg[,7:12])
pairs(data.reg[,12:7])
names(data.reg[,7:12])
names(data.reg)

attach(data.reg)
detach(data.reg)

model1<-lm(data.reg$Satisfaction~data.reg$Prod_Qlty+data.reg$Ecom_Activity+
             data.reg$Tech_Support+data.reg$Complnt_resol+data.reg$Advt+         
           +data.reg$Produc_Line,data=data.reg)
summary(model1)

model2<-step(model1,direction="forward")
summary(model2)
summary.aov(model2)

vif(model2)

model3<-update(model1,.~.-data.reg$Tech_Support)
summary(model3)
summary.aov(model3)
plot(model3)
install.packages("datasets")
library(datasets)


library(readxl)
shock<-read_excel("shock.xls")
str(shock)
names(shock)
shock[,6]
newshock<-shock[,-c(4,5,6,21)]
str(newshock)
names(newshock)
newshock<-na.omit(newshock)
boxplot(newshock)
model1<-lm(newshock$AGE~newshock$ID+newshock$AGE+newshock$HT+newshock$SBP+newshock$MAP
   +newshock$HR+newshock$DBP+newshock$MCVP+newshock$BSI+newshock$CI+newshock$AT
   +newshock$MCT+newshock$UO+newshock$PVI+newshock$RCI+newshock$HG+newshock$HCT)


names(computer)
str(computer)
outliers
outliers[1,1]
boxplot(computer1)

outlierReplace(computer1,"price",which(computer1$price<outliers[1,1]), NA)
outlierReplace(computer1,"price",which(computer1$price>outliers[1,2]), NA)










###########Multiple Linear Regression assignment###############################3

#1)      Read the dataset into R, check for missing values [5]
getwd()
setwd("F:/R")
computer<-read.csv("computers.csv")
str(computer)
names(computer)

#2)      Draw a scatterplot for the variables price, speed, hd, ram, screen, ads 
#and trend [5]

pairs(computer[,c(2,3,4,5,6,10,11)])

#3)      Run correlation and interpret what  you see in the dataset [10]
cor(computer[,c(2,3,4,5,6,10,11)])



#4)      Run a multiple linear regression by taking price as the dependent 
#variable and the speed, hd, ram, screen, ads and trend as the independent variables [5]
attach(computer)
model1<-lm(price~speed+hd+ram+screen+ads+trend,data=computer)
summary(model1)


# 5)Interpret the R-square and adjusted R square for the model, explain the 
#goodness of fit of the model [5]
summary.aov(model1)
install.packages("car")
library(car)
vif(model1)

6)      Interpret the coefficients of the model [5]
coef(model1)
model2<-lm(scale(price)~scale(speed)+scale(hd)+scale(ram)+scale(screen)+scale(ads)+scale(trend),data=computer)
coef(model2)
model3<-step(model1,direction="backward")
summary(model3)
par(mfrow=c(1,4))
plot(model3)
plot(model1)
plot(modelx)

In the 4th daiagram which is leverarage vs residual plot , it seems there is a pattern
exist which violates the rule of linear regrssion. Whereas the 2nd diagram shows 
the errors is skewed rather than normal.

#7)      Is the model overally statistically significant? [4]
It seems like its not.Let see after removing outliers and null values and stuff.


#8)      Report the outliers in present in the model [6]

#To check the outliers use box plot.......

str(computer)
names(computer)
computer1<-na.omit(computer[,-c(7,8,9)])
names(computer1)
computer2<-computer1[,2:8]
computer1<-computer2
names(computer1)


boxplot(computer1[,1])
boxplot(computer1[,2])
boxplot(computer1[,3])
boxplot(computer1[,4])


# This is to compute outliers in the dataset, less than 5th percentile and more 
#than 95th percentile

outliers<-data.frame()
for (i in 1:7)
{
outliers<-as.data.frame(rbind(outliers,quantile(computer1[,i],c(0.05,0.95))))
}
outliers

str(computer1)

outliers[1,1]







######################This one is failed attempt###############################
# This is using for loop instead of directly assessing the datase using the variable
#name and dollar sign which i have done after this


outliers1<-data.frame()


for( i in 1:nrow(computer1))
{
  for(j in 1:ncol(computer1))
  {
    if((computer1[i,j]<outliers[1,1])&(computer1[i,j]>outliers[1,1]))
    {
    computer1[i,j]=0
    }
    
    }
}

#from stack overflow
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  y <- x
  y[x < qnt[1]] <- NA
  y[x > qnt[2]] <- NA
  y
}








#This is direct way without using any loop
#After finding out the outliers and saving in outliers dataset


outliers
str(outliers)
str(outliers)
names(computer1)

computer1$price<-ifelse(computer1$price<outliers[1,1], 
NA,computer1$price)

computer1$price<-ifelse(computer1$price>outliers[1,2], 
                        NA,computer1$price)

computer1$speed<-ifelse(computer1$speed<outliers[2,1], 
                        NA,computer1$speed)
computer1$speed<-ifelse(computer1$speed>outliers[2,2], 
                        NA,computer1$speed)

computer1$hd<-ifelse(computer1$hd<outliers[3,1], 
                        NA,computer1$hd)
computer1$hd<-ifelse(computer1$hd>outliers[3,2], 
                        NA,computer1$hd)

computer1$ram<-ifelse(computer1$ram<outliers[4,1], 
                        NA,computer1$ram)
computer1$ram<-ifelse(computer1$ram>outliers[4,2], 
                        NA,computer1$ram)

computer1$screen<-ifelse(computer1$screen<outliers[5,1], 
                      NA,computer1$screen)
computer1$screen<-ifelse(computer1$screen>outliers[5,2], 
                      NA,computer1$screen)

computer1$ads<-ifelse(computer1$ads<outliers[6,1], 
                      NA,computer1$ads)
computer1$ads<-ifelse(computer1$ads>outliers[6,2], 
                      NA,computer1$ads)

computer1$trend<-ifelse(computer1$trend<outliers[7,1], 
                      NA,computer1$trend)
computer1$trend<-ifelse(computer1$trend>outliers[7,2], 
                      NA,computer1$trend)

computer2<-na.omit(computer1)
fix(computer)
fix(computer1)
fix(computer2)


computer1[,1]
computer2[,3]


# To see the outliers 
boxplot(computer2[,1])
boxplot(computer2[,2])
boxplot(computer2[,3])
boxplot(computer2[,4])
boxplot(computer2[,5])
boxplot(computer2[,6])
boxplot(computer2[,7])



9)If any variable is insignificant then remove that 
variable and re-run the model [5]

str(computer2)
modelx<-lm(computer2$price~computer2$speed+computer2$hd+computer2$ram+
  computer2$screen+computer2$ads+computer2$trend,data=computer)
summary(modelx)
par(mfrow=c(1,4))
plot(modelx)
vif(modelx)
summary.aov(modelx)
cor(computer2)
pairs(computer2)

#This is code to split the screen into two by two
por(nforw=c(2,2))

10)   Is there any influential observation in the model? [4]

No


11)   Check for multicollinearity and report the result. [5]
vif(modelx)

12)   Check for heteroskedasticity and report the result [5]
check out the residuals vs leverage plot and if the variance is constant then 
its homoschedestic...

13)   Check for autocorrelation and report the result [5]

mod = lm(prices[,1] ~ prices[,2])

res = modelx$res 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)



#You can do durbin watson test as well
install.packages("lmtest")
library(lmtest)
dwtest(prices[,1] ~ prices[,2])

##
The DW Test or the Linear Regression test are not robust to anomalies in the data.
If you have Pulses, Seasonal Pulses , Level Shifts or Local Time Trends these tests
are useless as these untreated components inflate the variance of the errors thus 
downward biasing the tests causing you ( as you have found out ) to incorrectly 
accept the null hypothesis of no auto-correlation. Before these two tests or any 
other parametric test that I am aware of can be used one has to "prove" that the
mean of the residuals is not statistically significantly different from 0.0 
EVERYWHERE otherwise the underlying assumptions are invalid. It is well known 
that one of the constraints of the DW test is its assumption that the regression
errors are normally distributed. Note normally distributed means among other 
things : No anomalies ( see http://homepage.newschool.edu/~canjels/permdw12.pdf 
). Additionally the DW test only test for auto-correlation
of lag 1. Your data might have a weekly/seasonal effect and this would go 
undiagnosed and furthermore , untreated , would downward bias the DW test.


14)   Take last 5 rows from the data set and using the information for 
the independent variables predict the price. Do you see any difference in
actual vs. predicted value of price for this 5 observations. [6]

computer2$price1<-predict(modelx,computer2)










##................lOgistic regression..................###############



setwd("F:/R")
data<-read.csv("dataset.csv")
names(data)
str(data)
data$Cust_Type<-as.factor(data$Cust_Type)
data$Indust_Type<-as.factor(data$Indust_Type)
data$Firm_Size<-as.factor(data$Firm_Size)
data$Dist_Sys<-as.factor(data$Dist_Sys)
data$Region<-as.factor(data$Region)
data$Start_Alliance<-as.factor(data$Start_Alliance)
str(data)
dim(data)

#verifying the existing of missing values
str(data)
summary(data)
sapply(data,sd)
#only customer id seems to have more s.d
#Model

attach(data)
model1<-glm(Start_Alliance~Cust_Type+Indust_Type+Firm_Size+Region+Dist_Sys+Prod_Qlty
+Ecom_Activity+Tech_Support+Complnt_resol+Advt+Produc_Line+
Salesforce_Image+Comp_Pricing+Warranty_Claims+New_Prodcts
+Ordering_Billing+Price_Flex+Delivry_Speed+Satisfaction+Liklihood_Recom+
Liklihood_Fut_Pur+Current_Purch,family =binomial(logit),data=data)
                                                                                  
#Model result components
summary(model1)
confint(model1) # 95% CI for the coefficients
exp(coef(model1))  # exponential coefficients
exp(confint(model1)) #95% CI for exponentiated coefficients

predict(model1,type="response") #predicted values
# Type value can be link as well. so according to the necessity we
have to choose type

residuals(model1,type="deviance")#residuals
anova(model1,test="Chisq")
# we can see the residual deviance of the model in the case of 
inclusion of each variable and how it is reduced from 273 to 93.

plot(model1$fitted)
#It will show the fitted plot of 1's and 0's and misfitted.

#predicted probabilities
library(MASS)
fit_step<-stepAIC(model1)
summary(fit_step)

#Multicollinearity check

library(MASS)
library(plyr)
library(car)
vif(fit_step)
No multicollinearity
#predicting the data
prob<-predict(fit_step,type=c("response"))
data$prob=prob #adding to the data itself


#############################Logistic regression 2##################################3

#Continued from above logistic regression 1

install.packages("pROC")
library(pROC)
g<-roc(Start_Alliance~prob,data=data)














##################################Project 2##########################################################
#################################Logistic regression###############################################

#reading data
bank<-read.csv("bank-full.csv")
backup<-bank
str(bank)

#Checking out overall summary of the data
summary(bank)

#One way of checking missing values
mv<-is.na(bank)
table(mv)["TRUE"]

#Removing missing values if any
bank<-na.omit(bank)


#outlier calculation and removal function
outlier<-function(value=vector())
{
  #m<-mean(value)
  #s<-sd(value)
#fifth<-(m-3*s)
#nin<-(m+3*s)
  fifth<-quantile(value,0.05)
  nin<-quantile(value,0.95)
  for(i in seq_along(value))
{
if(value[i]<=fifth){
  value[i]=NA
} else if(value[i]>=nin){
value[i]=NA
}else{
  }
}
value
}



#Test for the function 
d<-1:1000
ddd<-outlier(d)
ddd
names(bank)
str(bank)



#Removing outliers by executing the function...............

#Numeric variables
#age,balance,day,duration,campaign,pdays,previous

bank$age<-outlier(bank$age)
summary(bank$age)
bank$balance<-outlier(bank$balance)
bank$day<-outlier(bank$day)
bank$duration<-outlier(bank$duration)
bank$campaign<-outlier(bank$campaign)
bank$pdays<-outlier(bank$pdays)
bank$previous<-outlier(bank$previous)

summary(bank$balance)
bank<-na.omit(bank)

#To check if outliers still exist or not
boxplot(bank$age)
boxplot(bank$balance)
boxplot(bank$day)
boxplot(bank$duration)
boxplot(bank$campaign)
boxplot(bank$pdays)

str(bank)
###This above code ie function execution and then omit fucntion may have to be used more than 1 time so 
##keep checking using the box plot and until all the outliers gets removed keep repeating the process


#########################Creating dummy variables#########################################################
names(bank)
#Categorical variables
"job"       "marital"   "education" "default"   "housing"   "loan"     
"contact"   "month"   "poutcome" 

summary(bank$job)
summary(bank$marital)
summary(bank$education)
summary(bank$default)
summary(bank$housing)
summary(bank$loan)
summary(bank$contact)
summary(bank$month)
summary(bank$poutcome)

#This is to check the distribution of each categorical variables with the Y variable
mytable<-table(bank$job,bank$y)
mytable
prop.table(mytable,1)

table(bank$loan,bank$y)

## For variable marital
bank$dum_marital_single<-ifelse(bank$marital=="single",1,0)
bank$dum_marital_married<-ifelse(bank$marital=="married",1,0)
str(bank)

## For variable education
bank$dum_education_primary<-ifelse(bank$education=="primary",1,0)
bank$dum_education_secondary<-ifelse(bank$education=="secondary",1,0)
bank$dum_education_tertiary <-ifelse(bank$education=="tertiary ",1,0)
str(bank)

## For variable default
bank$dum_default_no<-ifelse(bank$default=="no",1,0)

## For variable loan
bank$dum_loan_no<-ifelse(bank$loan=="no",1,0)
str(bank)

## For variable Contact
bank$dum_contact_cellular<-ifelse(bank$contact=="cellular",1,0)

## For variable poutcome
bank$dum_poutcome_failure<-ifelse(bank$poutcome=="failure",1,0)
bank$dum_poutcome_other <-ifelse(bank$poutcome=="other",1,0)
bank$dum_poutcome_success<-ifelse(bank$poutcome=="success",1,0)



#Merging of certain categories of categorical variables having a lot of categories according to their 
#distribution to y=1

table(bank$job,bank$y)

#For job variable
no yes
admin.         92  12
blue-collar   153  13
services       53  12
unknown         1   2
entrepreneur   20   3
retired        11   1
self-employed  30   3
housemaid       9   6
student        13   6
unemployed     12   7
management    154  53
technician    113  36

bank$dum_job_abs<-ifelse(bank$job=="admin"|bank$job=="blue-collar" |bank$job=="services",1,0)
bank$dum_job_uershsu<-ifelse(bank$job=="unknown" | bank$job=="entrepreneur" |bank$job=="retired"
                             & bank$job=="self-employed"| bank$job=="housemaid"| bank$job=="student"
                             & bank$job=="unemployed",1,0)
bank$dum_job_mt<-ifelse(bank$job=="management"|bank$job=="technician",1,0)

str(bank)


#For variable month
table(bank$month,bank$y)
no yes

dec  10   6
jan  18   6
jul  13   8
mar   9   7
may 161  19
nov 184  19
oct   8  11
sep  10  12
apr  80  12
feb 118  16
jun  18  15
aug  32  23

bank$dum_month_djajuma<-ifelse(bank$month=="dec"|bank$job=="jan" |bank$month=="jul"|bank$month=="mar",1,0)

bank$dum_month_manovosafjaug<-ifelse(bank$month=="may"|bank$job=="nov" |bank$month=="oct"|bank$month=="jun"
                                  |bank$month=="sep"|bank$month=="apr"|bank$month=="feb"|bank$month=="aug",1,0)



#####Sampling: dividing the data into train and test dataset

###################################################################################################
####This is one way to do it but it didn't work for some reason########################
###########Didn't work###############################3333

install.packages("caret")
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(bank, p = .7,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

?trunc
################################################################################################



#################### Working one######################
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*0.7))#trunc is used for rounding off, 0.7 will divide it
  #into 70% and 30%......
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

##The logic behind the function is first the number of rows will be stored( from 1 to the total rows) in 
##a variable . Then that variable will be passed through sample function and a parameter which will say
##about the cutoff of the sampling process like 50-50,70-30,80-20 etc Now the sample function will arrange
##the numbers randomly then we can pass that numbers to the dataset which will give us one dataset. Similarly
##we can put -that numbers in the code (dataframe[-trainindex,]) to filter out the rest

########Executing that function######
splits<-splitdf(bank,seed=809)
str(splits)

#train dataset with 70%data
trainset<-splits[[1]]
str(trainset)

#test dataset with 30%data
testset<-splits$testset
str(testset)



########Now T-Test to check whether the data is well distributed between train and test

t.test(trainset$age,testset$age)
t.test(trainset$balance,testset$balance)
t.test(trainset$day,testset$day)
t.test(trainset$duration,testset$duration)
t.test(trainset$campaign,testset$campaign)
t.test(trainset$pdays,testset$pdays)
t.test(trainset$previous,testset$previous)

##Since all the p values are more than 0.05 which means null hypothesis is true that means the means of the
##two compared dataset is same. This further means the data is well distributed between train and test..

names(bank)
#character variables
                    "job"                     "marital"                 "education"              
"default"                                "housing"                 "loan"                   
"contact"                                      "month"                                 
                                                "poutcome"               
 "y"                       

 
 #######Now Chi square test for categorical variables############################################

 ##since directly we cannot use chisquare test because of the length difference 
table(trainset$job,testset$job)
chisq.test(as.factor(trainset$job),as.factor(testset$job))

#This is the function that will calculate the chisquare  
chitest<-function(v1=vector(),v2=vector())
{
  a<- c(v1,v2)
  length(answer)
  y1<- rep(1,length(trainset$age))
  y2<-rep(2,length(testset$age))
  y<-c(y1,y2)
  tab<-table(a,y)
  chisq.test(tab)
} 
  
chitest(trainset$job,testset$job)  
chitest(trainset$marital,testset$marital)  
chitest(trainset$education,testset$education)  
chitest(trainset$default,testset$default)  
chitest(trainset$housing,testset$housing)  
chitest(trainset$loan,testset$loan)  
chitest(trainset$month,testset$month)  
chitest(trainset$poutcome,testset$poutcome)  


names(bank)
  

######We can delete variable pdays, previous, poutcome because these variables are related to previous
######marketing campaign data.But we will keep it.

###We have to delete categorical variables of which we have created dummy variables

trainset$job<-NULL
trainset$contact<-NULL
trainset$marital<-NULL
trainset$education<-NULL
trainset$default<-NULL
trainset$housing<-NULL
trainset$loan<-NULL
trainset$month<-NULL
trainset$poutcome<-NULL

testset$job<-NULL
testset$contact<-NULL
testset$marital<-NULL
testset$education<-NULL
testset$default<-NULL
testset$housing<-NULL
testset$loan<-NULL
testset$month<-NULL
testset$poutcome<-NULL

names(trainset)

# THis a piece of code that i learned from coursera. It tells us which variable has the minimum
#cross validation error. So we could use only that variable to make the model.

costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA,24)
library(boot)
for (i in 1:24) {
  lmFormula = reformulate(names(trainset)[i], response = "y")
  glmFit = glm(lmFormula, family = "binomial", data = trainset)
  cvError[i] = cv.glm(trainset, glmFit, costFunction, 2)$delta[2]
}

cvError
names(trainset)
names(trainset)[which.min(cvError)]

table(trainset$dum_poutcome_success,trainset$y)


###The code ende here


################Model Building(Logistic regression)

#deleting y variable and then replacing it with a variables which contain the values 0 and 1 instead of yes
#and no
trainset$y1<-ifelse(trainset$y=="yes",1,0)
testset$y1<-ifelse(testset$y=="yes",1,0)
trainset$y<-NULL
testset$y<-NULL
library(plyr) 
trainset <- rename(trainset,c('y1'='y'))
testset <- rename(testset,c('y1'='y'))

#Logistic regression function
model1<-glm(y~age+balance+day+duration+
campaign+pdays+previous+dum_marital_single+dum_marital_married+dum_education_primary+
  dum_education_secondary+dum_education_tertiary+dum_default_no+dum_loan_no+dum_contact_cellular+
dum_poutcome_failure+dum_poutcome_other+dum_poutcome_success+dum_job_abs+dum_job_uershsu+dum_job_mt+
  dum_month_djajuma+dum_month_manovosafjaug,family=binomial(logit),data=trainset)

summary(model1)
vif(model1)
  
#stepAIC will automatically create more than 1 model and then choose the best out of it
fitstep<-stepAIC(model1)
summary(fitstep)
vif(fitstep)  

prob<-predict(fitstep,trainset,type=c("response"))
prob
trainset$prob<-prob

#ROC Curve
library(pROC)
g<-roc(y~prob,data=trainset)
plot(g)

#confustion matrix for specificity and sensitivity
for(T in seq(0.1,0.9,0.1))
{
  class1=ifelse(trainset$prob>T,1,0)
  #compute confusion matrix
  cat("threshold=",T,"\n")
  confusion=table(class1,trainset$y)
  cat("confusion matrix:","\n")
  print(confusion)
  pctcorrect=100*(confusion[1,1]+confusion[2,2])/sum(confusion)
  false.pos.rate=confusion[[3]]/(confusion[[1]]+confusion[[3]])
  false.neg.rate=confusion[[2]]/(confusion[[2]]+confusion[[4]])
  cat("% correct=", round(pctcorrect,1),"\n")
  print("---------------------------------")
  cat("false.pos.rate=",round(false.pos.rate,2),"\n")
  print("---------------------------------")
  cat("false.neg.rate=",round(false.neg.rate,2),"\n")
  print("---------------------------------")
}


###Applying the model to test dataset
prob1<-predict(fitstep,testset,type=c("response"))
prob1
testset$prob<-prob1

#ROC Curve
library(pROC)
g1<-roc(y~prob,data=testset)
plot(g1)

detach(trainset)
attach(testset)

#confustion matrix for specificity and sensitivity of testdataset
for(T in seq(0.1,0.9,0.1))
{
  class1=ifelse(testset$prob>T,1,0)
  #compute confusion matrix
  cat("threshold=",T,"\n")
  confusion=table(class1,testset$y)
  cat("confusion matrix:","\n")
  print(confusion)
  pctcorrect=100*(confusion[1,1]+confusion[2,2])/sum(confusion)
  false.pos.rate=confusion[[3]]/(confusion[[1]]+confusion[[3]])
  false.neg.rate=confusion[[2]]/(confusion[[2]]+confusion[[4]])
  cat("% correct=", round(pctcorrect,1),"\n")
  print("---------------------------------")
  cat("false.pos.rate=",round(false.pos.rate,2),"\n")
  print("---------------------------------")
  cat("false.neg.rate=",round(false.neg.rate,2),"\n")
  print("---------------------------------")
}

##Intrepretation

The model performs similarly between train and test dataset as we have seen from above results, lets
dive into the impact made by various independent variables on Y variable.

#odds ratio
exp(coef(fitstep))

(Intercept)                     day                duration                campaign 
0.29620123              1.03159973              1.00540482              0.73594030 
dum_education_primary dum_education_secondary             dum_loan_no    dum_poutcome_failure 
0.11880054              0.29848710              1.96325608              0.06478953 
dum_poutcome_other       dum_month_djajuma dum_month_manovosafjaug 
0.06733081              3.48595079              2.25736535 


If the contact with client happened in month of dec,jan,jul or march then the conversion probability is 
higher. Similarly if the client has loan he/she is more likely to convert. If the education of the client
is of primary and secondary level, he/she is less likely to convert.likewisely we can intrepret 
the results.


#coefficients
day                      0.031111      
duration                 0.005390   
  campaign                -0.306606   
  dum_education_primary   -2.130309  
  dum_education_secondary -1.209029 
  dum_loan_no              0.674604     
dum_poutcome_failure    -2.736611   
  dum_poutcome_other      -2.698137 
  dum_month_djajuma        1.248741   
  dum_month_manovosafjaug  0.814198 











################################project 1######################################

##The analytics problem of the business problem is to know how various characteristics(vaiables) are 
##related to the interest rate. For that we can build a linear regression model considering interest rate
##as dependent variable and other as independent variables.

#readint the data
loandata<-read.csv("loandata.csv")
backuploandata<-read.csv("loandata.csv")
str(loandata)
summary(loandata)

#removing missing values
na<-is.na(loandata)
table(na)["TRUE"]

loandata<-na.omit(loandata)

na<-is.na(loandata)
table(na)["TRUE"]


#removing outliers
names(loandata)

outlier<-function(value=vector())
{
  #m<-mean(value)
  #s<-sd(value)
  #fifth<-(m-3*s)
  #nin<-(m+3*s)
  fifth<-quantile(value,0.05)
  nin<-quantile(value,0.95)
  for(i in seq_along(value))
  {
    if(value[i]<=fifth){
      value[i]=NA
    } else if(value[i]>=nin){
      value[i]=NA
    }else{
    }
  }
  value
}
#numerical variables

"id"                             "Amount.Requested"               "Amount.Funded.By.Investors"    
"Interest.Rate"                                                     
"Debt.To.Income.Ratio"                                      
"Monthly.Income"                                  "Open.CREDIT.Lines"             
"Revolving.CREDIT.Balance"       "Inquiries.in.the.Last.6.Months" 

loandata$id<-outlier(loandata$id)
loandata$Amount.Requested<-outlier(loandata$Amount.Requested)
loandata$Amount.Funded.By.Investors<-outlier(loandata$Amount.Funded.By.Investors)
loandata$Debt.To.Income.Ratio<-outlier(loandata$Debt.To.Income.Ratio)
loandata$Monthly.Income<-outlier(loandata$Monthly.Income)
loandata$Open.CREDIT.Lines<-outlier(loandata$Open.CREDIT.Lines)
loandata$Revolving.CREDIT.Balance<-outlier(loandata$Revolving.CREDIT.Balance)
loandata$Inquiries.in.the.Last.6.Months<-outlier(loandata$Inquiries.in.the.Last.6.Months)
loandata$Interest.Rate<-outlier(loandata$Interest.Rate)

loandata<-na.omit(loandata)

boxplot(loandata$Amount.Funded.By.Investors)


#creating dummy variables(changing the categorical variables into factors)

summary(loandata)

names(loandata)
#categorical variables 
"Loan.Length"   "Loan.Purpose"                  
  "State"                                      
  "FICO.Range"                             
  "Employment.Length"

loandata$Loan.Length<-as.factor(loandata$Loan.Length)
loandata$Loan.Purpose<-as.factor(loandata$Loan.Purpose)
loandata$State<-as.factor(loandata$State)
loandata$FICO.Range<-as.factor(loandata$FICO.Range)
loandata$Employment.Length<-as.factor(loandata$Employment.Length)


#sampling

split<- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*0.7))#trunc is used for rounding off, 0.7 will divide it
  #into 70% and 30%......
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

splitt<-split(loandata,885)

#train dataset with 70%data
train<-splitt$trainset
str(train)

#test dataset with 30%data
test<-splitt$testset
str(test)


#numecial variables
"id"                             "Amount.Requested"               "Amount.Funded.By.Investors"    
"Interest.Rate"                                                     
"Debt.To.Income.Ratio"                                      
"Monthly.Income"                                  "Open.CREDIT.Lines"             
"Revolving.CREDIT.Balance"       "Inquiries.in.the.Last.6.Months" 




########Now T-Test to check whether the data is well distributed between train and test

t.test(train$id,test$id)
t.test(train$Amount.Requested,test$Amount.Requested)
t.test(train$Amount.Funded.By.Investors,test$Amount.Funded.By.Investors)
t.test(train$Debt.To.Income.Ratio,test$Debt.To.Income.Ratio)
t.test(train$Monthly.Income,test$Monthly.Income)
t.test(train$Open.CREDIT.Lines,test$Open.CREDIT.Lines)
t.test(train$Revolving.CREDIT.Balance,test$Revolving.CREDIT.Balance)
t.test(train$Inquiries.in.the.Last.6.Months,test$Inquiries.in.the.Last.6.Months)


##Since all the p values are more than 0.05 which means null hypothesis is true that means the means of the
##two compared dataset is same. This further means the data is well distributed between train and test..

names(bank)
#categorical variables 
"Loan.Length"   "Loan.Purpose"                  
"State"                                      
"FICO.Range"                             
"Employment.Length"
#character variables



#######Now Chi square test for categorical variables############################################

##since directly we cannot use chisquare test because of the length difference 
table(train$Loan.Length,test$Loan.Length)
chisq.test(as.factor(train$Loan.Length),as.factor(test$Loan.Length))


#This is the function that will calculate the chisquare  
chitest<-function(v1=vector(),v2=vector())
{
  a<- c(v1,v2)
  y1<- rep(1,length(train$Loan.Length))
  y2<-rep(2,length(test$Loan.Length))
  y<-c(y1,y2)
  tab<-table(a,y)
  chisq.test(tab)
} 

"Loan.Length"   "Loan.Purpose"                  
"State"                                      
"FICO.Range"                             
"Employment.Length"
#character variables
chitest(train$Loan.Length,test$Loan.Length)  
chitest(train$Loan.Purpose,test$Loan.Purpose)  
chitest(train$State,test$State)  
chitest(train$FICO.Range,test$FICO.Range)  
chitest(train$Employment.Length,test$Employment.Length)  




#linear regression model
names(train)
model1<-lm(Interest.Rate~Loan.Length+Loan.Purpose+State+FICO.Range+Employment.Length+id+Amount.Requested+
     Amount.Funded.By.Investors+Interest.Rate+Debt.To.Income.Ratio+Monthly.Income+
     Open.CREDIT.Lines+Revolving.CREDIT.Balance+Inquiries.in.the.Last.6.Months,data=train 
)
summary(model1)
library(MASS)
library(car)
model2<-stepAIC(model1)
summary(model2)

#multicollinearity check
vif(model2)

#Creating the best model automatically
model3<-step(model1,direction = "both")
summary(model3)
par(mfrow=c(1,4))
plot(model3)

pred<-predict(model3,train,type=c("response"))
train$pred<-pred  
train

#To check the quality of predicted values for train dataset
plot(train$Interest.Rate,train$pred)

#To see if the assumptions of linear regression are being met or not
par(mfrow=c(1,4))
plot(model3)

#For test dataset
test
test$FICO.Range<-ifelse(test$FICO.Range=="790-794"|test$FICO.Range=="805-809",NA,as.character(test$FICO.Range))
test<-na.omit(test)
test$FICO.Range<-as.factor(test$FICO.Range)

pred1<-predict(model2,test,type=("response"))
test$pred<-pred1

#quality of predicted values for test dataset
par(mfrow=c(1,1))
plot(test$Interest.Rate,test$pred)

#coefficient check
coef(model3)
#Coefficients are extremly small


#for beta or standard coefficients
#scale function is not used with categorical variables 
modelx<-lm(scale(Interest.Rate)~Loan.Length+Loan.Purpose+State+FICO.Range+Employment.Length+
scale(id)+scale(Amount.Requested)+
scale(Amount.Funded.By.Investors)+scale(Interest.Rate)+scale(Debt.To.Income.Ratio)+scale(Monthly.Income)+
             scale(Open.CREDIT.Lines)+scale(Revolving.CREDIT.Balance)+scale(Inquiries.in.the.Last.6.Months)
,data=train 
)


modely<-step(modelx,direction = "both")
coeff<-coef(modely)
coeff

###############################The final result##################################################
###Intrepretation###################
Loan.Length60 months 0.72189850
If the client asks for loan for 60 months period his/her chances of getting better interest rate increases
by 0.72.

FICO.Range780-784
-3.23360491

The clients having FICO.Range 78-784 is less likely to get good interest rates(change in 1 unit will
change the interest rate by -3.234)

Likewisely the following can be intrepreted

(Intercept)              Loan.Length60 months                 FICO.Range665-669 
1.30911978                        0.72189850                       -0.11456133 
FICO.Range670-674                 FICO.Range675-679                 FICO.Range680-684 
-0.49233848                       -0.52609842                       -0.92546951 
FICO.Range685-689                 FICO.Range690-694                 FICO.Range695-699 
-1.13887424                       -1.11301945                       -1.36887619 
FICO.Range700-704                 FICO.Range705-709                 FICO.Range710-714 
-1.52396309                       -1.66878052                       -1.63972585 
FICO.Range715-719                 FICO.Range720-724                 FICO.Range725-729 
-1.94995632                       -2.11419380                       -2.42170216 
FICO.Range730-734                 FICO.Range735-739                 FICO.Range740-744 
-2.67815805                       -2.94514231                       -2.73195909 
FICO.Range745-749                 FICO.Range750-754                 FICO.Range755-759 
-2.72940289                       -2.99753722                       -2.56803867 
FICO.Range760-764                 FICO.Range765-769                 FICO.Range770-774 
-3.28101534                       -2.92545635                       -2.73217344 
FICO.Range775-779                 FICO.Range780-784                 FICO.Range785-789 
-2.72780842                       -3.23360491                       -2.98575596 
FICO.Range795-799                         scale(id) scale(Amount.Funded.By.Investors) 
-2.83993478                        0.07337403                        0.30844765 
scale(Debt.To.Income.Ratio)          scale(Open.CREDIT.Lines) 
0.05410705                       -0.04661901 




##clustering
##K means clustering
setwd("F:/R")
install.packages("Rcmdr")
library(Rcmdr)
library(cluster)
cdata<-read.csv("Dataset.csv")
names(cdata)
str(cdata)
dim(cdata)
#Data transformation
cdata$Cust_ID<-as.character(cdata$Cust_ID)# converting data into factor
                            cdat<-cdata[,6:19]
                            names(cdat)
                            class(cdat)
                            
                            #scaling the data
                            dcl<-scale(cdat)
                            
                            #Detection of clusters from scree plot
                            sumsq<-(nrow(dcl)-1)*sum(apply(dcl,2,var))
                            for(i in 1:10)sumsq[i]<-sum(KMeans(dcl,centers=i,iter.max=50000,
                                                               num.seeds=5000)$withinss)
                            
                            plot(1:10,sumsq,type="b",xlab="Number of clusters",ylab=
                                   "within groups sum of squares")
                            
                            
                            #k-means cluster #default algo
                            km<-KMeans(dcl,centers=4,iter.max=5000,num.seeds=12345)
                            km$size
                            km$centers
                            km$withinss
                            km$cluster
                            summary(km)
                            
#Hierarchical clustering                            
                        setwd("f:/r")
                            library(cluster)
                            cdata<-read.csv("Dataset.csv")
                            names(cdata)
                            str(cdata)
                            cdata$Cust_ID<-as.character(cdata$Cust_ID)
                            cdat<-cdata[,6:19]
                            names(cdat)
                            class(cdat)
                            dcl<-scale(cdat)#scaling data. It needs to be done since the distance
                            #calculation without scaling will be messy.
                            
                            #hierarchical clustering average model
                            hc<-hclust(dist(dcl),method="ave") #ave means average, it can be other thing like ward 
                            plot(hc,hang=-1,labels=cdata$Cust_ID)
                            memb<-cutree(hc,k=4) # we have selected four clusters , that can be changed
                            rect.hclust(hc,k=4,border="red")# This will result in a dendogram which shows how the cluseters are sepearated and also the red line will show the cluters and how many dataset they contain.
                            result<-cbind(memb,cdata)	
                            write.csv(result,file="hclust.csv")
                            #The memb column will tell us about how the data is seperated.
                            
                            #hierachical clustering using ward method
                            #The dendogram is slightly different than the dendogram in average
                            #method. The arms of the clusters are shorter.
                            hcl<-hclust(dist(dcl),method="ward")
                            plot(hcl,hang=-1,labels=cdata$Cust_ID)
                            memb1<-cutree(hcl,k=5)
                            rect.hclust(hcl,k=5,border="red")
                            result1<-cbind(mem1,result)
                            write.csv
                            
                            #hirarchical clustering centroid method
                            hc2<-hclust(dist(dcl)^2, method="cen")
                            plot(hc2,hang=-1,labels=cdata$Cust_ID)
                            memb2<-cutree(hc2,k=5)
                            rect.hclust(hc2,k=5,border="red")
                            cent<-NULL
                            for(k in 1:5)
                            {
                              cent<-rbind(cent,colMeans(dcl[memb2==k, , drop=FALSE))
                            }
                            write.csv(result2,file="hclust2.csv")
                            
                            
                            

install.packages("cluster.datasets")
library(cluster.datasets)
bdrate<-birth.death.rates.1966
str(bdrate)
bdrate[1:5,]
bdrate$country[1:5]
barplot(bdrate[1:5,1],bdrate[1:5,2],height=5,width=2)
?barplot()


##########################coursera kernlab dataset clustering######################################

setwd("f:/r")
install.packages("kernlab")
library(kernlab)
spamdata<-spam
str(spamdata)
library(Rcmdr)
library(cluster)
names(spamdata)

spamdata<-scale(spamdata[,-58])

sumsq<-(nrow(spamdata)-1)*sum(apply(spamdata,2,var))
for(i in 1:10)sumsq[i]<-sum(KMeans(spamdata,centers=i,iter.max=50000,
                                   num.seeds=5000)$withinss)

plot(1:10,sumsq,type="b",xlab="Number of clusters",ylab=
       "within groups sum of squares")


km<-KMeans(spamdata,centers=2,iter.max=5000,num.seeds=12345)
km$size
km$centers
km$withinss
km$cluster
summary(km)



#Exploratory data analysis
setwd("f:/r")
loanamt<-read.csv("loandata.csv")
attach(loanamt)
names(loandata)
str(loandata)
plot(loanamt$Debt.To.Income.Ratio,loanamt$Interest.Rate)
pairs(loanamt)
Home.Ownership,FICO.Range,Inquiries.in.the.Last.6.Months

hist(Interest.Rate,col="blue")
rug(Interest.Rate)
abline(v=c(0.11,0.12,0.13,0.14),col="red")

table(Home.Ownership,Interest.Rate)
plot(Home.Ownership,Interest.Rate)
table(Home.Ownership)
?plot()

plot(Interest.Rate,Home.Ownership)
rug(Interest.Rate,Home.Ownership)
table.freq(Home.Ownership,Interest.Rate)
boxplot(Interest.Rate)
summary(loanamt)

plot(FICO.Range,Interest.Rate)
table(FICO.Range)

library(ggplot2)
qplot(FICO.Range,Interest.Rate,data=loanamt)
boxplot(Interest.Rate~FICO.Range,loanamt,xlab="Fico range",ylab="Interest rate",col=FICO.Range)
title("INterest rate vs fico range")
?legend()
legend(side=3,col=levels(FICO.Range),legend=levels(FICO.Range),cex=.2)
?text()
mtext("Buckle up",side=3)

example("text")
levels(FICO.Range)

names(loanamt)
str(loanamt)
table(Interest.Rate,Employment.Length)

boxplot(Interest.Rate~Employment.Length,loanamt,col=Employment.Length)
mtext(FICO.Range,side=3)
?mtext()

pdf(file="boxplot.pdf")
boxplot(Interest.Rate~Employment.Length,loanamt,col=Employment.Length)
dev.off()

install.packages("swirl")
library(swirl)
install_from_swirl("Exploratory Data Analysis")
swirl()
bye()


fileurl<-""
download.file(fileurl,"dataset.csv",method=)


install.packages("xlsx")
install.packages("rJava")
library(rJava)

library(xlsx)
setwd("d:/r")
bank<-read.xlsx("bank.xlsx",sheetIndex=1,header=TRUE)

install.packages("XLConnect")

install.packages("XML")
library(XML)
fileUrl<-"http://www.w3schools.com/xml/simple.xml"
doc<-xmlTreeParse(fileUrl,useInternal=TRUE)
rootNode<-xmlRoot(doc)
xmlName(rootNode)
doc



#Week 1 quiz : Cleaning data
setwd("f:/r")
url1<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url1,"commun.csv")
commun<-read.csv("commun.csv")
str(commun)

new1<-data.frame()
for(i in 14:24)
{
  new1<-as.data.frame(rbind(new1,as.data.frame(dt[dt$VAL==i,])))

  }

val<-numeric()
for(i in 14:24)
{
val<-append(val,commun$VAL[new1$VAL==i])
}
val<-val[complete.cases(val)]
val
val[val==24]


new
new<-dt[dt$VAL==14,]
class(new)
mew<-as.data.frame(new)
class(mew)
install.packages("data.table")
library(data.table)
dt<-data.table(commun)
dt[dt$VAL==c(14:24),]


dt[dt$VAL==24,]
cho<-dt[commun$VAL==c(14:24),]


names(commun)

commun1<-na.omit(commun)

total<-function(dataset)
{
  tots<-0
  for(i in 1:length(dataset))
  {
    for(j in 14:24)
    {
    if(dataset[i]==j)
      {
        tots=tots+1
    }
      
    }
  }
tots  
}

  


commun$val<-ifelse(is.na(commun$val),0,commun$val)

val<-commun[,37]
val1<-na.omit(val)
val1
val1<-complete.cases(val)
val2<-val[val1]
val2
total(val2)

nrow(commun)
total(commun)


####################Clustering in data R############################################

setwd("F:/R")
library(readxl)
app<-read_excel("Bank.xlsx",sheet=2)
str(app)

summary(app$Demat_Ac)
table(app$Demat_Ac)
pairs(app)

class(app$Mortgage)
class(app$CCAvg)
boxplot(Mortgage~Demat_Ac,data=app)
boxplot(CCAvg~Demat_Ac,data=app)



##K means clustering
install.packages("Rcmdr")
library(Rcmdr)
library(cluster)
cdata<-app
names(cdata)
str(cdata)
dim(cdata)
#Data transformation
cdata$ID<-as.character(cdata$ID)# converting data into factor
cdat<-cdata
names(cdat)
class(cdat)
cdat<-cdat[,-1]
#scaling the data
dcl<-scale(cdat)

#Detection of clusters from scree plot
sumsq<-(nrow(dcl)-1)*sum(apply(dcl,2,var))
for(i in 1:10)sumsq[i]<-sum(KMeans(dcl,centers=i,iter.max=50000,
                                   num.seeds=5000)$withinss)

plot(1:10,sumsq,type="b",xlab="Number of clusters",ylab=
       "within groups sum of squares")



#k-means cluster #default algo
km<-KMeans(dcl,centers=4,iter.max=5000,num.seeds=12345)
km$size
km$centers
km$withinss
km$cluster
summary(km)

str(km)
str(app)
app$cluster<-km$cluster
write.csv(file="newbank.csv",app)

newbank<-read.csv("newbank.csv")

boxplot()
?with()
?subset()
one<-subset(app,cluster==1)
two<-subset(app,cluster==2)
three<-subset(app,cluster==3)
four<-subset(app,cluster==4)

table(one$Demat_Ac)
table(two$Demat_Ac)
table(three$Demat_Ac)
table(four$Demat_Ac)


names(app)

?boxplot()
par(mfrow=c(1,4))
boxplot(one$Pexp,ylab="pexp")
boxplot(two$Pexp)
boxplot(three$Pexp)
boxplot(four$Pexp)
title("Pexp accross 4 clusters")
dev.off()

par(mfrow=c(1,4))
boxplot(one$Age,ylab="pexp")
boxplot(two$Age)
boxplot(three$Age)
boxplot(four$Age)
title("age accross 4 clusters")
dev.off()


par(mfrow=c(1,4))
boxplot(one$Income,ylab="income")
boxplot(two$Income)
boxplot(three$Income)
boxplot(four$Income)
title("Income accross 4 clusters")
dev.off()

par(mfrow=c(1,4))
boxplot(one$Family,ylab="family")
boxplot(two$Family)
boxplot(three$Family)
boxplot(four$Family)
title("Pexp accross 4 clusters")
dev.off()

par(mfrow=c(1,4))
boxplot(one$Mortgage,ylab="family")
boxplot(two$Mortgage)
boxplot(three$Mortgage)
boxplot(four$Mortgage)
title("Pexp accross 4 clusters")
dev.off()









