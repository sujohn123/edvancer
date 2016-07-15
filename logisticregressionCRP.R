


##################################Project 2##########################################################
#################################Logistic regression###############################################

#reading data
setwd("f:/r")
library(plyr)
library(dplyr)
bank<-read.csv("bank-full.csv")
backup<-bank
str(bank)

#Checking out overall summary of the data
summary(bank)

table(cut(bank$age,5))
boxplot(bank$age,bank$y)
library(ggplot2)
ggplot(bank,aes(y,age))+geom_boxplot()




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




#For job variable


#Merging of certain categories of categorical variables having a lot of categories according to their 
#distribution to y=1


table(bank$job,bank$y)
monty1<-as.data.frame(table(bank$job,bank$y))
monty1
yess1<-monty1$Freq[13:24]
monty1<-monty1[1:12,-2]
monty1$yes<-yess1
monty1
names(monty1)[1]<-"job"
names(monty1)[2]<-"no"
monty1$peryes<-(((monty1$yes)/(monty1$yes+monty1$no))*100)

monty1<-arrange(monty1,desc(peryes))

#job  no yes    peryes
#1        unknown   1   2 66.666667
#2      housemaid   9   6 40.000000
#3     unemployed  12   7 36.842105
#4        student  13   6 31.578947
#5     management 154  53 25.603865
#6     technician 113  36 24.161074
#7       services  53  12 18.461538
#8   entrepreneur  20   3 13.043478
#9         admin.  92  12 11.538462
#10 self-employed  30   3  9.090909
#11       retired  11   1  8.333333
#12   blue-collar 153  13  7.831325



bank$dum_job_one<-ifelse(bank$job=="unknown"|bank$job=="housemaid" |bank$job=="unemployed",1,0)
bank$dum_job_two<-ifelse(bank$job=="student" | bank$job=="management" |bank$job=="technician",1,0)
bank$dum_job_three<-ifelse(bank$job=="services" | bank$job=="entrepreneur" |bank$job=="admin.",1,0)


str(bank)


#For variable month
#calculating percentage of yes for each category of month variable in order to merge those which have 
# similar values 

class(table(bank$month,bank$y))

monty<-as.data.frame(table(bank$month,bank$y))
yess<-monty$Freq[13:24]
monty<-monty[1:12,-2]
monty$yes<-yess
monty
names(monty)[1]<-"Month"
names(monty)[2]<-"No"
monty$peryes<-(((monty$yes)/(monty$yes+monty$No))*100)
monty<-arrange(monty,desc(peryes))
monty

#Month     No yes    peryes
#1    oct   8  11 57.894737
#2    sep  10  12 54.545455
#3    jun  18  15 45.454545
#4    mar   9   7 43.750000
#5    aug  32  23 41.818182
#6    jul  13   8 38.095238
#7    dec  10   6 37.500000
#8    jan  18   6 25.000000
#9    apr  80  12 13.043478
#10   feb 118  16 11.940299
#11   may 161  19 10.555556
#12   nov 184  19  9.359606


bank$dum_month_one<-ifelse(bank$month=="oct"|bank$job=="sep" |bank$month=="jun",1,0)
bank$dum_month_two<-ifelse(bank$month=="mar"|bank$job=="aug" |bank$month=="july",1,0)
bank$dum_month_three<-ifelse(bank$month=="dec"|bank$job=="jan" |bank$month=="apr",1,0)






#################### Samplinge######################

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
######marketing campaign data and We have to delete categorical variables of which we have created dummy variables

trainset$campaign<-NULL
trainset$pdays<-NULL
trainset$previous<-NULL
trainset$poutcome<-NULL

trainset$job<-NULL
trainset$contact<-NULL
trainset$marital<-NULL
trainset$education<-NULL
trainset$default<-NULL
trainset$housing<-NULL
trainset$loan<-NULL
trainset$month<-NULL
trainset$poutcome<-NULL

testset$campaign<-NULL
testset$pdays<-NULL
testset$previous<-NULL
testset$poutcome<-NULL

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



################Model Building(Logistic regression)

#deleting y variable and then replacing it with a variables which contain the values 0 and 1 instead of yes
#and no
trainset$y1<-ifelse(trainset$y=="yes",1,0)
testset$y1<-ifelse(testset$y=="yes",1,0)
trainset$y<-NULL
testset$y<-NULL

names(trainset)[22]<-"y"
names(testset)[22]<-"y"


#Logistic regression function
model1<-glm(y~.,family=binomial(logit),data=trainset)

summary(model1)
library(car)
vif(model1)

#stepAIC will automatically create more than 1 model and then choose the best out of it
library(MASS)
fitstep<-stepAIC(model1)
summary(fitstep)

# To check the multicollinearity(to see if there is realtionship between independent variables or not)
vif(fitstep)  

prob<-predict(fitstep,trainset,type=c("response"))
prob
trainset$prob<-prob




##### Concordance and discordance

Concordance = function(GLM.binomial) {
        outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
        # get a subset of outcomes where the event actually happened
        ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
        # get a subset of outcomes where the event didn't actually happen
        zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
        # Equate the length of the event and non-event tables
        if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
        else {zeros = zeros[1:length(ones[,1]),]}
        # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
        ones_and_zeros = data.frame(ones, zeros)
        # initiate columns to store concordant, discordant, and tie pair evaluations
        conc = rep(NA, length(ones_and_zeros[,1]))
        disc = rep(NA, length(ones_and_zeros[,1]))
        ties = rep(NA, length(ones_and_zeros[,1]))
        for (i in 1:length(ones_and_zeros[,1])) {
                # This tests for concordance
                if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
                {conc[i] = 1
                disc[i] = 0
                ties[i] = 0}
                # This tests for a tie
                else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
                {
                        conc[i] = 0
                        disc[i] = 0
                        ties[i] = 1
                }
                # This should catch discordant pairs.
                else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
                {
                        conc[i] = 0
                        disc[i] = 1
                        ties[i] = 0
                }
        }
        # Here we save the various rates
        conc_rate = mean(conc, na.rm=TRUE)
        disc_rate = mean(disc, na.rm=TRUE)
        tie_rate = mean(ties, na.rm=TRUE)
        return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
        
}


Concordance(fitstep)

## The concordance of our model is 0.88 which is pretty good.


#Sensitivity and specificity and :ROC Curve for train dataset

library(pROC)
g<-roc(y~prob,data=trainset)
summary(g)
plot(g)

table(trainset$y)

#confustion matrix for specificity and sensitivity for train dataset
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
  cat("false.pos.rate=",round(false.pos.rate,2),"\n")
  cat("false.neg.rate=",round(false.neg.rate,2),"\n")
  cat("---------------------------------","\n")
  cat("---------------------------------","\n")
}


##Here since the value of % correct is more in 0.1 we take 0.1 as cutoff
#sensitivity and specificity

for(T in seq(0.1,0.9,0.1))
{
        cat("For cutoff=",T,"\n")
        cat("--------------------------","\n")
        class1=ifelse(trainset$prob>T,1,0)
        #compute confusion matrix
        confusion=table(class1,trainset$y)
        specificity=confusion[[1,1]]/(confusion[[1,1]]+confusion[[2,1]])
        sensitivity=confusion[[2,2]]/(confusion[[1,2]]+confusion[[2,2]])
        print(confusion)
        cat("specificity =",specificity,"\n")
        cat("sensitivity =",sensitivity,"\n")
        cat("\n")
        cat("\n")
}



###Applying the model to test dataset
prob1<-predict(fitstep,testset,type=c("response"))
prob1
testset$prob<-prob1

#ROC Curve of test dataset
library(pROC)
g1<-roc(y~prob,data=testset)
plot(g1)

detach(trainset)
attach(testset)

#confustion matrix 

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
  cat("false.pos.rate=",round(false.pos.rate,2),"\n")
  cat("false.neg.rate=",round(false.neg.rate,2),"\n")
  cat("---------------------------------","\n")
  cat("---------------------------------","\n")
  
}


#sensitivity and specificity at various cutpffs
for(T in seq(0.1,0.9,0.1))
{
        cat("For cutoff=",T,"\n")
        cat("--------------------------","\n")
        class1=ifelse(testset$prob>T,1,0)
        #compute confusion matrix
        confusion=table(class1,testset$y)
        specificity=confusion[[1,1]]/(confusion[[1,1]]+confusion[[2,1]])
        sensitivity=confusion[[2,2]]/(confusion[[1,2]]+confusion[[2,2]])
        print(confusion)
        cat("specificity =",specificity,"\n")
        cat("sensitivity =",sensitivity,"\n")
        cat("\n")
        cat("\n")
}




##Intrepretation

The model performs similarly between train and test dataset as we have seen from above results, lets
interpret the results.


#odds ratio
exp(coef(fitstep))
table<-as.data.frame(exp(coef(fitstep)))
table

## Odds ratio table


(Intercept)                     0.11426870
duration                        1.00513032
dum_education_primary           0.14823104
dum_education_secondary         0.42315039
dum_loan_no                     2.27243436
dum_contact_cellular            3.00516152
dum_poutcome_failure            0.06535238
dum_poutcome_other              0.06571956
dum_job_one                     3.28723984
dum_job_three                   0.31276568
dum_month_one                   3.86493919

names(table)[1]<-"oddsratio"

table$probability<-(table$oddsratio)/(1+(table$oddsratio))

table

To intrepret we will convert the odds into probability.

p/1-p=odds from this formula we can calculate probability.

                        oddsratio   probability
(Intercept)             0.11426870  0.10255040
duration                1.00513032  0.50127930
dum_education_primary   0.14823104  0.12909513
dum_education_secondary 0.42315039  0.29733357
dum_loan_no             2.27243436  0.69441709
dum_contact_cellular    3.00516152  0.75032218
dum_poutcome_failure    0.06535238  0.06134344
dum_poutcome_other      0.06571956  0.06166684
dum_job_one             3.28723984  0.76674970
dum_job_three           0.31276568  0.23824943
dum_month_one           3.86493919  0.79444758


If the customer doesn't have loan then the probability of that customer's conversion increases by 69%.
to that of the customers who has loan.

The customers who are contacted in their cell phones have additional probability of 75% to that of 
of landlines and other.

If customers are contacted in jun, oct, sep then their conversion probability increases by 79% to that
of months feb, may and nov.

Cuatomers with job as housemaid, unknown and unemployed are more likely to convert with additional 
probability of 76% to that of jobs like blue collar,self employed and retired.






