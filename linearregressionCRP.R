################################project 1######################################

##The analytics problem of the business problem is to know how various characteristics(vaiables) are 
##related to the interest rate. For that we can build a linear regression model considering interest rate
##as dependent variable and other as independent variables.

#readint the data
setwd("f:/r")
loandata<-read.csv("loandata.csv")
backuploandata<-read.csv("loandata.csv")
str(loandata)
summary(loandata)
names(loandata)

#removing missing values
na<-is.na(loandata)
table(na)["TRUE"]

loandata<-na.omit(loandata)

na<-is.na(loandata)
table(na)["TRUE"]

str(loandata)


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

# Removing outliers
loandata<-na.omit(loandata)


#Making sure outliers dont exist anymore
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


#numeical variables
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

names(loandata)
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






#linear regression model(id variable is not included and variables whose value we wont have while
#implementing this model are also excluded)

names(train)

model1<-lm(Interest.Rate~Amount.Requested+Interest.Rate+Loan.Length+Loan.Purpose+Debt.To.Income.Ratio+State+Home.Ownership+Monthly.Income+                
FICO.Range+Open.CREDIT.Lines +Revolving.CREDIT.Balance+Inquiries.in.the.Last.6.Months+Employment.Length
,data=train)

summary(model1)


# Optimizing model using step function
library(MASS)
model2<-step(model1,direction = "both")
summary(model2) # The r square is 0.7943


#multicollinearity check
library(car)
vif(model2) ##if vif is over 10 then we will have multicollinearity problem


# Making predictions on train
pred<-predict(model2,train,type=c("response"))
train$pred<-pred  

# Calculating Total sum of squared errors and mean sum of squared errors
sum((train$Interest.Rate-pred)^2)

sum((train$Interest.Rate-pred)^2)/nrow(train)





# prediction on test
newdat<-predict(model2,newdata=test,type="response")

# There is an error in here
lets check if fico range is equally divided between train and test or not.

# Calculating sum of square errors in test dataset
sum((newdat-test$Interest.Rate)^2)






#To see if the assumptions of linear regression are being met or not
par(mfrow=c(1,4))
plot(model2)


unique(train$FICO.Range)


#For test dataset
predict<-predict(model2,newdata=test,type="response")
predict

## Mean sum of square errors
sum((test$Interest.Rate-predict)^2)/length(predict)

coef(model2)




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


