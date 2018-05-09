### LOGISTIC REGRESSION
### Adult salary table
df <- read.csv('adult_sal.csv')
head(df)
library(dplyr)
### Data manupulation and data cleaning  
df <- select(df,-X)
### First column in X and that is repetition of index so removing those unnecessary column
head(df)
str(df)
### Strucure of each variables 
summary(df)
### Summary of variables 
table(df$type_employer)
library(Amelia)
### Amelia is used to locate missing variales index wise 
df1 <- missmap(df,main = 'Main',col= c('yellow','black'),legend = FALSE)
str(df1)
sum(is.na(df))
### This function is used to categories the variables to reduce the numder of factors

unem <- function(job){
  job <- as.character(job)
  if(job== 'Never-worked' | job=='Without-pay'){
    return('unempoyed')}
  else if(job== 'Local-gov' | job=='State-gov'){
    return('SL-gov')}
  else if(job== 'Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')}
  
  else{
    return(job)
  }
}

df$type_employer <- sapply(df$type_employer,unem)
### Applied above function to have reduced varibales to have less factors 
table(df$type_employer)

table(df$marital)

df$marital <- sapply(df$marital,group_marital)
table(df$marital)
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

table(df$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

country <- function(coun){
  if(coun %in% Asia){
    return('Asia')
  }else if(coun %in% North.America){
    return('North.America')
  }else if(coun %in% Europe){
    return('Europe')
  }else if(coun %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')
  }
}
### As we have huge numbers of country so cant be used as factor.so applied a function to reduce
### NUmber of factors
df$country <- sapply(df$country,country)
str(df)
library(Amelia)
is.na(df)
df[df == '?'] <- NA
table(df$type_employer)
df$type_employer <- sapply(df$type_employer,factor)
df$country <- sapply(df$country,factor)
df$marital <- sapply(df$marital,factor)
df$occupation <- sapply(df$occupation,factor)
#### COnvert country,marital,type_employer and occupation as a factor 
missmap(df,legend = FALSE,col = c('Yellow','Black'))

df <- na.omit(df)
df
### Removing all the null vaues with na.omit function
missmap(df,legend = FALSE,col = c('Yellow','Black'))
### Recheck it again for any null values
head(df)
library(ggplot2)
ggplot(df,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
ggplot(df,aes(x=age))+geom_histogram(aes(fill=income),binwidth = 1,color='black')
ggplot(df,aes(x=hr_per_week))+geom_histogram()

str(df)
rename(df,replace=c('country'='Region'))

names(df)[names(df)=="country"] <- "region"

ggplot(df,aes(region))+geom_bar(aes(fill=income))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
### changing name of country to region
### Plotting Graph of count and region based on income
head(df)
library(caTools)
### Spliting data set in the ration of 70:30 Train and Test data
sample <- sample.split(df$income,SplitRatio = 0.70)
df.train1 <- subset(df,sample==TRUE)
df.test1  <- subset(df,sample==FALSE)
df.test1
log.model <- glm(income ~.,family = binomial(link = 'logit'),data = df.train1)
### Applying logistic model on income
summary(log.model)
### Summary stats that their are many responsible features with * marks
new.step <- step(log.model)
### Select a formula-based model by AIC.
summary(new.step)
df.test1$predicted.income = predict(new.step, newdata=df.test1, type="response")
### Applied predict function on the model and new data(test data)
fitted.results <- ifelse(df.test1$predicted.income > 0.5,1,0)
### IF the value is more than 0.5 then change it to 1 and if value less than 0.5 then change it to 0
x <- mean(fitted.results != df.test1$income )
### Calculating mean of unmatched results
print(x)
table(df.test1$income, df.test1$predicted.income > 0.5)
accuracy <- (6406+1388)/(6406+514+907+1388)
### ACCURACY DEFIENES THE CORRECT RESULT FROM THE TABLE
accuracy
### 84% ACCURACY FOUND