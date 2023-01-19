# load the libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

#searching directory
getwd()


#Load the data

 Customer <- read_excel("Customer.xlsx")
 Travel_policies <- read_excel("Travel_policies.xlsx")
 Health_policies <- read_excel("Health Policies.xlsx") 
 Motor_policies <- read_excel("Motor Policies.xlsx")

#summary of all the tables
 
 summary(Customer)
 summary(Travel_policies)  
 summary(Health_policies)  
 summary(Motor_policies)  

 
 #joining All into one table
 
 ABT <-full_join(Customer, Travel_policies, by = c("TravelID" = "TravelID"),copy = FALSE)
  names(ABT)[names(ABT)=="policyStart"] = "PolicyStart_travel"
  names(ABT)[names(ABT)=="PolicyEnd"] = "PolicyEnd_travel"
 
  ABT <-full_join(ABT, Health_policies, by = c("HealthID" = "HealthID"),copy = FALSE)
  names(ABT)[names(ABT)=="policyStart"] = "PolicyStart_Health"
  names(ABT)[names(ABT)=="policyEnd"] = "PolicyEnd_Health"
  
  ABT <-full_join(ABT, Motor_policies, by = c("MotorID" = "MotorID"),copy = FALSE)
  names(ABT)[names(ABT)=="PolicyStart"] = "PolicyStart_Motor"
  names(ABT)[names(ABT)=="PolicyEnd"] = "PolicyEnd_Motor"
  
  summary(ABT) 
  view(ABT) 
  
 
#DATA Wrangling IN R
  
##Checking for Duplicates
  
  sum(duplicated(ABT))

#Check for duplicates in ID variables
  
  CustID=na.omit(ABT$CustomerID) #People with no customer Id have been ommited
  sum(duplicated(CustID)) # we have zero duplication in customer ID
  
  MotorID=na.omit(ABT$MotorID) 
  sum(duplicated(MotorID))
  
  HealthID=na.omit(ABT$HealthID) 
  sum(duplicated(HealthID))
  
  TravelID=na.omit(ABT$TravelID) 
  sum(duplicated(TravelID))
  
  
  #Check and Remove NA values
  
 summary(ABT$CustomerID)
 
 sum(is.na(ABT$CustomerID)) # 12 NA values in customer ID

 ABT%>%filter(!is.na(CustomerID))->ABTv2  #created a new table to remove NA values.

 summary(ABTv2$CustomerID) #summary shows no NA present
 
 sum(is.na(ABTv2))  # checking for overall null values
 mean(is.na(ABTv2)) # checking for overall mean
 

 #TO do numerical Analysis 
 
 str(ABTv2)
 ABT3_numerical = select_if(ABTv2,is.numeric) 
summary(ABT3_numerical) 

#AGE numerical Analysis
boxplot(ABT3_numerical$Age)
filter(ABT3_numerical,Age<0) #one has an age of -44
filter(ABT3_numerical, Age>90) #two of them have age more than 90(180 and 210)


#Solved AGE Data quality issues
ABTv2%>%
  mutate(Age=replace(Age, Age<18, NA),
         Age=replace(Age, Age>90,NA))->ABTv3_clean #used mutate to solve DQ issues of age and created new table ABTv3 insted of updating age in same ABTv2
boxplot(ABTv3_clean$Age) # now no age is less than 0 or more than 90
summary(ABTv3_clean$Age)

#Claim numerical Analysis
 boxplot(ABT3_numerical$Numclaims)
 
 #Dependent kids Analysis
 
  boxplot(ABTv2$DependentsKids)
  ABTv2%>%
    mutate(DependentsKids=replace(DependentsKids, DependentsKids<0, NA),
           DependentsKids=replace(DependentsKids, DependentsKids>3,NA))->ABTv3_clean
         
  boxplot(ABTv3_clean$DependentsKids)



#categorical Analysis of cardtype
  
table(ABTv2$CardType) # 721 have zero card type
ABTv2$CardType <- as.factor(ABTv2$CardType)
summary(ABTv2$CardType)

ABTv3_clean$CardType[ABTv3_clean$CardType == 0] <- NA
summary(ABTv3_clean$CardType) #replaced 0 from card type as NA's


#categorical Analysis of Gender
table(ABTv2$Gender)
ABTv2$Gender <- as.factor(ABTv2$Gender)
ABTv3_clean$Gender[ABTv3_clean$Gender == "f"] <- NA
ABTv3_clean$Gender[ABTv3_clean$Gender == "m"] <- NA
summary(ABTv3_clean$Gender) # removed f and m as NA's and improved the data quality

#comchanels

table(ABTv2$ComChannel)
ABTv3_clean$ComChannel[ABTv3_clean$ComChannel == "E"] <- NA 
ABTv3_clean$ComChannel[ABTv3_clean$ComChannel == "P"] <- NA
ABTv3_clean$ComChannel[ABTv3_clean$ComChannel == "S"] <- NA
table(ABTv3_clean$ComChannel) # solved com chanel and removed its quality issues.


#number of claims
table(ABTv3_clean$clm)
ABTv3_clean$clm[ABTv3_clean$clm == 0] <- "NO"
ABTv3_clean$clm[ABTv3_clean$clm == 1] <- "Yes"
table(ABTv3_clean$clm) # sorted no of claims to yes and no and made it more familiar


#barchart for analysis
library(psych)

hist(ABTv3_clean$Age) #only numerical data in barplot

plot(ABTv3_clean$Age, ABTv3_clean$DependentsKids )

ggplot(ABTv3_clean) + geom_bar(aes(x=Title))

ggplot(ABTv3_clean) + geom_bar(aes(x=Age))

ggplot(ABTv3_clean) + geom_bar(aes(x=ComChannel))

ggplot(ABTv3_clean) + geom_bar(aes(x=Location))

boxplot(ABTv3_clean$Age)

 summary(ABTv3_clean$Age)


















    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
 
   
  
  