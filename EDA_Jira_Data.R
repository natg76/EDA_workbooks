# Clearing the workspace
rm(list = ls())

#-------------------------------------------------------
# Section 1: Preliminary Tasks
#-------------------------------------------------------

# install.packages("dplyr")
library(dplyr)

# install.packages("dplyr")
library(tidyr)

# install.packages("ggplot2")
library(ggplot2)

library(MASS)

# Loading Demographic data in the working directory. 
jira_data <- read.csv("HadoopInfra Tableau JIRA data.csv", header = T, stringsAsFactors = F)

str(jira_data)

#View(jira_data)

summary(jira_data)


colSums(is.na(jira_data))


## Cont variables & Outlier Treatments

summary(jira_data$Age.Business.Days)

boxplot(jira_data$Age.Business.Days)

crop_outlier <- function(x) {
  quantiles <- quantile(x, c(0.05, 0.95), na.rm=TRUE )
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

quantile(jira_data$Age.Business.Days, c(0.05,0.95), na.rm=TRUE)

jira_data$Age.Business.Days <- crop_outlier(jira_data$Age.Business.Days)

boxplot(jira_data$Age.Business.Days)

boxplot(jira_data$Age.Business.Days ~ jira_data$Issue.Type, col="light grey") 
title ("Ticket Age by Issue Type")

boxplot(jira_data$Age.Business.Days ~ jira_data$Tags1, col="light blue") 
title("Ticket Age by Tag")

boxplot(jira_data$Age.Business.Days ~ jira_data$Issue.Type, col="light blue") 
title("Ticket Age by Issue Type")

boxplot(jira_data$Age.Business.Days ~ jira_data$Assignee, col="light blue") 
title("Ticket Age by Issue Type")


## Factor Variables


sapply(jira_data, function(x) sum(is.na(x)))

# jiras_cont <- Filter(is.numeric, jira_data)
# jiras_cat <- Filter(is.factor, jira_data)

summary(jira_data$Age.Business.Days)
       
boxplot(jira_data$Age.Business.Days)


## Date fields 

unique(jira_data$Created)

## It shows some of the fields having timestamp attached to it. 
## Removing it as we are not going to do Time of the day analysis.

jira_data$created_date <- as.Date(jira_data$Created, format='%d-%m-%Y')
jira_data$resolved_date <- as.Date(jira_data$Resolved, format='%d-%m-%Y')
jira_data$update_date <- as.Date(jira_data$Updated, format='%d-%m-%Y')
jira_data$due_date <- as.Date(jira_data$Due.Date, format='%d-%m-%Y')


table(jira_data$Tags1, jira_data$Issue.Type )

## Data in mixed case in the follwong text fields and also have white spaces.
## Text cleaning

jira_data$Tags1 <- tolower(trimws(jira_data$Tags1))
jira_data$Tags2 <- tolower(trimws(jira_data$Tags2))
jira_data$Summary <- tolower(trimws(jira_data$Summary))


jira_data <- jira_data %>% filter(created_date >= '2018-01-01') 

str(jira_data)

jira_data %>% group_by(Issue.Type) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Tags1) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Tags2) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Tags1, Tags2) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Labels) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))
jira_data$Labels <- NULL

jira_data %>% group_by(Labels.1) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))
jira_data$Labels.1 <- NULL

# Many records dont have label fields attached. No use of having these fields for any analysis

jira_data %>% group_by(Resolution) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Status) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Priority) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Reporter) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

top_users <- jira_data %>% group_by(Assignee) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt))

jira_data %>% group_by(Summary) %>% summarize(cnt=n(), avg_duration=mean(Age.Business.Days, na.rm=T)) %>% arrange(desc(cnt)) -> top_10_desc

View(top_users)

## Plotting
summary(jira_data$Age.Business.Days)


## EDA Plots

jira1 <- filter(jira_data, !is.na(Age.Business.Days))

ggplot(jira1, aes(x=Age.Business.Days)) + geom_histogram(binwidth = 5, col="red", fill="light blue") + ggtitle("Tickets Age Distribution")

ggplot(jira1, aes(x=Age.Business.Days)) + geom_histogram(binwidth = 5, col="red", fill="light blue") + ggtitle("Tickets Age Distribution")
       
## Code for Text Field Analysis to be added..
