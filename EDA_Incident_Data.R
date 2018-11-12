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
incident_data <- read.csv("Asknow Incdients Data 6 months.csv", header = T)

str(incident_data)

summary(incident_data)

summary(as.date(substr(incident_data$Resolved,1,10),"dd-mm-yyyy"))

sapply(incident_data, function(x) sum(is.na(x)))

# incidents_cont <- Filter(is.numeric, incident_data)
# incidents_cat <- Filter(is.factor, incident_data)

summary(incident_data$DurationHrs)

str(inc2)

incident_data %>% group_by(Assigned.to) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% group_by(Assignment.group) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% group_by(Classificaion) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% group_by(Caller1) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% group_by(Classificaion) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% filter(grepl("Integration", Caller1)) %>% group_by(Classificaion) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% filter(grepl("Integration", Caller1)) %>% group_by(Classificaion, Category) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt))

incident_data %>% group_by(Short.description) %>% summarize(cnt=n(), avg_duration=mean(DurationHrs, na.rm=T)) %>% arrange(desc(cnt)) -> top_10_desc

View(top_10_desc)
## Plotting
summary(incident_data$DurationHrs)

incident_data[is.na(incident_data$DurationHrs),]

#incident_data[!is.na(incident_data$DurationHrs),]

incident_data <- incident_data[!is.na(incident_data$DurationHrs),]

ggplot(incident_data, aes(DurationHrs)) + geom_histogram(bins=10, col="red", fill="grey")

table(incident_data$Assignment.group)

table(incident_data$Assignment.group, incident_data$First.assignment.group)
