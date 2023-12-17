################################################################
#   Course        : BC2407 Analytics II Group Project          #
#   Team          : 07                                         #
#   Seminar Group : 01                                         #
################################################################


library(data.table)
library(stringr)
library(ggplot2)

setwd("/Users/penglee/Desktop/NBS/BC2407 Course Materials/Group Project/Datasets")
job = fread("Dataset2.csv")


#------------------------------------Data Cleaning--------------------------------------------------------------------------



#remove any NAs in the rows
job1 = na.omit(job[, c("JobSatisfaction", "JobSeekingStatus", "ImportantBenefits")])



#------------------------------------Text Mining--------------------------------------------------------------------------



#Text Mining to categorise the Important Benefits for employees
benefits = data.matrix(strsplit(job1$ImportantBenefits, ";"))
bfts = data.frame("Retirement" = ifelse(str_detect(benefits, "Retirement"), 1, 0))
bfts["Vacation/days off"]= ifelse(str_detect(benefits, "Vacation/days off"), 1, 0)
bfts["Health benefits"]= ifelse(str_detect(benefits, "Health benefits"), 1, 0)
bfts["Charitable match"]= ifelse(str_detect(benefits, "Charitable match"), 1, 0)
bfts["Annual bonus"]= ifelse(str_detect(benefits, "Annual bonus"), 1, 0)
bfts["Expected work hours"]= ifelse(str_detect(benefits, "Expected work hours"), 1, 0)
bfts["Equipment"]= ifelse(str_detect(benefits, "Equipment"), 1, 0)
bfts["Remote options"]= ifelse(str_detect(benefits, "Remote options"), 1, 0)
bfts["Long-term leave"]= ifelse(str_detect(benefits, "Long-term leave"), 1, 0)
bfts["Private office"]= ifelse(str_detect(benefits, "Private office"), 1, 0)
bfts["Professional development sponsorship"]= ifelse(str_detect(benefits, "Professional development sponsorship"), 1, 0)
bfts["Meals"]= ifelse(str_detect(benefits, "Meals"), 1, 0)
bfts["Stock options"]= ifelse(str_detect(benefits, "Stock options"), 1, 0)
bfts["Education sponsorship"]= ifelse(str_detect(benefits, "Education sponsorship"), 1, 0)
bfts["Child/elder care"]= ifelse(str_detect(benefits, "Child/elder care"), 1, 0)

#Change the datatype of categorical variable to factor
bfts$Retirement = factor(bfts$Retirement)
bfts$`Vacation/days off` = factor(bfts$`Vacation/days off`)
bfts$`Health benefits` = factor(bfts$`Health benefits`)
bfts$`Charitable match` = factor(bfts$`Charitable match`)
bfts$`Annual bonus` = factor(bfts$`Annual bonus`)
bfts$`Expected work hours` = factor(bfts$`Expected work hours`)
bfts$Equipment = factor(bfts$Equipment)
bfts$`Remote options` = factor(bfts$`Remote options`)
bfts$`Long-term leave` = factor(bfts$`Long-term leave`)
bfts$`Private office` = factor(bfts$`Private office`)
bfts$`Professional development sponsorship` = factor(bfts$`Professional development sponsorship`)
bfts$Meals = factor(bfts$Meals)
bfts$`Stock options` = factor(bfts$`Stock options`)
bfts$`Education sponsorship` = factor(bfts$`Education sponsorship`)
bfts$`Child/elder care` = factor(bfts$`Child/elder care`)

#Restructure the dataset 
job1 = cbind(job1[, -"ImportantBenefits"], bfts)
job1$JobSeekingStatus = ifelse(job1$JobSeekingStatus=="I am actively looking for a job", 1, 0)
job1$JobSatisfaction = ifelse(job1$JobSatisfaction >7, "High", ifelse(job1$JobSatisfaction >3, "Average", "Low"))
job1$JobSatisfaction = factor(job1$JobSatisfaction, levels = c("Low", "Average", "High"))
job1$JobSeekingStatus = factor(job1$JobSeekingStatus)

#test out the dependency of job seeking status and job satisfaction
chisq.test(job1$JobSeekingStatus, job1$JobSatisfaction, correct = FALSE)
#p-value = 3.476e-10 --> <0.05 so it is rejected, the 2 variables are dependent occurrences


#------------------------------------Data Visualization--------------------------------------------------------------------------

plot(job1$JobSatisfaction)
ggplot(data = job1, aes(JobSatisfaction, fill= Retirement)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes
ggplot(data = job1, aes(JobSatisfaction, fill= `Vacation/days off`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but no trend
ggplot(data = job1, aes(JobSatisfaction, fill= `Health benefits`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but no trend
ggplot(data = job1, aes(JobSatisfaction, fill= `Charitable match`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #no too little
ggplot(data = job1, aes(JobSatisfaction, fill= `Annual bonus`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes
ggplot(data = job1, aes(JobSatisfaction, fill= `Expected work hours`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but no trend
ggplot(data = job1, aes(JobSatisfaction, fill= Equipment)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #no below 0.50 and no trend
ggplot(data = job1, aes(JobSatisfaction, fill= `Long-term leave`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but only a few people
ggplot(data = job1, aes(JobSatisfaction, fill= `Remote options`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but the increase very small
ggplot(data = job1, aes(JobSatisfaction, fill= `Private office`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but not applicable
ggplot(data = job1, aes(JobSatisfaction, fill= `Professional development sponsorship`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #no
ggplot(data = job1, aes(JobSatisfaction, fill= Meals)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #yes but not relevant because workfromhome
ggplot(data = job1, aes(JobSatisfaction, fill= `Stock options`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #no
ggplot(data = job1, aes(JobSatisfaction, fill= `Education sponsorship`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #no
ggplot(data = job1, aes(JobSatisfaction, fill= `Child/elder care`)) + geom_bar(stat = "count", position = "fill") + ylab("Proportion of employees") #no


#------------------------------------Analysis & Conclusion--------------------------------------------------------------------------

#2 criteria:
  #(1) across all the job satisfaction, there is at least 1 more than 0.50 
  #(2) if there is a trend from low to average to high 

#Satisfy both conditions:
  #Retirement 
  #`Remote options`

#in general: every employees wants (no trend)
  #`Expected work hours`
  #`Vacation/days off`
  #`Health benefits`

#important benefits to different levels of job satisfaction  
  #`Long-term leave`
  #equipment
  #annual bonus
















