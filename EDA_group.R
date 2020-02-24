rm(list=ls())
setwd("C:\\Users\\86136\\Documents\\RPI\\Spring 2020\\6790-BUSINESS ANALYTICS CAPSTONE\\Project")
mydata = read.csv("year_2018_state_NY.CSV")

# Data processing and cleaning
data = mydata[which(mydata$action_taken %in% c(1,2,3)),]
data[which((data$action_taken==1)|(data$action_taken==2)),"action_taken"] = 1
data[which(data$action_taken==3),"action_taken"] = 0
#a = which((data$applicant_race_1 %in% c(6,7)))
#data = data[-a,]
#data = subset(data,!is.na(data$hud_median_family_income))

# Histogram
library(ggplot2)
ggplot(data) + 
  theme_bw() + theme(panel.grid.major=element_line(colour=NA))+
  geom_bar(aes(x = derived_race ))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5))+
  labs(title = "Race of applicants")
boxplot(data$loan_purpose~data$action_taken, main = "loan purpose to action taken")
ggplot(data) + 
  theme_bw()+theme(panel.grid=element_blank())+
  geom_bar(aes(x = derived_race ))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5))+
  labs(title = "Race of applicants")
# Correlation
#x = data[,c('loan_purpose','loan_amount_000s','census_tract_number','applicant_race_1','applicant_sex','applicant_income_000s','action_taken')]
#y = data[,c('loan_purpose','loan_amount_000s','census_tract_number','applicant_race_1','applicant_sex','applicant_income_000s','action_taken')]
#res = cor(x, y,use="pairwise.complete.obs")
#library(psych)
#corr.test(x, y)
#library(corrplot)
#corrplot(res)
#corrplot(res,add=TRUE, type="lower", method="number",diag=FALSE,tl.pos="n", cl.pos="n")
#summary(data$hud_median_family_income)
mydata$debt
x = mydata[c("loan_purpose","loan_amount","loan_to_value_ratio","interest_rate","total_loan_costs","lender_credits","loan_term","property_value","income","applicant_ethnicity.1","applicant_race.1","applicant_sex","action_taken")]
x$action_taken <- as.numeric(x$action_taken)
y <- as.numeric(unlist(y))
res = cor(x, x,use="pairwise.complete.obs")
str(x)
library(psych)
corr.test(x, x)
library(corrplot)
corrplot(as.matrix(res))
corrplot(as.matrix(res),add=TRUE, type="lower", method="number",diag=FALSE,tl.pos="n", cl.pos="n")
