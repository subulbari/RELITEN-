# ~ R Code for the Assignment ~ #


#Set your working directory
getwd() #Finding the right path to set working directory
setwd("S:/SUMMER 2021/PSYC08/ASSIGNMENT") #Working directory has been set

#Load the data
gssdata = read.csv("General_Social_survey (1).csv")

#Load the packages
library(tidyverse)
library(lsr) #Needed to calculate Cohen's d
library(plotrix) #Need this to calculate standard error
library(psych) #Need this package to use the describe function

#Exploring Data
str(gssdata) #Making sure that the dependent variables are on ratio/interval scale of measurement and the independent variable is on categorical scale of measurement. This is to ensure that independent samples t-tests can be run.

#Recoding variables - changing numerical variable into character variable and creating 2 groups for the RELITEN (strength of religious preference) variable: "strong" and "weak/none."
gssdata$RELITEN_strength <- ordered(gssdata$RELITEN, levels = c(1,2,3,4), labels = c("strong", "strong", "weak/none", "weak/none"))

#Descriptive statistics
describe(gssdata$RELITEN) #Descriptive statistics to add to Appendix A in Final Report
describe(gssdata$PREMARSX) #Descriptive statistics to add to Appendix A in Final Report
describe(gssdata$SEXEDUC) #Descriptive statistics to add to Appendix A in Final Report
describe(gssdata$PILLOK) #Descriptive statistics to add to Appendix A in Final Report
describe(gssdata$AGE) #To find the mean and standard deviation of participant ages to be included in the "Participants" section of the final report.
describe(gssdata$SEX) #To understand the distribution of sex for the "Participants" section of the final report. 
describe(gssdata$POLVIEWS) #Political views of participants are relevant to the research question. This variable is used to describe the demographics of the participants. 

#Conduct first analysis

#Information about variables:
  # RELITEN: 1 = People who consider themselves to have strong religious preference, 2 = People who consider themselves to have weak or no religious preference
  # PREMARSX: Lower scores on PREMARSX mean people believe that they are more against premarital sex and high scores indicate support for premarital sex

#Conduct the t-test
ttest.independent_premarsx = t.test(PREMARSX ~ RELITEN_strength, data=gssdata, var.equal=T) #code to run independent samples t-test
print(ttest.independent_premarsx) #code to print the results of the first t-test

##First analysis: Effect size
cohensD(PREMARSX ~ RELITEN_strength, data=gssdata) #Cohen's d for first analysis

##First analysis: Find means, standard errors, standard deviations
 ###Calculating the mean of PREMARSX per RELITEN_strength:
tapply(gssdata$PREMARSX, gssdata$RELITEN_strength, mean, na.rm=T)

 ###Calculating the standard deviation of PREMARSX per RELITEN_strength:
tapply(gssdata$PREMARSX, gssdata$RELITEN_strength, sd, na.rm=T)

  ###Calculating the standard error of PREMARSX per RELITEN_strength:
tapply(gssdata$PREMARSX, gssdata$RELITEN_strength, std.error, na.rm=T)


#Conduct second analysis

#Information about variables:
  # RELITEN: 1 = people who consider themselves to have strong religious preference, 2 = people who consider themselves to have weak or no religious preference
  # SEXEDUC: Lower scores on SEXEDUC mean people consider themselves to be more in favour of sex education being taught in public school. Higher scores indicate that people are against sex education being taught in public schools.  

#Conduct the t-test
ttest.independent_sexeduc = t.test(SEXEDUC ~ RELITEN_strength, data=gssdata, var.equal=T) #code to run independent samples t-test
print(ttest.independent_sexeduc) #code to print the results of the first t-test 


##Second analysis: Effect size  
cohensD(SEXEDUC ~ RELITEN_strength, data=gssdata) #Cohen's d calculated for effect size


##Second analysis: Find means, standard errors, standard deviations 
  ###Calculating the mean of SEXEDUC per RELITEN_strength:
tapply(gssdata$SEXEDUC, gssdata$RELITEN_strength, mean, na.rm=T)

  ###Calculating the standard deviation of SEXEDUC per RELITEN_strength:
tapply(gssdata$SEXEDUC, gssdata$RELITEN_strength, sd, na.rm=T)

  ###Calculating the standard error of SEXEDUC per RELITEN_strength:
tapply(gssdata$SEXEDUC, gssdata$RELITEN_strength, std.error, na.rm=T)


#Conduct third analysis

#Information about variables:
  # RELITEN: 1 = People who consider themselves to have strong religious preference, 2 = People who consider themselves to have weak or no religious preference
  # PILLOK: Lower scores on PILLOK mean people consider themselves to be more in favour of methods of birth control being available to teenagers between the ages of 14 and 16 without their parents' approval. Higher scores on PILLOK mean people consider themselves to be more in favour of methods of birth control being available to teenagers between the ages of 14 and 16 without their parents' approval.

#Conduct the t-test
ttest.independent_pillok = t.test(PILLOK ~ RELITEN_strength, data=gssdata, var.equal=T) #code to run independent samples t-test
print(ttest.independent_pillok) #code to print the results of the first t-test 

#Third analysis: Effect size 
cohensD(PILLOK ~ RELITEN_strength, data=gssdata) #Cohen's d calculated for effect size

##Third analysis: Find means, standard errors, standard deviations 

  ###Calculating the mean of PILLOK per RELITEN_strength:
tapply(gssdata$PILLOK, gssdata$RELITEN_strength, mean, na.rm=T)

  ###Calculating the standard deviation of PILLOK per RELITEN_strength:
tapply(gssdata$PILLOK, gssdata$RELITEN_strength, sd, na.rm=T)

  ###Calculating the standard error of PILLOK per RELITEN_strength:
tapply(gssdata$PILLOK, gssdata$RELITEN_strength, std.error, na.rm=T)

