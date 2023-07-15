########## Question 1 

  

#To import the dataset in R 

  

KenyaCarInsurance<-read.csv("KenyaCarInsurance.csv",header=TRUE,sep=',') 

head(KenyaCarInsurance) 

 # Turn the rating factors Age_group, Gender and License_type into categorical variables
  
KenyaCarInsurance <- within(KenyaCarInsurance, { 

    age <- factor(Age_group) 

    gender <- factor(Gender) 

    license <- factor(License_Type) 

}) 

# The number of tariff cells is  
  

#There are 3 rating factors Age_group, Gender and License_type
# and the number of categories for each rating factor are 

levels(KenyaCarInsurance$age) 

levels(KenyaCarInsurance $gender) 

levels(KenyaCarInsurance $license) 

  

  

#The predictor age has 6 categories, while the predictors gender and license-type have 2 categories 

#For each tariff cell, we have information about duration (the number of policyholder years of experience within the tariff cell), number (number of claims in the tariff cell) and severity (average claim for each tariff cell) 

########## Question 2 

#The new basecell is 

  

print(basecell<- KenyaCarInsurance[which.max(KenyaCarInsurance$exposure_years),1:3]) 

#Change basecell 

library(dplyr) 

KenyaCarInsurance$age<-relevel(KenyaCarInsurance$age, as.character(basecell$Age_group)) 

KenyaCarInsurance$gender<-relevel(KenyaCarInsurance$gender, as.character(basecell$Gender)) 

KenyaCarInsurance$license<-relevel(KenyaCarInsurance$license, as.character(basecell$License_Type)) 

########## Question 3 

#relative Poisson glm model 

  

summary(freq <-glm(nb_claims ~ age + gender + license + offset(log(exposure_years)), family=poisson("log"), data=KenyaCarInsurance[KenyaCarInsurance$exposure_years > 0,])) 

#Scaled deviance 

cbind(scaled.deviance=freq$deviance,df=freq$df.residual,p=1-pchisq(freq$deviance,freq$df.residual)) 

# The fit of the model as measured by the deviance is not good. The (scaled) deviance statistic is  

freq$deviance  

#which is above the critical value at a 95% confidence level of a chi-square rv with  

freq$df.residual 

# degrees of freedom, which is  

qchisq(0.95,freq$df.residual) 

# As such, we have evidence to reject the null hypothesis and we therefore conclude that the relative Poisson model does  not provide a good fit for the data set in question. 

 

########## Question 4 

#relative quasiPoisson glm model 

summary(freqqp <-glm(nb_claims ~ age + gender + license + offset(log(exposure_years)), family=quasipoisson("log"), data=KenyaCarInsurance[KenyaCarInsurance$exposure_years > 0,])) 

#with estimated dispersion

print(freqqp.phi<-summary(freqqp)$dispersion)

#we compute the scaled deviance and this time we do not reject the null hypothesis that the quais-Poisson model provides a good fit for the data, as our p-value is now 0.429 

cbind(scaled.deviance=freqqp$deviance/freqqp.phi,df=freqqp$df.residual,p=1-pchisq(freqqp$deviance/freqqp.phi,freqqp$df.residual)) 
 

########## Question 5 

#suggestion to simplify the relative Poisson glm model: based on the parameter estimation, we suggest to remove gender

summary(freqqp1 <-glm(nb_claims~ age + license + offset(log(exposure_years)), family=quasipoisson("log"), data=KenyaCarInsurance[KenyaCarInsurance$exposure_years > 0,])) 

#Likelihood ratio test H0: beta of the rating factor "gender" is 0 Ha: this betas is different than 0 

print(ltest1<-anova(freqqp1,freqqp)) 

qchisq(0.95,ltest1$Df[2]) 

#with the scaled deviance of  

ltest1$Deviance[2]/freqqp.phi 

1-pchisq(ltest1$Deviance[2]/freqqp.phi,ltest1$Df[2]) 

  
#The (scaled) deviance moves up from  

freqqp$deviance/freqqp.phi 

# for the model in Q4 to  

freqqp1$deviance/freqqp.phi 

  

#when the predictor gender is not included  in the relative Poisson glm model. Given that  


ltest1$Df[2] 

#degrees of freedom are gained with the simplified model, we shall compare the gain in deviance of  

freqqp1$deviance/freqqp.phi - freqqp$deviance/freqqp.phi  


#to the critical value at a 95% confidence level of a chi-square rv with  

ltest1$Df[2] 

# df which is  

qchisq(0.95,ltest1$Df[2]) 

#As such, we are statistically justified to simplify the model  

########## Question 6 

###### Part (a) 

#Relativities and CI (at a 95% confidence level) for the relative Poisson glm model 

cbind(exp(freqqp1$coefficients),exp(freqqp1$coefficients-qnorm(0.975)*sqrt(diag(vcov(freqqp1)))),exp(freqqp1$coefficients+qnorm(0.975)*sqrt(diag(vcov(freqqp1))))) 

###### Part (b) 

#The tariff cell with the most expected claim per exposure period is age group 1 and license 2 

#You identify this tariff cell by picking for each rating factor the beta which is the greatest 

#The expected number of claim over one policy yearsfor a policyholder in this tariff cell is  

prod(exp(freqqp1$coefficients)[c(1,2,7)]) 

#The tariff cell with the less expected claim per exposure period is age group 5 and license 1  

#You identify this tariff cell by picking for each rating factor the beta which is the smallest 

#The expected number of claim over two policy years for a policyholder in this tariff cell is  

prod(exp(freqqp1$coefficients)[c(1,5)] ) 

########## Question 7 

#gamma glm for severity

summary(sev <- glm(severity ~ age + gender + license, family = Gamma("log"), data = KenyaCarInsurance[KenyaCarInsurance$nb_claims > 0, ], weights = nb_claims)) 

sev.phi<-summary(sev)$dispersion 

cbind(res.deviance = sev$deviance/sev.phi, df = sev$df.residual, p = 1-pchisq(sev$deviance/sev.phi, sev$df.residual)) 

# As measured by the deviance statistic, the gamma glm fit seems to be quite good. We obtain an unscaled deviance of  

sev$deviance  

# with an estimated dispersion parameter of  

sev.phi  

#which leads to a scaled deviance of  

sev$deviance/sev.phi 

  

#This should be compared with the critical value at a 95% confidence level of a chi-square rv with  

 
sev$df.residual 
 
# df which is  

qchisq(0.95,sev$df.residual)  

# yielding a p-value for the test of  

1-pchisq(sev$deviance/sev.phi, sev$df.residual) 

# The gamma glm seems to offer a very good fit to the data 


########## Question 8 


# Merge categories 6 with 4 for age  
 
levels(KenyaCarInsurance$age)<-recode(levels(KenyaCarInsurance$age), "4"="4 & 6") 
levels(KenyaCarInsurance$age)<-recode(levels(KenyaCarInsurance$age), "6"="4 & 6") 

levels(KenyaCarInsurance$age)<-recode(levels(KenyaCarInsurance$age), "5"="5") 

 summary(sev1 <- glm(severity~ age + gender + license, family = Gamma("log"), data = KenyaCarInsurance[KenyaCarInsurance$nb_claims > 0, ], weights = nb_claims)) 

 

#Likelihood ratio test  


print(ltest3<-anova(sev1,sev)) 

#compare the critical value of  

qchisq(0.95,ltest3$Df[2]) 

# with the scaled deviance of  

  

ltest3$Deviance[2]/sev.phi 

  

# Otherwise, the p-value of the test is  

  

1-pchisq(ltest3$Deviance[2]/sev.phi,ltest3$Df[2]) 

  

  

# The likelihood ratio test consists in H0: combined beta for age group 4 and age group 6 vs Ha: More general model in Q7 

  

# The unscaled deviance moves up from  

  

sev$deviance 

  

# for the model in Q7 to  

  

sev1$deviance  

  

# for the model in Q8. Given that  

  

ltest3$Df[2] 

  

# degrees of freedom are gained with the simplified model, we shall compare the gain in scaled deviance of  

  

(sev1$deviance-sev$deviance)/sev.phi 

  

# to the critical value at a 95% confidence level of a chi-square rv with  

  

ltest3$Df[2]  

  

# df which is  

  

qchisq(0.95,ltest3$Df[2]) 

  

#As such, we are statistically justified to simplify the model in Q7 to the model in Q8 
  

########## Question 9 

#Multipliers with 95% confidence interval 

  

cbind(exp(sev1$coefficients),exp(sev1$coefficients-qt(0.975,sev1$df.residual)*sqrt(diag(vcov(sev1)))),exp(sev1$coefficients+qt(0.975,sev1$df.residual)*sqrt(diag(vcov(sev1))))) 

  

  

#The tariff cell with the largest expected claim amount is age = 1, gender =2  and license is 2 

  

prod(exp(sev1$coefficients)[c(1,2,6,7)] ) 

  

#while the tariff cell with the less expected claim amount is age = 5, gender = 1, license = 1  

prod(exp(sev1$coefficients)[c(1,5)] ) 

 

########## Question 10 

  

  

# For the tariff cell age = 2, gender = male (1) , license = 2 the expected key frequency ratio is  

  

prod(exp(freqqp1$coefficients)[c(1,3,7)]) 

  

# while the expected severity key ratio is  

  

prod(exp(sev1$coefficients)[c(1,3,6,7)]) 

  

# for a pure premium of  

  

prod(exp(freqqp1$coefficients)[c(1,3,7)])*prod(exp(sev1$coefficients)[c(1,3,6,7)]) 

  

  

  

# For the tariff cell age = 6, gender = female and license = 1 the expected key frequency ratio is  

prod(exp(freqqp1$coefficients)[c(1,6)]) 

  

  

# while the expected severity key ratio is  

  

prod(exp(sev1$coefficients)[c(1)]) 

  

  

# for a pure premium of  

  

prod(exp(freqqp1$coefficients)[c(1,6)])*prod(exp(sev1$coefficients)[c(1)]) 

  

  

  

  

# Therefore, the expected pure premium for the 60 policies is 

  

20*prod(exp(freqqp1$coefficients)[c(1,3,7)])*prod(exp(sev1$coefficients)[c(1,3,6,7)])+40*prod(exp(freqqp1$coefficients)[c(1,6)])*prod(exp(sev1$coefficients)[c(1)]) 

  

 ## 30622.59 

 

  

  

# For the set of policies, the expected number of claims is  

print(meannb<-20*prod(exp(freqqp1$coefficients)[c(1,3,7)])+40*prod(exp(freqqp1$coefficients)[c(1,6)])) 

 
  

  

  

## To produce a R markdown file 

  

#library(rmarkdown) 

#render("CodeTest2.R") 