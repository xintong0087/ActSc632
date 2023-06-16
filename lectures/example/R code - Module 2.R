### Module 2 - moped data set

## command to indicate where to read the data file and store plots on your computer - please modify the path based on your own needs

setwd('C:/Users/X/Documents/GitHub/ActSc632/lectures/example')
## to read the moped insurance data and visualize the data

moped<-read.csv("moped.csv",header=TRUE,sep=',')
head(moped)
moped

# Observation: some tariff cells have very low duration and some have no claims over the duration period


## the predictors class, age and zone are categorical variables
## make use of the function "factor" to turn each variable into a categorical variable (otherwise, they are treated as quantitative)

moped <- within(moped, {
    class <- factor(class)
    age <- factor(age)
    zone <- factor(zone)
})


## the function "levels" enumerate the different outcomes/categories of each predictor

levels(moped$class)
levels(moped$age)
levels(moped$zone)

# Observation: class and age have only two categories, while zone has a total of 7 categories


## to better understand the relationship between predictors and claim frequency


plot(moped$age, moped$frequency, col="red",xlab="age",ylab="claim frequency")
plot(moped$class, moped$frequency, col="red",xlab="class",ylab="claim frequency")
plot(moped$zone, moped$frequency, col="red",xlab="zone",ylab="claim frequency")


# Observations: claim frequency seems to vary quite a bit between the different "zone" categories, this seems to be less pronounced for age and even less so for class  


## GLM fitting 

# if nothing is done, (1,1,1) is the base tariff cell
# we usually want the base tariff cell to be the one with the largest exposure (e.g., largest duration) so we pick tariff cell (1,2,4) to be the base tariff cell
# this is because all tariff cells can be easily compared to the base tariff cell (which preferably should be a tariff cell well known by the insurer)  
# You can reorder the levels of each categorical variable to achieve this

print(basecell<- moped[which.max(moped[,4]),1:3])
print(moped$class<- relevel(moped$class, as.character(basecell$class)))
print(moped$age<- relevel(moped$age, as.character(basecell$age)))
print(moped$zone<- relevel(moped$zone, as.character(basecell$zone)))

# fit relative Poisson glm (with phi=1) for nb of claims that uses an offset

summary(freq<-glm(number ~ class + age + zone + offset(log(duration)), data = moped[moped$duration>0,], family=poisson("log")))


# IF the glm function could admit "relative.poisson" as a family (which is not the case), this is how we would code it
# summary(freq <- glm(frequency ~ class + age + zone, data = moped[moped$duration > 0, ], family = relative.poisson("log"), weights = duration))


# fits a gamma glm on claim severity, using only the classes that have more than one claim

summary(sev <- glm(severity ~ class + age + zone, data = moped[moped$number > 0, ], family = Gamma("log"), weights = number))

# Observation: Check beta coefficients obtained via MLE under the column "Estimate" of the output


############### everything above covers material up to and including Section 2.6 in the lecture notes



## Deviance

# Frequency model: fit of the relative Poisson model
# Use the residual deviance statistic
# This computes the p-value corresponding to the residual deviance provided in R
# The deviance test indicates there is not enough evidence to reject the fitted model at a 95% confidence level (just barely though) as the p-value is slightly superior to 5%

cbind(scaled.deviance=freq$deviance,df=freq$df.residual, p=1-pchisq(freq$deviance, freq$df.residual))

# Gamma severity model
# Need to compute the scaled deviance by first extracting the phi parameter
# seems to indicate a good fit using the deviance statistic as the p-value is slightly above 50%

sev.phi<-summary(sev)$dispersion
cbind(scaled.deviance = sev$deviance/sev.phi, df = sev$df.residual, p = 1-pchisq(sev$deviance/sev.phi, sev$df.residual))



## Pearson's goodness of fit

# Frequency model: fit of the relative Poisson model
# This time, the goodness of fit test rejects the null hypothesis that the relative Poisson provides a good fit as the p-value of the test is < 5%

chifreq<-sum(residuals(freq,type="pearson")^2)
cbind(scaled.pearson = chifreq, df = freq$df.residual, p = 1-pchisq(chifreq, freq$df.residual))


# Severity model: fit of the gamma model
# The goodness of fit test is consistent with the conclusion we reached with the deviance test. No evidence to reject the fitted gamma model

chisev<-sum(residuals(sev,type="pearson")^2)
cbind(scaled.pearson = chisev/sev.phi, df = sev$df.residual, p = 1-pchisq(chisev/sev.phi, sev$df.residual))



## Estimation of phi for the gamma severity model
# Check to verify that R uses Method 1 in the notes to estimate phi

chisev/sev$df.residual
print(sev.phi)


## Hierarchical model

summary(freq)

# from the fitted frequency model, we see that zones 5, 6 and 7 do not seem to be statistically different than zone 4 so we want to combine all 4 zones into one using the function "recode" in R under the library "dplyr"
install.packages("dplyr")
library(dplyr)

levels(moped$zone)<-recode(levels(moped$zone), "4"="4+")
levels(moped$zone)<-recode(levels(moped$zone), "5"="4+")
levels(moped$zone)<-recode(levels(moped$zone), "6"="4+")
levels(moped$zone)<-recode(levels(moped$zone), "7"="4+")

# run the simplified poisson glm, fit is improved relative to the number of parameters
# the residual deviance statistic improves

summary(freq.new<-glm(number ~ class + age + zone + offset(log(duration)), data = moped[moped$duration>0,], family=poisson("log")))
cbind(scaled.deviance = freq.new$deviance, df = freq.new$df.residual, p = 1-pchisq(freq.new$deviance, freq.new$df.residual))


# show that we are statistically justified to choose the simplified model (over the more complicated model)

cbind(diff.scaled.deviance=freq.new$deviance-freq$deviance,df=freq.new$df.residual-freq$df.residual,p = 1-pchisq(freq.new$deviance-freq$deviance, freq.new$df.residual-freq$df.residual))

# or equivalently 

anova(freq.new,freq)

# with p-value for the test of

1-pchisq(anova(freq.new,freq)[2,]$Deviance,anova(freq.new,freq)[2,]$Df)


# Move on to severity

summary(sev)

## see if we can drop zone from the severity analysis - the result shows that we are justified to do so as the p-value of the test is 25.4%

summary(sev.new <- glm(severity ~ class + age, family = Gamma("log"), data = moped[moped$number > 0, ], weights = number))
cbind(scaled.deviance = sev.new$deviance/sev.phi, df = sev.new$df.residual, p = 1-pchisq(sev.new$deviance/sev.phi, sev.new$df.residual))

cbind(diff.deviance=(sev.new$deviance-sev$deviance)/sev.phi,df=sev.new$df.residual-sev$df.residual,p = 1-pchisq((sev.new$deviance-sev$deviance)/sev.phi, sev.new$df.residual-sev$df.residual))


# Equivalently 

anova(sev.new,sev)

# with p-value for the test of

1-pchisq(anova(sev.new,sev)[2,]$Deviance/sev.phi,anova(sev.new,sev)[2,]$Df)


## Variance covariance matrix of the beta coefficients
# use the vcov function to get the scaled variance-covariance matrix

vcov(freq.new)
vcov(sev.new)


# to produce a R markdown file

#install.packages("rmarkdown")
#install.packages("installr")
#installr::install.pandoc()
#library(installr)
#library(rmarkdown)
#render("R code - Module 2.R")


