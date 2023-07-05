# Install the CASdatasets package if necessary
# install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")

library(CASdatasets)
library(doParallel)

# Load the data
data(swautoins)

# change categorical columns into factors
swautoins <- within(swautoins, {
  Kilometres <- factor(Kilometres)
  Zone <- factor(Zone)
  Bonus <- factor(Bonus)
  Make <- factor(Make) })

summary(swautoins)
# There are 2182 observations in this data set.

# Calculate the frequency
swautoins$frequency = swautoins$Claims / swautoins$Insured
swautoins$severity = swautoins$Payment / swautoins$Claims

# Initialize a dataframe 
relsw <- data.frame(rating.factor = c(rep("Kms", nlevels(swautoins$Kilometres)),
                                      rep("Zone", nlevels(swautoins$Zone)),
                                      rep("Bonus", nlevels(swautoins$Bonus)),
                                      rep("Make", nlevels(swautoins$Make))),
                    class = c(levels(swautoins$Kilometres),
                              levels(swautoins$Zone),
                              levels(swautoins$Bonus),
                              levels(swautoins$Make)),
                    stringsAsFactors = FALSE)

new.cols <- 
  foreach (rating.factor = c("Kilometres", "Zone", "Bonus","Make"),
           .combine = rbind) %do%
  {
    nclaims <- tapply(swautoins$Claims, swautoins[[rating.factor]], sum)
    sums <- tapply(swautoins$Insured, swautoins[[rating.factor]], sum)
    costs <- tapply(swautoins$Payment, swautoins[[rating.factor]], sum) / 1000000
    n.levels <- nlevels(swautoins[[rating.factor]])
    contrasts(swautoins[[rating.factor]]) <-
      contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
    data.frame(duration = sums, n.claims = nclaims, total.costs.mil = costs)
  }

relsw <- cbind(relsw, new.cols)
rm(new.cols)
print(relsw)

# plot(swautoins$Zone,swautoins$frequency ,xlab="Zone",ylab="claim frequency")
# plot(swautoins$Bonus,swautoins$frequency,xlab="Bonus",ylab="claim frequency")
# plot(swautoins$Make,swautoins$frequency, xlab="Make",ylab="claim frequency")
# plot(swautoins$Zone,swautoins$Kilometres, xlab="Zone",ylab="Kms")

# plot(swautoins$Claims,swautoins$Insured)
# plot(swautoins$Payment,swautoins$Claims)

%%frees does not order according to largest to smallest for Insured
model.frequency.nocov=glm(Claims ~ 1, offset=log(Insured),data=swautoins, family=poisson)
summary(model.frequency.nocov)

summary(model.frequency<-glm(Claims/Insured ~ Kilometres + Zone + Bonus + Make, family=quasipoisson, data=swautoins, weights=Insured))

with(model.frequency, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

summary(model.frequencyoff <- glm(Claims ~ Kilometres + Zone + Bonus + Make + offset(log(Insured)), data = swautoins, family = poisson))

TableQ1=matrix(0,25,2) 
for (i in 1:25){
  TableQ1[i,1]<=summary(model.frequencyoff)$coeff[i,1]
  TableQ1[i,2]<=summary(model.frequencyoff)$coeff[i,2]
}

col1<-summary(model.frequencyoff)$coeff[,0]
FinalTableQ1<-data.frame(col1,TableQ1)

write.table(format(FinalTableQ1,dig=4),"/Users/clemieux/cours/act632/s2017/Projects/TableQ1exam.txt",col.names=FALSE,quote=FALSE,sep=",")


anova(model.frequency.nocov, model.frequencyoff, test = "Chisq")


model.freq.negbin<-glm.nb(Claims ~ Kilometres + Zone + Bonus + Make+offset(log(Insured)),data=swautoins,link=log)

with(model.freq.negbin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

##
##> with(model.freq.negbin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))
##     res.deviance   df         p
##[1,]     2229.906 2157 0.1339613

##donc du point de vue de la deviance c'est pas mal mieux que Poisson....

(diff2loglik <- as.numeric(2 * (logLik(model.freq.negbin) - logLik(model.frequencyoff))))
##[1] 276.3922
pchisq(diff2loglik, df = 1, lower.tail = FALSE)
##[1] 4.589969e-62


##note 2182 realized cells (with some Insured) so 2182-4-6-6-8-1=2157 df



model.f2 <- update(model.frequency, . ~ . - Make)
anova(model.f2, model.frequency, test = "Chisq")

###Analysis of Deviance Table
###
###Model 1: Claims/Insured ~ Kilometres + Zone + Bonus
###Model 2: Claims/Insured ~ Kilometres + Zone + Bonus + Make
###  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
###1      2165     4456.8                          
###2      2157     2966.1  8   1490.7 < 2.2e-16 ***
###---
###Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## so we should include make


#  MAKE THIS STATISTIC ROUTINE TO SAVE WORK
PearsonI = function(y){
  temp = (swautoins$Claims - y)
  PearsonIp = sum(temp*temp/y);PearsonIp}

PearsonI(model.frequency$fitted.values*swautoins$Insured)
## gives 3002.581
#same thing with
#PearsonI(model.frequencyoff$fitted.values)
#probably better because less confusing
#> PearsonI(model.frequency.nocov$fitted.values)
#[1] 44638.51

PearsonI(model.freq.negbin$fitted.values)
##> PearsonI(model.freq.negbin$fitted.values)
##[1] 3077.296


# note that in R when calling residuals, default is to compute deviance residuals
pdf("swautoFreqResidHist.pdf")
hist(residuals(model.frequencyoff,type=c("deviance")),xlab="Deviance Residuals",main="")
dev.off()

pdf("swautoFreqResidvsIndex.pdf")
plot(residuals(model.frequencyoff,type=c("deviance")))
dev.off()



## should ask for all the multipliers and then different rates based on certain combinations, with associated CI
## should also combine the models

summary(model.frequencyoff.interac <- glm(Claims ~ Kilometres + Zone + Bonus * Make + offset(log(Insured)), data = swautoins, family = poisson))

summary(model.sev<-glm(Payment/Claims~Kilometres + Zone + Bonus +Make,data = swautoins[swautoins$Claims>0],family=Gamma("log"),weights=Claims)
        
        with(model.frequencyoff.interac, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))
        ##
        ##     res.deviance   df            p
        ##[1,]     2743.555 2109 1.889605e-19
        ##pas tres bon
        ----
          healthexp<-read.table("HealthExpend.csv",header=TRUE,sep=",")
        healthexpv2<-healthexp[healthexp$COUNTOP>0,]
        healthexpv2<-healthexpv2[,c(1:11,14:28)]
        
        n<-dim(healthexpv2)[1]
        set.seed(1)
        train<-sample(1:nrow(healthexpv2),0.7*nrow(healthexpv2))
        tree.healthexp<-tree(formula = EXPENDOP ~.-COUNTOP, data=healthexpv2,subset=train)
        
        pred.tree.hexp<-predict(tree.healthexp,newdata=healthexpv2[-train,])
        y.test<-healthexpv2$EXPENDOP[-train]
        
        mean((pred.tree.hexp-y.test)^2)
        