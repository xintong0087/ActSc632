# Install the packages if necessary
# install.packages("insuranceData")
# install.packages("doParallel")
# install.packages("dplyr")

library(insuranceData)
library(foreach)
library(dplyr)

# Q1: Download and briefly describe the dataset

# Load the data
data(dataOhlsson)

# change categorical columns into factors
dataOhlsson <- within(dataOhlsson, {
    zon <- factor(zon)
    mcklass <- factor(mcklass)
    bonuskl <- factor(bonuskl)
    kon <- factor(kon)})

# regroup the column bonuskl into 3 levels
levels(dataOhlsson$bonuskl) <- list("1" = c("1","2"),
                                    "2" = c("3","4"),
                                    "3" = c("5","6","7"))  

# regroup the column mcklass into 3 levels
# For details, visit: https://stackoverflow.com/questions/13559076/group-numeric-values-by-the-intervals
dataOhlsson$motorage_gr <- cut(dataOhlsson$fordald, 
                               breaks = c(0, 2, 5, 100), 
                               labels = c("1", "2", "3"), 
                               right = FALSE)

# Q2: Compute exposure, frequency, and severity for each combination of the rating factors

motorcycle <- data.frame(rating.factor = c(rep("Zone", nlevels(dataOhlsson$zon)),
                                           rep("Vehicle class", nlevels(dataOhlsson$mcklass)),
                                           rep("Vehicle age", nlevels(dataOhlsson$motorage_gr)),
                                           rep("Bonus class", nlevels(dataOhlsson$bonuskl))),
                         class = c(levels(dataOhlsson$zon),
                                   levels(dataOhlsson$mcklass),
                                   levels(dataOhlsson$motorage_gr),
                                   levels(dataOhlsson$bonuskl)),
                         stringsAsFactors = FALSE)

new.cols <- foreach (rating.factor = c("zon","mcklass","motorage_gr","bonuskl"), .combine = rbind) %do% {
                nclaims <- tapply(dataOhlsson$antskad,dataOhlsson[[rating.factor]], sum)
                sums <- tapply(dataOhlsson$duration, dataOhlsson[[rating.factor]],sum)
                severity <- tapply(dataOhlsson$skadkost,dataOhlsson[[rating.factor]],sum)
                n.levels <- nlevels(dataOhlsson[[rating.factor]])
                contrasts(dataOhlsson[[rating.factor]]) <-
                    contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
                data.frame(duration = sums, n.claims = nclaims, totcostperm = severity/1000)}

motorcycle <- cbind(motorcycle, new.cols)
rm(new.cols)

# Q3: Model claim frequency using a Poisson GLM

# Group the data by rating factors
grDataOhlsson <- dataOhlsson %>% group_by(bonuskl, zon, mcklass, motorage_gr) %>% summarize(duration = sum(duration), 
                                                                                            antskad=sum(antskad),
                                                                                            skadkost=sum(skadkost))

# Fit the Poisson GLM
model.freq <- glm(antskad ~ zon + mcklass + motorage_gr + bonuskl + offset(log(duration)), 
                  family = poisson, 
                  data = grDataOhlsson[grDataOhlsson$duration>0,])

# Examine model fit
with(motor.freq, cbind(res.deviance = deviance, 
                       df = df.residual, 
                       p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# Generate the table for relative frequency
P.zon = contrasts(dataOhlsson$zon)
P.mcklass = contrasts(dataOhlsson$mcklass)
P.motorage_gr = contrasts(dataOhlsson$motorage_gr)
P.bonuskl = contrasts(dataOhlsson$bonuskl)

mat.freq = summary(model.freq)$coefficients[, 1:2]

table.q3 = matrix(0, nrow = dim(motorcycle)[1] + 1, ncol = 5)
colnames(table.q3) = c("Rating factor", "Class", "Multiplier", "Lower bound", "Upper bound")

# Intercept
estimate = mat.freq[1, 1]
std.error = mat.freq[1, 2]
table.q3[1, ] = c("Intercept", "0", exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Zone
estimate = P.zon %*% mat.freq[2:7, 1]
std.error = P.zon %*% mat.freq[2:7, 2]
table.q3[2:8, ] = cbind(rep("Zone", 7), seq(7), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Vehicle class
estimate = P.mcklass %*% mat.freq[8:13, 1]
std.error = P.mcklass %*% mat.freq[8:13, 2]
table.q3[9:15, ] = cbind(rep("Vehicle class", 7), seq(7), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Vehicle age
estimate = P.motorage_gr %*% mat.freq[14:15, 1]
std.error = P.motorage_gr %*% mat.freq[14:15, 2]
table.q3[16:18, ] = cbind(rep("Vehicle age", 3), seq(3), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Bonus class
estimate = P.motorage_gr %*% mat.freq[16:17, 1]
std.error = P.motorage_gr %*% mat.freq[16:17, 2]
table.q3[19:21, ] = cbind(rep("Bonus class", 3), seq(3), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

print(table.q3)

# Q4: Model claim severity using a Gamma GLM

model.sev <- glm(skadkost/antskad ~ zon + mcklass + motorage_gr + bonuskl, 
                 family=Gamma("log"), 
                 data=grDataOhlsson[grDataOhlsson$skadkost>0,],
                 weights=antskad)

with(model.sev, cbind(res.deviance = deviance, 
                      df = df.residual, 
                      p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# Generate the table for relative severity
mat.sev = summary(model.sev)$coefficients[, 1:2]

table.q4 = matrix(0, nrow = dim(motorcycle)[1] + 1, ncol = 5)
colnames(table.q4) = c("Rating factor", "Class", "Multiplier", "Lower bound", "Upper bound")

# Intercept
estimate = mat.sev[1, 1]
std.error = mat.sev[1, 2]
table.q4[1, ] = c("Intercept", "0", exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Zone
estimate = P.zon %*% mat.sev[2:7, 1]
std.error = P.zon %*% mat.sev[2:7, 2]
table.q4[2:8, ] = cbind(rep("Zone", 7), seq(7), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Vehicle class
estimate = P.mcklass %*% mat.sev[8:13, 1]
std.error = P.mcklass %*% mat.sev[8:13, 2]
table.q4[9:15, ] = cbind(rep("Vehicle class", 7), seq(7), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Vehicle age
estimate = P.motorage_gr %*% mat.sev[14:15, 1]
std.error = P.motorage_gr %*% mat.sev[14:15, 2]
table.q4[16:18, ] = cbind(rep("Vehicle age", 3), seq(3), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

# Bonus class
estimate = P.motorage_gr %*% mat.sev[16:17, 1]
std.error = P.motorage_gr %*% mat.sev[16:17, 2]
table.q4[19:21, ] = cbind(rep("Bonus class", 3), seq(3), exp(estimate), exp(estimate - 1.96 * std.error), exp(estimate + 1.96 * std.error))

print(table.q4)

# Q5: Model using age and sex information

dataOhlsson$driverage_gr <- cut(dataOhlsson$agarald, 
                                breaks = c(0, 30,100), 
                                labels = c("1", "2"), 
                                right = FALSE)

motorcycle.new <-
    data.frame(rating.factor = c(rep("Bonus class", nlevels(dataOhlsson$bonuskl)),
                                 rep("Vehicle class", nlevels(dataOhlsson$mcklass)),
                                 rep("Vehicle age", nlevels(dataOhlsson$motorage_gr)),
                                 rep("Zone", nlevels(dataOhlsson$zon)),
                                 rep("Driver age", nlevels(dataOhlsson$driverage_gr)),
                                 rep("Sex", nlevels(dataOhlsson$kon))),
               class = c(levels(dataOhlsson$bonuskl),
                         levels(dataOhlsson$mcklass),
                         levels(dataOhlsson$motorage_gr),
                         levels(dataOhlsson$zon),
                         levels(dataOhlsson$driverage_gr),
                         levels(dataOhlsson$kon)),
               stringsAsFactors = FALSE)

new.cols <- foreach (rating.factor = c("zon", "mcklass", "motorage_gr", "bonuskl", "driverage_gr", "kon"), .combine = rbind) %do% {
    nclaims <- tapply(dataOhlsson$antskad,dataOhlsson[[rating.factor]], sum)
    sums <- tapply(dataOhlsson$duration, dataOhlsson[[rating.factor]],sum)
    severity <- tapply(dataOhlsson$skadkost,dataOhlsson[[rating.factor]],sum)
    n.levels <- nlevels(dataOhlsson[[rating.factor]])
    contrasts(dataOhlsson[[rating.factor]]) <-
        contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
    data.frame(duration = sums, n.claims = nclaims, sev = severity/nclaims)}

motorcycle.new <- cbind(motorcycle.new, new.cols)
rm(new.cols)

grDataOhlsson.new <- dataOhlsson %>% group_by(bonuskl, zon, mcklass, motorage_gr, kon, driverage_gr) %>% summarize(duration = sum(duration),
                                                                                                                   antskad=sum(antskad),
                                                                                                                   skadkost=sum(skadkost))

# Fit the Poisson GLM for frequency
model.freq.new <- glm(antskad ~ zon + mcklass + motorage_gr + bonuskl + kon * driverage_gr +offset(log(duration)),
                     family = poisson, 
                     data = grDataOhlsson.new[grDataOhlsson.new$duration>0,])

with(model.freq.new, 
     cbind(res.deviance = deviance, df = df.residual, 
     p = pchisq(deviance, df.residual, lower.tail = FALSE)))


pchisq(model.freq.new$deviance - model.freq$deviance,
       model.freq.new$df.residual - model.freq$df.residual, 
       lower.tail=FALSE)

# Fit the Gamma GLM for severity
model.sev.new <- glm(skadkost/antskad ~ zon + mcklass + motorage_gr + bonuskl + kon * driverage_gr, 
                     family = Gamma("log"), 
                     data = grDataOhlssonv2[grDataOhlssonv2$antskad>0,],
                     weights = antskad)

with(model.sev.new, 
     cbind(res.deviance = deviance, df = df.residual, 
     p = pchisq(deviance, df.residual, lower.tail = FALSE)))

pchisq(model.sev.new$deviance - model.sev$deviance * summary(model.sev)$dispersion / summary(model.sev.new)$dispersion, 
       model.sev.new$df.residual - model.sev$df.residual,
       lower.tail=FALSE)

# Generate the table for new tariff
table.q5 = matrix(0, nrow = dim(motorcycle)[1] + 1, ncol = 5)
colnames(table.q5) = c("Rating factor", "Class", "Multiplier", "Lower bound", "Upper bound")

# Intercept
estimate.freq = mat.freq[1, 1]
std.error.freq = mat.freq[1, 2]
estimate.sev = mat.sev[1, 1]
std.error.sev = mat.sev[1, 2]
table.q5[1, ] = c("Intercept", "0", exp(estimate.freq + estimate.sev), 
                  exp(estimate.freq + estimate.sev - 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)), 
                  exp(estimate.freq + estimate.sev + 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)))

# Zone
estimate.freq = P.zon %*% mat.freq[2:7, 1]
std.error.freq = P.zon %*% mat.freq[2:7, 2]
estimate.sev = P.zon %*% mat.sev[2:7, 1]
std.error.sev = P.zon %*% mat.sev[2:7, 2]
table.q5[2:8, ] = cbind(rep("Zone", 7), seq(7), exp(estimate.freq + estimate.sev), 
                  exp(estimate.freq + estimate.sev - 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)), 
                  exp(estimate.freq + estimate.sev + 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)))

# Vehicle class
estimate.freq = P.mcklass %*% mat.freq[8:13, 1]
std.error.freq = P.mcklass %*% mat.freq[8:13, 2]
estimate.sev = P.mcklass %*% mat.sev[8:13, 1]
std.error.sev = P.mcklass %*% mat.sev[8:13, 2]
table.q5[9:15, ] = cbind(rep("Vehicle class", 7), seq(7), exp(estimate.freq + estimate.sev), 
                   exp(estimate.freq + estimate.sev - 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)), 
                   exp(estimate.freq + estimate.sev + 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)))
# Vehicle age
estimate.freq = P.motorage_gr %*% mat.freq[14:15, 1]
std.error.freq = P.motorage_gr %*% mat.freq[14:15, 2]
estimate.sev = P.motorage_gr %*% mat.sev[14:15, 1]
std.error.sev = P.motorage_gr %*% mat.sev[14:15, 2]
table.q5[16:18, ] = cbind(rep("Vehicle age", 3), seq(3), exp(estimate.freq + estimate.sev), 
                    exp(estimate.freq + estimate.sev - 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)), 
                    exp(estimate.freq + estimate.sev + 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)))
# Bonus class
estimate.freq = P.motorage_gr %*% mat.freq[16:17, 1]
std.error.freq = P.motorage_gr %*% mat.freq[16:17, 2]
estimate.sev = P.motorage_gr %*% mat.sev[16:17, 1]
std.error.sev = P.motorage_gr %*% mat.sev[16:17, 2]
table.q5[19:21, ] = cbind(rep("Bonus class", 3), seq(3), exp(estimate.freq + estimate.sev), 
                    exp(estimate.freq + estimate.sev - 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)), 
                    exp(estimate.freq + estimate.sev + 1.96 * sqrt(std.error.freq^2 + std.error.sev^2)))

# Append the old tariff
tariff.old <- c(0, 
                7.768, 4.227, 1.336, 1.000, 1.734, 1.402, 1.402,
                0.625, 0.769, 1.000, 1.406, 1.875, 4.062, 6.873,
                2.000, 1.200, 1.000,
                1.250, 1.125, 1.000)

table.q5 <- cbind(table.q5, tariff.old)
print(table.q5)


