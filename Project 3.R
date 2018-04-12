#PROJECT 3 - LOGISTIC REGRESSION

library(MASS)
data("birthwt")
head(birthwt)

#STEP 1: DATA PRE-PROCESSING
lapply(birthwt, class)
adj_birthwt <- with(birthwt, data.frame(data.frame(low=low, age=age, lwt=lwt,
                                                   race=as.factor(race), 
                                                   smoke=as.factor(smoke),
                                                   ptl=as.factor(ptl), ht=as.factor(ht),
                                                  ui=as.factor(ui), 
                                                  ftv=as.factor(ftv))))
#As stated in the problem description, I have ignored the column "bwt".

lapply(adj_birthwt, class)

#STEP 2: EXPLORATORY DATA ANALYSIS
summary(adj_birthwt)

boxplot(age~low, data = adj_birthwt, ylab="Age of mother", xlab="Baby normal weight                  Baby under weight")
#Under weight babies come from younger moms by a low margin, on average.

boxplot(lwt~low, data = adj_birthwt, ylab="Weight of mother", xlab="Baby normal weight                  Baby under weight")
#Fairly similar, although normal weights come from a range of fatter mothers

with(adj_birthwt, tapply(age,low, mean))
with(adj_birthwt, tapply(lwt,low, mean))

with(adj_birthwt, table(low, race))

with(adj_birthwt, table(low, smoke))

with(adj_birthwt, table(low, ptl))

with(adj_birthwt, table(low, ht))

with(adj_birthwt, table(low, ui))

with(adj_birthwt, table(low, ftv))

#STEP 3: FIT THE MODEL

mod1 <- glm(low~age+lwt+race+smoke+ptl+ht+ui+ftv, data = adj_birthwt, family = binomial)

## MODEL DIAGNOSTICS
plot(residuals(mod1) ~ predict(mod1, type="link"), xlab="Fitted log of odds",
                                                    ylab="Deviance residuals")

plot(residuals(mod1) ~ predict(mod1, type="response"), xlab="Fitted probabilities",
     ylab="Deviance residuals")

require(faraway)
halfnorm(rstudent(mod1))

#CONCLUSION
summary(mod1)

require(aod)

#Testing if race is significant
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 4:5) #WEIRD because black is significant ????????????????????????
#Terms=4:5 refer to rows 4 & 5 from summary(mod1) output

#Testing if number of premature labours is significant
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 7:9)
#Terms=7:9 refer to rows 7 & 9 from summary(mod1) output

#Testing if number of physician visits during first trimester is significant
wald.test(b = coef(mod1), Sigma = vcov(mod1), Terms = 12:16)
#Terms=12:16 refer to rows 12 & 16 from summary(mod1) output
