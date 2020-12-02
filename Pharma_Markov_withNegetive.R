##R-script Code:############################################################################################################################################################
setwd("C:/Users/cchow/Documents")
m12 <- read.csv("Pharma_Markov12_n.csv", header = T)
m23 <- read.csv("Pharma_Markov23_n.csv", header = T)
m3a <- read.csv("Pharma_Markov3a_n.csv", header = T)


Time  <- m12$P1P2
Event <- m12$TR12
Time2 <- m23$P2P3
Event2 <- m23$TR23
Time3 <- m3a$P3PA
Event3 <- m3a$TR3A


library(survival)
km.model <- survfit(Surv(Time, Event) ~ 1, type = "kaplan-meier")
km.mode2 <- survfit(Surv(Time2, Event2) ~ 1, type = "kaplan-meier")
km.mode3 <- survfit(Surv(Time3, Event3) ~ 1, type = "kaplan-meier")

#summary(km.model)
#summary(km.mode2)
#summary(km.mode3)


plot(km.model, conf.int = T, xlab = 'Time(in yrs)', ylab ='Transition Probability',main = 'Phase Transition 1 to 2: Kaplan-Meier Model', las = 1, mark.time = T, fun = 'event', ylim = c(0.00,0.60), yaxt = 'n')
axis(side = 1, at =c(0:30))
axis(side = 2, at = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60))
plot(km.mode2, conf.int = T, xlab = 'Time(in yrs)', ylab ='Transition Probability',main = 'Phase Transition 2 to 3: Kaplan-Meier Mode2', las = 1, mark.time = T, fun = 'event', ylim = c(0.00,0.60), yaxt = 'n')
axis(side = 1, at =c(0:30))
axis(side = 2, at = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60))
plot(km.mode3, conf.int = T, xlab = 'Time(in yrs)', ylab ='Transition Probability',main = 'Phase Transition 3 to App: Kaplan-Meier Mode3', las = 1, mark.time = T, fun = 'event', ylim = c(0.00,0.60), yaxt = 'n')
axis(side = 1, at =c(0:30))
axis(side = 2, at = c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60))

sum1 <- summary(km.model)
sum2 <- summary(km.mode2)
sum3 <- summary(km.mode3)

time1 <- sum1$time
time2 <- sum2$time
time3 <- sum3$time

count1 <- sum1$n.risk
count2 <- sum2$n.risk
count3 <- sum3$n.risk

transition1 <- sum1$n.event
transition2 <- sum2$n.event
transition3 <- sum3$n.event

stderror1 <- sum1$std.err
stderror2 <- sum2$std.err
stderror3 <- sum3$std.err

lower0.95CI1 <- sum1$lower
lower0.95CI2 <- sum2$lower
lower0.95CI3 <- sum3$lower

upper0.95CI1 <- sum1$upper
upper0.95CI2 <- sum2$upper
upper0.95CI3 <- sum3$upper

surv1 <- sum1$surv
surv2 <- sum2$surv
surv3 <- sum3$surv

df1 <- data.frame(time1, count1, transition1, stderror1, lower0.95CI1, upper0.95CI1, surv1)
df2 <- data.frame(time2, count2, transition2, stderror2, lower0.95CI2, upper0.95CI2, surv2)
df3 <- data.frame(time3, count3, transition3, stderror3, lower0.95CI3, upper0.95CI3, surv3)

df1 <- transform(df1, transition_probability = 1 - surv1)
df2 <- transform(df2, transition_probability = 1 - surv2)
df3 <- transform(df3, transition_probability = 1 - surv3)

df1 <- transform(df1, lwr = 1 - upper0.95CI1)
df1 <- transform(df1, upr = 1 - lower0.95CI1)
df2 <- transform(df2, lwr = 1 - upper0.95CI2)
df2 <- transform(df2, upr = 1 - lower0.95CI2)
df3 <- transform(df3, lwr = 1 - upper0.95CI3)
df3 <- transform(df3, upr = 1 - lower0.95CI3)

dataframe1 <- data.frame(df1$time1, df1$count1, df1$transition1, df1$transition_probability, df1$lwr, df1$upr)
dataframe2 <- data.frame(df2$time2, df2$count2, df2$transition2, df2$transition_probability, df2$lwr, df2$upr)
dataframe3 <- data.frame(df3$time3, df3$count3, df3$transition3, df3$transition_probability, df3$lwr, df3$upr)

names(dataframe1)[names(dataframe1) == 'df1.time1'] <- 'Time'
names(dataframe1)[names(dataframe1) == 'df1.count1'] <- 'CumCount'
names(dataframe1)[names(dataframe1) == 'df1.transition1'] <- 'Transition'
names(dataframe1)[names(dataframe1) == '.transition_probability'] <- 'Transition_Probability'
names(dataframe1)[names(dataframe1) == 'df1.lwr'] <- 'LowerCI(0.95)'
names(dataframe1)[names(dataframe1) == 'df1.upr'] <- 'UpperCI(0.95)'


names(dataframe2)[names(dataframe2) == 'df2.time2'] <- 'Time'
names(dataframe2)[names(dataframe2) == 'df2.count2'] <- 'CumCount'
names(dataframe2)[names(dataframe2) == 'df2.transition2'] <- 'Transition'
names(dataframe2)[names(dataframe2) == '.transition_probability'] <- 'Transition_Probability'
names(dataframe2)[names(dataframe2) == 'df2.lwr'] <- 'LowerCI(0.95)'
names(dataframe2)[names(dataframe2) == 'df2.upr'] <- 'UpperCI(0.95)'

names(dataframe3)[names(dataframe3) == 'df3.time3'] <- 'Time'
names(dataframe3)[names(dataframe3) == 'df3.count3'] <- 'CumCount'
names(dataframe3)[names(dataframe3) == 'df3.transition3'] <- 'Transition'
names(dataframe3)[names(dataframe3) == '.transition_probability'] <- 'Transition_Probability'
names(dataframe3)[names(dataframe3) == 'df3.lwr'] <- 'LowerCI(0.95)'
names(dataframe3)[names(dataframe3) == 'df3.upr'] <- 'UpperCI(0.95)'

dataframe1
dataframe2
dataframe3


######plot comparison for survminer#####
###Case1: Survival plots using standard plot function 
#plot(km.model, conf.int = T, xlab = 'Time(in yrs)', ylab ='Survival Probability',main = 'Phase Transition 1 to 2: Kaplan-Meier Model', las = 1, mark.time = T)
#plot(km.mode2, conf.int = T, xlab = 'Time(in yrs)', ylab ='Survival Probability',main = 'Phase Transition 2 to 3: Kaplan-Meier Mode2', las = 1, mark.time = T)
#plot(km.mode3, conf.int = T, xlab = 'Time(in yrs)', ylab ='Survival Probability',main = 'Phase Transition 3 to Approval: Kaplan-Meier Model', las = 1, mark.time = T)

###Case2: Survival plots using survminer package (ggsurvplot function)
#library(survminer)
#ggsurvplot(km.model1, data = m12)
#ggsurvplot(km.model2, data = m23)
#ggsurvplot(km.model3, data = m3a)

##########OR

###Case1: Transition plots using standard plot function 
#plot(km.model, conf.int = T, xlab = 'Time(in yrs)', ylab ='Survival Probability',main = 'Phase Transition 1 to 2: Kaplan-Meier Model', las = 1, mark.time = T, fun = 'event')
#plot(km.mode2, conf.int = T, xlab = 'Time(in yrs)', ylab ='Survival Probability',main = 'Phase Transition 2 to 3: Kaplan-Meier Mode2', las = 1, mark.time = T, fun = 'event')
#plot(km.mode3, conf.int = T, xlab = 'Time(in yrs)', ylab ='Survival Probability',main = 'Phase Transition 3 to Approval: Kaplan-Meier Model', las = 1, mark.time = T, fun = 'event')

###Case2: Transition plots using survminer package (ggsurvplot function)
#library(survminer)
#ggsurvplot(km.model1, data = m12, fun = 'event')
#ggsurvplot(km.model2, data = m23, fun = 'event')
#ggsurvplot(km.model3, data = m3a, fun = 'event')


#Since there is not a sigle entry for 2020 in all three columns, I am unable to filter or provide a counter.
