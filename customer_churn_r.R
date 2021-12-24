library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(readr)

surv_data <- read_csv("experiments/surv_data.csv", col_types = cols(X1 = col_skip()))
sur_d = surv_data %>% mutate(Total_Business_Value=`Total Business Value`,Joining_Designation = `Joining Designation`) %>% select(-`Total Business Value`,-`Joining Designation`)
km_fit=survfit(Surv(workdays, notWorking)~1,data=surv_data)
summary(km_fit)
autoplot(km_fit)
km_ed_fit=survfit(Surv(workdays,notWorking)~Education_Level, data = surv_data)
autoplot(km_ed_fit)
km_ge_fit=survfit(Surv(workdays,notWorking)~Gender,data=surv_data)
autoplot(km_ge_fit)
km_age_fit=survfit(Surv(workdays,notWorking)~age_groups,data=sur_d)
autoplot(km_age_fit)
km_d_fit=survfit(Surv(workdays,notWorking)~Designation,data = sur_d)
autoplot(km_d_fit)
sur_d=mutate(surv_data,
             age_groups=cut(Age, breaks=5),
             Quarterly_Rating = as.factor(Quarterly_Rating),
             Designation = as.factor(Designation),
             `Joining Designation`=as.factor(`Joining Designation`))
sur_d = sur_d %>% mutate_if(is.character,as.factor)

'''cox proportional model'''
cox= coxph(Surv(workdays, notWorking)~Gender+City+Education_Level+Joining_Designation+Salary
           +Total_Business_Value+Quarterly_Rating+Age+Designation, data = sur_d)
summary(cox)
cox2= coxph(Surv(workdays, notWorking)~Gender+City+Education_Level+Joining_Designation+Salary
           +Total_Business_Value+Quarterly_Rating+age_groups+Designation, data = sur_d)
summary(cox2)
cox_fit=survfit(cox)
autoplot(cox_fit)

ss_fit=aareg(Surv(workdays, notWorking)~Gender+City+Education_Level+Joining_Designation+Salary
             +Total_Business_Value+Quarterly_Rating+Age+Designation, data = sur_d)
ss_fit


#survival forest
r_fit = ranger(Surv(workdays,notWorking)~Gender+City+Education_Level+Joining_Designation+Salary
               +Total_Business_Value+Quarterly_Rating+Age+Designation, data = sur_d,mtry = 4,importance = 'permutation',
               splitrule = 'extratrees',verbose = TRUE)
# Average the survival models
death_times = r_fit$unique.death.times
surv_prob = data.frame(r_fit$survival)
avg_prob = sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Employee churn prob")

#
cols <- colors()
for (n in sample(c(2:dim(sur_d)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

#prediction
library(caTools)
sample = sample.split(sur_d$notWorking, SplitRatio = 0.7)
train = subset(sur_d, sample ==TRUE)
test  =subset(sur_d, sample == FALSE)

model = ranger(Surv(workdays,notWorking)~Gender+City+Education_Level+Joining_Designation+Salary
               +Total_Business_Value+Quarterly_Rating+Age+Designation, data = train,mtry = 4,importance = 'permutation',
               splitrule = 'extratrees',verbose = TRUE)
pred = predict(model, data = test)

cat("Prediction Error = 1 - Harrell's c-index = ", model$prediction.error)

