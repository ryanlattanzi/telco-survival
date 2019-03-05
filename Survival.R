library(survival)
library(survminer)
library(dplyr)
library(ranger)

telco = read.csv("data.csv")
telco$customerID = NULL
telco$Churn = as.numeric(telco$Churn)
# Means for missing values
missing = which(is.na(telco$TotalCharges))
telco$TotalCharges[missing] = mean(telco$TotalCharges[-missing])

# Survival Object
sur = Surv(telco$tenure,telco$Churn == 2)

# KM for Partner
partner = survfit(sur ~ Partner, data = telco)
summary(partner, times = seq(10,70,5))
ggsurvplot(partner, data = telco, pval=TRUE)

# KM for Multiple Lines
ml = survfit(sur ~ MultipleLines, data = telco)
summary(ml, times = seq(10,70,5))
ggsurvplot(ml, data = telco, pval=TRUE)

# KM for Seniors
sen = survfit(sur ~ SeniorCitizen, data = telco)
summary(sen, times = seq(10,70,5))
ggsurvplot(sen, data = telco, pval=TRUE)

# KM for Gender
gen = survfit(sur ~ gender, data = telco)
summary(gen, times = seq(10,70,5))
ggsurvplot(gen, data = telco, pval=TRUE)


# Cox Proportional Hazards Model
cox = coxph(sur ~ Partner + gender + 
              SeniorCitizen + Dependents + PhoneService + MultipleLines + 
              InternetService + OnlineSecurity + OnlineBackup + DeviceProtection 
            + TechSupport + StreamingTV + StreamingMovies + Contract + 
              PaperlessBilling + PaymentMethod + MonthlyCharges + 
              TotalCharges,data = telco)
summary(cox)

cox = coxph(sur ~ Partner + InternetService + Contract + 
              PaperlessBilling + PaymentMethod + TotalCharges,data = telco)
summary(cox)

x = survfit(cox , data = telco)
summary(x, times = seq(10,70,5))
ggsurvplot(x, data = telco, pval=TRUE)

