setwd("~/Documents/Lund/R course")
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

source("GraphPlot.R") #for graphs window

library(psych) # for describing
library(tidyverse) # for tidy code
library(stats) #for cooks distance
library(lmtest) #For Breush Pagan Test
library(lm.beta) #For APA Table
library(car) #residual plots
library(boot) # for bootstrapping


#Setting functions

#for regrrssion coefficients
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}

#for adjusted R^2
adjR2_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(summary(fit)$adj.r.squared)	
}	

#Check on data to find missing data or errors

View(data_sample_1)  
str(data_sample_1)
summary(data_sample_1) #Found two errors: negative household income and non-possible score in STAI_traits; IQ consideration as mentaly impaired people participated
describe(data_sample_1)

#Create a dataset without the errors
clean_data_sample = data_sample_1
clean_data_sample = clean_data_sample[!clean_data_sample$STAI_trait <= 20, ]
clean_data_sample = clean_data_sample[!clean_data_sample$household_income <= 0, ]

View(clean_data_sample)
summary(clean_data_sample) #visualize clean datset

#visualize pain scores in overall and finding outliers
clean_data_sample %>% ggplot() + aes(x = pain) + geom_histogram()

#General overviw and finding outliers within every predictor

boxplot(clean_data_sample$pain, xlab = "Pain")
boxplot(clean_data_sample$age, xlab = "Age")
boxplot(clean_data_sample$STAI_trait, xlab = "STAI Trait")
boxplot(clean_data_sample$pain_cat, xlab = "Pain Catastrophizing")
boxplot(clean_data_sample$cortisol_serum, xlab = "Cortisol Serum")
boxplot(clean_data_sample$cortisol_saliva, xlab = "Cortisol Saliva")
boxplot(clean_data_sample$mindfulness, xlab = "Mindfulness")
boxplot(clean_data_sample$weight, xlab = "Weight")
boxplot(clean_data_sample$IQ, xlab = "IQ")
boxplot(clean_data_sample$household_income, xlab = "HH Income")


#Build our models

#Model 1

mod1 <- lm(pain ~ sex + age, data = clean_data_sample)

    #Finding and removing outliers 

cook = cooks.distance(mod1)
plot(cooks.distance(mod1))


cooksd <- cooks.distance(mod1)
sample_size <- nrow(clean_data_sample)
plot(cooksd, pch="º", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="grey")  

#Creating a dataset wout the outliers
NOOmod1_clean_data_sample=clean_data_sample
NOOmod1_clean_data_sample=NOOmod1_clean_data_sample[!cooksd> (4/sample_size),]
View(NOOmod1_clean_data_sample)

mod1_WO <- lm(pain ~ sex + age, data = NOOmod1_clean_data_sample)
summary(mod1)
summary(mod1_WO)

#Model 2

mod2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + 
             cortisol_saliva + mindfulness, data = clean_data_sample)

    #Finding and removing outliers 

cook = cooks.distance(mod2)
plot(cooks.distance(mod2))


cooksd <- cooks.distance(mod2)
sample_size <- nrow(clean_data_sample)
plot(cooksd, pch="º", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4/sample_size, col="pink")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="pink")  

#Creating a dataset wout the outliers
NOOmod2_clean_data_sample=clean_data_sample
NOOmod2_clean_data_sample=NOOmod2_clean_data_sample[!cooksd> (4/sample_size),]
View(NOOmod2_clean_data_sample)

mod2_WO <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + 
             cortisol_saliva + mindfulness, data = NOOmod2_clean_data_sample)

summary(mod2)
summary(mod2_WO)

#Building models wout outliers

mod1_WO <- lm(pain ~ sex + age, data = NOOmod1_clean_data_sample)
mod2_WO <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + 
                cortisol_saliva + mindfulness, data = NOOmod2_clean_data_sample)

#Visualize the models
mod1
mod1_WO #without outlier

mod2
mod2_WO #witout outliers



#Didn't find much differnce between models w and wout outliers

#Check the assumptions for both models: normality (of the residuals), linearity (of the relationship), 
#homogeneity of variance (also called homoscedasticity) and that there is no excess multicollinearity 

# 1.normality (of the residuals)
  #Model 1 - Normality of the Residuals
    
Residuals_mod1 <- residuals(mod1)	
shapiro.test(Residuals_mod1)
hist(Residuals_mod1)
mod1 %>% plot(which = 2)#for qqplot
describe(residuals(mod1)) #for skew and kurtosis

Residuals_mod1WO <- residuals(mod1_WO)	
shapiro.test(Residuals_mod1WO)
hist(Residuals_mod1WO)
  
  #Model 2 - Residuals
Residuals_mod2 <- residuals(mod2)
shapiro.test(Residuals_mod2)
hist(Residuals_mod2)
mod2 %>% plot(which = 2)#For qq-plot
describe(residuals(mod2)) #for skew and kurtosis

Residuals_mod2WO <- residuals(mod2_WO)	
shapiro.test(Residuals_mod2WO)
hist(Residuals_mod2WO)



# 2. linearity 
  #Model 1
plot(mod1)  
plot(mod1_WO) 
mod1 %>% residualPlots()

  #Model 2
plot(mod2,2) 
plot(mod2_WO,2) 
mod2 %>% residualPlots()

# 3. homogeneity of variance (also called homoscedasticity)
  #Model 1
plot(mod1,1)  
plot(mod1,3)  
bptest(mod1)
bptest(mod1_WO)


  #Model 2
plot(mod2,1)  
plot(mod2,3)  
bptest(mod2)
bptest(mod2_WO)

# 4. excess of multicollinearity
  #Model 1
vif(mod1) #The numerical value for VIF tells you (in decimal form) what percentage the variance is inflated for each coefficient (cutoff 10 or 5).
vif(mod1_WO) 
  #Model 2
vif(mod2) #The numerical value for VIF tells you (in decimal form) what percentage the variance is inflated for each coefficient (cutoff 10 or 5).
vif(mod2_WO) 

#Cortisol Saliva excedes rule of thumb
#Correlation matrix

cor(clean_data_sample[,c("age","STAI_trait","pain_cat","cortisol_serum","cortisol_saliva")]) #Cortisol serum and slaiva correlate strogly

#build and compare models to choose which predictor we should exclude

mod2_serum <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + 
                            mindfulness, data = clean_data_sample)

mod2_saliva <-lm(pain ~ sex + age + STAI_trait + pain_cat +  
                            cortisol_saliva + mindfulness, data = clean_data_sample)

summary(mod2_saliva)
summary(mod2_serum)

#Building final model and Comparing the models (w final model)

mod2_Final <- lm(pain ~ sex + age + STAI_trait + pain_cat + 
             cortisol_saliva + mindfulness, data = clean_data_sample)
summary(mod1)
summary(mod2_Final)

#Checking outliers for final model
cook = cooks.distance(mod2_Final)
plot(cooks.distance(mod2_Final))


cooksd <- cooks.distance(mod2_Final)
sample_size <- nrow(clean_data_sample)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="orange")  

#Creating a dataset wout the outliers
NOOmod2F_clean_data_sample=clean_data_sample
NOOmod2F_clean_data_sample=NOOmod2F_clean_data_sample[!cooksd> (4/sample_size),]
View(NOOmod2F_clean_data_sample)

mod2_Final_WO <- lm(pain ~ sex + age + STAI_trait + pain_cat + 
                      cortisol_saliva + mindfulness,data = NOOmod2F_clean_data_sample)
summary(mod2_Final)
summary(mod2_Final_WO)

#Checking assumptions and errors for final model
# 1.normality (of the residuals)
#Model 2F - Normality of the Residuals

Residuals_mod2F <- residuals(mod2_Final)	
shapiro.test(Residuals_mod2F)
hist(Residuals_mod2F)
mod2_Final %>% plot(which = 2)#for qqplot
describe(residuals(mod2_Final)) #for skew and kurtosis


# 2. linearity 
#Model 2F
plot(mod2_Final)  
plot(mod2_Final_WO) 
mod2_Final %>% residualPlots()


# 3. homogeneity of variance (also called homoscedasticity)

#Model 2F
plot(mod2_Final,1)  
plot(mod2_Final,3)  
bptest(mod2_Final)
bptest(mod2_Final_WO)

# 4. excess of multicollinearity
#Model 2F
vif(mod2_Final) #The numerical value for VIF tells you (in decimal form) what percentage the variance is inflated for each coefficient (cutoff 10 or 5).
vif(mod2_Final_WO) 


#Akaike Information Criterion to compare models (lower score, better model)

AIC(mod1,mod2_Final)

#Anova test to compare models 

anova(mod1,mod2_Final)

#Confidence Intervals

confint.lm(mod1)
confint.lm(mod2_Final)

#Coefficients
summary(mod1)
summary(mod2_Final)

lm.beta(mod1)
lm.beta(mod2_Final)

### wiiiii ### consider it..

library(apaTables)
apa.reg.table(mod2_Final)

#Table mod1
coef_table = function(mod1){
  require(lm.beta)
  mod_sum = summary(mod1)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3)) 
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(mod1), confint(mod1), c(0, lm.beta(mod1)$standardized.coefficients[c(2:length(mod1$coefficients))])), 2)), mod_sum_p_values) 
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  mod_sum_table["(Intercept)","Std.Beta"] = "0" 
  return(mod_sum_table)
}
coef_table(mod1)

#Table mod2F
coef_table = function(mod2_Final){
  require(lm.beta)
  mod_sum = summary(mod2_Final)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3)) 
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(mod2_Final), confint(mod2_Final), c(0, lm.beta(mod2_Final)$standardized.coefficients[c(2:length(mod2_Final$coefficients))])), 2)), mod_sum_p_values) 
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  mod_sum_table["(Intercept)","Std.Beta"] = "0" 
  return(mod_sum_table)
}
coef_table(mod2_Final)





