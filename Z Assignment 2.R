data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

library(psych) # for describe
library(tidyverse) # for tidy code
library(stats) #for cooks distance
library(lmtest) #For Breush Pagan Test

#Check on data and possible missing data or errors

View(data_sample_1)  
str(data_sample_1)
summary(data_sample_1) 
describe(data_sample_1)

#IQ issue as the min shows someone mentaly impaired 
#STAI_trait has an error as the min is 3.5 and the lowest possible score is 20
#Somene has a negative household income 

clean_data_sample = data_sample_1
clean_data_sample = clean_data_sample[!clean_data_sample$STAI_trait <= 20, ]
clean_data_sample = clean_data_sample[!clean_data_sample$household_income <= 0, ]  #delete people with errors

View(clean_data_sample)
summary(clean_data_sample)  #check if it worked


clean_data_sample %>% ggplot() + aes(x = pain) + geom_histogram() #check how pain scores behaves in our sample

#Build our model (w outliers)

mod3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + 
          + mindfulness + weight + IQ + household_income, data = clean_data_sample)
mod3
cook = cooks.distance(mod3)
plot(cooks.distance(mod3))


cooksd <- cooks.distance(mod3)
sample_size <- nrow(clean_data_sample)
plot(cooksd, pch=".", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="grey") 

#Creating a dataset wout the outliers
clean_data_sample_NOoutlier3=clean_data_sample
clean_data_sample_NOoutlier3=clean_data_sample_NOoutlier3[!cooksd> (4/sample_size),]
View(clean_data_sample_NOoutlier3)

#Building a lm wout outliers

mod3_WO <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + 
             + mindfulness + weight + IQ + household_income, data = clean_data_sample_NOoutlier3)
summary(mod3)
summary(mod3_WO)

#Check the assumptions: normality (of the residuals), linearity (of the relationship), 
#homogeneity of variance (also called homoscedasticity) and that there is no excess multicollinearity 

# 1.normality (of the residuals)

Residuals_mod3 <- residuals(mod3)	#Residuals of the final model
shapiro.test(Residuals_mod3)
hist(Residuals_mod3)
mod3 %>% plot(which = 2)#for qqplot
describe(residuals(mod3)) #for skew and kurtosis

Residuals_mod3WO <- residuals(mod3_WO)	#Residuals of the final model wout outliers
shapiro.test(Residuals_mod3WO)
hist(Residuals_mod3WO)

# 2. linarity (of the relationship)

plot(mod3) #Plotting the model /obtaining qq-plot linearity
plot(mod3_WO) #Plotting the model wout outliers /obtaining qq-plot linearity wout outliers
residualPlots(mod3)#For Tukey test
mod3%>% residualPlots()

# 3. homogeneity of variance (also called homoscedasticity)

plot(mod3,1)  
plot(mod3,3)  
bptest(mod3)

plot(mod3_WO,1)  
plot(mod3_WO,3)  
bptest(mod3_WO)

# 4. excess of multicollinearity

vif(mod3) #The numerical value for VIF tells you (in decimal form) what percentage the variance is inflated for each coefficient (cutoff 10).
vif(mod3_WO) #same wout outliers


#Backwards regression

step(mod3, direction = "backward") #Keep this one to be consistent as it doesn't violate assumptions
step(mod3_WO, direction = "backward")

Backward_mod3 <- lm(formula = pain ~ sex + age + pain_cat + cortisol_serum + mindfulness + 
                     weight, data = clean_data_sample)
Backward_mod3
summary(Backward_mod3)
lm.beta(Backward_mod3)
confint(Backward_mod3)

#Table Backwards Model
coef_table = function(Backward_mod3){
  require(lm.beta)
  mod_sum = summary(Backward_mod3)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3)) 
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"])) 
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(Backward_mod3), confint(Backward_mod3), c(0, lm.beta(Backward_mod3)$standardized.coefficients[c(2:length(Backward_mod3$coefficients))])), 2)), mod_sum_p_values) 
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value") 
  mod_sum_table["(Intercept)","Std.Beta"] = "0" 
  return(mod_sum_table)
}
coef_table(Backward_mod3)

#Adding the previous final model

mod2_Final <- lm(pain ~ sex + age + STAI_trait + pain_cat + 
                   cortisol_saliva + mindfulness, data = clean_data_sample)

mod2_Final
summary(mod2_Final)

#Akaike Information Criterion to compare models (lower score, better model)

AIC(Backward_mod3,mod2_Final)
AIC( mod3,Backward_mod3)
#Anova test to compare models 

anova(Backward_mod3,mod2_Final)
anova( mod3,Backward_mod3)

#make predictions on pain using the regression models or equations of the backward model and the theory-based model 
#which were “trained” on data file 1.

data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
View(data_sample_2)

#Predictions mod2_Final
head(data_sample_2)#to see first subjects data

sex = c("female","female","male","female","male","male")
age = c(45,50,43,36,42,39)
STAI_trait = c(45,44,41,40,40,35)
pain_cat = c(35,33,32,30,30,29)  
cortisol_saliva = c(5.412905,4.073101,4.066109, 5.523710,5.687106,4.544738)
mindfulness = c(1.943967,3.998201,2.375808,2.624612,2.256292,3.044178)

Pred_ds2 = as_tibble(sex,age,STAI_trait,pain_cat,cortisol_saliva,midfulness)	

predictions = predict(mod2_Final, newdata = Pred_ds2)	

Pred_ds2_with_predicted = cbind(Pred_ds2, predictions)	
Pred_ds2_with_predicted	

Preditcmodel_2Fin <- predict(mod2_Final, data_sample_2, allow.new.levels = TRUE)
Preditcmodel_2Fin


#Predictions Backward_mod3

sex = c("female","female","male","female","male","male")
age = c(45,50,43,36,42,39)
pain_cat = c(35,33,32,30,30,29)
cortisol_serum = c(5.731991,4.885175,3.997039,5.845967,5.468741,3.611170)
mindfulness = c(1.943967,3.998201,2.375808,2.624612,2.256292,3.044178)
weight = c(73.07633,77.27140,77.50331,70.54675,64.94016,67.21125)

Pred_ds2B = as_tibble(sex,age,pain_cat,cortisol_serum,midfulness,weight)	

predictions = predict(Backward_mod3, newdata = Pred_ds2B)	

Pred_ds2B_with_predicted = cbind(Pred_ds2B, predictions)	
Pred_ds2B_with_predicted	

Preditcmodel_3Fin <- predict(Backward_mod3, data_sample_2, allow.new.levels = TRUE)
Preditcmodel_3Fin

#See predictions
View(data_sample_2)

#Report the prediction performance of the backward model and the theory-based model on the new data (data file 2).
  #Model 2 Final
RSS = sum((data_sample_2$pain - predict(mod2_Final))^2)	
RSS	
  #Model Backward
RSS = sum((data_sample_2$pain - predict(Backward_mod3))^2)	
RSS	


#Predictions data file 1

Preditcmodel_2Findata1 <- predict(mod2_Final, clean_data_sample, allow.new.levels = TRUE)
Preditcmodel_2Findata1

Preditcmodel_3Findata1 <- predict(Backward_mod3, clean_data_sample, allow.new.levels = TRUE)
Preditcmodel_3Findata1

#Model 2 Final
RSS = sum((clean_data_sample$pain - predict(mod2_Final))^2)	
RSS	
#Model Backward
RSS = sum((clean_data_sample$pain - predict(Backward_mod3))^2)	
RSS	