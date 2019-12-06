#Linear Mixed Effects Models

data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")
data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")


library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	
library(lattice)
library(influence.ME)

#Check on data and possible missing data or errors

View(data_sample_3)  
str(data_sample_3)
summary(data_sample_3) 
describe(data_sample_3)

#Cleaning errors
Data_sample_3_cleaned <- data_sample_3 %>% 
  mutate(
  sex = droplevels(replace(sex,sex == "Female", "female")), 
  sex = droplevels(replace(sex,sex == "Male", "male")))
str(Data_sample_3_cleaned)

Data_sample_3clean <- Data_sample_3_cleaned %>%
  mutate(hospital = recode(hospital,
                           "hospital_10"=10,
                           "hospital_9"= 9,
                           "hospital_8"= 8,
                           "hospital_7"= 7,
                           "hospital_6"= 6,
                           "hospital_5"= 5,
                           "hospital_4"= 4,
                           "hospital_3"= 3,
                           "hospital_2"= 2,
                           "hospital_1"= 1,))
View(Data_sample_3clean)    
str(Data_sample_3clean)

mod_int_hosp = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_saliva 
                    + mindfulness + (1 | hospital), data = Data_sample_3clean)

Data_sample_3clean = Data_sample_3clean %>% mutate(resid = residuals(mod_int_hosp))

#Check on assumptions
    #First. Influential outliers
influence_observation = influence(mod_int_hosp, obs = T)$alt.fixed 
influence_group = influence(mod_int_hosp, group = "hospital")$alt.fixed

    #Plot them
data_plot_inflience = as_tibble(influence_group) %>% gather(colnames(influence_group),
                                                            value = coefficient, key = predictor)
data_plot_inflience %>% ggplot() + aes(x = 1, y = coefficient,
                                       group = predictor) + geom_violin() + facet_wrap(~predictor,
                                                                                       scales = "free")
    #Plot for normality
qqmath(mod_int_hosp, id = 0.05)

    #Plot normality random effects
qqmath(ranef(mod_int_hosp))

    #Plot linearoty
plot(mod_int_hosp, arg = "pearson")

    #Plot linearity for each predictor
Data_sample_3clean %>% ggplot() + aes(x = hospital, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = sex, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = age, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = STAI_trait, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = pain_cat, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = cortisol_saliva, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = mindfulness, y = resid) +
  geom_point()

Data_sample_3clean %>% ggplot() + aes(x = ".", y = resid) +
  geom_point() #Think about this pure fun

    #Plot for Homoscedasticity

plot(mod_int_hosp, arg = "pearson")
vif(mod_int_hosp) #w car package

    #Check the complete model F-test p-value. If it is < 0.05, heteroscedasticity on
    #the cluster level might be problematic.
homosced_mod = lm(resid^2 ~ hospital, data = Data_sample_3clean)
summary(homosced_mod)

#Plot it

# caluclate interquartile range within each cluster
IQR_of_residuals_by_hospital = sapply(split(Data_sample_3clean,
                                               f = Data_sample_3clean$hospital), function(x) IQR(x$resid))
# rank ordering them
rank = rank(IQR_of_residuals_by_hospital)
# adding rank to the dataframe containing the residuals
Data_sample_3clean$rank = rep(rank, each = length(c(1,2,3,4,5,6,7,8,9,10 )))
# creating a vector of participant IDs ordered based on the
# rank, this will be used as labels
IDforplot = unique(Data_sample_3clean$hospital[order(Data_sample_3clean$rank)])
# create the plot

ggplot(Data_sample_3clean, aes(y = resid, x = factor(rank), labels = hospital)) +
  geom_boxplot() + scale_x_discrete(labels = IDforplot) + coord_flip()


    #Plot for multicollinearity
pairs.panels(Data_sample_3clean[, c("sex","age","STAI_trait","pain_cat","cortisol_saliva",
                                    "mindfulness", "hospital")], col = "red", lm = T)

#Run the analisys 

mod_int_hosp = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_saliva 
                    + mindfulness + (1 | hospital), data = Data_sample_3clean)
summary(mod_int_hosp)
confint(mod_int_hosp)

mod_int_hosp
#Comparing the model coefficients and intervals with those at assignment 1

mod2_Final <- lm(pain ~ sex + age + STAI_trait + pain_cat + 
                   cortisol_saliva + mindfulness, data = clean_data_sample)
summary(mod2_Final)
confint(mod2_Final)

#Residuals and AIC comparison

sum(residuals(mod_int_hosp)^2)
sum(residuals(mod2_Final)^2)

AIC(mod2_Final)
cAIC(mod_int_hosp)

#variance explained by the fixed effect predictors using marginal R2 
#and the variance explained by the fixed and random effect terms combined using conditional R2.

r.squaredGLMM(mod_int_hosp)

r2beta(mod_int_hosp, method = "nsj", data = Data_sample_3clean)	#with intervals/ extra thing

#Making predictions in new dataset 
    #change hospitals for new dataset

data_sample_4 <- data_sample_4 %>%
  mutate(hospital = recode(hospital,
                           "hospital_11"=11,
                           "hospital_12"= 12,
                           "hospital_13"= 13,
                           "hospital_14"= 14,
                           "hospital_15"= 15,
                           "hospital_16"= 16,
                           "hospital_17"= 17,
                           "hospital_18"= 18,
                           "hospital_19"= 19,
                           "hospital_20"= 20,))
View(data_sample_4)    
str(data_sample_4)
head(data_sample_4)

    #Predictions
Preditcmodel_mixed <- predict(mod_int_hosp, data_sample_4, allow.new.levels = TRUE)
Preditcmodel_mixed

MeanModel <- lmer(pain~ 1 + (1|hospital), data = data_sample_3, REML = FALSE)
anova(MeanModel, mod_int_hosp)

# Compute the variance explained by the model on data file 3
RSS = sum((data_sample_4$pain - predict(mod_int_hosp, Data_sample_3clean, allow.new.levels = TRUE))^2)	
TSS = sum((data_sample_4$pain - predict(MeanModel, Data_sample_3clean, allow.new.levels = TRUE ))^2)

#Now compute the variance explained by the model on data file 3.
1-(RSS/TSS)

# Compute the variance explained by the model on data file 4
RSS = sum((data_sample_4$pain - predict(mod_int_hosp, data_sample_4, allow.new.levels = TRUE))^2)	
TSS = sum((data_sample_4$pain - predict(MeanModel, data_sample_4, allow.new.levels = TRUE ))^2)

#Now compute the variance explained by the model on data file 4.
1-(RSS/TSS)

#Build a new linear mixed effects model on dataset 3 predicting pain


#Finding our best predictor 

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

stdCoef.merMod(mod_int_hosp)


#Based on the coefficients I pick cortisol_saliva and build the intercept and slope

mod_rnd_slope = lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), data = Data_sample_3clean)
summary(mod_rnd_slope)

#Visualize it for separate hospital


Data_rnd_slope = Data_sample_3clean %>% mutate(pred_slope = predict(mod_rnd_slope))


Data_rnd_slope %>% ggplot() + aes(y = pain, x = cortisol_saliva,
group = hospital) + geom_point(aes(color = hospital), size = 3) +
geom_line(color = "red", aes(y = pred_slope, x = cortisol_saliva)) +
facet_wrap(~hospital, ncol = 2)

confint(mod_int_hosp, level = 0.95)
