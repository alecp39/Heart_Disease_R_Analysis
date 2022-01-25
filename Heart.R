heart <- heart

#cleaning/restructuring is at bottom

head(heart)
summary(heart)

summary(heart$st_depression)

# compute significance levels for pearson and spearman correlations first one correlation matrix, second is pvalue correlation 
install.packages("Himsc")
library("Hmisc")
corr2 <- rcorr(as.matrix(heart))
corr2

correlation <- cor(as.matrix(heart))

#correlation plot <-- very cool 
install.packages("corrplot")
library(corrplot)
corrplot(correlation, type = "full", tl.col = "black", tl.srt = 45)
# positive correlations are in blue, negatives are in red, color intensity and size of circle are proportional to correlation coefficients

hist(heart$Age, main = "Age of Patient", xlab = "Age")
hist(heart$resting_blood_pressure, main = "Resting Blood Pressure on Admission to Hospital", xlab = "RBP")
hist(heart$cholesterol, main = "Cholesterol", xlab = "Cholesterol Level")
hist(heart$st_depression, main = "ST depression induced by exercise relative to rest", xlab = "Level")

counts <- table(heart$Sex)
barplot(counts, main = "Sex", xlab = "Female vs Male")

counts1 <- table(heart$chest_pain_type)
barplot(counts1, main = "Chest Pain", xlab = "Chest Pain Type")

counts2 <- table(heart$fasting_blood_sugar)
barplot(counts2, main = "Fasting Blood Sugar > 120 mg/dl")

counts3 <- table(heart$rest_ecg)
barplot(counts3, main = "Resting Electrocardiographic Results")

counts4 <- table(heart$exercise_induced_angina)
barplot(counts4, main = "Exercised induced angia")

counts5 <- table(heart$st_slope)
barplot(counts5, main = "Slope of the Peak Exercise ST Segment")

counts6 <- table(heart$num_major_vessels)
barplot(counts6, main = "Number of Major Vessels", xlab = "Number")

counts7 <- table(heart$thalassemia)
barplot(counts7, main = "Thalassemia", xlab = "3 = normal, 6 = fixed defect, 7 = reversable defect")

counts8 <- table(heart$`Heart Disease`)
barplot(counts8, main = "Heart Disease")

#histogram with ages
ggplot(heart, aes(Age)) + geom_histogram(bins = 40, fill = "lightskyblue3") +
  labs(title = "Age of Patient", x = "Age") + 
  theme_bw(base_size = 18) 
#box plot with ages 
ggplot(heart, aes(Age)) + geom_boxplot(fill = "lightskyblue3") +
  labs(title = "Age", x = "") + theme_bw(base_size = 18) 

#histogram with cholesterol 
ggplot(heart, aes(cholesterol)) + geom_histogram(bins = 40, fill = "lightskyblue3") +
  labs(title = "Cholesterol", x = "Cholesterol") + 
  theme_bw(base_size = 18) 
#box plot cholesterol 
ggplot(heart, aes(cholesterol)) + geom_boxplot(fill = "lightskyblue3") +
  labs(title = "Cholesterol", x = "Cholesterol") + theme_bw(base_size = 18) 

#resting blood pressure 
#hist
ggplot(heart, aes(resting_blood_pressure)) + geom_histogram(bins = 40, fill = "lightskyblue3") +
  labs(title = "Resting Blood Pressure", x = "Resting Blood Pressure") + 
  theme_bw(base_size = 18) 
#box
ggplot(heart, aes(resting_blood_pressure)) + geom_boxplot(fill = "lightskyblue3") +
  labs(title = "Resting Blood Pressure", x = "Resting Blood Pressure") + theme_bw(base_size = 18)

#max heart rate 
#hist
ggplot(heart, aes(max_heart_rate_achieved)) + geom_histogram(bins = 40, fill = "lightskyblue3") +
  labs(title = "Maximum Heart Rate", x = "Max Heart Rate") + 
  theme_bw(base_size = 18) 

#box
ggplot(heart, aes(max_heart_rate_achieved)) + geom_boxplot(fill = "lightskyblue3") +
  labs(title = "Max Heart Rate", x = "Heart Rate") + theme_bw(base_size = 18) 

# st_depression
#hist
ggplot(heart, aes(st_depression)) + geom_histogram(bins = 40, fill = "lightskyblue3") +
  labs(title = "ST Depression", x = "ST Depression") + 
  theme_bw(base_size = 18) 
#box
ggplot(heart, aes(st_depression)) + geom_boxplot(fill = "lightskyblue3") +
  labs(title = "ST Depression", x = "ST Depression") + theme_bw(base_size = 18)

#heart disease counts 
ggplot(heart, aes(`Heart Disease`, fill = `Heart Disease`)) + 
  geom_bar(stat = "count") + scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Heart Disease") +  theme_bw(base_size = 18) +
  theme(legend.position="bottom")

#chest pain type counts 
ggplot(heart, aes(chest_pain_type, fill = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Chest Pain", x = "") +  theme_bw(base_size = 18) +
  theme(legend.position="bottom")

#fasting blood sugar counts 
ggplot(heart, aes(fasting_blood_sugar, fill = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Fasting Blood Sugar", x = "") +  
  theme_bw(base_size = 18) + theme(legend.position="bottom")

#resting ecg 
ggplot(heart, aes(rest_ecg, fill = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Resting Electrocardiographic Results", x = "") +  
  theme_bw(base_size = 18) + theme(legend.position="bottom")

#slope of peak exercise ST segment 
ggplot(heart, aes(st_slope, fill = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Slope of Peak Exercise ST Segment", x = "") +  
  theme_bw(base_size = 18) + theme(legend.position="bottom")

#number of major vessels
ggplot(heart, aes(num_major_vessels, fill = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Number of Major Vessels", x = "") +  
  theme_bw(base_size = 18) + theme(legend.position="bottom")

#Thalassemia
ggplot(heart, aes(thalassemia, fill = `Heart Disease`)) + 
  geom_bar(stat = "count", position = "dodge") + 
  scale_fill_manual(values=c('grey70', 'lightskyblue3')) + 
  labs(title = "Thalassemia", x = "") +  
  theme_bw(base_size = 18) + theme(legend.position="bottom")


#installing additional packages from sample analysis
install.packages("cowplot")
install.packages("tidyverse")
install.packages("highcharter")  
install.packages("GGally") 
install.packages("lubridate")
install.packages("lattice")
install.packages("psych")
install.packages("DataExplorer")
install.packages("reshape2")
install.packages("car")
install.packages("caret")
install.packages("e1071")
install.packages("scales")
install.packages("stringr")
install.packages("gridGraphics")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("lmtest")
install.packages("gvlma")
install.packages("modelr")
install.packages("broom")
install.packages("ROCR")
install.packages("catools")

library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(lattice)
library(psych)
library(DataExplorer)
library(reshape2)
library(car)
library(caret)
library(e1071) 
library(scales)
library(stringr)
library(gridGraphics)

library(gridExtra)
library(cowplot)
library(lmtest)
library(gvlma)
library(modelr) 
library(broom)
library(ROCR)
library(caTools)


#cleaning up the data and refactoring things 
heart <- heart %>% 
  rename(
    'Age' = 'age',
    'Sex' = 'sex', 
    'chest_pain_type' = 'cp', 
    'resting_blood_pressure' = 'trestbps', 
    'cholesterol' = 'chol', 
    'fasting_blood_sugar' = 'fbs', 
    'rest_ecg' = 'restecg', 
    'max_heart_rate_achieved' = 'thalach',
    'exercise_induced_angina' = 'exang', 
    'st_depression' = 'oldpeak', 
    'st_slope' = 'slope', 
    'num_major_vessels' = 'ca', 
    'thalassemia' = 'thal', 
    'Heart Disease' = 'target') %>%
  mutate(HD = `Heart Disease`); glimpse(df)
# adding HD as a numeric value to retain 0,1 
# for model building - changing to a factor
# changes 0,1 to 1,2 which will not work with probabilities

View(heart)
#relevel factors 

heart$Sex = factor(heart$Sex, 
                labels = c("Female", "Male"))

heart$chest_pain_type = factor(heart$chest_pain_type, 
                            labels = c('typical angina',
                                       'atypical angina',
                                       'non-anginal pain',
                                       'asymptomatic'))

heart$fasting_blood_sugar = factor(heart$fasting_blood_sugar, 
                                labels = c('lower than 120mg/ml',
                                           'greater than 120mg/ml'))

heart$rest_ecg = factor(heart$rest_ecg, 
                     labels = c('normal',
                                'ST-T wave abnormality',
                                'left ventricular hypertrophy'))

heart$exercise_induced_angina = factor(heart$exercise_induced_angina, 
                                    labels = c('No', 'Yes'))

heart$st_slope = factor(heart$st_slope,
                     labels = c('upsloping', 'flat',
                                'downsloping'))

heart <- heart %>% filter(thalassemia != 0)
heart$thalassemia = factor(heart$thalassemia,
                        labels = c('normal', 'fixed defect',
                                   'reversable defect'))

heart <- heart %>% filter(num_major_vessels != 4)
heart$num_major_vessels <- as.factor(heart$num_major_vessels)

heart$`Heart Disease` = factor(heart$`Heart Disease`,
                            labels = c('No', 'Yes'))

View(heart)

#make sure everything is a factor 
heart$Sex <- as.factor(heart$Sex)
heart$fasting_blood_sugar <- as.factor(heart$fasting_blood_sugar)
heart$rest_ecg <- as.factor(heart$rest_ecg)
heart$exercise_induced_angina <- as.factor(heart$exercise_induced_angina)
heart$st_slope <- as.factor(heart$st_slope)
heart$thalassemia <- as.factor(heart$thalassemia)
heart$heart_disease <- as.factor(heart$heart_disease)

#Run the regression 

#do the log transformation
#St depression 
#cholesterol 
#resting BPM 
#Max Heart Rate

#relevel what you need 

levels(heart$chest_pain_type)

heart$chest_pain_type <- relevel(heart$chest_pain_type, "asymptomatic", "atypical angina", "non-anginal pain", "typical angina")
heart$st_slope <- relevel(heart$st_slope, "flat", "downsloping","upsloping")
heart$fasting_blood_sugar <- relevel(heart$fasting_blood_sugar, "lower than 120mg/ml","greater than 120mg/ml")
heart$rest_ecg <- relevel(heart$rest_ecg, "normal", "ST-T wave abnormality", "left ventricular hypertrophy")


#regression 
heartmod <- heart %>% select("HD", 
                    # "exercise_induced_angina",
                    "max_heart_rate_achieved",
                    "chest_pain_type",
                    "st_depression",
                    "num_major_vessels",
                    "thalassemia",
                    # "st_slope",
                    "Sex",
                    # "Age",
                    "resting_blood_pressure") %>% 
  #"rest_ecg") %>% 
  glm(family = binomial(link = 'logit'))

summary(heartmod)

#rename heart disease column 
names(heart) [names(heart) == "Heart Disease"] <- "heart_disease"
view(heart)




