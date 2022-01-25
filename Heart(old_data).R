dat <- heart

# Change labels of features
dat['sex'][dat['sex'] == 1] = 'Male'
dat['sex'][dat['sex'] == 0] = 'Female'

dat['target'][dat['target'] == 1] = 'Yes' 
dat['target'][dat['target'] == 0] = 'No' 

view(dat)

#Check to make sure R is treating this as a categorical variable
levels(dat$fbs)


#Define the variables as categorical (i.e., factors)
dat$restecg <- as.factor(dat$restecg)
dat$exang <- as.factor(dat$exang)
dat$slope <- as.factor(dat$slope)
dat$thal <- as.factor(dat$thal)


is.factor(dat$cp)


#Change the baseline
dat$cp <- relevel(dat$cp, "Asymptomatic", "Atypical angina", "Non-anginal pain", "Typical angina")
dat$restecg <- relevel(dat$restecg, "Normal", "ST-T wave abnormality", "Left ventricular hypertrophy")
dat$thal <- relevel(dat$thal, "Normal", "Fixed defect", "Reversable defect")
dat$thal <- relevel(dat$thal, "Normal", "Fixed defect", "Reversable defect")
dat$slope <- relevel(dat$slope, "Flat", "Downsloping","Upsloping")
dat$fbs <- relevel(dat$fbs, "Lower than 120mg/ml","Greater than 120mg/ml")


dat['cp'][dat['cp'] == 0] = 'Typical angina'
dat['cp'][dat['cp'] == 1] = 'Atypical angina'
dat['cp'][dat['cp'] == 2] = 'Non-anginal pain'
dat['cp'][dat['cp'] == 3] = 'Asymptomatic'

dat['fbs'][dat['fbs'] == 0] = 'Lower than 120mg/ml'
dat['fbs'][dat['fbs'] == 1] = 'Greater than 120mg/ml'

dat['restecg'][dat['restecg'] == 0] = 'Normal'
dat['restecg'][dat['restecg'] == 1] = 'ST-T wave abnormality'
dat['restecg'][dat['restecg'] == 2] = 'Left ventricular hypertrophy'

dat['exang'][dat['exang'] == 0] = 'No'
dat['exang'][dat['exang'] == 1] = 'Yes'

dat['slope'][dat['slope'] == 0] = 'Upsloping'
dat['slope'][dat['slope'] == 1] = 'Flat'
dat['slope'][dat['slope'] == 2] = 'Downsloping'

dat['thal'][dat['thal'] == 1] = 'Normal'
dat['thal'][dat['thal'] == 2] = 'Fixed defect'
dat['thal'][dat['thal'] == 3] = 'Reversable defect'

dat$oldpeakLog10<- log10(dat$oldpeak + 1)
view(dat)

dat$thalachLog10<- log10(dat$thalach)
dat$oldpeakLog10<- log10(dat$oldpeak)
dat$trestbpsLog10<- log10(dat$trestbps)
hist(STLog10)
RestLog<- log(dat$trestbps)
hist(RestLog)
chollog10 <- log10(dat$chol)
hist(chollog10)
thalog10 <- log10(dat$thalach)

dat <- dat %>% filter(ca != 4)
dat <- dat %>% filter(thal != 0)

colnames(dat)[1] <- c("Age")

heart_mod1 <- glm(target ~ ., data = dat, family  = binomial())
summary(heart_mod1)

heart_mod2 <- glm(target ~ sex + cp + slope + ca + thal, data = dat, family  = binomial())
summary(heart_mod2)

modelChi <- heart_mod1$null.deviance - heart_mod2$deviance
chidf <- heart_mod2$df.null -heart_mod2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

heart_mod2$aic

heart_mod1$coeff
exp(heart_mod1$coeff)

library(MASS)

exp(confint(heart_mod1))

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

