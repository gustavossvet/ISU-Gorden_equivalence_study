####################################################################################################
#
### Title: Randomized Non-Inferiority Study Evaluating the Efficacy of Shut-Out vs. Orbeseal 
#          Internal Teat Sealants in Dairy Cows
#
### Iowa State University
### PI:  Dr. Patrick Gorden
### Data Analysis: Dr. Gustavo Silva
### date: "8/7/2022"
#
#
##################################################################################################
#########                                   0 - PACKAGES                                 #########
packages<-c("readxl", "nlme", "lme4","lmerTest","car","lattice","insight","performance",
            "HLMdiag","ggplot2", "sjPlot","sjmisc","emmeans","multcomp","multcompView",
            "lmtest", "rcompanion","MASS","blmeco", "knitr", "stargazer", "dplyr", "plyr",
            "summarytools", "table1", "survival","survminer" )

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
ipak(packages)


##################################################################################################
#########                                 1 - LOAD DATA                                  #########

dataquarter = read_excel("Quarter-Level Analysis-8-7-22.xlsx", 
                         sheet = "Sheet 1-Minus Not enrolled", na = ".") 
str(dataquarter)

##################################################################################################
#########                               2 - PREPARE DATA                                 #########

dataquarter$CowID <- as.factor(dataquarter$CowID)
dataquarter$Farm <- as.factor(dataquarter$Farm)
dataquarter$Treatment <- as.factor(dataquarter$Treatment)
dataquarter$Treatment <- relevel(dataquarter$Treatment, ref = "ORB")
dataquarter$Lactation <- as.factor(dataquarter$Lactation)
dataquarter$LactationNumber <- as.numeric(dataquarter$Lactation)
dataquarter$Quarter <- as.factor(dataquarter$Quarter)

dataquarter$IMIatDO <- as.factor(dataquarter$IMIatDO)
dataquarter$IMIatPF <- as.factor(dataquarter$IMIatPF)
dataquarter$Cure <- as.factor(dataquarter$Cure)
dataquarter$NewIMI <- as.factor(dataquarter$NewIMI)

str(dataquarter)


##################################################################################################
#########                              3 - DESCRIBE DATA                                 #########

summary(dataquarter)
Desc1 <- dataquarter %>% subset(select=c(CowID,	Farm,	Treatment,	Lactation, 
                                        LactationNumber,DryPeriodlength,	Quarter, 
                                        IMIatDO,	IMIatPF,	Cure,	NewIMI)) 

## Descriptive statistics:
table1::table1(~ Farm +	Lactation + DryPeriodlength +	IMIatDO +	IMIatPF +	
                 Cure +	NewIMI + Treatment, data=Desc1)


## Descriptive statistics by treatment:
table1::table1(~ Farm +	Lactation + DryPeriodlength +	IMIatDO +	IMIatPF +	
                 Cure +	NewIMI | Treatment, data=Desc1)

## Descriptive statistics for pathogen by treatment:

### Culture DO:
table1::table1(~ DOCulture1 | Treatment, data=dataquarter)

### Culture PF:
table1::table1(~ PFCulture1	| Treatment, data=dataquarter)

##################################################################################################
#########                            4 - Statistical analysis                            #########

## Data analysis - Quarter-level dynamics:

## Statistical models to assess non-inferiority between products:
  
#    * Model 1: IMI at DO.
#    * Model 2: IMI at PF.
#    * Model 3: Cure.
#    * Model 4: IMI risk.

##################################################################################################

## Model 1 - IMI at DO:

### Statistical model: generalized linear mixed model (logistic regression)
#       * Outcome: IMIatDO.
#       * Fixed effect: Treatment.
#       * Covariate: Lactation Number.
#       * Random effects: Farm, Cows within farms, and Cows.

model1 <- glmer(IMIatDO ~ Treatment +	Lactation  + (1 | Farm/CowID)  , 
                family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"),
                data = Desc1)
summary(model1)
tab_model(model1)

### ANOVA:
model1aov<- Anova(model1, test="Chisq", type=3)
kable(as.data.frame(model1aov))
 

### LS Means - Adjusted Risk(%):
 
marginal.bin = lsmeans(model1, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")

ab <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                 `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(ab)


### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))

kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = 2.5, SE = betta$SE, 
                 Lower_CI = 2.5 - (1.96*betta$SE), 
                 Upper_CI = 2.5 + (1.96*betta$SE))
kable(az)


### LS Means - Plot:
 
ggplot(ab,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000"))+
  theme_bw() +
  theme(legend.position="bottom")


### LS Means - Plot:
 
ggplot(ab,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000"))+ 
  theme(legend.position="bottom")


 

##################################################################################################

## Model 2 - IMI at PF:

### Statistical model: generalized linear mixed model (logistic regression)
#       * Outcome: IMIatPF.
#       * Fixed effect: Treatment.
#       * Covariate: covariates tested and removed if p-value > 0.5 or if there were no changes in the estimates.
#       * Random effects: Farm and Cows.

model2 <- glmer(IMIatPF ~ Treatment  + IMIatDO + (1 | Farm/CowID) , 
                family = binomial, control = glmerControl(optimizer = "bobyqa"),
                data = Desc1)
summary(model2)
tab_model(model2)
 

### ANOVA:
model2aov<- Anova(model2, test="Chisq", type=3)
kable(as.data.frame(model2aov))


### LS Means - Adjusted Risk(%):
marginal.bin = lsmeans(model2, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")
abc <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                  `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(abc)
 

### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))
kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = 2.6, SE = betta$SE, 
                 Lower_CI = 2.6 - (1.96*betta$SE), 
                 Upper_CI = 2.6 + (1.96*betta$SE))
kable(az)


### LS Means - plot:
ggplot(abc,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000"))+
  theme_bw() +
  theme(legend.position="bottom")


### LS Means - Plot:
ggplot(abc,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000")) +
  theme(legend.position="bottom")

##################################################################################################

## Model 3 - Cure:

### Statistical model: generalized linear mixed model (logistic regression)
#       * Outcome: Cure.   
#       * Fixed effect: Treatment.
#       * Covariates: No covariates.
#       * Random effects: Farm, and Cows.

model3 <- glmer(Cure ~ Treatment  + (1| Farm/CowID), 
                family = binomial, control = glmerControl(optimizer = "bobyqa"),
                data = Desc1)
summary(model3)
tab_model(model3)


### ANOVA:
model3aov<- Anova(model3, test="Chisq", type=3)
kable(as.data.frame(model3aov))
 

### LS Means - Adjusted Risk(%):
marginal.bin = lsmeans(model3, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")
abcd <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                   `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(abcd)


### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))
kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = -0.8, SE = betta$SE, 
                 Lower_CI = -0.8 - (1.96*betta$SE), 
                 Upper_CI = -0.8 + (1.96*betta$SE))
kable(az)


### LS Means - Plot --  plot:
ggplot(abcd,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000"))+
  theme_bw() +
  theme(legend.position="bottom")


### LS Means - Plot --  Third plot:
ggplot(abcd,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000")) +
  theme(legend.position="bottom")

##################################################################################################

## Model 4 - IMI risk:

### Statistical model: generalized linear mixed model (logistic regression)
#       * Outcome: NewIMI.
#       * Fixed effect: Treatment.
#       * Covariate: Farm as a confounder.
#       * Random effects: Cows.

model4 <- glmer(NewIMI ~ Treatment  + IMIatDO  + (1 | Farm/CowID), 
                family = binomial, control = glmerControl(optimizer = "bobyqa"),
                data = Desc1)
summary(model4)
tab_model(model4)
 

### ANOVA:
model4aov<- Anova(model4, test="Chisq", type=3)
kable(as.data.frame(model4aov))


### LS Means - Adjusted Risk(%):
marginal.bin = lsmeans(model4, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")

ac <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                 `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(ac)
 

### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))
kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = 1.6, SE = betta$SE, 
                 Lower_CI = 1.6 - (1.96*betta$SE), 
                 Upper_CI = 1.6 + (1.96*betta$SE))
kable(az)

 
### LS Means - Plot --  Second plot:
ggplot(ac,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000"))+
  theme_bw() +
  theme(legend.position="bottom")


### LS Means - Plot --  Third plot:
ggplot(ac,
       aes(x     = Treatment,
           y     = Probability,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  Lower.CI,
                    ymax  =  Upper.CI),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Probability, %")+
  xlab("Treatment Group")+
  scale_color_manual(values = c("#999999", "#000000")) +
  theme(legend.position="bottom")

##################################################################################################
