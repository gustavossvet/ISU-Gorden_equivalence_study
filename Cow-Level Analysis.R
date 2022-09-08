###################################################################################################
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

datacow = read_excel("Cow-level analysis data-8-7-22.xlsx", 
                     sheet = "CowLevel_Data-minus unenrolled", na = ".") 
str(datacow)

datascc = read_excel("Cow-level analysis data-8-7-22.xlsx", 
                     sheet = "CowLevel_SCC", na = ".") 
str(datascc)


datamilk = read_excel("Cow-level analysis data-8-7-22.xlsx", 
                      sheet = "CowLevel_Milk", na = ".") 
str(datamilk)


##################################################################################################
#########                               2 - PREPARE DATA                                 #########

# Cow dataset:
datacow$CowID  <- as.factor(datacow$CowID )
datacow$LactationNumber  <- as.factor(datacow$LactationNumber )
datacow$LactationNumberNumeric  <- as.numeric(datacow$LactationNumber )
datacow$Farm  <- as.factor(datacow$Farm )
datacow$Treatment  <- as.factor(datacow$Treatment)
datacow$Treatment <- relevel(datacow$Treatment, ref = "ORB")

datacow$MastitisEvent  <- as.factor(datacow$MastitisEvent )
datacow$Death  <- as.factor(datacow$Death)
datacow$Cull  <- as.factor(datacow$Cull )
datacow$Survival  <- as.factor(datacow$Survival )
datacow$Retention  <- as.factor(datacow$Retention )
datacow$DeathCull  <- as.factor(datacow$DeathCull )

datacow$SurvMastitis  <- as.numeric(datacow$MastitisEvent )
datacow$SurvDeath  <- as.numeric(datacow$Death)
datacow$SurvCull  <- as.numeric(datacow$Cull )
datacow$SurvSurvival  <- as.numeric(datacow$Survival)
datacow$SurvRetention  <- as.numeric(datacow$Retention )
datacow$SurvDeathCull  <- as.numeric(datacow$DeathCull )

str(datacow)


# SCC dataset:
datascc$Treatment  <- as.factor(datascc$Treatment)
datascc$LactationNumber  <- as.factor(datascc$LactationNumber)
datascc$CowID <- as.factor(datascc$CowID)
datascc$QMonth <- as.numeric(datascc$Month)
datascc$Month <- as.factor(datascc$Month)
datascc$Farm <- as.factor(datascc$Farm)

str(datascc)

# Milk dataset:
datamilk$Treatment  <- as.factor(datamilk$Treatment)
datamilk$LactationNumber  <- as.factor(datamilk$LactationNumber)
datamilk$CowID <- as.factor(datamilk$CowID)
datamilk$QMonth <- as.numeric(datamilk$Month)
datamilk$Month <- as.factor(datamilk$Month)
datamilk$Farm <- as.factor(datamilk$Farm)

str(datamilk)

##################################################################################################
#########                              3 - DESCRIBE DATA                                 #########


summary(datacow)

Desc2 <- datacow %>% subset(select=c(CowID,	Farm,	Treatment,DOCurrentSCC,	DOLogCurrentSCC,
                                     DOPreviousSCC,DOLogPreviousSCC, `1_LogSCC`,	`2_LogSCC`,	
                                     `3_LogSCC`,	`4_LogSCC`, DOMilkWeight_kg, `1_Milkyield`, 
                                     `2_Milkyield`,	`3_Milkyield`,	`4_Milkyield`, LactationNumber, 
                                     DaysDry, DIMatPFculture, MastitisEvent, Death,	Cull, DeathCull, 
                                     NumberEvents,	DaysFreshToEvent)) 

table1::table1(~ Farm +	LactationNumber + DaysDry +	MastitisEvent	+ Death	+ Cull + DeathCull	+ 
                 NumberEvents +  DaysFreshToEvent + DOCurrentSCC	+ DOLogCurrentSCC	+ DOPreviousSCC	+ 
                 DOLogPreviousSCC +  `1_LogSCC`+	`2_LogSCC`+	`3_LogSCC`+	`4_LogSCC`+`1_Milkyield`+ 
                 `2_Milkyield`+	`3_Milkyield`+ `4_Milkyield`| Treatment,
               data=Desc2)

##################################################################################################
#########                            4 - Statistical analysis                            #########

## Model 1 - Risk of a mastitis event:

## Statistical model: generalized linear mixed model (logistic regression)
#     * Outcome: MastitisEvent.
#     * Fixed effect: Treatment.
#     * Covariates: number of days dry	and number of lactations (as a numeric variable).
#     * Random effects: Farm, Cows within farms, and Cows.

model5 <- glmer(MastitisEvent ~ Treatment + DaysDry	+  
                  (1 | Farm), 
                family = binomial, control = glmerControl(optimizer = "bobyqa"),
                data = datacow)
summary(model5)
tab_model(model5)
 

### ANOVA:
model5aov<- Anova(model5, test="Chisq", type=3)
kable(as.data.frame(model5aov))
 

### LS Means - Adjusted Risk(%):
marginal.bin = lsmeans(model5, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")

abce <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                   `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(abce)
 

### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))
kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = 1.6, SE = betta$SE, 
                 Lower_CI = 1.6 - (1.96*betta$SE), 
                 Upper_CI = 1.6 + (1.96*betta$SE))
kable(az)


### LS Means - Plot:
ggplot(abce,
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
  
  ylab("Probability of Mastitis event, %")+
  xlab("Treatment Group")+
  geom_text(nudge_x = c(0.1, 0.1, 0.1),
            nudge_y = c(0.1, 0.1, 0.1),
            color   = "black")+ 
  theme(legend.position="bottom")

 
### LS Means - Plot:
ggplot(abce,
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

## Model 2 - Risk of a Death:

## Statistical model: generalized linear mixed model (logistic regression)
#     * Outcome: Death.
#     * Fixed effect: Treatment.
#     * Covariates: Days dry.
#     * Random effects: Farm, Cows within farms, and Cows.

model6 <- glmer(Death ~ Treatment + DaysDry +
                  (1 | Farm), 
                family = binomial, control = glmerControl(optimizer = "bobyqa"),
                data = Desc2)
summary(model6)
options(digits=3)
tab_model(model6)
 

### ANOVA:
model6aov<- Anova(model6, test="Chisq", type=3)
kable(as.data.frame(model6aov))


### LS Means - Adjusted Risk(%):
marginal.bin = lsmeans(model6, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")
a <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(a)
 

### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))
kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = -0.5, SE = betta$SE, 
                 Lower_CI = -0.5 - (1.96*betta$SE), 
                 Upper_CI = -0.5 + (1.96*betta$SE))
kable(az)

### LS Means - Plot:
ggplot(a,
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
  theme_bw()


### LS Means - Plot:
ggplot(a,
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
  scale_color_manual(values = c("#999999", "#000000"))

##################################################################################################

## Model 3 - Cull:

### Statistical model: generalized linear mixed model (logistic regression)
#     * Outcome: Cull.
#     * Fixed effect: Treatment.
#     * Covariates: covariates tested and removed if p-value > 0.5 or if there were no changes in the estimates.
#     * Random effects: Farm, Cows within farms, and Cows.

model6 <- glmer(Cull ~ Treatment + 
                  (1 | Farm), 
                family = binomial, control = glmerControl(optimizer = "bobyqa"),
                data = Desc2)
summary(model6)
options(digits=3)
tab_model(model6)
 

### ANOVA:
model6aov<- Anova(model6, test="Chisq", type=3)
kable(as.data.frame(model6aov))


### LS Means - Adjusted Risk(%):
marginal.bin = lsmeans(model6, ~ Treatment)
b <- cld(marginal.bin, alpha   = 0.05, Letters = letters,    
         type    = "response", ### Report emmeans in orginal scale
         adjust =  "sidak")
abbc <- data.frame(Treatment = b$Treatment, Probability = b$prob, SE = b$SE, 
                   `Lower CI`= b$asymp.LCL, `Upper CI` = b$asymp.UCL, Group = b$.group)
kable(abbc)


### Betta and SE:
betta = as.data.frame(pairs(marginal.bin, adjust="tukey"))
kable(marginal.bin)
kable(betta)


### 95% CI of risk difference (%):
az <- data.frame(RiskDifference = 3.1, SE = betta$SE, 
                 Lower_CI = 3.1 - (1.96*betta$SE), 
                 Upper_CI = 3.1 + (1.96*betta$SE))
kable(az)


### LS Means - Plot:
ggplot(abbc,
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
  theme_bw()


### LS Means - Plot:
 
ggplot(abbc,
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
  scale_color_manual(values = c("#999999", "#000000"))

##################################################################################################

## Model 4 - Somatic cell count (SCC) over months:


### Statistical model: repeated measures model
#     * Outcome: SCC
#     * Fixed effect: Treatment and Month.
#     * Covariates: covariates tested and removed if p-value > 0.5 or if there were no changes in the estimates.

model2 <- lme(SCC ~ Treatment * Month,
              random= ~1|CowID,
              correlation = corCompSymm(form = ~ QMonth|CowID),
              data=datascc, na.action="na.omit")

summary(model2)
#library(rcompanion)
#plotNormalHistogram(residuals(model2))
 

### ANOVA:
anova2 <- Anova(model2, type=3)
kable(as.data.frame(anova2))
 

### LS Means:
marginal2<- lsmeans(model2, "Treatment", by="Month", data=datascc)
#kable(marginal2)
b2 <- cld(marginal2, alpha   = 0.05, Letters = letters,    ### Use lower-case letters for .group
          type    = "response", ### Report emmeans in orginal scale
          adjust =  "sidak")
abc2 <- data.frame(Month = b2$Month, Treatment = b2$Treatment, SCC = b2$lsmean, SEM = b2$SE,
                   Group = b2$.group, lower.CL = b2$lower.CL, upper.CL = b2$upper.CL)
kable(abc2)
 

### Difference and 95% CI:
marginal3<- lsmeans(model2, "Treatment", data=datascc)
pairs(marginal3,  adjust="tukey")
aza <- data.frame(RiskDifference = 0.101 , SE = 0.110  , 
                  Lower_CI = 0.101  - (1.96*0.110  ), 
                  Upper_CI = 0.101  + (1.96*0.110  ))
kable(aza)


### LS Means - Plot:
ggplot(abc2,
       aes(x     = Month,
           y     = SCC,
           color = Treatment,
           label = Group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_line(aes(y = SCC))+
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  
  ylab("Log SCC")+
  xlab("Month")+
  geom_text(nudge_x = c(0.01, 0.01, 0.01, 0.01),
            nudge_y = c(0.07, 0.07, 0.07, 0.07),
            color   = "black")


### LS Means - Treatment differnce:
marginal2<- lsmeans(model2, "Treatment", data=datascc)
#kable(marginal2)
b2 <- cld(marginal2, alpha   = 0.05, Letters = letters,    ### Use lower-case letters for .group
          type    = "response", ### Report emmeans in orginal scale
          adjust =  "Tukey")
kable(b2)


# New:

(scc_bg <- ggplot(abc2, aes(x=Month , y=SCC, group=Treatment, color=Treatment)) +
    geom_line(size=1.25, position=position_dodge(0.2)) +
    geom_point(size=2, position=position_dodge(0.2))+
    scale_y_continuous(limits = c(1, 3))+
    scale_color_manual(values = c("#999999", "#000000"))+
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.05, colour = NA)+
    ggtitle("Somatic cell count")+
    ylab("Geometric mean SCC (log scale)")+
    xlab("Month")+
    theme_bw()+ 
    theme(legend.position="bottom")+
    theme(plot.title = element_text(size = 14))+ 
    theme(axis.title = element_text(size = 12))  +
    theme(axis.title.x = element_text(size = 12)) +  
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) 
)

# New 2

(scc_color <- ggplot(abc2, aes(x=Month , y=SCC, group=Treatment, color=Treatment, fill = Treatment)) +
    geom_line(size=1.25, position=position_dodge(0.2)) +
    geom_point(size=2, position=position_dodge(0.2))+
    scale_y_continuous(limits = c(1, 3))+
    #scale_color_manual(values = c("#999999", "#000000"))+
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.05, colour = NA)+
    ggtitle("Somatic cell count")+
    ylab("Geometric mean SCC (log scale)")+
    xlab("Month")+
    theme_bw()+ 
    theme(legend.position="bottom")+
    theme(plot.title = element_text(size = 14))+ 
    theme(axis.title = element_text(size = 12))  +
    theme(axis.title.x = element_text(size = 12)) +  
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) 
)



##################################################################################################

## Model 10 - Milk Yield over months:

### Statistical model: repeated measures model
#     * Outcome: Milk
#     * Fixed effect: Treatment and Month.
#     * Covariates: covariates tested and removed if p-value > 0.5 or if there were no changes in the estimates.

model2 <- lme(Milk ~ Treatment * Month,
              random= ~1|CowID,
              correlation = corCompSymm(form = ~ QMonth|CowID),
              data=datamilk, na.action="na.omit")
summary(model2)
#plotNormalHistogram(residuals(model1))
 

### ANOVA:
anova2 <- Anova(model2, type=3)
kable(as.data.frame(anova2))
 

### LS Means:
marginal2<- lsmeans(model2, "Treatment", by="Month", data=datascc)
#kable(marginal2)
b2 <- cld(marginal2, alpha   = 0.05, Letters = letters,    ### Use lower-case letters for .group
          type    = "response", ### Report emmeans in orginal scale
          adjust =  "sidak")

abc2 <- data.frame(Month = b2$Month, Treatment = b2$Treatment, Yield = b2$lsmean, SEM = b2$SE,
                   Group = b2$.group, lower.CL = b2$lower.CL, upper.CL = b2$upper.CL)
kable(abc2)


### Difference and 95% CI:
pairs(marginal2,  adjust="tukey")
az <- data.frame(RiskDifference = 0.701, SE = 0.712, 
                 Lower_CI = 0.701 - (1.96*0.712), 
                 Upper_CI = 0.701 + (1.96*0.712))
kable(az)


### LS Means - Plot:
ggplot(abc2,
       aes(x     = Month,
           y     = Yield,
           color = Treatment,
           label = Group)) +
  geom_line() +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7) +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  
  ylab("Milk Yield")+
  xlab("Month")+
  geom_text(nudge_x = c(0.1, 0.1, 0.1, 0.1),
            nudge_y = c(4, 4, 4, 4),
            color   = "black")


### LS Means - Treatment differnce:
marginal2<- lsmeans(model2, "Treatment", data=datascc)
#kable(marginal2)
b2 <- cld(marginal2, alpha   = 0.05, Letters = letters,    ### Use lower-case letters for .group
          type    = "response", ### Report emmeans in orginal scale
          adjust =  "Tukey")
kable(b2)


# New

(milk_bg <- ggplot(abc22, aes(x=Month , y=Yield, group=Treatment, color=Treatment)) +
    geom_line(size=1.25, position=position_dodge(0.2)) +
    geom_point(size=2, position=position_dodge(0.2))+
    scale_y_continuous(limits = c(32, 48))+
    scale_color_manual(values = c("#999999", "#000000"))+
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.05, colour = NA)+
    ggtitle("Milk yield")+
    ylab("Average daily milk yield(kg)")+
    xlab("Month")+
    theme_bw()+ 
    theme(legend.position="bottom")+
    theme(plot.title = element_text(size = 14))+ 
    theme(axis.title = element_text(size = 12))  +
    theme(axis.title.x = element_text(size = 12)) +  
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) 
)

# New 2

(milkcolor <- ggplot(abc22, aes(x=Month , y=Yield, group=Treatment, color=Treatment, fill = Treatment)) +
    geom_line(size=1.25, position=position_dodge(0.2)) +
    geom_point(size=2, position=position_dodge(0.2))+
    scale_y_continuous(limits = c(32, 48))+
    #scale_color_manual(values = c("#999999", "#000000"))+
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.05, colour = NA)+
    ggtitle("Milk yield")+
    ylab("Average daily milk yield(kg)")+
    xlab("Month")+
    theme_bw()+ 
    theme(legend.position="bottom") +
    theme(plot.title = element_text(size = 14))+ 
    theme(axis.title = element_text(size = 12))  +
    theme(axis.title.x = element_text(size = 12)) +  
    theme(axis.text.x = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 12)) 
)
