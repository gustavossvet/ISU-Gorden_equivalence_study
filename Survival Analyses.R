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

datacow = read_excel("Cow-level analysis data-8-7-22.xlsx", 
                     sheet = "CowLevel_Data-minus unenrolled", na = ".") 

str(datacow)

##################################################################################################
#########                               2 - PREPARE DATA                                 #########

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


datasurv <- datacow %>% subset(select=c(CowID,	Farm,	Treatment,DaysDry, SurvMastitis, SurvDeath,	
                                        SurvCull, SurvDeathCull, 
                                        NumberEvents,	DaysFreshToEvent, LactationNumber )) 
datasurv <- na.omit(datasurv)

##################################################################################################
#########                              3 - DESCRIBE DATA                                 #########

table1::table1(~ Treatment , data=datasurv)

##################################################################################################
#########                            4 - Statistical analysis                            #########

#### Time-to-event models:


### Statistical model: cox regression model (time-to-event) - Mastititis
#       * Outcome: Mastitis
#       * Fixed effect: Treatment.

### Model - Survival for Mastitis:

# Kaplan Meyer
fit2 <- su1rvfit(Surv(DaysFreshToEvent, SurvMastitis) ~ Treatment, data = datasurv)
print(fit2)
ggsurvplot(fit2, data = datasurv) # Survival Plot

## Log-rank
surv_diff <- survdiff(Surv(DaysFreshToEvent, SurvMastitis) ~ Treatment, data = datasurv)
surv_diff


### Survival Plot for Clinical Mastitis:
 
(Mastitis <- ggsurvplot(fit2, data = datasurv,
                        surv.median.line = "hv", # Add medians survival
                        
                        # Change legends: title & labels
                        title = "Clinical Mastitis",
                        legend = "bottom",
                        legend.title = "Product",
                        legend.labs = c("ORB", "SO"),
                        xlab = "DIM",
                        
                        # Change font size, style and color at the same time
                        font.main = c(14, "bold"),
                        font.x = c(12, "bold"),
                        font.y = c(12, "bold"),
                        font.tickslab = c(12, "plain"),
                        
                        # Add p-value and tervals
                        pval = TRUE,
                        
                        conf.int = FALSE,
                        # Add risk table
                        #risk.table = TRUE,
                        #tables.height = 0.2,
                        #tables.theme = theme_cleantable(),
                        
                        # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
                        # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
                        palette = c("#E7B800", "#2E9FDF"),
                        ggtheme = theme_bw() # Change ggplot2 theme
))


### Cox regression model for Mastitis and Coefficients:

multi.cox <- coxph(Surv(DaysFreshToEvent, SurvMastitis) ~ Treatment + LactationNumber , 
                   cluster = Farm, data =  datasurv)
summary(multi.cox)

exp(coef(multi.cox))
exp(confint(multi.cox))

 
##################################################################################################

## Statistical model: cox regression model (time-to-event) - Cull or Death
#       * Outcome: Cull or Death
#       * Fixed effect: Treatment.

### Model - Survival for Cull:
 
# Kaplan
fit3 <- survfit(Surv(DaysFreshToEvent, SurvDeathCull) ~ Treatment, data = datasurv)
print(fit3)
ggsurvplot(fit3, data = datasurv) # Survival Plot

## Log-rank
surv_diff <- survdiff(Surv(DaysFreshToEvent, SurvDeathCull) ~ Treatment, data = datasurv)
surv_diff
 

### Survival PLot for Cull or Death.
(DeathCull <- ggsurvplot(fit3, data = datasurv,
                         #surv.median.line = "hv", # Add medians survival
                         
                         # Change legends: title & labels
                         title = "Culling or Death",
                         legend = "bottom",
                         legend.title = "Product",
                         legend.labs = c("ORB", "SO"),
                         xlab = "DIM",
                         
                         # Change font size, style and color at the same time
                         font.main = c(14, "bold"),
                         font.x = c(12, "bold"),
                         font.y = c(12, "bold"),
                         font.tickslab = c(12, "plain"),
                         
                         # Add p-value and tervals
                         pval = TRUE,
                         
                         conf.int = FALSE,
                         # Add risk table
                         #risk.table = TRUE,
                         #tables.height = 0.2,
                         #tables.theme = theme_cleantable(),
                         
                         # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
                         # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
                         palette = c("#E7B800", "#2E9FDF"),
                         ggtheme = theme_bw() # Change ggplot2 theme
))


### Cox regression model for Cull or Death:
 
multi.cox <- coxph(Surv(DaysFreshToEvent, SurvDeathCull) ~ Treatment + LactationNumber, 
                   cluster = Farm, data =  datasurv)
summary(multi.cox)

exp(coef(multi.cox))
exp(confint(multi.cox))


## Survival Plot together:
require("survminer")
splots <- list()
splots[[1]] <- Mastitis
splots[[2]] <- DeathCull

# Arrange multiple ggsurvplots and print the output
arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 1, risk.table.height = 0.4)

 
##################################################################################################

## Statistical model: cox regression model (time-to-event) - Cull
#       * Outcome: Cull or Death
#       * Fixed effect: Treatment.

### Model - Survival for Cull:
 
# Kaplan
fit3 <- survfit(Surv(DaysFreshToEvent, SurvCull) ~ Treatment, data = datasurv)
print(fit3)
ggsurvplot(fit3, data = datasurv) # Survival Plot

## Log-rank
surv_diff <- survdiff(Surv(DaysFreshToEvent, SurvCull) ~ Treatment, data = datasurv)
surv_diff


### Survival PLot for Cull or Death.
(Cull <- ggsurvplot(fit3, data = datasurv,
                    surv.median.line = "hv", # Add medians survival
                    
                    # Change legends: title & labels
                    title = "Culling",
                    legend = "bottom",
                    legend.title = "Product",
                    legend.labs = c("ORB", "SO"),
                    xlab = "DIM",
                    
                    # Change font size, style and color at the same time
                    font.main = c(14, "bold"),
                    font.x = c(12, "bold"),
                    font.y = c(12, "bold"),
                    font.tickslab = c(12, "plain"),
                    
                    # Add p-value and tervals
                    pval = TRUE,
                    
                    conf.int = FALSE,
                    # Add risk table
                    #risk.table = TRUE,
                    #tables.height = 0.2,
                    #tables.theme = theme_cleantable(),
                    
                    # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
                    # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
                    palette = c("#E7B800", "#2E9FDF"),
                    ggtheme = theme_bw() # Change ggplot2 theme
))
 

### Cox regression model for Cull or Death:
 
multi.cox <- coxph(Surv(DaysFreshToEvent, SurvCull) ~ Treatment + LactationNumber, 
                   cluster = Farm, data =  datasurv)
summary(multi.cox)
exp(coef(multi.cox))
exp(confint(multi.cox))


################################################################################################## 

## Statistical model: cox regression model (time-to-event) - Death
#       * Outcome: Cull or Death
#       * Fixed effect: Treatment.

### Model - Survival for Cull:
 
## Log-rank
surv_diff <- survdiff(Surv(DaysFreshToEvent, SurvDeath) ~ Treatment, data = datasurv)
surv_diff


### Survival PLot for Death.
(Death <- ggsurvplot(fit3, data = datasurv,
                     surv.median.line = "hv", # Add medians survival
                     
                     # Change legends: title & labels
                     title = "Death",
                     legend = "bottom",
                     legend.title = "Product",
                     legend.labs = c("ORB", "SO"),
                     xlab = "DIM",
                     
                     # Change font size, style and color at the same time
                     font.main = c(14, "bold"),
                     font.x = c(12, "bold"),
                     font.y = c(12, "bold"),
                     font.tickslab = c(12, "plain"),
                     
                     # Add p-value and tervals
                     pval = TRUE,
                     
                     conf.int = FALSE,
                     # Add risk table
                     #risk.table = TRUE,
                     #tables.height = 0.2,
                     #tables.theme = theme_cleantable(),
                     
                     # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
                     # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
                     palette = c("#E7B800", "#2E9FDF"),
                     ggtheme = theme_bw() # Change ggplot2 theme
))
 

### Cox regression model for Death:
 
multi.cox <- coxph(Surv(DaysFreshToEvent, SurvDeath) ~ Treatment + LactationNumber, 
                   cluster = Farm, data =  datasurv)
summary(multi.cox)

exp(coef(multi.cox))
exp(confint(multi.cox))

##################################################################################################

## Adding plots together.

require("survminer")
splots <- list()
splots[[1]] <- Mastitis
splots[[2]] <- Cull
splots[[3]] <- Death

# Arrange multiple ggsurvplots and print the output
arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 3, nrow = 1, risk.table.height = 0.4)


##################################################################################################
