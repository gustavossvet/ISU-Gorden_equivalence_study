# ISU-Gorden_non-inferiority_study

Manuscript title: A randomized non-inferiority study evaluating the efficacy of two commercially available teat sealants in dairy cows.

Authors: Michelle P. Buckley1, Jenna Bayne2#, Tiago Tomazi3, Brian E. Miller3, Sandra M. Godden4, Gustavo S. Silva2, and Patrick J. Gorden2*

Affiliations: 
1Department of Veterinary Microbiology and Preventative Medicine, Iowa State University, Ames, IA 50011
2Department of Veterinary Diagnostic and Production Animal Medicine, Iowa State University, Ames, IA 50011
3Dairy Technical Services, Merck Animal Health, Lenexa, KS 66219 
4Department of Veterinary Population Medicine, University of Minnesota, St. Paul, MN 55108

*Corresponding author: Patrick Gorden
2416 Lloyd Vet Med Center, 1809 S Riverside Dr, Ames, IA 50011
Email: pgorden@iastate.edu 

Abstract: The use of internal teat sealants is commonplace in the US dairy industry to prevent new intramammary infections during the dry period. The objective of this study was to compare the efficacy of a new internal teat sealant (ShutOut®, Merck & Co., Inc., Rahway, NJ, SO) to the first product introduced to the US dairy industry (Orbeseal®, Zoetis, Parsippany, NJ; ORB). The efficacy hypothesis was tested by completing a multi-site randomized, positively controlled non-inferiority evaluation of quarter-level new intramammary infection risk difference over the dry period on six dairy farms, with a margin of non-inferiority set a priori at 5%. The effect of treatment group on quarter-level new intramammary infection risk, cured intramammary infection risk, and intramammary risk at 1 to 13 DIM was determined using generalized linear mixed models. Cow-level factors, including incidence of clinical mastitis, culling, and death, as well as performance in early lactation based on test day milk production and somatic cell count, were also evaluated during the first 120 days in milk. At dry-off, aseptic, quarter-level, duplicate milk samples were collected for bacterial culture and then 500 mg cloxacillin benzathine (Orbenin®DC, Merck & Co., Inc., Rahway, NJ) was administered, followed by the randomly assigned internal teat sealant. After calving, repeat quarter-level milk sample were collected. The dry period new intramammary infection adjusted risk difference was -1.6 [CI -1.8, -1.37].  Final models demonstrated that there was no difference in risk rates of quarter-level outcomes between treatment groups. Analysis of cow-level factors, including clinical mastitis, culling, and death rate within the first 120 DIM, also revealed no differences. The results of this study indicate that ShutOut was not inferior to Orbeseal when utilizing blanket dry cow therapy.
Key Words: Dry cow therapy, intramammary infection, mastitis, teat sealant

Study objective: The objective of this study was to compare an alternative ITS (ShutOut® [SO], Merck & Co., Inc., Rahway, NJ) to ORB in a randomized, positively controlled non-inferiority trial evaluating quarter-level IMI dynamics during the dry period and cow-level health events during the first 120 DIM. Our hypothesis was that SO would not be inferior to ORB in efficacy at preventing new intramammary infections in the subsequent lactation.

MATERIALS AND METHODS
Statistical Analysis

Sample Size Calculation
Study size was determined based on a non-inferiority hypothesis evaluating the effect of dry cow sealant on new intramammary infection risk (NIMI) over the dry period. Treatments were assigned at the cow level, with an a priori NIMI margin of non-inferiority () established at 5%, α = 2.5%, and power of 80%. We assumed that 20% of quarters would develop a NIMI over the dry period (Johnson et al., 2016) and the non-inferiority margin was selected based on previous work by Rowe et al. (2020). Based on these assumptions, we estimated that 1,320 quarters (330 cows) would be required for each treatment. These estimates were inflated 1.2× to account for clustering within the data and to account for missing NIMI statuses, contaminated samples, and loss to follow up post-calving. The final sample size was determined to be 1,600 quarters (400 cows) per treatment group.

Data Management
All cow and quarter-level treatment and laboratory data were captured in spreadsheets (Excel) housed within a cloud-based data management and sharing environment (Box, Box, Inc, Redwood City, CA). Milk yield and loge SCC were generated through monthly DHIA testing. Days in milk at each test day were converted to month in lactation by dividing the DIM at each cow’s individual test day by 30. DHIA test data and disease event (clinical mastitis, culling, and death) data were captured from the farms’ electronic data management systems. 
        Data merging and cleaning, as well as data analyses, were performed using commercial statistical software (R Statistical Programing Environment, v.4.1.0) using the packages lme4, nlme, car, emmeans, multcomp, stats, and survival. Continuous variables were evaluated for normality visually using quantile-quantile plots. The data sets and analyses can be downloaded at https://github.com/gustavossvet/ISU-Gorden_non-inferiority_study. Individual cows were retrospectively excluded from all analysis if their dry period was outside the 30-to-90-day range, or if they received a parenteral or IMM antimicrobial treatment between calving and collection of the PF samples. Quarters that failed to have a determined IMI status were excluded from prevalence evaluations for that time point and risk evaluations. Additionally, if the PF sample was not collected within 14 days after parturition, these quarters were excluded from further evaluation. 

Effect of Treatment Group on Quarter-Level IMI Dynamics
        A comparison of culture results from DO and PF milk samples was used to evaluate dry period IMI dynamics. Per Rowe et al. (2020), a cured intramammary infection (CIMI) was defined as a quarter with an IMI at dry-off and either no growth or the presence of a different IMI pathogen at the post-fresh sampling. A NIMI was defined as either a quarter with no growth at dry-off and a positive post-fresh culture result, or a positive culture result at dry-off and different species-level pathogen present at the post-fresh sampling. Isolates were matched at the genus-level if species was not able to be determined.
 	A generalized linear mixed model (logistic regression, binomial family, and logit link) was used to build univariable and multivariable models exploring a relationship between explanatory variables of interest and IMI at enrollment, IMI at 1-14 DIM, CIMI risk, and NIMI risk. Potential cow-level confounders were determined as described in Rowe et al. (2020b), which included parity at dry-off, milk yield (kg) at the last DHIA test-day before enrollment, loge SCC at the last DHIA test-day before enrollment, and DIM at post-calving sample. Models accounted for clustering of quarters and cows by using random intercepts of cows and herds. Since the reporting of the risk difference (RD) is more appropriate for non-inferiority trials, we used the eemeans package in a commercial statistical software program (R Statistical Programing Environment, v.4.1.0) to compute the marginal means instead of reporting the odds ratio or using a log-binomial model to obtain a risk ratio. 
        The multivariable models were built including all potential cow-level confounders. Before entering the potential confounders in the model, variables that were highly correlated to each other, as determined by having a Pearson or Kendall’s tau correlation coefficient > 0.7, were removed and only one variable was used to assess confounding. In these instances, the most suitable variable was chosen based on biological plausibility for each model. Confounders were then added to the model one by one and retained in the final model as fixed effects if they were significant. Interaction terms with Wald tests at P < 0.05 were retained in the final model and potential confounders were removed from the model one at a time. If removal changed the effect estimate by more than 10%, the covariate was added back to the model (Greenland & Pearce, 2015). 
        A non-inferiority analysis was conducted to evaluate effect of ITS treatment on quarter-level dry period NIMI risk with an a priori margin of non-inferiority () established at 5%. The null hypothesis tested was that risk of NIMI for SO was >+5% than the risk of NIMI for ORB (NIMI risk difference >+5%), thus SO is inferior to ORB. To conduct the hypothesis test, a 2-sided, 95% confidence interval for the RD was used. If the limits of the 95% confidence interval are wholly above the margin of non-inferiority (), SO would be inferior to ORB. No superiority tests were conducted. 
 
 Effect of Treatment Group on Cow-Level Outcomes
 The effect of treatment group on milk yield and loge SCC were analyzed using repeated measures models. A linear mixed model was constructed, with cow and herd as random intercepts to account for the clustering of tests within cows, and cows within herds. A compound symmetry correlation structure was used. To assess the differences over time, the time in lactation (month) was included in the model (levels: 1, 2, 3, 4). To assess the differences between treatment over time, an interaction term between treatment and time was added in the model. Procedures for model building and potential confounder interactions were the same as those utilized when evaluating quarter-level IMI dynamics. Least squares means were used to compare differences between treatment groups. 
        Clinical mastitis, culling, and death events before 120 DIM were assessed using generalized linear mixed models for each individual outcome. Clinical mastitis cases were defined as the first case of mastitis noted by farm personnel from 0 to 120 DIM. The analysis followed the same process described above. In addition, survival analysis was conducted to determine the effect of ITS treatment on clinical mastitis and culling and death during the first 120 d of lactation. Cows that died or were culled before calving were excluded (left-censored) from this measurement. Cows were right-censored if they developed clinical mastitis, at culling, death, or when they reached 120 DIM. Kaplan-Meier survival curves were generated and tested with the log-rank test to compare the association between treatment groups and hazard ratios (HR) were estimated using Cox proportional hazards regression. Clustering of cows within herds was mitigated using a robust sandwich estimator within these estimates. Schoenfeld residuals were compared over time in order to assess proportional hazards assumptions for each covariate using the Schoenfeld test. Generalized linear models (log-binomial) were used to calculate risk ratio estimates from univariable models for the relationship between explanatory variables of interest and risk of clinical mastitis and culling or death risk in the first 120 DIM. The final Cox proportional hazards models for effect of treatment group on cow-level outcomes were produced using the same potential confounders and model building strategies that were applied to evaluation of quarter-level IMI dynamics.
