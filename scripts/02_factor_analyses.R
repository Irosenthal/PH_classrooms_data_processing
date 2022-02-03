#Code for EFA and CFA on planet hunters survey data
#interpretation: https://easystats.github.io/effectsize/reference/interpret_gfi.html

#libraries ####
library(tidyverse)
library(lavaan)
library(psych)


#load data ####
#use combined semester 1/2 matched pairs
pretest <- read_csv("./data/output/pre_ph_survey_matched_duration_filter.csv") %>% 
  select(-c("Q1.3_1_raw", "IPAddress"))

#for the factor analysis, filter out post only questions
posttest <- read_csv("./data/output/post_ph_survey_matched_duration_filter.csv") %>% 
  select(-c("Q5.5", "Q5.6", "Q1.3_1_raw", "IPAddress"))


#drop responseID column. this is just a unique ID for each row, we need to drop this one and replace it with the actual respondant ID
pretest <- select(pretest, -ResponseId)
posttest <- select(posttest, -ResponseId)

#clean up the colnames
#rename Q1.3_1 to participant_id so that it actually contains the participant_id code
new_names<- c("participant_id", "Q19","Q20","Q21","Q22", "Q23", "Q24", "Q25", "Q27",     
              "Q30","Q31")

names(pretest) <- new_names
names(posttest) <- new_names

#now split each of these in half randomly
#we want to use half of the data for our CFA while retaining half of it for an EFA

#set a seed to make this reproducible
set.seed(42)

#generare random indices
#doing it this way should retain matched pairs in pre/post samples
randomized_rows <- sample(nrow(pretest))

pre_for_cfa <- pretest[randomized_rows[1:(nrow(pretest)/2)], ]
pre_for_efa <- pretest[randomized_rows[round((nrow(pretest)/2) +1):(nrow(pretest))], ]

post_for_cfa <- posttest[randomized_rows[1:(nrow(posttest)/2)], ]
post_for_efa <- posttest[randomized_rows[round((nrow(posttest)/2) +1):(nrow(posttest))], ]

#drop cols that are not part of factors
pre_for_cfa_clean <- pre_for_cfa %>%
  select(-participant_id) %>%
  select(-c(Q19, Q20, Q23))

pre_for_efa_clean <- pre_for_efa %>%
  select(-participant_id) %>%
  select(-c(Q19, Q20, Q23))

post_for_cfa_clean <- post_for_cfa %>%
  select(-participant_id) %>%
  select(-c(Q19, Q20, Q23))

post_for_efa_clean <- post_for_efa %>%
  select(-participant_id) %>%
  select(-c(Q19, Q20, Q23))


#Drop all NAs
pre_for_cfa_clean <- pre_for_cfa_clean[complete.cases(pre_for_cfa_clean),]
pre_for_efa_clean <- pre_for_efa_clean[complete.cases(pre_for_efa_clean),]
post_for_cfa_clean <- post_for_cfa_clean[complete.cases(post_for_cfa_clean),]
post_for_efa_clean <- post_for_efa_clean[complete.cases(post_for_efa_clean),]

#Add latent factor labels  to columns
latent_labels <- c("Q21_SE", "Q22_SE", "Q24_DL", "Q25_DL", "Q27_DL", "Q30_CS", "Q31_CS")


names(pre_for_cfa_clean) <- latent_labels
names(pre_for_efa_clean) <- latent_labels
names(post_for_cfa_clean) <- latent_labels
names(post_for_efa_clean) <- latent_labels

#convert to a matrix
pre_for_cfa_clean <- as.matrix(pre_for_cfa_clean)
pre_for_efa_clean <- as.matrix(pre_for_efa_clean)
post_for_cfa_clean <- as.matrix(post_for_cfa_clean)
post_for_efa_clean <- as.matrix(post_for_efa_clean)




# Exploratory Factor Analysis ####

#run on both pre_for_cfa_clean and post_for_cfa_clean - they should come out the same

#this makes a scree plot as well as running a parallel analysis
#we want to id the point where the line levels off
psych::fa.parallel(x=pre_for_efa_clean, fm="mle", fa="fa")
psych::fa.parallel(x=post_for_efa_clean, fm="mle", fa="fa")
#altho its saying 3 and 2, the scree plots are very similar and EFAs are finicky. 

#can also make a correlation plot with hierarchical clustering to visualize
corrplot::corrplot(cor(pre_for_efa_clean), order = "hclust")

#now run the EFA
#It is likely that our factors are correlated with each-other, so use oblimin rotation (oblique)
fa_model <- psych::fa(r = pre_for_efa_clean, nfactors = 2, 
                      rotate = "oblimin", 
                      fm = "pa",max.iter = 100)
fa_model
#diagnostics
#model fit: 
#SS loadings are all >1, satisfying the kaiser rule 
#commonalities all > 0.2

#plot it
fa.diagram(fa_model)


# Confirmatory Factor Analysis ####
#check assumptions
#missing values - separate task. these matched pairs by definition have no missing values, but we do need to confirm the missing values aren't fishy.

#factorability: Kaiser-Meyer-Olkin (KMO) Test for Sampling Adequacy 
KMO(cor(pre_for_cfa_clean))
#pre: overall is 0.76, all above 0.6. min threshold is 0.6. some sources say this is "middling" and we want above 0.8
KMO(cor(post_for_cfa_clean))
#post: overall is 0.83, all items over 0.8

#outliers:
boxplot(pre_for_cfa_clean, use.cols = TRUE)
boxplot(post_for_cfa_clean, use.cols = TRUE)

#this seems ok? we definitely have most of our responses in the upper end. 
#can ANY response in a likert scale be an outlier? researchgate and stats exchange say no

#normality and linearity - skew/kurtosis
MVN::mvn(pre_for_cfa_clean)
MVN::mvn(post_for_cfa_clean)
#nothing is normal. I expected this, we should be able to account for this with our estimator and nonparametric stats for our later comparisons
#skew and kurtosis seem pretty bad, but should also be handled by estimator
#to accomodate: use the MLM estimator - it has Satorra-Bentler scaled chi squared test of model fit to account for non-normal data


#define latents

all_latents <- '
  personal_sci_self_efficacy =~ Q21_SE + Q22_SE
  data_literacy =~ Q24_DL + Q25_DL + Q27_DL
  citizen_science =~ Q30_CS + Q31_CS
  '
#fit CFA

cc_cfa_fit <- cfa(all_latents, data = post_for_cfa_clean, estimator = "MLM")
summary(cc_cfa_fit, fit.measures= TRUE)

#model fit statistics

#presurveys
#Robust Chi square: 0.679  bad, but i don't think we care. very sensitive
#CFI: 0.936 0.9 is the minimally accepted cut-off, >0.95 is desired
#RMSEA: 0.072 (robust is 0.083)  should be < 0.08
#TLI: 0.877. 0.9 is the minimally accepted cut-off, >0.95 is desired
#SRMR: 0.052 <0.08 is desirable

#postsurveys
#some issues. some indices returning 0 or 1. maybe try fitting with the entire dataset? rule of thumb is N = 200, we are below that here. 

