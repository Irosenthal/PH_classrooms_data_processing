#code to calculate Cronbach's alpha, run Wilcoxan signed rank tests on pre/post results, and calculate effect size
library(broom)
library(tidyverse)
library(rstatix)
#Initial data wrangling ####

#load matched data and drop unnecessary columns
pretest <- read_csv("./data/output/pre_ph_survey_matched_for_analysis.csv")
posttest <- read_csv("./data/output/post_ph_survey_matched_for_analysis.csv")

#or load  unmatched data #
#pretest <- read_csv("./data/output/pre_ph_survey_nonmatched.csv")
#posttest <- read_csv("./data/output/post_ph_survey_nonmatched.csv")

#load question metadata
question_LUT <- read_csv("./data/raw_data/ph_question_item_numbers.csv")[c(1:5)]




#pivot these long - for this comparison we care about group change, not individual

#make semester a character so it will pivot
#pretest$semester <- as.character(pretest$semester)
#posttest$semester <- as.character(posttest$semester)

pre_test_long <- pretest %>%
  pivot_longer(!Q1.3_1, names_to = "question", values_to = "response",
               values_transform = list(response = as.character))

post_test_long <- posttest %>%
  pivot_longer(!Q1.3_1, names_to = "question", values_to = "response",
               values_transform = list(response = as.character))
################################################################
#this box is to spot check question means

pre_test_means <- pre_test_long %>%
  left_join(question_LUT, by = c("question" = "name"))

pre_test_means_summary <- pre_test_means %>%
  #drop response ID for this as its not a number and chokes the summary
  filter(question != "Q1.3_1") %>%
  group_by(question) %>%
  summarise(mean_resp = mean(as.numeric(response), na.rm = F))
##################################################################

#now we need to make sure that there are consistent item numbers to go with each question.


#join these item numbers to each dataset
pre_test_long <- left_join(pre_test_long, question_LUT, by = c("question" = "name"))
post_test_long <- left_join(post_test_long, question_LUT, by = c("question" = "name"))


#now select only likert questions. I categorized each question as likert, binary, or text.
#this also drops ResponseID, which makes the pivots work better
likert_pre <- pre_test_long %>%
  filter(question_type == "likert")

likert_post <- post_test_long %>%
  filter(question_type == "likert")

#convert response to numeric to make future operations smooth
likert_pre$response <- as.numeric(likert_pre$response)
likert_post$response <- as.numeric(likert_post$response)

#Calculate mean/SD for pre and post survey results ####
likert_summary_pre <- likert_pre %>%
  group_by(Item) %>%
  summarize(mean_response_pre = mean(response, na.rm = T),
            sd_response_pre = sd(response, na.rm = T))

likert_summary_post <- likert_post %>%
  group_by(Item) %>%
  summarize(mean_response_post = mean(response, na.rm = T),
            sd_response_post = sd(response, na.rm = T))

#calculate %6 or 7
percent_6_pre <- likert_pre %>%
  group_by(Item) %>%
  mutate(total_response = n()) %>%
  group_by(Item, response) %>%
  summarise(n_responses = n(),
            percent_of_responses = (n()/total_response[1])*100,
            total_responses = total_response[1])


#Chronbach's alpha ####
#make data wide again - psych::alpha() wants one row per respondent and one column per question. 

likert_pre_wide <- pivot_wider(likert_pre,
                               id_cols = "Q1.3_1",
                               names_from = "Item", 
                               values_from = "response")

likert_post_wide <- pivot_wider(likert_post,
                                id_cols = "Q1.3_1",
                                names_from = "Item", 
                                values_from = "response")


#remove responseID column before feeding to alpha
pre_for_chronbach <- likert_pre_wide
pre_for_chronbach$Q1.3_1 <- NULL

post_for_chronbach <- likert_post_wide
post_for_chronbach$Q1.3_1 <- NULL

psych::alpha(pre_for_chronbach)
psych::alpha(post_for_chronbach)

#grouped alphas
self_efficacy_pre <- select(pre_for_chronbach, c("21", "22"))
data_literacy_pre <- select(pre_for_chronbach, c("24", "25", "27"))
citsci_pre <- select(pre_for_chronbach, c("30", "31"))

self_efficacy_post <- select(post_for_chronbach, c("21", "22"))
data_literacy_post <- select(post_for_chronbach, c("24", "25", "27"))
citsci_post <- select(post_for_chronbach, c("30", "31"))
#
psych::alpha(citsci_post)



#
###Data cleaning for wilcoxon signed rank tests ####


#need one column for treatment (pre/post), and one column per question.

#add a treatment column
likert_pre_wide_for_effsize <- likert_pre_wide %>%
  mutate(treatment = rep("pre", nrow(likert_pre_wide)))

likert_post_wide_for_effsize <- likert_post_wide %>%
  mutate(treatment = rep("post", nrow(likert_post_wide)))


#now drop post-survey only questions
likert_post_wide_for_effsize <- likert_post_wide_for_effsize %>%
  select(-c("37", "38"))


#now rbind them
all_data_for_effsize <- rbind(likert_pre_wide_for_effsize, likert_post_wide_for_effsize)

#rename columns to not be numbers
col_nums <- names(all_data_for_effsize)
new_names<- c("ResponseId", "Q19","Q20","Q21","Q22", "Q23", "Q24", "Q25", "Q27","Q30","Q31", "treatment")

names(all_data_for_effsize) <- new_names


#make treatment a factor so that effect size is calculated in the right direction
all_data_for_effsize$treatment  <- factor(all_data_for_effsize$treatment, c("pre", "post"))


####Wilcoxon signed rank tests ####

#we want paired = T. This does a Wilcoxon signed rank test, as opposed to paired = F which performs a Wilcoxon rank sum test.
#you want ranked sum for independant samples
#you want signed rank for paired samples (ie pre/post survey data)

all_wilcox <- all_data_for_effsize %>% 
  gather(measure, value, -treatment, -ResponseId) %>%
  nest(-measure) %>%
  #YOU MUST TOGGLE PAIRED = T/F DEPENDING ON IF USING MATCHED OR NONMATCHED DATA
  mutate(fit = map(data, ~ wilcox_test(value ~ treatment, data = .x, paired = T))) %>%
  unnest(fit)

#### Effect size ####
#terrible helper function to calculate all effect sizes. 
calculate_all_effect_sizes <- function(x){
  #add one entry for each question you care about
  Q19 <- wilcox_effsize(Q19 ~ treatment, data = x) 
  Q20 <- wilcox_effsize(Q20 ~ treatment, data = x) 
  Q21 <- wilcox_effsize(Q21 ~ treatment, data = x) 
  Q22 <- wilcox_effsize(Q22 ~ treatment, data = x) 
  Q23 <- wilcox_effsize(Q23 ~ treatment, data = x) 
  Q24 <- wilcox_effsize(Q24 ~ treatment, data = x) 
  Q25 <- wilcox_effsize(Q25 ~ treatment, data = x) 
  Q27 <- wilcox_effsize(Q27 ~ treatment, data = x) 
  Q30 <- wilcox_effsize(Q30 ~ treatment, data = x) 
  Q31 <- wilcox_effsize(Q31 ~ treatment, data = x) 
  
  
  all_effect_sizes <- rbind(Q19,Q20,Q21,Q22, Q23, Q24, Q25, Q27, Q30,Q31)
  return(all_effect_sizes)
}

effect_sizes <- calculate_all_effect_sizes(all_data_for_effsize)
#make treatment a factor so that effect size is calculated in the right direction

#Create summary table ####

#join pre and most mean/SD together
summaries <- left_join(likert_summary_pre, likert_summary_post)

#calculate pre/post difference
summaries <- mutate(summaries, pre_post_difference = mean_response_post - mean_response_pre)

#clean up "measure" column for join with summaries
#need to drop "Q" that was added as a result of pivoting from column names
all_wilcox$measure <- as.integer(gsub("Q", "", all_wilcox$measure))
#only keep ID, statistic, and p value
wilcox_for_join <- select(all_wilcox, measure, statistic, p)

#join
summaries <- left_join(summaries, wilcox_for_join,by = c("Item" = "measure"))

#now clean up effect sizes
effect_sizes$.y. <- as.integer(gsub("Q", "", effect_sizes$.y.))
effect_sizes_for_join <- select(effect_sizes, -n1, -n2)

#join
summaries <- left_join(summaries, effect_sizes_for_join,by = c("Item" = ".y."))

#and include question text
summaries <- inner_join(question_LUT, summaries) %>%
  select(-name, -question_type)

write_csv(summaries, "./data/output/analyses/summary_stats_paired_jan_2022_rerun_final.csv")


summaries <- read_csv("./data/output/analyses/summary_stats_paired_jan_2022_rerun_final.csv")






