#code to compare pre and post scores for those with and without matched results
library(tidyverse)
`%!in%` <- Negate(`%in%`)


#step 1:load nonmatched and matched datasets

pretest_matched <- read_csv("./data/output/pre_ph_survey_matched_for_analysis.csv") %>%
  select(-IPAddress)
posttest_matched <- read_csv("./data/output/post_ph_survey_matched_for_analysis.csv") %>%
  select(-IPAddress)

pretest_nonmatched <- read_csv("./data/output/pre_ph_survey_nonmatched_for_analysis.csv") %>%
  select(-IPAddress)
posttest_nonmatched <- read_csv("./data/output/post_ph_survey_nonmatched_for_analysis.csv") %>%
  select(-IPAddress)
#step 2: drop all responses on nonmatched datasets that correspond to a a participant with a matched response

matched_participant_ids <- pretest_matched$Q1.3_1

unmatchable_pretests <- pretest_nonmatched %>%
  filter(Q1.3_1 %!in% matched_participant_ids)

unmatchable_posttests <- posttest_nonmatched %>%
  filter(Q1.3_1 %!in% matched_participant_ids)

#drop post only questions
posttest_nonmatched <- posttest_nonmatched %>%
  select(-c("Q5.5", "Q5.6"))

unmatchable_posttests <- unmatchable_posttests %>%
  select(-c("Q5.5", "Q5.6"))

posttest_matched <- posttest_matched %>%
  select(-c("Q5.5", "Q5.6"))


#combine data for stats
#make a column for "matched" that will be either Y or N, then bind rbind together

pretest_matched$matched <- "yes"
unmatchable_pretests$matched <- "no"

posttest_matched$matched <- "yes"
unmatchable_posttests$matched <- "no"

all_pre_responses <- rbind(pretest_matched, unmatchable_pretests)
all_post_responses <- rbind(posttest_matched, unmatchable_posttests)

#remove extra cols for stats
all_pre_responses <- all_pre_responses %>%
  select(-Q1.3_1, -Q1.3_1_raw)

#all_pre_responses <- all_pre_responses %>% 
 # select(-Q3.1, -Q3.2)

all_post_responses <- all_post_responses %>%
  select(-Q1.3_1, -Q1.3_1_raw, -Q3.1, -Q3.2)

#all_post_responses <- all_post_responses %>%
#  select(-Q3.1, -Q3.2)

#step 3: unpaired wilcox tests
pre_wilcox <- all_pre_responses %>% 
  gather(measure, value, -matched, -ResponseId) %>%
  nest(-measure) %>%
  #YOU MUST TOGGLE PAIRED = T/F DEPENDING ON IF USING MATCHED OR NONMATCHED DATA
  mutate(fit = map(data, ~ wilcox_test(value ~ matched, data = .x, paired = F))) %>%
  unnest(fit)

post_wilcox <- all_post_responses %>% 
  gather(measure, value, -matched, -ResponseId) %>%
  nest(-measure) %>%
  #YOU MUST TOGGLE PAIRED = T/F DEPENDING ON IF USING MATCHED OR NONMATCHED DATA
  mutate(fit = map(data, ~ wilcox_test(value ~ matched, data = .x, paired = F))) %>%
  unnest(fit)


#also do this not grouped by question, just need 1 col with all matched and 1 col with all unmatched
pre_long <- all_pre_responses %>%
  pivot_longer(-c(matched, ResponseId), names_to = "item", values_to = "score")


wilcox_test(pre_long, score ~ matched, paired = F)

post_long <- all_post_responses %>%
  pivot_longer(-c(matched, ResponseId), names_to = "item", values_to = "score")


_test(post_long, score ~ matched, paired = F)


#or do I want to do this on averages?
pre_long_means <- pre_long %>%
  group_by(ResponseId, matched) %>%
  summarise(mean_score = mean(score)) %>%
  #this ungroup almost ruined everything
  ungroup()

wilcox_test(pre_long_means, mean_score ~ matched)

#or do I want to do this on averages?
post_long_means <- post_long %>%
  group_by(ResponseId, matched) %>%
  summarise(mean_score = mean(score)) %>%
  #this ungroup almost ruined everything
  ungroup()

wilcox_test(post_long_means, mean_score ~ matched)

#we check consitancy with all 10
#but drop 2 for gains bc we don't expect them to change 



            