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
  select(-Q1.3_1_raw)

all_post_responses <- all_post_responses %>%
  select(-Q1.3_1_raw)

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


##final analysis starting here. drop 2 baseline questions and compare overall average pre to overall average post, both in the nonmatched data and the matched data

#also do this not grouped by question, just need 1 col with all matched and 1 col with all unmatched
pre_long <- all_pre_responses %>%
  select(-Q1.3_1_raw) %>%
  pivot_longer(-c(matched, ResponseId, Q1.3_1), names_to = "item", values_to = "score")


wilcox_test(pre_long, score ~ matched, paired = F)

post_long <- all_post_responses %>%
  select(-Q1.3_1_raw) %>%
  pivot_longer(-c(matched, ResponseId, Q1.3_1), names_to = "item", values_to = "score")


wilcox_test(post_long, score ~ matched, paired = F)


#or do I want to do this on averages?
pre_long_means <- pre_long %>%
  group_by(ResponseId, matched) %>%
  summarise(mean_score = mean(score)) %>%
  #this ungroup almost ruined everything
  ungroup()

t_test(pre_long_means, mean_score ~ matched)

#or do I want to do this on averages?
post_long_means <- post_long %>%
  group_by(ResponseId, matched) %>%
  summarise(mean_score = mean(score)) %>%
  #this ungroup almost ruined everything
  ungroup()

t_test(post_long_means, mean_score ~ matched)

post_long_means <- post_long_means %>%
  summarise(mean = mean(mean_score)) 


post_long_mean <- post_long %>%
  group_by(ResponseId) %>%
  summarise(mean_score = mean(score)) %>%
  mutate(pre_post = "post") 


#Final analysis here
#drop questions and find mean scores
post_long_subset <- post_long %>%  
  filter(item != "Q3.1") %>%
  filter(item != "Q3.2")%>%
  group_by(ResponseId, Q1.3_1, matched) %>%
  summarise(mean_score_post = mean(score))%>%
  ungroup()

pre_long_subset <- pre_long %>%  
  filter(item != "Q3.1") %>%
  filter(item != "Q3.2") %>%
  group_by(ResponseId, Q1.3_1,matched) %>%
  summarise(mean_score_pre = mean(score)) %>%
  ungroup()

pre_long <- pre_long %>%
  group_by(ResponseId) %>%
  summarise(mean_score = mean(score)) %>%
  mutate(pre_post = "pre")

t.test(pre_long_subset$mean_score_pre, post_long_subset$mean_score_post)


all_matched_data_for_paired_t_test <- left_join(pre_long_subset, post_long_subset, by = "Q1.3_1") %>% ungroup() %>%
  select(Q1.3_1, mean_score_pre, mean_score_post)

t.test(all_matched_data_for_paired_t_test, mean_score_pre ~ mean_score_post, paired = T)

#this comparison says that when we remove our 2 baseline questions, the average score changes from pre to post, regardless of if people are matched.             
t.test(all_matched_data_for_paired_t_test$mean_score_pre, 
       all_matched_data_for_paired_t_test$mean_score_post, paired = T)

#make sure that this is paired correctly, but this is the analysis we want for question 3

#overall, pre was lower than post. when you look at matched participants, we also see lower prescores than post scores. 
#we know matched pretests are representative of all pretests

#report overall, report matched, use matched for the rest