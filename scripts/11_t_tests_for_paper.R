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

#Drop 2 baseline questions and compare overall average pre to overall average post, both in the nonmatched data and the matched data

pre_long <- all_pre_responses %>%
  pivot_longer(-c(matched, ResponseId, Q1.3_1), names_to = "item", values_to = "score")

post_long <- all_post_responses %>%
  pivot_longer(-c(matched, ResponseId, Q1.3_1), names_to = "item", values_to = "score")


#also generate for just matched data (we will need it later)
matched_pre_long <- pretest_matched %>%
  select(-Q1.3_1_raw) %>%
  pivot_longer(-c(matched, ResponseId, Q1.3_1), names_to = "item", values_to = "score")

matched_post_long <- posttest_matched %>%
  select(-Q1.3_1_raw) %>%
  pivot_longer(-c(matched, ResponseId, Q1.3_1), names_to = "item", values_to = "score")


#drop questions and find mean scores
pre_long_subset <- pre_long %>%  
  filter(item != "Q3.1") %>%
  filter(item != "Q3.2") %>%
  group_by(ResponseId, Q1.3_1,matched) %>%
  summarise(mean_score_pre = mean(score)) %>%
  ungroup()

post_long_subset <- post_long %>%  
  filter(item != "Q3.1") %>%
  filter(item != "Q3.2")%>%
  group_by(ResponseId, Q1.3_1, matched) %>%
  summarise(mean_score_post = mean(score))%>%
  ungroup()

#also do on matched
matched_pre_long_subset <- matched_pre_long %>%  
  filter(item != "Q3.1") %>%
  filter(item != "Q3.2") %>%
  group_by(ResponseId, Q1.3_1, matched) %>%
  summarise(mean_score_pre = mean(score)) %>%
  ungroup()

matched_post_long_subset <- matched_post_long %>%  
  filter(item != "Q3.1") %>%
  filter(item != "Q3.2")%>%
  group_by(ResponseId, Q1.3_1, matched) %>%
  summarise(mean_score_post = mean(score))%>%
  ungroup()


#unpaired t test on unmatched data
t.test(pre_long_subset$mean_score_pre, post_long_subset$mean_score_post)


#prepare matched data for paired t.test. put all data into 1 df so pairing works properly.
all_matched_data_for_paired_t_test <- left_join(matched_pre_long_subset, matched_post_long_subset, by = "Q1.3_1") %>% ungroup() %>%
  select(Q1.3_1, mean_score_pre, mean_score_post) %>%
  ungroup()

t.test(all_matched_data_for_paired_t_test$mean_score_pre, 
       all_matched_data_for_paired_t_test$mean_score_post, paired = T)

sd(all_matched_data_for_paired_t_test$mean_score_pre)
sd(all_matched_data_for_paired_t_test$mean_score_post)

unmatched_for_t_test_pre <- filter(pre_long_subset, matched == "no")
unmatched_for_t_test_post <- filter(post_long_subset, matched == "no")

 

t.test(unmatched_for_t_test_pre$mean_score_pre, 
       unmatched_for_t_test_post$mean_score_post)

#unmatched data has no significant difference between pre and post