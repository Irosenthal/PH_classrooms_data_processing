#Final data wrangling for PH analysis
#code to create pre/pst scatterplots for each item
library(broom)
library(tidyverse)

#load matched data 
pretest <- read_csv("./data/output/pre_ph_survey_matched_jan_2022_rerun.csv")
posttest <- read_csv("./data/output/post_ph_survey_matched_jan_2022_rerun.csv")


#load question metadata
question_LUT <- read_csv("./data/raw_data/ph_question_item_numbers.csv")[c(1:5)]


#drop responseID, raw participant ID,  and IP address
#also drop Q3.1 and  Q3.2: these are baseline questions that we don't expect to change
#^now that we are not doing a grouped analysis, disregard the above and include these questions

pretest <- select(pretest, 
                  -ResponseId, 
                  -IPAddress, 
                  -Q1.3_1_raw)
posttest <- select(posttest, 
                   -ResponseId, 
                   -IPAddress, 
                   -Q1.3_1_raw)

#remove post only questions (may need to do this in a different place)

posttest <- select(posttest,-c("Q5.5",  "Q5.6"))



pre_names<- c("participant_id","Q19_pre","Q20_pre","Q21_pre","Q22_pre", "Q23_pre", "Q24_pre", "Q25_pre", "Q27_pre", "Q30_pre","Q31_pre")

post_names<- c("participant_id", "Q19_post","Q20_post","Q21_post","Q22_post", "Q23_post", "Q24_post", "Q25_post", "Q27_post", "Q30_post","Q31_post")

names(pretest) <- pre_names
names(posttest) <- post_names

#join pre and post data

pre_and_post <- left_join(pretest, posttest, by = "participant_id")

#put columns in a more useful order
pre_and_post <- pre_and_post %>%
  select("participant_id", sort(colnames(.)))

#Report an overall average, average pre score for the whole assessment 
#: Q1 total +..... Qn total /10 = Pre AVE.   and do the same for post.  And calculate a Post#-pre.  And see if it's sig..  
#This is a 2 sample t test on pre vs post


#calculate total score for each question by summing columns

total_scores <- as.data.frame(colSums(pre_and_post[,-1])) %>%
  rownames_to_column()

score_col_names <- c("item", "total_item_score")
names(total_scores) <- score_col_names

total_scores <- total_scores %>%
  separate(col = item, into = c("item", "survey"))

#pivot into a col for pres and a col for posts

total_scores_wide <- total_scores %>%
  pivot_wider(names_from = survey, values_from = total_item_score)

#and do a t test
t.test(total_scores_wide$pre, total_scores_wide$post)


#For each question, for folks who have matched pairs, calculate difference score for each #person, (delta), report mean of delta, sd of delta, and p-value of t-test (maybe non #parametric for this) for a difference from 0 (1 sample). Delta of each person, averaged by #question

#calculate each person's delta
#average by question
#1 sample t test on delta from 0


#now calculate delta
pre_and_post <- pre_and_post %>%
  mutate(Q19_delta = Q19_post - Q19_pre,
         Q20_delta = Q20_post - Q20_pre,
         Q21_delta = Q21_post - Q21_pre,
         Q22_delta = Q22_post - Q22_pre,
         Q23_delta = Q23_post - Q23_pre,
         Q24_delta = Q24_post - Q24_pre,
         Q25_delta = Q25_post - Q25_pre,
         Q27_delta = Q27_post - Q27_pre,
         Q30_delta = Q30_post - Q30_pre,
         Q31_delta = Q31_post - Q31_pre)

#select just delta columns
pre_and_post_deltas <- pre_and_post %>%
  select(participant_id,
         Q19_delta,
         Q20_delta,
         Q21_delta,
         Q22_delta,
         Q23_delta,
         Q24_delta,
         Q25_delta,
         Q27_delta,
         Q30_delta,
         Q31_delta)





#t tests on each question 
ph_t_test <- function(data_frame){
  
Q19 <- wilcox_test(data_frame, Q19_delta ~ 1, mu = 0)
Q20 <- wilcox_test(data_frame, Q20_delta ~ 1, mu = 0)
Q21 <- wilcox_test(data_frame, Q21_delta ~ 1, mu = 0)
Q22 <- wilcox_test(data_frame, Q22_delta ~ 1, mu = 0)
Q23 <- wilcox_test(data_frame, Q23_delta ~ 1, mu = 0)
Q24 <- wilcox_test(data_frame, Q24_delta ~ 1, mu = 0)
Q25 <- wilcox_test(data_frame, Q25_delta ~ 1, mu = 0)
Q27 <- wilcox_test(data_frame, Q27_delta ~ 1, mu = 0)
Q30 <- wilcox_test(data_frame, Q30_delta ~ 1, mu = 0)
Q31 <- wilcox_test(data_frame, Q31_delta ~ 1, mu = 0)

t_tests <- rbind(Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q27, Q30, Q31)
return(t_tests)
}

ph_t_tests <- ph_t_test(pre_and_post_deltas)

#write out results
write_csv(ph_t_tests, "./data/output/analyses/difference_score_t_tests_jan_2022_rerun.csv")


#now get col means and sds

#means
delta_means <- pre_and_post_deltas %>% 
  summarise(across(where(is.numeric),mean))

delta_means <- delta_means %>%  
  pivot_longer(cols = starts_with("Q"), names_to = "item", values_to = "mean")

#standard dev

delta_sd <- pre_and_post_deltas %>% 
  summarise(across(where(is.numeric),sd))

delta_sd <- delta_sd %>%  
  pivot_longer(cols = starts_with("Q"), names_to = "item", values_to = "sd")

#join t tests, means, sds together for a clean summary table

delta_summary_table <- left_join(ph_t_tests, delta_means, by = c(".y." = "item"))
delta_summary_table <- left_join(delta_summary_table, delta_sd, by = c(".y." = "item"))


#Calculate a total for each person, divide by 10 to put on same scale. Report mean(sd) of #pre-, post- and post–pre of this total, and p-value of t-test for difference from 0.
#This is a 1 sample t test on delta


#add up all pre scores for each person
#easier to go back to base pre and post data

#pivot it so that there's one column for participant id, 1 column for question id, and one for score

pretest_long <- pretest %>%
  pivot_longer(-participant_id, names_to = "item", values_to = "score")

#summarize some stuff

pretest_summary <- pretest_long %>%
  group_by(participant_id) %>%
  summarise(pre_sum = sum(score),
            pre_mean = mean(score),
            pre_sd = sd(score))

#now do it for post tests
posttest_long <- posttest %>%
  pivot_longer(-participant_id, names_to = "item", values_to = "score")

posttest_summary <- posttest_long %>%
  group_by(participant_id) %>%
  summarise(post_sum = sum(score),
            post_mean = mean(score),
            post_sd = sd(score))

#join together
participant_summary_stats <- left_join(pretest_summary, posttest_summary) %>%
  mutate(mean_delta = post_mean - pre_mean)


#write out results
write_csv(participant_summary_stats, "./data/output/analyses/participant_summary_stats_t_tests_jan_2022_rerun.csv")


#1 sample t-test on mean_delta

t_test(participant_summary_stats, mean_delta ~ 1, mu = 0)


#misc wrangling
#flip rows and cols, we want to average everyone's deltas for each question
pre_and_post_deltas_wide <- pre_and_post_deltas %>% 
  rownames_to_column() %>% 
  select(-participant_id) %>%
  pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>% 
  pivot_wider(names_from = "rowname", values_from = "col2")

