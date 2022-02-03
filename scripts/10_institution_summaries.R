#institution summaries

#load look up tables

pre_lut <- read_csv("./data/output/pre_ph_school_lut.csv")[,1:5]
post_lut <- read_csv("./data/output/post_ph_school_lut.csv")[,1:6]

#load nonmatched and matched datasets

pretest_matched <- read_csv("./data/output/pre_ph_survey_matched_jan_2022_rerun.csv") %>%
  select(-IPAddress)
posttest_matched <- read_csv("./data/output/post_ph_survey_matched_jan_2022_rerun.csv") %>%
  select(-IPAddress)

pretest_nonmatched <- read_csv("./data/output/pre_ph_survey_nonmatched_jan_2022_rerun.csv") %>%
  select(-IPAddress)
posttest_nonmatched <- read_csv("./data/output/post_ph_survey_nonmatched_jan_2022_rerun.csv") %>%
  select(-IPAddress)



#drop all responses on nonmatched datasets that correspond to a a participant with a matched response

matched_participant_ids <- pretest_matched$Q1.3_1

unmatchable_pretests <- pretest_nonmatched %>%
  filter(Q1.3_1 %!in% matched_participant_ids)

unmatchable_posttests <- posttest_nonmatched %>%
  filter(Q1.3_1 %!in% matched_participant_ids)


#to fix the errors in username added by ms excel we need to cbind the original user ids to the school luts
pre_big <- read_csv("./data/output/pre_ph_survey_nonmatched_with_nonlikert_no_gtcc_raw.csv")$Q1.3_1
post_big <- read_csv("./data/output/post_ph_survey_nonmatched_with_nonlikert_no_gtcc_raw.csv")$Q1.3_1


pre_lut <- pre_lut %>%
  mutate(fixed_ids = pre_big) %>%
  select(-Q1.3_1, -Q1.3_1_raw, -Q4.1, -Q4.2)

post_lut <- post_lut %>%
  mutate(fixed_ids = post_big) %>%
  select(-semester, -Q1.3_1, -Q1.3_1_raw, -Q4.1, -Q4.2)


#now subset school luts based on matched and unmatched responses
unmatched_schools_pre <- unmatchable_pretests %>%
  left_join(pre_lut, by = c("Q1.3_1" = "fixed_ids")) %>%
  select(Q1.3_1, institution)

unmatched_schools_post <- unmatchable_posttests %>%
  left_join(post_lut, by = c("Q1.3_1" = "fixed_ids")) %>%
  select(Q1.3_1, institution)

matched_schools_pre <- pretest_matched %>%
  left_join(pre_lut, by = c("Q1.3_1" = "fixed_ids")) %>%
  select(Q1.3_1, institution)

matched_schools_post <- posttest_matched %>%
  left_join(post_lut, by = c("Q1.3_1" = "fixed_ids")) %>%
  select(Q1.3_1, institution)

#now summarise
unmatched_schools_pre_summary <- unmatched_schools_pre %>%
  group_by(institution) %>%
  summarise(total = n())

unmatched_schools_post_summary <- unmatched_schools_post %>%
  group_by(institution) %>%
  summarise(total = n())

matched_schools_summary <- matched_schools_pre %>%
  group_by(institution) %>%
  summarise(total = n())

#total pre
total_pre_summary <- rbind(unmatched_schools_pre_summary, matched_schools_summary) %>%
  group_by(institution) %>%
  summarise(total = sum(total))

#sanity check: there should be 986 total responses in here
sum(total_pre_summary$total)

#total post
total_post_summary <- rbind(unmatched_schools_post_summary, matched_schools_summary) %>%
  group_by(institution) %>%
  summarise(total = sum(total))

#sanity check: there should be 500 total responses in here
sum(total_post_summary$total)


#check gtcc fall sem1 results

pretest_nonmatched <- read_csv("./data/output/pre_ph_survey_nonmatched_with_nonlikert_no_gtcc.csv")

posttest_nonmatched <- read_csv("./data/output/post_ph_survey_nonmatched_with_nonlikert_no_gtcc.csv")


nonmatched_schools_pre <- pretest_nonmatched %>%
  left_join(pre_lut, by = c("Q1.3_1" = "fixed_ids")) %>%
  select(Q1.3_1, semester, institution, Q4.2)

nonmatched_schools_post <- posttest_nonmatched %>%
  left_join(post_lut, by = c("Q1.3_1" = "fixed_ids")) %>%
  select(Q1.3_1, semester, institution, Q4.2)
