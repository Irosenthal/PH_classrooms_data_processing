library(tidyverse)

pre_1_ec <- read_csv("./data/raw_data/Fall 2020 Pre-Test_ Planet Hunters (EC)_June 9, 2021_12.24.csv")[-c(1:2),] 

pre_1_no_ec <- read_csv("./data/raw_data/Fall 2020 Pre-Test_ Planet Hunters (Not EC)_June 9, 2021_12.23.csv")[-c(1:2),] 

post_1 <- read_csv("./data/raw_data/Fall 2020 Post-Test_ Planet Hunters_June 9, 2021_12.28.csv")[-c(1:2),]

pre_2 <- read_csv("./data/raw_data/Spring 2021 Pre-Test_ Planet Hunters_June 9, 2021_12.26.csv")[-c(1:2),]

post_2 <- read_csv("./data/raw_data/Spring 2021 Post-Test_ Planet Hunters_June 9, 2021_12.30.csv")[-c(1:2),]

#combine pre_1_ec and pre_1_no_ec, as ec was not tracked in the post survey

pre_1 <- rbind(pre_1_ec, pre_1_no_ec)

#add semester columns
pre_1$semester <- "1"
post_1$semester <- "1"
pre_2$semester <- "2"
post_2$semester <- "2"




#confirm column names are consistent between semesters
#these should evaluate as TRUE
identical(names(pre_1), names(pre_2))
identical(names(post_1), names(post_2))

#they are equal, safe to rbind

raw_pretest <- rbind(pre_1, pre_2)
raw_posttest <- rbind(post_1, post_2)

#make all IDs lowercase to standardize for joins/filters
raw_pretest$Q1.3_1_raw <- raw_pretest$Q1.3_1
raw_posttest$Q1.3_1_raw <- raw_posttest$Q1.3_1
raw_pretest$Q1.3_1 <- tolower(raw_pretest$Q1.3_1)
raw_posttest$Q1.3_1 <- tolower(raw_posttest$Q1.3_1)

#rename duraction column to be more clean
colnames(raw_pretest)[colnames(raw_pretest) == 'Duration (in seconds)'] <- 'duration'
colnames(raw_posttest)[colnames(raw_posttest) == 'Duration (in seconds)'] <- 'duration'
raw_pretest$duration <- as.numeric(raw_pretest$duration)
raw_posttest$duration <- as.numeric(raw_posttest$duration)





#keep only surveys of duration between 30s and 30m (1800s)
raw_pretest_valid <- raw_pretest %>%
  filter(duration >= 30 & duration <= 1800)

raw_posttest_valid <- raw_posttest %>%
  filter(duration >= 30 & duration <= 1800)

#complete surveys only

complete_pre_tests <- raw_pretest_valid %>%
  filter(!is.na(Q4.6)) 
#select(Q1.3_1, IPAddress, pre_post)

complete_post_tests <- raw_posttest_valid %>%
  filter(!is.na(Q4.6)) 

#save complete raw data
#write_csv(complete_pre_tests, "./data/output/pre_ph_compiled_raw.csv")
#write_csv(complete_post_tests, "./data/output/post_ph_compiled_raw.csv")

#to see all the missing responses
#spot check to make sure there's nothing systematic
NA_post_tests <- raw_posttest %>%
  filter(is.na(Q4.6)) 


complete_pre_tests_no_mars <- filter(complete_pre_tests, Q1.3_1 != "mar9991")
complete_post_tests_no_mars <- filter(complete_post_tests, Q1.3_1 != "mar9991")




#deal with people that completed survey twice by only keeping their most recent response
#uncomment the select if you need to trim the data for troubleshooting
complete_pre_tests_no_dupes <- complete_pre_tests_no_mars %>%
  group_by(Q1.3_1) %>%
  filter(EndDate == max(EndDate)) %>%
  #select(Q1.3_1, IPAddress, pre_post) %>%
  ungroup()

complete_post_tests_no_dupes <- complete_post_tests_no_mars %>%
  group_by(Q1.3_1) %>%
  filter(EndDate == max(EndDate)) %>%
  #select(Q1.3_1, IPAddress, pre_post) %>% 
  ungroup()




#NEW JANUARY 2022:
#Fall Guilford Tech did the PH lab in person
#Filter these responses out so that we only have virtual courses in the dataset
#make operator
`%!in%` = Negate(`%in%`)

#load look up tables

pre_lut <- read_csv("./data/output/pre_ph_school_lut.csv")%>%
  select(-Q1.3_1_raw, -Q4.1, -Q4.2)
post_lut <- read_csv("./data/output/post_ph_school_lut.csv") %>%
  select(-Q1.3_1_raw, -Q4.1, -Q4.2)

#fix excel nonsense
pre_lut$Q1.3_1 <- tolower(pre_lut$Q1.3_1)
post_lut$Q1.3_1 <- tolower(post_lut$Q1.3_1)


#drop duplicate IDs (we do this later on the actual data, just have to do it on the looktable here)
pre_lut <- pre_lut %>%
  distinct(Q1.3_1, .keep_all = TRUE)

post_lut <- post_lut %>%
  distinct(Q1.3_1, .keep_all = TRUE)
#make character for join
pre_lut$semester <- as.character(pre_lut$semester)
post_lut$semester <- as.character(post_lut$semester)


complete_pre_tests_no_dupes <- left_join(complete_pre_tests_no_dupes, pre_lut, by = c("Q1.3_1" = "Q1.3_1", "semester" = "semester"))

complete_post_tests_no_dupes <- left_join(complete_post_tests_no_dupes, post_lut, by = c("Q1.3_1" = "Q1.3_1", "semester" = "semester"))

#make sure only garbage is being dropped
test <- filter(complete_pre_tests_no_dupes, is.na(Institution)) %>%
  select(Q1.3_1, Q4.2,	Q4.1,	Institution)


#find all semester 1 gtcc students (pre)
GTCC_pre <- complete_pre_tests_no_dupes %>%
  filter(Institution == "GTCC" & semester == 1)

#and drop them
complete_pre_tests_no_gtcc <- complete_pre_tests_no_dupes %>%
  filter(Q1.3_1 %!in% GTCC_pre$Q1.3_1)

#also drop kentucky result
complete_pre_tests_no_gtcc_kc <- complete_pre_tests_no_gtcc %>%
  filter(Q1.3_1 != "jan3211")

#do the same for post tests

#find all semester 1 gtcc students (pre)
GTCC_post <- complete_post_tests_no_dupes %>%
  filter(institution == "GTCC" & semester == 1)

#and drop them
complete_post_tests_no_gtcc <- complete_post_tests_no_dupes %>%
  filter(Q1.3_1 %!in% GTCC_post$Q1.3_1)


#write out cleaned data before dropping any questions
write_csv(complete_pre_tests_no_gtcc, "./data/output/pre_ph_survey_nonmatched_with_nonlikert_for_analysis_all_questions.csv")
write_csv(complete_post_tests_no_gtcc, "./data/output/post_ph_survey_nonmatched_with_nonlikert_for_analysis_all_questions.csv")


#drop junk questions
#confirm these numbers didnt change
#note we change the name here to stop rest of script from breaking
complete_pre_tests_no_dupes <- complete_pre_tests_no_gtcc[,-c(1:3,5:8, 10:17)]
complete_post_tests_no_dupes <- complete_post_tests_no_gtcc[,-c(1:3, 5:8, 10:17)]


#write out now - the next step drop all non likert questions
#write_csv(complete_pre_tests_no_dupes, "./data/output/pre_ph_survey_nonmatched_with_nonlikert_no_gtcc.csv")
#write_csv(complete_post_tests_no_dupes, "./data/output/post_ph_survey_nonmatched_with_nonlikert_no_gtcc.csv")

#replace all character responses with numeric values

pre_vals <- complete_pre_tests_no_dupes %>% 
  mutate_at(
    vars(-ResponseId, -Q1.3_1, -Q1.3_1_raw, -IPAddress),
    funs(case_when(
      . == "Strongly agree"  ~ 7,
      . == "Agree" ~ 6,
      . == "Somewhat agree" ~ 5,
      . == "Neither agree nor disagree" ~ 4,
      . == "Somewhat disagree" ~ 3,
      . == "Disagree" ~ 2,
      . == "Strongly disagree"~ 1)))


post_vals <- complete_post_tests_no_dupes %>% 
  mutate_at(
    vars(-ResponseId, -Q1.3_1, -Q1.3_1_raw, -IPAddress),
    funs(case_when(
      . == "Strongly agree"  ~ 7,
      . == "Agree" ~ 6,
      . == "Somewhat agree" ~ 5,
      . == "Neither agree nor disagree" ~ 4,
      . == "Somewhat disagree" ~ 3,
      . == "Disagree" ~ 2,
      . == "Strongly disagree"~ 1)))




#these drop all columns with NA. in this case, any non-likert column was NA, so it is dropped here. 
post_vals <- post_vals %>%
  select_if(~ !any(is.na(.)))

pre_vals <- pre_vals %>%
  select_if(~ !any(is.na(.)))



#save these - they are all complete and unique responses. they are NOT  matched up yet.
write_csv(pre_vals, "./data/output/pre_ph_survey_nonmatched_for_analysis.csv")
write_csv(post_vals, "./data/output/post_ph_survey_nonmatched_for_analysis.csv")

#This is all the matched pairs except for mar9991. At this point, individual duplicates have been resolved. 
joined_surveys <- semi_join(pre_vals,
                             post_vals, 
                             by = "Q1.3_1") %>% 
  ungroup()

####
#for troubleshooting
post_surveys_with_no_match <- anti_join(complete_post_tests_no_dupes,
                                        complete_pre_tests_no_dupes, 
                                        by = "Q1.3_1") %>% 
  ungroup()

pre_surveys_with_no_match <- anti_join(complete_pre_tests_no_dupes,
                                       complete_post_tests_no_dupes, 
                                       by = "Q1.3_1") %>% 
  ungroup()

write_csv(post_surveys_with_no_match, "./data/output/ph_post_bad_matches.csv")
write_csv(pre_surveys_with_no_match, "./data/output/ph_pre_bad_matches.csv")



####

#write out complete matched datasets
pre_tests_with_matches <- filter(pre_vals, Q1.3_1 %in% joined_surveys$Q1.3_1)
post_tests_with_matches <- filter(post_vals, Q1.3_1 %in% joined_surveys$Q1.3_1)

write_csv(pre_tests_with_matches, "./data/output/pre_ph_survey_matched_for_analysis.csv")
write_csv(post_tests_with_matches, "./data/output/post_ph_survey_matched_for_analysis.csv")

