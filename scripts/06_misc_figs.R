#misc figures for PH data
library(tidyverse)

#histograms for post only questions - use nonmatched data
#load post data
#posttest <- read_csv("./data/output#/post_ph_survey_nonmatched_nonlikert.csv") %>%
 # select(-IPAddress , -ResponseId, -Q1.3_1_raw)

#posttest <- read_csv("./data/output#/post_ph_survey_nonmatched_with_nonlikert_no_gtcc.csv") %>%
#  select(-IPAddress , -ResponseId, -Q1.3_1_raw)

posttest <- read_csv("./data/output/post_ph_survey_nonmatched_with_nonlikert_for_analysis_all_questions.csv") %>%
  select(-IPAddress , -ResponseId, -Q1.3_1_raw)


#load question metadata just to eyeball the list
question_LUT <- read_csv("./data/raw_data/ph_question_item_numbers.csv")[c(1:5)]

#Q5.1: This activity made me more likely to classify on Planet Hunters again.
ggplot(data = posttest, mapping = aes(x = Q5.1)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#Q5.3: This activity made me more likely to go to the Zooniverse website and explore other citizen science projects I can contribute to on my own.	
ggplot(data = posttest, mapping = aes(x = Q5.3)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#Q5.5  This activity improved my ability to understand how data and evidence are used to inform scientific conclusions.	
#reorder response factor
posttest$Q5.5 <- factor(posttest$Q5.5, 
                        levels = c("Strongly disagree",
                                   "Disagree",
                                   "Somewhat disagree",
                                   "Neither agree nor disagree",
                                   "Somewhat agree",
                                   "Agree",
                                   "Strongly agree"))


ggplot(data = posttest, mapping = aes(x = Q5.5)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 35, hjust=1))

#Q5.6  I would look forward to doing another citizen science-based activity again in my class.	
posttest$Q5.6 <- factor(posttest$Q5.6, 
                        levels = c("Strongly disagree",
                                   "Disagree",
                                   "Somewhat disagree",
                                   "Neither agree nor disagree",
                                   "Somewhat agree",
                                   "Agree",
                                   "Strongly agree"))




ggplot(data = posttest, mapping = aes(x = Q5.6)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 35, hjust=1)) 


#3.10  Do you know what citsci is?

#take unmatched
prettest_unmatched <- read_csv("./data/output/pre_ph_survey_nonmatched_nonlikert.csv") %>%
  select(-IPAddress , -ResponseId, -Q1.3_1_raw)

#and matched
pretest_matched <- read_csv("./data/output/pre_ph_survey_matched_duration_filter.csv")

#%in% from matched on particpant id
pretest_matched_nonlikert <- prettest_unmatched %>%
  filter(Q1.3_1 %in% pretest_matched$Q1.3_1)



ggplot(data = pretest_matched_nonlikert, mapping = aes(x = Q3.10)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_blank()) 



#make 1 plot with 4 panels ####
#select columns we want
posttest_subset <- posttest2 %>%
  select(Q1.3_1, Q5.1, Q5.3, Q5.5, Q5.6)

#pivot long
posttest_long_subset <- posttest_subset %>%
  pivot_longer(-c(Q1.3_1), names_to = "item", values_to = "score")

#words to numbers
posttest_long_subset <- posttest_long_subset %>%
  mutate(response_val = case_when(
    score == "Strongly agree"  ~ 7,
    score == "Agree" ~ 6,
    score == "Somewhat agree" ~ 5,
    score == "Neither agree nor disagree" ~ 4,
    score == "Somewhat disagree" ~ 3,
    score == "Disagree" ~ 2,
    score == "Strongly disagree"~ 1,
    score == "Yes" ~ 8,
    score == "No" ~ 9))

posttest_long_subset$response_val <- as.character(posttest_long_subset$response_val)

posttest_long_subset <- posttest_long_subset %>%
  mutate(response_val = str_replace(response_val, "8", "Yes")) %>%
  mutate(response_val = str_replace(response_val, "9", "No"))
  


#join on question text to simplify facet titles
item_text <- select(question_LUT, name, value)

posttest_long_subset <- left_join(posttest_long_subset, item_text, by = c("item" = "name"))

#and add linebreaks
swr = function(string, nwrap=50) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

#function to control y axis - lets us have different scales
breaks_fun_y <- function(x) {
  if (max(x) < 300) {
    seq(0, 250, 50)
  } else {
    seq(0, 400, 100)
  }
}

ylim_fun <- function(x) {
  if (max(x) < 300) {
    c(0, 250)
  } else {
    c(0, 410)
  }
}

breaks_fun_x <- function(x) {
  if (max(x) == 7) {
    seq(1, 7, 1)
  } else {
    seq(8, 9, 1)
  }
}

#and add linebreaks
swr = function(string, nwrap=53) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)
#apply it to our data
posttest_long_subset$value <- swr(posttest_long_subset$value)



#plot


#or plot with all text labels on top of bars
sig_plots <- ggplot(data = posttest_long_subset) + 
  geom_bar(mapping = aes(x = response_val)) +
  xlab("Response") +
  ylab("Response Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = 3),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        strip.background = element_rect(fill="gray87"),
        strip.text.x = element_text(size = 12),
        plot.margin = unit(c(.25, .1, .25, 1), "cm")) +
  facet_wrap(~value, scales = "free") +
  geom_text(aes(x = response_val, label = ..count..), size = 5, stat = "count", position = "dodge", vjust = -.6) +
  scale_y_continuous(breaks = breaks_fun_y, limits = ylim_fun)

sig_plots
ggsave("post_histograms_final.png", dpi = 600)



test <- filter(posttest_long_subset, response_val %in% c("1", "2", "3", "4", "5", "6", "7"))
  
