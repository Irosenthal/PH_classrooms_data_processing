#code to make comprehensive histogram
library(tidyverse)


#Histograms by item ####
#x = response value
#y = frequency of response value

#load matched data 
pretest <- read_csv("./data/output/pre_ph_survey_matched_duration_filter.csv")
posttest <- read_csv("./data/output/post_ph_survey_matched_duration_filter.csv")


#load question metadata
question_LUT <- read_csv("./data/raw_data/ph_question_item_numbers.csv")[c(1:5)]


#drop responseID, raw participant ID,  and IP address
#also drop Q3.1 and  Q3.2: these are baseline questions that we don't expect to change

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

#pivot long
pretest_long <- pretest %>%
  mutate(pre_post = "pre") %>%
  pivot_longer(-c(Q1.3_1, pre_post), names_to = "item", values_to = "score")

posttest_long <- posttest %>%
  mutate(pre_post = "post") %>%
  pivot_longer(-c(Q1.3_1, pre_post), names_to = "item", values_to = "score")

#rbind data together
pre_post_long <- rbind(pretest_long, posttest_long)

#fix factor order
pre_post_long$pre_post <- factor(pre_post_long$pre_post, 
                                  levels = c("pre",
                                             "post"))

#adjust factor names for cleaner plot
pre_post_long <- pre_post_long %>% 
  mutate(pre_post_clean = case_when(pre_post == "pre" ~ "Pre-Survey",
                                    pre_post == "post"  ~ "Post-Survey"))

pre_post_long$pre_post_clean <- factor(pre_post_long$pre_post_clean, 
                                       levels = c("Pre-Survey",
                                                  "Post-Survey"))


#test for plot function
test_df <- pre_post_long %>%
  filter(item == "Q3.9")


ggplot(data = test_df, mapping = aes(x = score, fill = pre_post)) +
  geom_bar(position = "dodge")
  


likert_plot <- function(data, survey_item){
  #subset data
  data <- filter(data, item == survey_item)
  #grab item text from metadata
  title_text <- question_LUT %>%
    filter(name ==  survey_item) %>%
    select(value)
  
  clean_item_number <-question_LUT %>%
  filter(name ==  survey_item) %>%
    select(Item)
  
  ggplot(data = data) + 
    geom_bar(mapping = aes(x = as.factor(score), 
                           fill = pre_post_clean), 
             position = "dodge") +
    theme(plot.title = element_text(size = 12))+
    xlab("Response") +
    scale_fill_manual(name = "Pre or Post", values = c("darkgrey", "black"))  +
    labs(title = str_wrap(paste0(clean_item_number, ".) ", title_text), 90)) +
    scale_x_discrete(breaks = c(1:7)) +
    theme_bw()
    
}

#make them 
#question list:
#Q3.1, Q3.2, Q3.3,  Q3.4,  Q3.5,  Q3.6,  Q3.7,  Q3.9,  Q3.12, Q3.13

likert_plot(pre_post_long, "Q3.1")

p1 <- likert_plot(pre_post_long, "Q3.4")
p2 <- likert_plot(pre_post_long, "Q3.9") #significant positive 
p3 <- likert_plot(pre_post_long, "Q3.12")#significant positive
p4 <- likert_plot(pre_post_long, "Q3.13")

#make 4 panel figure of all significant results
#filter to just the items we want to plot
items_of_interest <- c("Q3.4", "Q3.9", "Q3.12", "Q3.13")

pre_post_plot_subset <- pre_post_long %>%
  filter(item %in% items_of_interest)

#join on question text to simplify facet titles
item_text <- select(question_LUT, name, value)

pre_post_plot_subset <- left_join(pre_post_plot_subset, item_text, by = c("item" = "name"))

#and add linebreaks
swr = function(string, nwrap=54) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

#function to control y axis
breaks_fun <- function(x) {
  if (max(x) > 150) {
    seq(0, 150, 50)
  } else {
    seq(0, 100, 25)
  }
}

#and add linebreaks
swr = function(string, nwrap=50) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

pre_post_plot_subset$value <- swr(pre_post_plot_subset$value)

sig_plots <- ggplot(data = pre_post_plot_subset) + 
  geom_bar(mapping = aes(x = as.factor(score), 
                         fill = pre_post_clean), 
           position = "dodge") +
  xlab("Response") +
  ylab("Response Frequency") +
  scale_fill_manual(name = "Survey", values = c("darkgrey", "black"))  +
  #labs(title = str_wrap(paste(title_text), 90)) +
  #ggtitle(str_wrap(paste(title_text), 42)) +
  scale_x_discrete(breaks = c(1:7)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(vjust = 3),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.background = element_rect(fill="gray87"),
        strip.text.x = element_text(size = 11.5),
        plot.tag.position = c(.901, .44),
        plot.tag = element_text(size = 11),
        plot.margin = unit(c(.25, .1, .25, 1), "cm")) +
  facet_wrap(~value, scales = "free") + #toggle fixed vs free 
  scale_y_continuous(breaks = breaks_fun, limits = c(0, 175)) +
  labs(tag = "N = 347")
  
  #annotate("text", x = Inf, y = 2.9, label = , size = 30)
  
sig_plots 


ggsave("pre_post_histograms.png", dpi = 600)


#make a listcol of the tables I want
summaries <- read_csv("./data/output/analyses/summary_stats_paired.csv")

items_of_interest <- c(22, 27, 30, 31)

data_for_insets <- summaries %>%
  filter(Item %in% items_of_interest) %>%
  select(value, pre_post_difference ,  p)


dat_text <- data.frame(
  label = c("Overall Pre/Post Difference: 0.360",
            "Overall Pre/Post Difference: 0.222",
            "Overall Pre/Post Difference: 0.182",
            "Overall Pre/Post Difference: 0.110"),
  value   = c(pre_post_plot_subset$value[1],
              pre_post_plot_subset$value[2],
              pre_post_plot_subset$value[3],
              pre_post_plot_subset$value[4])
)

Time <- c(, "p")

Value_A <- 10:12
Value_B <- 1:3

tableA <- data.frame(Time, Value_A)
tableB <- data.frame(Time, Value_B)

d <- tibble(x = c(0.95, 0.95), y = c(0.95, 0.95),
            name = c("A", "B"), tb = list(tableA, tableB))

#overall pre vs post ####


#x axis: pre vs post, grouped by item
#y axis, average score per item

summaries <- read_csv("./data/output/analyses/summary_stats_paired.csv")

summaries_long <- summaries %>%
  select(Item , mean_response_pre,mean_response_post) %>%
  pivot_longer(-Item, names_to = "metric", values_to = "score")

#fix pre/post labels
summaries_long <- summaries_long %>%
  mutate(pre_post = case_when(metric == "mean_response_pre" ~ "pre",
                              metric == "mean_response_post" ~ "post")) %>%
  select(-metric)

#fix factor order
summaries_long$pre_post <- factor(summaries_long$pre_post, 
                                  levels = c("pre",
                                             "post"))

#plot
ggplot(data = summaries_long, mapping = aes(x = as.factor(Item), y = score, fill = pre_post)) +
  geom_col(position = "dodge")

