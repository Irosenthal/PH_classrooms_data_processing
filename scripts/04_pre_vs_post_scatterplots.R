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




#now calculate delta
pre_and_post <- pre_and_post %>%
  mutate(Q19_delta = Q19_post - Q19_pre)


         
#gain plot 
ggplot(data = pre_and_post, 
       mapping = aes(x = Q19_pre, y = Q19_delta))  +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2, ymax = Inf),
            fill = "grey") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 2),
            fill = "lightgrey") +
  geom_point(size = 10, alpha = 0.2) +
  theme_bw()


#how do we make the legend look good?
#hard when just using alpha
#calculate number of each group so that we can have a nice legend
pre_and_post <- pre_and_post %>%
  group_by(Q19_pre, Q19_delta) %>%
  mutate(my_alpha = n())
#use my_alpha for alpha in geom_point above
#not much contrast because most of them are the same. 

#is this supposed to be by item, or do I add up each participant's scores to get something like
#participant 1 prescore, which would be a sum of all presurvey responses?


#if its the above:
#columns of interest
item_cols_pre <- c("Q21_pre","Q22_pre", "Q23_pre", "Q24_pre", "Q25_pre", "Q27_pre", "Q30_pre","Q31_pre")

item_cols_post<- c("Q21_post","Q22_post", "Q23_post", "Q24_post", "Q25_post", "Q27_post", "Q30_post","Q31_post")

pretest_participant_totals <- pretest %>% 
  group_by(participant_id) %>%  # for each row
  nest(item_cols_pre) %>%                   # nest selected columns
  mutate(prescore = map_dbl(data, sum)) # calculate the sum of those columns



posttest_participant_totals <- posttest %>% 
  group_by(participant_id) %>%  # for each row
  nest(item_cols_post) %>%                   # nest selected columns
  mutate(postscore = map_dbl(data, sum)) 

pre_post_participant_scores <- left_join(pretest_participant_totals, posttest_participant_totals, by = "participant_id")

#calculate delta 
pre_post_participant_scores <- pre_post_participant_scores %>%
  mutate(delta = postscore - prescore)


#pre_post_participant_scores number of each group so that we can have a nice legend
pre_post_participant_scores <- pre_post_participant_scores %>%
  group_by(prescore, delta) %>%
  mutate(my_alpha = n())

pre_post_participant_scores$my_alpha <- as.double(pre_post_participant_scores$my_alpha)

ggplot(data = pre_post_participant_scores, 
       mapping = aes(x = prescore, y = delta))  +
  geom_point(size = 3, mapping = aes(fill = my_alpha, color = my_alpha), shape = 21, color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  theme_bw() +
  theme(legend.key.height = unit(2.4, "cm")) +
  geom_hline(yintercept = 0) +
  annotate("text", x = 2.5, y = 1.5, label = paste0("n =", length(which(pre_post_participant_scores$delta > 0)))) +
  annotate("text", x = 2.5, y = -2, label = paste0("n =", length(which(pre_post_participant_scores$delta < 0)))) +
  annotate("text", x = 2.5, y = -0.25, label = paste0("n =", length(which(pre_post_participant_scores$delta == 0))))
  
  

#pre vs post
ggplot(data = pre_post_participant_scores, 
       mapping = aes(x = prescore/8, y = postscore/8)) +
  geom_point(size = 3, mapping = aes(fill = my_alpha, color = my_alpha), shape = 21, color = "black") +
  scale_fill_gradient(low = "lightgrey", high = "black") + 
  geom_abline() +
  theme_bw() +
  theme(legend.key.height = unit(2.4, "cm"))
  

greater_than_0 <- pre_post_participant_scores %>%
  filter(delta < 0)

#geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = Inf),
#fill = "grey65") +
  #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10),
  #fill = "grey90") +
#making alpha into a bar instead of discrete points might be tricky, but a problem for a different day
#https://stackoverflow.com/questions/44168996/how-to-create-a-continuous-legend-color-bar-style-for-scale-alpha