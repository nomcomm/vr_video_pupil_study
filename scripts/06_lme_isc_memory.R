library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lmerTest)
library(lme4)
library(emmeans)
library(readr)
library(performance)

#set directory and load file
setwd("~/Documents/01_GITHUB/nomcomm/GITHUB_PAPERS_WORKING/vr_video_pupil_study_rerun/scripts")
df <- read_csv('/Users/schmaelz/Documents/01_GITHUB/nomcomm/GITHUB_PAPERS_WORKING/vr_video_pupil_study_rerun/data/03_spots_memory_data/all_subs_spots_conditions_memory_isc_reproduce.csv')

colnames(df)
df$subject <- factor(df$subject)
df$condition <- factor(df$condition)
df$spot <- factor(df$spot)
df$condition <- relevel(df$condition, ref = "50nd")

#recall
recall_model3 <- 
  glmer(recall ~ isc_ind2rest*condition + (1 | subject) + (1 | spot), df, family=binomial)
summary(recall_model3)
Anova(recall_model3, type="3")
model_performance(recall_model3)

recall_predict2 <- 
  emmeans(recall_model3, ~ isc_ind2rest*condition, at = list(isc_ind2rest = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)))
recall_predict2_res <- summary(recall_predict2, type = "response")
recall_predict2_df <- as.data.frame(recall_predict2_res)

ggplot(recall_predict2_df, aes(x = isc_ind2rest, y = prob, color = condition)) +
  geom_line(size = 1.25) + 
  labs(x = "ISC", y = "Predicted Recall", color = "Condition") +
  theme_bw() + 
  scale_colour_manual(values = c("100" = "black", "50nd" = "darkgrey", "50wd" = "lightgrey" )) +
  ylim(0,1) + 
  xlim(0,1) +
  theme(
    # Increasing font size for different text elements
    text = element_text(size = 20),  # Change font size for all text
    axis.title = element_text(size = 20),  # Change axis label font size
    axis.text = element_text(size = 20),  # Change axis tick label font size
    plot.title = element_text(size = 20, face = "bold"),  # Change plot title font size and style
    legend.text = element_text(size = 20)  # Change legend text font size
  )


# recognition

recog_model3 <- 
  glmer(recognition ~ isc_ind2rest*condition + (1 | subject) + (1 | spot), df, family=binomial)
summary(recog_model3)
Anova(recog_model3, type="3")
model_performance(recog_model3)

recog_predict2 <- 
  emmeans(recog_model3, ~ isc_ind2rest*condition, at = list(isc_ind2rest = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)))
recog_predict2_res <- summary(recog_predict2, type = "response")
recog_predict2_df <- as.data.frame(recog_predict2_res)

ggplot(recog_predict2_df, aes(x = isc_ind2rest, y = prob, color = condition)) +
  geom_line(size = 1.25) + 
  labs(x = "ISC", y = "Predicted Recognition", color = "Condition") +
  theme_bw() + 
  scale_colour_manual(values = c("100" = "black", "50nd" = "darkgrey", "50wd" = "lightgrey" )) +
  ylim(0,1) + 
  xlim(0,1) +
  theme(
    # Increasing font size for different text elements
    text = element_text(size = 20),  # Change font size for all text
    axis.title = element_text(size = 20),  # Change axis label font size
    axis.text = element_text(size = 20),  # Change axis tick label font size
    plot.title = element_text(size = 20, face = "bold"),  # Change plot title font size and style
    legend.text = element_text(size = 20)  # Change legend text font size
  )
