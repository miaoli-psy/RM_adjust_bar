# ---------------libraries------------------------
library(dplyr)
library(tidyverse)

# ---------------read data------------------------
# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")

my_data <- readxl::read_excel(file.choose()) # data_clean.xlsx

# RM summary
RM_summary <- my_data %>%
  group_by(RM) %>%
  dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
  dplyr::mutate(percentage_RM = (count / sum(count)) * 100)

# add spacing condition
my_data <- my_data %>%
  dplyr::mutate(spacing = case_when(
    correct_space <= 0.6 ~ "small",
    correct_space > 0.6 & correct_space <= 0.8 ~ "middle",
    correct_space > 0.8 ~ "large",
    TRUE ~ NA_character_
  ))

# spacing summary
spacing_summary <- my_data %>%
  group_by(spacing) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::mutate(percentage_spacing = (count / sum(count)) * 100)


# arrange data
data_by_pp <- my_data %>%
  group_by(correct_num, correct_width, subID, spacing) %>%
  dplyr::summarise(
    number_deviation_mean = mean(number_deviation),
    width_deviation_mean = mean(width_deviation),
    number_deviation_sd = sd(number_deviation),
    width_deviation_sd = sd(width_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_by_pp$correct_width <- as.factor(data_by_pp$correct_width)
data_by_pp$spacing <- as.factor(data_by_pp$spacing)
#levels(data_by_pp$correct_width)

data_by_pp$subID <- factor(data_by_pp$subID, levels = c("S1","S2","S3","S4","S5",
                                                        "S6","S7","S8","S9","S10",
                                                        "S11","S12","S13","S14",
                                                        "S15","S16","S17","S18",
                                                        "S19","S20"))
str(data_by_pp)

# -------------LMM num ds------------
hist(data_by_pp$number_deviation_mean, main="Histogram of deviation", xlab="deviation", breaks=30)
qqnorm(data_by_pp$number_deviation_mean)
qqline(data_by_pp$number_deviation_mean, col = "red")


model.glmm <-glmmTMB::glmmTMB(number_deviation_mean ~ poly(correct_num,2)
                                + (1 + correct_num|subID) + (1|correct_width) + (1|spacing),
                               data = data_by_pp)

summary(model.glmm)

# type2 Wald chisquare tests
car::Anova(model.glmm3, type = 2)

pred <- ggeffects::ggpredict(model.glmm2, terms = 
                               c("correct_num [all]", "spacing", "correct_width"), 
                             type="fe")  


# model details
sjPlot::tab_model(
  model.glmm2,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

# pairwise comparisons
emms <- emmeans::emmeans(model.glmm2,
                         list(pairwise ~ spacing + correct_width | poly(correct_num, 2)),
                         adjust = "tukey")

summary(emms, infer = TRUE)


# keep only RM and correct
my_data <- my_data %>% 
  filter(RM %in% c('RM', "no RM"))
