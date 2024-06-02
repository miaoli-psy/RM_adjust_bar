# ---------------libraries------------------------
library(dplyr)
library(tidyverse)

# ---------------read data------------------------
# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")

my_data <- readxl::read_excel(file.choose()) # data_clean.xlsx


# add spacing condition
my_data <- my_data %>%
  dplyr::mutate(spacing = case_when(
    correct_space <= 0.6 ~ "small",
    correct_space > 0.6 & correct_space <= 0.8 ~ "middle",
    correct_space > 0.8 ~ "large",
    TRUE ~ NA_character_
  ))


my_data$correct_width <- factor(my_data$correct_width, levels = c("0.1", "0.25", "0.4"))
my_data$spacing <- factor(my_data$spacing, levels = c("small", "middle", "large"))
my_data$correct_num <- factor(my_data$correct_num, levels = c("3", "4", "5"))


my_data$subID <- factor(my_data$subID, levels = c("S1","S2","S3","S4","S5",
                                                        "S6","S7","S8","S9","S10",
                                                        "S11","S12","S13","S14",
                                                        "S15","S16","S17","S18",
                                                        "S19","S20"))
str(my_data)


# ------------------number deviation------------------------
nd.model.lmm.full <- lme4::lmer(
  number_deviation ~ correct_num + correct_width + spacing + (1| subID),
  data = my_data
)

nd.model.lmm.reduce <- lme4::lmer(
  number_deviation ~  correct_width + spacing + (1| subID),
  data = my_data
)

anova(nd.model.lmm.full, nd.model.lmm.reduce)

summary(nd.model.lmm.full)

# evaluating the significance of fixed effects in the model
car::Anova(nd.model.lmm.full, type = 2)

# normality check of residuals
qqnorm(resid(nd.model.lmm.full))
qqline(resid(nd.model.lmm.full), col = "red")

# estimate marginal means, test if significantly different from zero
emmeans_results <- emmeans::emmeans(nd.model.lmm.full, ~ correct_width * spacing|correct_num)
contrast_results <- emmeans::test(emmeans_results, null = 0)
summary(contrast_results, infer = c(TRUE, TRUE))


pred <- ggeffects::ggpredict(nd.model.lmm.full, terms = 
                               c("correct_num [all]", "spacing", "correct_width"), 
                             type="fe")  


# model details
sjPlot::tab_model(
  nd.model.lmm.full,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)



# ------------------width deviation------------------------
check_num_dv <- my_data %>%
  group_by(number_deviation) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::mutate(percentage_num_dv = (count / sum(count)) * 100)
  

filtered_data <- subset(my_data, number_deviation %in% c(-1, 0)) #89.1% of trials

filtered_data$number_deviation <- factor(filtered_data$number_deviation, levels = c(-1, 0))

# -1 is coded as -0.5 and 0 as 0.5.
contrasts(filtered_data$number_deviation) <- matrix(c(-0.5, 0.5), ncol = 1)

wd.model.lmm.full <- lme4::lmer(
  width_deviation ~ correct_num + correct_width + spacing + number_deviation + (1| subID),
  data = filtered_data
)

summary(wd.model.lmm.full)

# interaction?

wd.model.lmm.interaction <- lme4::lmer(
  width_deviation ~ correct_num + correct_width * number_deviation + spacing * number_deviation + (1 | subID),
  data = filtered_data
)

wd.model.lmm.interaction.reduce <- lme4::lmer(
  width_deviation ~ correct_num + correct_width * number_deviation + spacing + (1 | subID),
  data = filtered_data
  
)

wd.model.lmm.interaction.reduce1 <- lme4::lmer(
  width_deviation ~ correct_num * correct_width * number_deviation + spacing + (1 | subID),
  data = filtered_data
  
)

anova(wd.model.lmm.interaction, wd.model.lmm.interaction.reduce)

summary(wd.model.lmm.interaction.reduce)

# model details
sjPlot::tab_model(
  wd.model.lmm.interaction.reduce,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

# In our model, correct_width0.1 serves as the reference category. 
# The intercept, representing the combination of correct_width0.1 and 
# number_deviation at its reference level (e.g., -1), 
# is estimated at 0.023 (SE = 0.006, t = 3.782). 
# This estimate indicates the baseline level of width_deviation.
# For correct_width0.25, the interaction with number_deviation shows a 
# coefficient of -0.003 (SE = 0.003, t = -1.120), indicating a slight, 
# non-significant decrease in the effect of number_deviation 
# from -1 to 0 compared to the baseline set by correct_width0.1.
# For correct_width0.4, the interaction with number_deviation shows 
# a more pronounced change with a coefficient of -0.014 (SE = 0.003, t = -4.364),
# suggesting that the effect of number_deviation from -1 to 0 
# substantially decreases width_deviation compared to the baseline.


# emmeans for the interaction of interest
emm <- emmeans::emmeans(wd.model.lmm.interaction.reduce1, specs = pairwise ~ number_deviation | correct_num * correct_width)

# pairwise comparisons+
pairs <- emmeans::contrast(emm, method = "tukey")
pairs_summary <- summary(pairs)


# estimate marginal means, test if significantly different from zero
emmeans_results <- emmeans::emmeans(wd.model.lmm.interaction.reduce, ~ number_deviation * correct_width|correct_num)
contrast_results <- emmeans::test(emmeans_results, null = 0)
summary(contrast_results, infer = c(TRUE, TRUE))


# ------------spacing deviation-------------------

sd.model.lmm.full <- lme4::lmer(
  space_deviation ~ correct_num  * spacing * number_deviation + correct_width + (1| subID),
  data = filtered_data
)

summary(sd.model.lmm.full)

# model details
sjPlot::tab_model(
  sd.model.lmm.full,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)


# estimate marginal means, test if significantly different from zero
emmeans_results <- emmeans::emmeans(sd.model.lmm.full, ~ number_deviation * spacing|correct_num)
contrast_results <- emmeans::test(emmeans_results, null = 0)
summary(contrast_results, infer = c(TRUE, TRUE))


# emmeans for the interaction of interest
emm <- emmeans::emmeans(sd.model.lmm.full, specs = pairwise ~ number_deviation | correct_num * spacing)

# pairwise comparisons+
pairs <- emmeans::contrast(emm, method = "tukey")
pairs_summary <- summary(pairs)

