# ---------------libraries------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(rstatix)
library(svglite)

# ---------------read data------------------------

exp <- "exp1"

# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")

my_data <- readxl::read_excel(file.choose()) # data_clean.xlsx

# RM summary
RM_summary <- my_data %>%
  group_by(RM, correct_num) %>%
  dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
  dplyr::mutate(percentage_RM = (count / sum(count)) * 100)


# percentage RM for each condition 
p1 <- ggplot(RM_summary, aes(x = factor(correct_num), y = percentage_RM, fill = factor(RM))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percentage_RM, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25, 
            size = 3) +
  theme_minimal() +
  labs(title = "Percentage of RM by Correct Number",
       x = "Set size",
       y = "Percentage of RM",
       fill = "RM") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

p1

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
  dplyr::summarise(count = n(), 
                   .groups = 'drop',
                   avg_spacing = mean(correct_space)) %>%
  dplyr::mutate(percentage_spacing = (count / sum(count)) * 100)


p2 <- ggplot(spacing_summary, aes(x = factor(spacing), y = percentage_spacing)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  geom_text(aes(label = round(percentage_spacing, 1)), 
            vjust = -0.25, 
            size = 3) +
  theme_minimal() +
  labs(title = "Percentage of Spacing",
       x = "Spacing",
       y = "Percentage of Spacing") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

p2


# spacing distribution

p3 <-
  ggplot(my_data, aes(x = correct_space)) +
  geom_histogram(binwidth = 0.1, fill = "white", color = "black") +
  facet_wrap(~ trial_type) +
  theme_minimal() +
  labs(title = "Distribution of Spacing Under Each Trial Type Condition",
       x = "Center-to-center Spacing",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

p3

# ---Deviation - set size- separate for spacing----------

data_by_pp <-my_data %>%
  group_by(correct_num, correct_width, subID, spacing) %>%
  dplyr::summarise(
    number_deviation_mean = mean(number_deviation),
    number_deviation_sd = sd(number_deviation),
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spac_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spac_deviation_CI = spac_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp <- data_by_pp %>%
  group_by(correct_num, correct_width, spacing) %>%
  dplyr::summarise(
    num_dv_mean = mean(number_deviation_mean),
    num_dv_sd = sd(number_deviation_mean),
    spac_dv_mean = mean(spacing_deviation_mean),
    spac_dv_sd = sd(spacing_deviation_mean),
    n = n()
  ) %>%
  dplyr::mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spac_dv_SEM = spac_dv_sd / sqrt(n),
    spac_dv_CI = spac_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp$correct_width <- as.factor(data_by_pp$correct_width)
data_across_pp$correct_width <- as.factor(data_across_pp$correct_width)

data_by_pp$spacing <- factor(data_by_pp$spacing, levels = c("small", "middle", "large"))
data_across_pp$spacing <- factor(data_across_pp$spacing, levels = c("small", "middle", "large"))

data_by_pp$subID <-
  factor(
    data_by_pp$subID,
    levels = c(
      "S1",
      "S2",
      "S3",
      "S4",
      "S5",
      "S6",
      "S7",
      "S8",
      "S9",
      "S10",
      "S11",
      "S12",
      "S13",
      "S14",
      "S15",
      "S16",
      "S17",
      "S18",
      "S19",
      "S20"
    )
  )

my_plot1.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_pp, aes(x = correct_num,
                                    y = number_deviation_mean,
                                    fill = spacing,
                                    color = spacing),
             alpha = 0.2,
             position = position_jitterdodge(),
             show.legend = FALSE)+
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-2, 2))+
  
  scale_color_manual(
    labels = c("0.5°", "0.7°", "1.2°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Spacing"
  ) +

  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )


my_plot1.0

# # model the data           
# model <-
#   lme4::lmer(number_deviation_mean ~ correct_num + 
#             correct_width + 
#             spacing + 
#             (1|subID),
#           data = data_by_pp)
# 
# pred.mm <-
#   ggeffects::ggpredict(
#     model,
#     terms = c("correct_num [all]", "correct_width", "spacing"),
#     type = "fe"
#   )

# # rename columns 
# pred.mm <- pred.mm %>% 
#   dplyr::rename(
#     correct_num = x ,
#     correct_width = group,
#     spacing = facet
#   )

my_plot1.0.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_pp, aes(x = correct_num,
                                    y = number_deviation_mean,
                                    fill = correct_width,
                                    color = correct_width),
             alpha = 0.2,
             position = position_jitterdodge(),
             show.legend = FALSE)+
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-2, 2))+
  
  scale_color_manual(
    labels = c("0.1°", "0.25°", "0.4°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Line width"
  ) +

  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ spacing, labeller = labeller(
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      ))
  )


my_plot1.0.0

my_plot1.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  # geom_line(
  #   data = pred.mm,
  #   aes(
  #     x = correct_num,
  #     y = predicted, 
  #     group = spacing,
  #     color = spacing),
  #   size = 1,
  #   alpha = 0.5,
  #   position = position_dodge(0.5)
  # ) +
  
  stat_smooth(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing
    ),
    method = "glm",
    size = 1,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
  )+
  # geom_ribbon(
  #   data = pred.mm,
  #   aes(
  #     x = correct_num,
  #     y = predicted,
  #     ymin = conf.low,
  #     ymax = conf.high,
  #     group = spacing,
  #     fill = spacing
  #   ),
  #   size = 1,
  #   alpha = .1,
  #   position = position_dodge(0.5),
  #   color = NA
  # ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
    
  scale_color_manual(
    labels = c("0.5°", "0.7°", "1.2°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Spacing"
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )


my_plot1.1

my_plot1.1.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  # geom_line(
  #   data = pred.mm,
  #   aes(
  #     x = correct_num,
  #     y = predicted, 
  #     group = spacing,
  #     color = spacing),
  #   size = 1,
  #   alpha = 0.5,
  #   position = position_dodge(0.5)
  # ) +
  
  stat_smooth(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width
    ),
    method = "glm",
    size = 1,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
  )+
  # geom_ribbon(
  #   data = pred.mm,
  #   aes(
  #     x = correct_num,
  #     y = predicted,
  #     ymin = conf.low,
  #     ymax = conf.high,
  #     group = spacing,
  #     fill = spacing
  #   ),
  #   size = 1,
  #   alpha = .1,
  #   position = position_dodge(0.5),
  #   color = NA
  # ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  scale_color_manual(
    labels = c("0.1°", "0.25°", "0.4°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Line width"
  ) +

  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ spacing, labeller = labeller(
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      ))
  )


my_plot1.1.0

my_plot1.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  scale_color_manual(
    labels = c("0.5°", "0.7°", "1.2°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Spacing"
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )


my_plot1.2

my_plot1.2.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  

  scale_color_manual(
    labels = c("0.1°", "0.25°", "0.4°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Line width"
  ) +

  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ spacing, labeller = labeller(
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      ))
  )


my_plot1.2.0
# save figures
# ggsave(file = "1.0.svg", plot = my_plot1.0, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.1.svg", plot = my_plot1.1, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.2.svg", plot = my_plot1.2, width = 8.47, height = 3.79, units = "in")


# ggsave(file = "1.0.png", plot = my_plot1.0, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.0.0.png", plot = my_plot1.0.0, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.1.png", plot = my_plot1.1, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.1.0.png", plot = my_plot1.1.0, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.2.png", plot = my_plot1.2, width = 8.47, height = 3.79, units = "in")
# ggsave(file = "1.2.0.png", plot = my_plot1.2.0, width = 8.47, height = 3.79, units = "in")

# ---------check effect of center-to-center spacing on number deviaiton----

my_data_ini <- my_data %>% 
  dplyr::filter(trial_type %in% c('initial'))

data_by_pp1.1 <- my_data_ini %>%
  group_by(correct_num, correct_width, subID, correct_space) %>%
  dplyr::summarise(
    number_deviation_mean = mean(number_deviation),
    number_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp1.1 <- data_by_pp1.1 %>%
  group_by(correct_num, correct_width, correct_space) %>%
  dplyr::summarise(
    num_dv_mean = mean(number_deviation_mean),
    num_dv_sd = sd(number_deviation_mean),
    n = n()
  ) %>%
  dplyr::mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp1.1$correct_width <- as.factor(data_by_pp1.1$correct_width)
data_across_pp1.1$correct_width <- as.factor(data_across_pp1.1$correct_width)

data_by_pp1.1$subID <-
  factor(
    data_by_pp1.1$subID,
    levels = c(
      "S1",
      "S2",
      "S3",
      "S4",
      "S5",
      "S6",
      "S7",
      "S8",
      "S9",
      "S10",
      "S11",
      "S12",
      "S13",
      "S14",
      "S15",
      "S16",
      "S17",
      "S18",
      "S19",
      "S20"
    )
  )

data_by_pp1.1$correct_space <-
  factor(
    data_by_pp1.1$correct_space,
    levels = c(
      "0.5",
      "0.7",
      "0.9")
  )


data_across_pp1.1$correct_space <-
  factor(
    data_across_pp1.1$correct_space,
    levels = c(
      "0.5",
      "0.7",
      "0.9")
  )


my_plot1.3 <-  ggplot() +
  
  geom_point(
    data = data_across_pp1.1,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_space,
      color = correct_space
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp1.1,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_space,
      color = correct_space,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  
  scale_color_manual(
    labels = c("0.5°", "0.7°", "0.9°"),
    values = c( "#5E4C5F", "#EA801C", "#1A80BB"),
    name = "Spacing"
  ) +
  

  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )



my_plot1.3

# ---Deviation - set size--clasp spacing---------------

data_by_pp2 <- my_data %>%
  group_by(correct_num, correct_width, subID) %>%
  dplyr::summarise(
    number_deviation_mean = mean(number_deviation),
    number_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp2 <- data_by_pp2 %>%
  group_by(correct_num, correct_width) %>%
  dplyr::summarise(
    num_dv_mean = mean(number_deviation_mean),
    num_dv_sd = sd(number_deviation_mean),
    n = n()
  ) %>%
  dplyr::mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp2$correct_width <- as.factor(data_by_pp2$correct_width)
data_across_pp2$correct_width <- as.factor(data_across_pp2$correct_width)


data_by_pp2$subID <-
  factor(
    data_by_pp2$subID,
    levels = c(
      "S1",
      "S2",
      "S3",
      "S4",
      "S5",
      "S6",
      "S7",
      "S8",
      "S9",
      "S10",
      "S11",
      "S12",
      "S13",
      "S14",
      "S15",
      "S16",
      "S17",
      "S18",
      "S19",
      "S20"
    )
  )

my_plot2.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean
      ),
    position = position_dodge(0.4),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_pp2, 
             aes(x = correct_num,
                 y = number_deviation_mean,
                 fill = correct_num,
                 color = subID
                 ),
             alpha = 0.2,
             position = position_jitterdodge(),
             show.legend = FALSE)+
  
  geom_errorbar(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.4)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-2, 2))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )


my_plot2.0


my_plot2.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean
    ),
    position = position_dodge(0.4),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.4)
  ) +
  
  stat_smooth(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean
    ),
    method = "glm",
    size = 2,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )


my_plot2.1

my_plot2.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean
    ),
    position = position_dodge(0.4),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.4)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Deviation score (DS +/- 95% CI) ", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 20,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 20,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    # legend size
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 20, face = "bold")
  )   +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ))
  )


my_plot2.2


# ---Deviation - set size--collapse width---------------

data_by_pp2.0 <- my_data %>%
  group_by(correct_num, spacing, subID) %>%
  dplyr::summarise(
    number_deviation_mean = mean(number_deviation),
    number_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp2.0 <- data_by_pp2.0 %>%
  group_by(correct_num, spacing) %>%
  dplyr::summarise(
    num_dv_mean = mean(number_deviation_mean),
    num_dv_sd = sd(number_deviation_mean),
    n = n()
  ) %>%
  dplyr::mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp2.0$spacing <- factor(data_by_pp2.0$spacing, levels = c("small", "middle", "large"))
data_across_pp2.0$spacing <- factor(data_across_pp2.0$spacing, levels = c("small", "middle", "large"))



my_plot2.2.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2.0,
    aes(
      x = correct_num,
      y = num_dv_mean
    ),
    position = position_dodge(0.4),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp2.0,
    aes(
      x = correct_num,
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.4)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Number deviation", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  )   +
  
  facet_wrap(~ spacing, labeller = labeller(
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      ))
  )


my_plot2.2.0




# save figures
# ggsave(file = "2.0.svg", plot = my_plot2.0, width = 7.17, height = 3.79, units = "in")
# ggsave(file = "2.1.svg", plot = my_plot2.1, width = 7.17, height = 3.79, units = "in")
ggsave(file = "2.2.svg", plot = my_plot2.2, width = 7.17, height = 3.79, units = "in")

# ggsave(file = "2.0.png", plot = my_plot2.0, width = 7.17, height = 3.79, units = "in")
# ggsave(file = "2.1.png", plot = my_plot2.1, width = 7.17, height = 3.79, units = "in")
# ggsave(file = "2.2.png", plot = my_plot2.2, width = 7.17, height = 3.79, units = "in")
# ggsave(file = "2.2.0.png", plot = my_plot2.2.0, width = 7.17, height = 3.79, units = "in")
# -------------Width dv - set size-------------------

# remove overestimation trials 8.58% trials removed
my_data <- my_data %>% 
  filter(RM %in% c('RM', "no RM"))

data_by_pp3 <- my_data %>%
  group_by(correct_num, correct_width, RM, subID) %>%
  dplyr::summarise(
    width_deviation_mean = mean(width_deviation),
    width_deviation_sd = sd(width_deviation),
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spacing_deviation_CI = spacing_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp3 <- data_by_pp3 %>%
  group_by(correct_num, correct_width, RM) %>%
  dplyr::summarise(
    width_dv_mean = mean(width_deviation_mean),
    width_dv_sd = sd(width_deviation_mean),
    spacing_dv_mean = mean(spacing_deviation_mean),
    spacing_dv_sd = sd(spacing_deviation_mean),
    n = n()
  ) %>%
  dplyr::mutate(
    width_dv_SEM = width_dv_sd / sqrt(n),
    width_dv_CI = width_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_dv_SEM = spacing_dv_sd / sqrt(n),
    spacing_dv_CI = spacing_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp3$correct_width <- as.factor(data_by_pp3$correct_width)
data_across_pp3$correct_width <- as.factor(data_across_pp3$correct_width)


data_by_pp3$RM <- factor(data_by_pp3$RM, levels = c("RM", "no RM"))
data_across_pp3$RM <- factor(data_across_pp3$RM, levels = c("RM", "no RM"))


my_plot3.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = width_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.1,
    position = position_jitterdodge(),
    show.legend = FALSE,

    
  )+


  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,

    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot3.0


my_plot3.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = width_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  stat_smooth(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM
    ),
    method = "glm",
    size = 1,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +

  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot3.1

my_plot3.1.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = width_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  stat_smooth(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM
    ),
    method = "glm",
    size = 1,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.1, 0.1)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot3.1.0

my_plot3.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = width_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c( "RM trials (DS = -1, -2, -3)", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot3.2

my_plot3.2.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE,
    size = 4
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  # geom_point(
  #   data = data_by_pp3,
  #   aes(
  #     x = correct_num,
  #     y = width_deviation_mean,
  #     fill = RM,
  #     group = RM,
  #     color = RM,
  #     size = n
  #   ),
  #   alpha = 0.0,
  #   position = position_jitterdodge(),
  #   show.legend = FALSE,
  #   
  #   
  # )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (° +/- 95% CI)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.12, 0.1)) +
  
  scale_color_manual(
    labels = c( "RM trials (DS = -1, -2 -3)", "Correct trials (DS = 0)"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 20,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 20,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    # legend size
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 20, face = "bold")
  )   +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )


my_plot3.2.0


# save figures
# ggsave(file = "3.0.svg", plot = my_plot3.0, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.1.svg", plot = my_plot3.1, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.1.0.svg", plot = my_plot3.1.0, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.2.svg", plot = my_plot3.2, width = 8.87, height = 3.79, units = "in")
ggsave(file = "3.2.0.svg", plot = my_plot3.2.0, width = 10, height = 4.6, units = "in")
ggsave(file = "3.2.0.0.svg", plot = my_plot3.2.0, width = 10, height = 4.6, units = "in")

# ggsave(file = "3.0.png", plot = my_plot3.0, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.1.png", plot = my_plot3.1, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.1.0.png", plot = my_plot3.1.0, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.2.png", plot = my_plot3.2, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "3.2.0.png", plot = my_plot3.2.0, width = 8.87, height = 3.79, units = "in")

# ----deviation width - set size, sep for width and spacing-----

data_by_pp3.0 <- my_data %>%
  group_by(correct_num, correct_width, RM, subID, spacing) %>%
  summarise(
    width_deviation_mean = mean(width_deviation),
    width_deviation_sd = sd(width_deviation),
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    n = n()
  ) %>%
  mutate(
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spacing_deviation_CI = spacing_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp3.0 <- data_by_pp3.0 %>%
  group_by(correct_num, correct_width, RM, spacing) %>%
  summarise(
    width_dv_mean = mean(width_deviation_mean),
    width_dv_sd = sd(width_deviation_mean),
    spacing_dv_mean = mean(spacing_deviation_mean),
    spacing_dv_sd = sd(spacing_deviation_mean),
    n = n()
  ) %>%
  mutate(
    width_dv_SEM = width_dv_sd / sqrt(n),
    width_dv_CI = width_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_dv_SEM = spacing_dv_sd / sqrt(n),
    spacing_dv_CI = spacing_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp3.0$correct_width <- as.factor(data_by_pp3.0$correct_width)
data_across_pp3.0$correct_width <- as.factor(data_across_pp3.0$correct_width)

data_by_pp3.0$RM <- factor(data_by_pp3.0$RM, levels = c("RM", "no RM"))
data_across_pp3.0$RM <- factor(data_across_pp3.0$RM, levels = c("RM", "no RM"))


data_by_pp3.0$spacing <- factor(data_by_pp3.0$spacing, levels = c("small", "middle", "large"))
data_across_pp3.0$spacing <- factor(data_across_pp3.0$spacing, levels = c("small", "middle", "large"))

my_plot3.3 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3.0,
    aes(
      x = correct_num,
      y = width_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.15, 0.15)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width * spacing, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ),
    spacing = 
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      )
  )
  )

my_plot3.3

my_plot3.3.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = width_dv_mean,
      color = RM,
      group = interaction(RM, spacing),
      size = spacing
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8
  ) +
  
  geom_errorbar(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = width_dv_mean,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      group = interaction(RM, spacing),
      color = RM
      
    ),
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  # geom_point(
  #   data = data_by_pp3.0,
  #   aes(
  #     x = correct_num,
  #     y = width_deviation_mean,
  #     fill = RM,
  #     group = RM,
  #     color = RM,
  #     size = spacing
  #   ),
  #   alpha = 0.0,
  #   position = position_jitterdodge(),
  #   show.legend = FALSE
  #   
  # )+
  
  scale_size_manual(values = c("large" = 6,"middle"= 4,"small" = 2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.15, 0.15)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°")
  )
  )

my_plot3.3.0

# ggsave(file = "3.3.svg", plot = my_plot3.3, width = 8.27, height = 6.66, units = "in")
# ggsave(file = "3.3.0.svg", plot = my_plot3.3.0, width = 8.87, height = 3.79, units = "in")

# ggsave(file = "3.3.png", plot = my_plot3.3, width = 8.27, height = 6.66, units = "in")
# ggsave(file = "3.3.0.png", plot = my_plot3.3.0, width = 8.87, height = 3.79, units = "in")


# ------width dv - set size, collapse width, sep for spacing----------------
data_by_pp3.1 <- my_data %>%
  group_by(correct_num, RM, subID, spacing) %>%
  summarise(
    width_deviation_mean = mean(width_deviation),
    width_deviation_sd = sd(width_deviation),
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    n = n()
  ) %>%
  mutate(
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spacing_deviation_CI = spacing_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp3.1 <- data_by_pp3.1 %>%
  group_by(correct_num, RM, spacing) %>%
  summarise(
    width_dv_mean = mean(width_deviation_mean),
    width_dv_sd = sd(width_deviation_mean),
    spacing_dv_mean = mean(spacing_deviation_mean),
    spacing_dv_sd = sd(spacing_deviation_mean),
    n = n()
  ) %>%
  mutate(
    width_dv_SEM = width_dv_sd / sqrt(n),
    width_dv_CI = width_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_dv_SEM = spacing_dv_sd / sqrt(n),
    spacing_dv_CI = spacing_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )
data_by_pp3.1$RM <- factor(data_by_pp3.1$RM, levels = c("RM", "no RM"))
data_across_pp3.1$RM <- factor(data_across_pp3.1$RM, levels = c("RM", "no RM"))

data_by_pp3.1$spacing <- factor(data_by_pp3.1$spacing, levels = c("small", "middle", "large"))
data_across_pp3.1$spacing <- factor(data_across_pp3.1$spacing, levels = c("small", "middle", "large"))

my_plot3.4.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.1,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3.1,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3.1,
    aes(
      x = correct_num,
      y = width_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.11, 0.1)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 16,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 16,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    # legend size
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 20, face = "bold")
  ) +
  
  
  facet_wrap(~ spacing, labeller = labeller(
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      ))
  )

my_plot3.4.0

# ggsave(file = "3.4.0.png", plot = my_plot3.4.0, width = 8.87, height = 3.79, units = "in")

#---------------- width dv - num dv-----------------------------------------

# check percentage of trials num dv is not -1 and 0
num_dv_summary <- my_data %>%
  group_by(number_deviation) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::mutate(percentage_num_dv = (count / sum(count)) * 100)

# keep only number dv is -1 and 0; %2.45 trials are removed
my_data <- my_data %>% 
  dplyr::filter(number_deviation %in% c(-1, 0)) 

data_by_pp4 <- my_data %>%
  group_by(correct_num, correct_width, number_deviation, subID) %>%
  dplyr::summarise(
    width_deviation_mean = mean(width_deviation),
    width_deviation_sd = sd(width_deviation),
    n = n()
  ) %>%
  dplyr::mutate(
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp4 <- data_by_pp4 %>%
  group_by(correct_num, correct_width, number_deviation) %>%
  dplyr::summarise(
    width_dv_mean = mean(width_deviation_mean),
    width_dv_sd = sd(width_deviation_mean),
    n = n()
  ) %>%
  dplyr::mutate(
    width_dv_SEM = width_dv_sd / sqrt(n),
    width_dv_CI = width_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
  )

data_by_pp4$correct_width <- as.factor(data_by_pp4$correct_width)
data_across_pp4$correct_width <- as.factor(data_across_pp4$correct_width)

data_by_pp4$number_deviation <- factor(data_by_pp4$number_deviation, levels = c("-1", "0"))
data_across_pp4$number_deviation <- factor(data_across_pp4$number_deviation, levels = c("-1", "0"))

my_plot4.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      color = number_deviation,
      group = number_deviation,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  
  geom_point(
    data = data_by_pp4,
    aes(x = correct_num,
        y = width_deviation_mean,
        color = number_deviation,
        group = number_deviation,
        fill = number_deviation,
        size = n),
    alpha = 0.1,
    position = position_jitterdodge(),
    show.legend = FALSE
  )+
  
  geom_errorbar(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      color = number_deviation,
      group = number_deviation,
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5)) +
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c( "RM trials (Num DS = -1)", "Correct trials (Num DS = 0)"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  facet_wrap( ~ correct_width,
              labeller = labeller(
                correct_width =
                  c(
                    "0.1" = "Width 0.1°",
                    "0.25" = "Width 0.25°",
                    "0.4" = "Width 0.4°"
                  )
              ))


my_plot4.0


my_plot4.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      color = number_deviation,
      group = number_deviation,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      color = number_deviation,
      group = number_deviation,
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp4,
    aes(x = correct_num,
        y = width_deviation_mean,
        color = number_deviation,
        group = number_deviation,
        fill = number_deviation,
        size = n),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE
  )+
  
  stat_smooth(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = number_deviation,
      color = number_deviation,
    ),
    method = "glm",
    size = 2,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
  )+
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5)) +
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c( "RM trials (Num DS = -1)", "Correct trials (Num DS = 0)"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  facet_wrap( ~ correct_width,
              labeller = labeller(
                correct_width =
                  c(
                    "0.1" = "Width 0.1°",
                    "0.25" = "Width 0.25°",
                    "0.4" = "Width 0.4°"
                  )
              ))


my_plot4.1


my_plot4.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      color = number_deviation,
      group = number_deviation,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_point(
    data = data_by_pp4,
    aes(x = correct_num,
        y = width_deviation_mean,
        color = number_deviation,
        group = number_deviation,
        fill = number_deviation,
        size = n),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE
  )+
  
  geom_errorbar(
    data = data_across_pp4,
    aes(
      x = correct_num,
      y = width_dv_mean,
      ymin = width_dv_mean - width_dv_CI,
      ymax = width_dv_mean + width_dv_CI,
      color = number_deviation,
      group = number_deviation,
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5)) +
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c( "RM trials (Num DS = -1)", "Correct trials (Num DS = 0)"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  facet_wrap( ~ correct_width,
              labeller = labeller(
                correct_width =
                  c(
                    "0.1" = "Width 0.1°",
                    "0.25" = "Width 0.25°",
                    "0.4" = "Width 0.4°"
                  )
              ))


my_plot4.2

# save figures
# ggsave(file = "4.0.svg", plot = my_plot4.0, width = 10, height = 3.79, units = "in")
# ggsave(file = "4.1.svg", plot = my_plot4.1, width = 10, height = 3.79, units = "in")
# ggsave(file = "4.2.svg", plot = my_plot4.2, width = 10, height = 3.79, units = "in")



# -------------spacing dv - set size for each correct spacing and width-----

my_plot5.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      ymin = spacing_dv_mean - spacing_dv_CI,
      ymax = spacing_dv_mean + spacing_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = spacing_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.1,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot5.0

my_plot5.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      ymin = spacing_dv_mean - spacing_dv_CI,
      ymax = spacing_dv_mean + spacing_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = spacing_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE
  )+
  
  stat_smooth(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM
    ),
    method = "glm",
    size = 1,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot5.1

my_plot5.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      ymin = spacing_dv_mean - spacing_dv_CI,
      ymax = spacing_dv_mean + spacing_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3,
    aes(
      x = correct_num,
      y = spacing_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE
  )+

  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      )
  )
  )

my_plot5.2

# save figures
# ggsave(file = "5.0.svg", plot = my_plot5.0, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "5.1.svg", plot = my_plot5.1, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "5.2.svg", plot = my_plot5.2, width = 8.87, height = 3.79, units = "in")


# ggsave(file = "5.0.png", plot = my_plot5.0, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "5.1.png", plot = my_plot5.1, width = 8.87, height = 3.79, units = "in")
# ggsave(file = "5.2.png", plot = my_plot5.2, width = 8.87, height = 3.79, units = "in")

my_plot5.3 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  
  geom_errorbar(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      ymin = spacing_dv_mean - spacing_dv_CI,
      ymax = spacing_dv_mean + spacing_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  geom_point(
    data = data_by_pp3.0,
    aes(
      x = correct_num,
      y = spacing_deviation_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.0,
    position = position_jitterdodge(),
    show.legend = FALSE,
    
    
  )+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width * spacing, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°"
      ),
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      )
  )
  )

my_plot5.3

my_plot5.3.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      color = RM,
      group = interaction(RM, spacing),
      size = spacing
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8
  ) +
  
  geom_errorbar(
    data = data_across_pp3.0,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      ymin = spacing_dv_mean - spacing_dv_CI,
      ymax = spacing_dv_mean + spacing_dv_CI,
      group = interaction(RM, spacing),
      color = RM
      
    ),
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  # geom_point(
  #   data = data_by_pp3.0,
  #   aes(
  #     x = correct_num,
  #     y = width_deviation_mean,
  #     fill = RM,
  #     group = RM,
  #     color = RM,
  #     size = spacing
  #   ),
  #   alpha = 0.0,
  #   position = position_jitterdodge(),
  #   show.legend = FALSE
  #   
  # )+
  
  scale_size_manual(values = c("large" = 6,"middle"= 4,"small" = 2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  
  scale_color_manual(
    labels = c( "RM trials", "non-RM trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "Width 0.1°",
        "0.25" = "Width 0.25°",
        "0.4" = "Width 0.4°")
  )
  )

my_plot5.3.0
# ggsave(file = "5.3.svg", plot = my_plot5.3, width = 8.27, height = 6.66, units = "in")
# ggsave(file = "5.3.0.svg", plot = my_plot5.3.0, width = 8.87, height = 3.79, units = "in")


# ggsave(file = "5.3.png", plot = my_plot5.3, width = 8.27, height = 6.66, units = "in")
# ggsave(file = "5.3.0.png", plot = my_plot5.3.0, width = 8.87, height = 3.79, units = "in")

# ------spacing dv -set size, collpase width, sep for spacing

my_plot5.4 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.1,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE,
    size = 5
  ) +
  
  geom_errorbar(
    data = data_across_pp3.1,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      ymin = spacing_dv_mean - spacing_dv_CI,
      ymax = spacing_dv_mean + spacing_dv_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (° +/- 95% CI)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 1.0)) +
  

  scale_color_manual(
    labels = c( "RM trials (DS = -1, -2, -3)", "Correct trials (DS = 0)"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 20,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 20,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    # legend size
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    # facet wrap title
    strip.text.x = element_text(size = 20, face = "bold")
  ) +
  
  
  facet_wrap(~ spacing, labeller = labeller(
    spacing =
      c(
        "small" = "Spacing 0.5°",
        "middle" = "Spacing 0.7°",
        "large" = "Spacing 1.2°"
      ))
  )

my_plot5.4

ggsave(file = "5.4.svg", plot = my_plot5.4, width = 10, height = 3.2, units = "in")

