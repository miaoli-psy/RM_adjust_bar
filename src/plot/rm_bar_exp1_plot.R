# ---------------libraries------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(rstatix)

# ---------------read data------------------------

exp <- "exp1"

# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")


my_data <- readxl::read_excel(file.choose()) # data_clean.xlsx

# RM summary
RM_summary <- my_data %>%
  group_by(RM) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage_RM = (count / sum(count)) * 100)

# add spacing condition
my_data <- my_data %>%
  mutate(spacing = case_when(
    correct_space <= 0.6 ~ "small",
    correct_space > 0.6 & correct_space <= 0.8 ~ "middle",
    correct_space > 0.8 ~ "large",
    TRUE ~ NA_character_
  ))

# spacing summary
spacing_summary <- my_data %>%
  group_by(spacing) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage_spacing = (count / sum(count)) * 100)


# ---Deviation - set size- separate for spacing----------

data_by_pp <- my_data %>%
  group_by(correct_num, correct_width, subID, spacing) %>%
  summarise(
    number_deviation_mean = mean(number_deviation),
    number_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp <- data_by_pp %>%
  group_by(correct_num, correct_width, spacing) %>%
  summarise(
    num_dv_mean = mean(number_deviation_mean),
    num_dv_sd = sd(number_deviation_mean),
    n = n()
  ) %>%
  mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp$correct_width <- as.factor(data_by_pp$correct_width)
data_across_pp$correct_width <- as.factor(data_across_pp$correct_width)

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
    position = position_dodge(0.4),
    stat = "identity",
    alpha = 0.8,
    size = 4
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_pp, aes(x = correct_num,
                                    y = number_deviation_mean,
                                    color = spacing),
             alpha = 0.2,
             position = position_dodge(0.4))+
  
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
    position = position_dodge(0.4)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Deviation (°) for number task", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-2, 2))+
  
  scale_color_manual(
    labels = c("large", "middle", "small"),
    values = c( "#B8B8B8", "#EA801C", "#1A80BB"),
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


my_plot1.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing
    ),
    position = position_dodge(0.4),
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
    position = position_dodge(0.4)
  ) +
  
  stat_smooth(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing,
    ),
    method = "glm",
    size = 2,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
  )+

  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Deviation (°) for number task", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
    
  scale_color_manual(
      labels = c("large", "middle", "small"),
      values = c( "#B8B8B8", "#EA801C", "#1A80BB"),
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

my_plot1.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = spacing,
      color = spacing
    ),
    position = position_dodge(0.4),
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
    position = position_dodge(0.4)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Deviation (°) for number task", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-1.5, 1))+
  
  scale_color_manual(
    labels = c("large", "middle", "small"),
    values = c( "#B8B8B8", "#EA801C", "#1A80BB"),
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

# ---Deviation - set size--clasp spacing---------------

data_by_pp2 <- my_data %>%
  group_by(correct_num, correct_width, subID) %>%
  summarise(
    number_deviation_mean = mean(number_deviation),
    number_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  mutate(
    num_deviation_SEM = number_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp2 <- data_by_pp2 %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    num_dv_mean = mean(number_deviation_mean),
    num_dv_sd = sd(number_deviation_mean),
    n = n()
  ) %>%
  mutate(
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
  
  labs(y = "Deviation (°) for number task", x = "Set size") +
  
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
  
  labs(y = "Deviation (°) for number task", x = "Set size") +
  
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
  
  labs(y = "Deviation (°) for number task", x = "Set size") +
  
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


my_plot2.2



# -------------Width dv - set size-------------------

# remove overestimation trials 8.58% trials removed
my_data <- my_data %>% 
  filter(RM %in% c('RM', "no RM"))

data_by_pp3 <- my_data %>%
  group_by(correct_num, correct_width, RM, subID) %>%
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


data_across_pp3 <- data_by_pp3 %>%
  group_by(correct_num, correct_width, RM) %>%
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

data_by_pp3$correct_width <- as.factor(data_by_pp3$correct_width)
data_across_pp3$correct_width <- as.factor(data_across_pp3$correct_width)


data_by_pp3$RM <- factor(data_by_pp3$RM, levels = c("no RM", "RM"))


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
    show.legend = FALSE
    
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
    labels = c("non-RM trials", "RM trials"),
    values = c( "#298C8C", "#800074"),
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
  
  stat_smooth(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      color = RM
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
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c("non-RM trials", "RM trials"),
    values = c( "#298C8C", "#800074"),
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
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  
  scale_color_manual(
    labels = c("non-RM trials", "RM trials"),
    values = c( "#298C8C", "#800074"),
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

# -------------spacing dv - set size for each correct spacing and width-----
my_plot2.1 <-  ggplot() +
  
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
    alpha = 0.8
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  # geom_point(
  #   data = data_by_pp2,
  #   aes(
  #     x = correct_num,
  #     y = spacing_deviation_mean,
  #     group = RM,
  #     color = RM,
  #     size = n
  #   ),
  #   alpha = 0.1,
  #   position = position_dodge(0.5)
  # )+

  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      ymin = spacing_dv_mean - spacing_dv_SEM,
      ymax = spacing_dv_mean + spacing_dv_SEM,
      
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  
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
  ) +
  
  facet_wrap(~ correct_width, labeller = labeller(
    correct_width =
      c(
        "0.1" = "actual width 0.1",
        "0.25" = "actual width 0.25",
        "0.4" = "actual width 0.4"
      )
  ))

my_plot2.1

# -------spacing dv - set size, sep for bar width and spacing

data_ini <- subset(my_data, trial_type == "initial")
data_match <- subset(my_data, trial_type == "match")

data_by_pp2.2 <- data_ini %>%
  group_by(correct_num, RM, subID, correct_space) %>%
  summarise(
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    n = n()
  ) %>%
  mutate(
    spacing_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spacing_deviation_CI = spacing_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

library(lme4)

m1 <- 
  lmer(spacing_deviation_mean ~ correct_space + correct_num + (1|subID),
       data = data_by_pp2.2)

m2 <- 
  lmer(spacing_deviation_mean ~ correct_num + (1|subID),
       data = data_by_pp2.2)

anova(m1, m2)


data_across_pp2.2 <- data_by_pp2.2 %>%
  group_by(correct_num, RM,correct_space) %>%
  summarise(
    spacing_dv_mean = mean(spacing_deviation_mean),
    spacing_dv_sd = sd(spacing_deviation_mean),
    n = n()
  ) %>%
  mutate(
    spacing_dv_SEM = spacing_dv_sd / sqrt(n),
    spacing_dv_CI = spacing_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


# data_by_pp2.2$correct_width <- as.factor(data_by_pp2.2$correct_width)
# data_across_pp2.2$correct_width <- as.factor(data_across_pp2.2$correct_width)

data_by_pp2.2$correct_space <- as.factor(data_by_pp2.2$correct_space)
data_across_pp2.2$correct_space <- as.factor(data_across_pp2.2$correct_space)


my_plot2.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2.2,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      color = RM,
      size = n
    ),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  # geom_point(
  #   data = data_by_pp2.2,
  #   aes(
  #     x = correct_num,
  #     y = spacing_deviation_mean,
  #     group = RM,
  #     color = RM,
  #     size = n
  #   ),
  #   alpha = 0.1,
  #   position = position_dodge(0.5)
  # )+
  
  geom_errorbar(
    data = data_across_pp2.2,
    aes(
      x = correct_num,
      y = spacing_dv_mean,
      group = RM,
      ymin = spacing_dv_mean - spacing_dv_SEM,
      ymax = spacing_dv_mean + spacing_dv_SEM,
      
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Set size") +
  
  scale_x_continuous(breaks = c(3, 4, 5), 
                     labels = c("3", "4", "5"), limits = c(2.5, 5.5))+
  
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  
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
  ) +
  
  # facet_wrap(~ correct_width + correct_space, labeller = labeller(
  #   correct_width =
  #     c(
  #       "0.1" = "actual width 0.1",
  #       "0.25" = "actual width 0.25",
  #       "0.4" = "actual width 0.4"
  #     )
  # )) +
  facet_wrap(~ correct_space, labeller = labeller(
    correct_space =
        c(
          "0.5" = "spacing 0.1",
          "0.7" = "spacing 0.7",
          "0.9" = "spacing 0.9"
        ))
  )

    
  

my_plot2.2





# -------------Deviation width - Deviation Num --------

# ----add predicted width---------

my_data$predicted_width <-
  (my_data$correct_width * my_data$correct_num) / my_data$response_num

my_data$width_deviation_p <- my_data$response_width - my_data$predicted_width

# ----add new y-axis: width dv in percentage of line width----
my_data$correct_width_all <- my_data$correct_num * my_data$correct_width
my_data$reported_width_all <- my_data$response_width * my_data$response_num
my_data$dv_in_percentage_width <- (my_data$reported_width_all - my_data$correct_width_all) / my_data$correct_width_all * my_data$correct_num 


data_by_pp3 <- my_data %>%
  group_by(correct_num, correct_width, number_deviation, subID) %>%
  summarise(
    width_deviation_mean = mean(width_deviation),
    width_deviation_sd = sd(width_deviation),
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    width_deviation_p_mean = mean(width_deviation_p),
    width_deviation_p_sd = sd(width_deviation_p),
    dv_in_percentage_width_mean = mean(dv_in_percentage_width),
    dv_in_percentage_width_sd = sd(dv_in_percentage_width),
    n = n()
  ) %>%
  mutate(
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spacing_deviation_CI = spacing_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    width_deviation_p_SEM = width_deviation_p_sd / sqrt(n),
    width_deviation_p_CI = width_deviation_p_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    dv_in_percentage_width_SEM = dv_in_percentage_width_sd / sqrt(n),
    dv_in_percentage_width_CI = dv_in_percentage_width_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp3 <- data_by_pp3 %>%
  group_by(correct_num, correct_width, number_deviation) %>%
  summarise(
    width_dv_mean = mean(width_deviation_mean),
    width_dv_sd = sd(width_deviation_mean),
    spacing_dv_mean = mean(spacing_deviation_mean),
    spacing_dv_sd = sd(spacing_deviation_mean),
    width_dv_p_mean = mean(width_deviation_p_mean),
    width_dv_p_sd = sd(width_deviation_p_mean),
    width_dv_normalized_mean = mean(dv_in_percentage_width_mean),
    width_dv_normalized_sd = sd(dv_in_percentage_width_mean),
    n = n()
  ) %>%
  mutate(
    width_dv_SEM = width_dv_sd / sqrt(n),
    width_dv_CI = width_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_dv_SEM = spacing_dv_sd / sqrt(n),
    spacing_dv_CI = spacing_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    width_dv_p_SEM = width_dv_p_sd / sqrt(n),
    width_dv_p_CI = width_dv_p_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    width_dv_normalized_SEM = width_dv_normalized_sd / sqrt(n),
    width_dv_normalized_CI = width_dv_normalized_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_pp3$correct_width <- as.factor(data_by_pp3$correct_width)
data_across_pp3$correct_width <- as.factor(data_across_pp3$correct_width)

my_plot3 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = width_dv_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.8
  ) +
  
  
  geom_point(
    data = data_by_pp3,
    aes(x = number_deviation,
        y = width_deviation_mean,
        color = as.factor(number_deviation),
        size = n),
    alpha = 0.1,
    position = position_dodge(0.2)
  )+

  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = width_dv_mean,
      ymin = width_dv_mean - width_dv_SEM,
      ymax = width_dv_mean + width_dv_SEM
    ),

    size  = 1.0,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.2)
  ) +
  

  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation1 (response width - actual width)", x = "Deviation (number task)") +
  
  scale_x_continuous(breaks = c( -3, -2, -1, 0, 1, 2, 3), 
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-4.5, 4.5))+
  
  
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
  ) +
  
  facet_wrap( ~ correct_num + correct_width,
              labeller = labeller(
                correct_num =
                  c("3" = "set size 3",
                    "4" = "set size 4",
                    "5" = "set size 5"),
                correct_width =
                  c(
                    "0.1" = "actual width 0.1",
                    "0.25" = "actual width 0.25",
                    "0.4" = "actual width 0.4"
                  )
              ))
    

my_plot3


my_plot3.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = width_dv_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = width_dv_mean,
      ymin = width_dv_mean - width_dv_SEM,
      ymax = width_dv_mean + width_dv_SEM
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.2)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation1 (response width - actual width)", x = "Deviation (number task)") +
  
  scale_x_continuous(breaks = c( -3, -2, -1, 0, 1, 2, 3), 
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-4.5, 4.5))+
  
  
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
  ) +
  
  facet_wrap( ~ correct_num + correct_width,
              labeller = labeller(
                correct_num =
                  c("3" = "set size 3",
                    "4" = "set size 4",
                    "5" = "set size 5"),
                correct_width =
                  c(
                    "0.1" = "actual width 0.1",
                    "0.25" = "actual width 0.25",
                    "0.4" = "actual width 0.4"
                  )
              ))


my_plot3.0


my_plot3.0.0 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = width_dv_p_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5,
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = width_dv_p_mean,
      ymin = width_dv_p_mean - width_dv_SEM,
      ymax = width_dv_p_mean + width_dv_SEM
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.2)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation2 (response width - predicted width)", x = "Deviation (number task)") +
  
  scale_x_continuous(breaks = c( -3, -2, -1, 0, 1, 2, 3), 
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-4.5, 4.5))+
  
  
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
  ) +
  
  facet_wrap( ~ correct_num + correct_width,
              labeller = labeller(
                correct_num =
                  c("3" = "set size 3",
                    "4" = "set size 4",
                    "5" = "set size 5"),
                correct_width =
                  c(
                    "0.1" = "actual width 0.1",
                    "0.25" = "actual width 0.25",
                    "0.4" = "actual width 0.4"
                  )
              ))


my_plot3.0.0


# -----------Width deviation in percentage of line width-----

data_across_pp3_subset <- subset(data_across_pp3, number_deviation %in% c(-2, -1, 0, 1)) 

my_plot3.0.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3_subset,
    aes(
      x = number_deviation,
      y = width_dv_normalized_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5,
  ) +
  
  geom_errorbar(
    data = data_across_pp3_subset,
    aes(
      x = number_deviation,
      y = width_dv_normalized_mean,
      ymin = width_dv_normalized_mean - width_dv_normalized_SEM,
      ymax = width_dv_normalized_mean + width_dv_normalized_SEM
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.2)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "blue") +
  
  labs(y = "Width deviation in propotion of line width", x = "Deviation (number task)") +
  
  scale_x_continuous(breaks = c(-2, -1, 0, 1), 
                     labels = c("-2", "-1", "0", "1"), limits = c(-2.5, 1.5))+
  
  
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
  ) +
  
  facet_wrap( ~ correct_num + correct_width,
              labeller = labeller(
                correct_num =
                  c("3" = "set size 3",
                    "4" = "set size 4",
                    "5" = "set size 5"),
                correct_width =
                  c(
                    "0.1" = "actual width 0.1",
                    "0.25" = "actual width 0.25",
                    "0.4" = "actual width 0.4"
                  )
              ))


my_plot3.0.1


# -------------Deviation spacing - Deviation Num --------

my_plot3.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = spacing_dv_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(
    data = data_by_pp3,
    aes(
      x = number_deviation,
      y = spacing_deviation_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    alpha = 0.2,
    position = position_dodge(0.2)
  )+

  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = spacing_dv_mean,
      ymin = spacing_dv_mean - spacing_dv_SEM,
      ymax = spacing_dv_mean + spacing_dv_SEM,
      group = correct_width
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Deviation (number task)") +
  
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), 
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-4.5, 4.5))+
  
  
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
  ) +
  
  facet_wrap( ~ correct_num + correct_width,
              labeller = labeller(
                correct_num =
                  c("3" = "set size 3",
                    "4" = "set size 4",
                    "5" = "set size 5"),
                correct_width =
                  c(
                    "0.1" = "actual width 0.1",
                    "0.25" = "actual width 0.25",
                    "0.4" = "actual width 0.4"
                  )
              ))


my_plot3.1



my_plot3.2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = spacing_dv_mean,
      color = as.factor(number_deviation),
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5
  ) +
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = number_deviation,
      y = spacing_dv_mean,
      ymin = spacing_dv_mean - spacing_dv_SEM,
      ymax = spacing_dv_mean + spacing_dv_SEM,
      group = correct_width
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (°)", x = "Deviation (number task)") +
  
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), 
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-4.5, 4.5))+
  
  
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
  ) +
  
  facet_wrap( ~ correct_num + correct_width,
              labeller = labeller(
                correct_num =
                  c("3" = "set size 3",
                    "4" = "set size 4",
                    "5" = "set size 5"),
                correct_width =
                  c(
                    "0.1" = "actual width 0.1",
                    "0.25" = "actual width 0.25",
                    "0.4" = "actual width 0.4"
                  )
              ))

my_plot3.2


# spacing deviation -- width deviation
# check number dv -1 and 0

data_across_pp3.0 <- subset(data_across_pp3, number_deviation %in% c(0, -1))
data_by_pp3.0 <- subset(data_by_pp3, number_deviation %in% c(0, -1))

data_across_pp3.0$number_deviation <- as.factor(data_across_pp3.0$number_deviation)
data_across_pp3.0$correct_width <- as.factor(data_across_pp3.0$correct_width)

my_plot3.3 <-  ggplot() +
  
  geom_point(
    data = data_across_pp3.0,
    aes(
      x = width_dv_mean,
      y = spacing_dv_mean,
      color = correct_width,
      size = n
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  # geom_point(
  #   data = data_by_pp3.0,
  #   aes(
  #     x = width_deviation_mean,
  #     y = spacing_deviation_mean,
  #     color = as.factor(correct_width),
  #     size = n
  #   ),
  #   alpha = 0.2,
  #   position = position_dodge(0.2)
  # )+
  # 
  geom_errorbar(
    data = data_across_pp3.0,
    aes(
      x = width_dv_mean,
      y = spacing_dv_mean,
      ymin = spacing_dv_mean - spacing_dv_SEM,
      ymax = spacing_dv_mean + spacing_dv_SEM,
      group = correct_width
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  labs(y = "Spacing deviation (dva)", x = "Width deviation 1 (dva)") +
  
  # scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), 
  #                    labels = c("-3", "-2", "-1", "0", "1", "2", "3"), limits = c(-4.5, 4.5))+
  # 
  
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
  ) +
  
  facet_wrap(
    ~ correct_num + number_deviation,
    nrow = 3,
    labeller = labeller(
      correct_num =
        c("3" = "set size 3",
          "4" = "set size 4",
          "5" = "set size 5"),
      number_deviation =
        c("0" = "Number DV == 0",
          "-1" = "Number DV == -1")
    )
  )


my_plot3.3

# ----------------temporal write the data for analysis----------------
data_by_pp4 <- my_data %>% 
  group_by(correct_num, correct_width, number_deviation, subID) %>%
  summarise(
    width_deviation_mean = mean(width_deviation),
    width_deviation_sd = sd(width_deviation),
    spacing_deviation_mean = mean(space_deviation),
    spacing_deviation_sd = sd(space_deviation),
    width_deviation_p_mean = mean(width_deviation_p),
    width_deviation_p_sd = sd(width_deviation_p),
    dv_in_percentage_width_mean = mean(dv_in_percentage_width),
    dv_in_percentage_width_sd = sd(dv_in_percentage_width),
    n = n()
  ) %>%
  mutate(
    width_deviation_SEM = width_deviation_sd / sqrt(n),
    width_deviation_CI = width_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    spacing_deviation_SEM = spacing_deviation_sd / sqrt(n),
    spacing_deviation_CI = spacing_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    width_deviation_p_SEM = width_deviation_p_sd / sqrt(n),
    width_deviation_p_CI = width_deviation_p_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    dv_in_percentage_width_SEM = dv_in_percentage_width_sd / sqrt(n),
    dv_in_percentage_width_CI = dv_in_percentage_width_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

write.csv(data_by_pp4, file = "exp1_data.csv")


library(lme4)

model1 <- lmer(dv_in_percentage_width_mean ~ number_deviation + correct_width + correct_num + (1|subID), data = data_by_pp4)

summary(model1)

library(emmeans)

# pairwise comparisons
emms <- emmeans::emmeans(model1,
                         list(pairwise ~ number_deviation * correct_width * correct_num),
                         adjust = "tukey")

summary(emms, infer = TRUE)
