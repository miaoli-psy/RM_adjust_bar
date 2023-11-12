# ---------------libraries------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
install.packages('rstatix')
library(rstatix)

# ---------------read data------------------------

exp <- "exp1"

# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")
path <- "D:/OneDrive/projects/RM_adjust_bar/data/"


my_data <- readxl::read_excel(file.choose())

# -------------Deivation - set size-------------------

data_by_pp <- my_data %>%
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


data_across_pp <- data_by_pp %>%
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

data_by_pp$correct_width <- as.factor(data_by_pp$correct_width)
data_across_pp$correct_width <- as.factor(data_across_pp$correct_width)

my_plot <-  ggplot() +
  
  geom_point(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      group = correct_width,
      color = correct_width
    ),
    position = position_dodge(0.2),
    stat = "identity",
    alpha = 0.5
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_pp, aes(x = correct_num,
                                         y = number_deviation_mean,
                                         group = correct_width,
                                         color = correct_width),
             alpha = 0.2,
             position = position_dodge(0.2))+
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = correct_num,
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_SEM,
      ymax = num_dv_mean + num_dv_SEM,
      group = correct_width,
      color = correct_width
    ),
    
    size  = 1.2,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Deviation (number task)", x = "Set size") +
  
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
  ) 

my_plot

# -------------Width dv - set size-------------------
data_by_pp2 <- my_data %>%
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


data_across_pp2 <- data_by_pp2 %>%
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


data_by_pp2$correct_width <- as.factor(data_by_pp2$correct_width)
data_across_pp2$correct_width <- as.factor(data_across_pp2$correct_width)

my_plot2 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2,
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
  ) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_pp2, aes(x = correct_num,
                                         y = width_deviation_mean,
                                         group = RM,
                                         color = RM,
                                     size = n),
             alpha = 0.1,
             position = position_dodge(0.5))+

  
  geom_errorbar(
    data = data_across_pp2,
    aes(
      x = correct_num,
      y = width_dv_mean,
      group = RM,
      ymin = width_dv_mean - width_dv_SEM,
      ymax = width_dv_mean + width_dv_SEM,

    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.5,
    color = "black",
    position = position_dodge(0.5)
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Width deviation (°)", x = "Set size") +
  
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
  )
  )

my_plot2


# -------------spacing dv - set size for each correct spacing and width-----
my_plot2.1 <-  ggplot() +
  
  geom_point(
    data = data_across_pp2,
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
  geom_point(
    data = data_by_pp2,
    aes(
      x = correct_num,
      y = spacing_deviation_mean,
      group = RM,
      color = RM,
      size = n
    ),
    alpha = 0.1,
    position = position_dodge(0.5)
  )+

  geom_errorbar(
    data = data_across_pp2,
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


# -------------Deviation width - Deviation Num --------
data_by_pp3 <- my_data %>%
  group_by(correct_num, correct_width, number_deviation, subID) %>%
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
  group_by(correct_num, correct_width, number_deviation) %>%
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
  
  labs(y = "Width deviation (°)", x = "Deviation (number task)") +
  
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
  
  labs(y = "Width deviation (°)", x = "Deviation (number task)") +
  
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


