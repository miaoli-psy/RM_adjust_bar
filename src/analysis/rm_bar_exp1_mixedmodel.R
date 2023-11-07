# ---------------libraries------------------------
library(dplyr)
library(tidyverse)
library(readxl)
library(lme4)

# ---------------read data------------------------

exp <- "exp1"

# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")
path <- "D:/OneDrive/projects/RM_adjust_bar/data/"


my_data <- readxl::read_excel(file.choose())

# -------------LMM-------------------


my_data$RM <- as.factor(my_data$RM)
my_data$correct_num <- as.factor(my_data$correct_num)
my_data$number_deviation <- as.factor(my_data$number_deviation)
my_data$correct_width <- as.factor(my_data$correct_width)
my_data$subID <- as.factor(my_data$subID)

model <-
  lmer(width_deviation ~ correct_width + RM + correct_num +  (1 | subID), data = my_data)
model2 <-
  lmer(width_deviation ~ RM +  correct_width +  (1 | subID), data = my_data)


anova(model, model2)



