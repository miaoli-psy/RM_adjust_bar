# ------------library-----------------
library(dplyr)
library(ggplot2)


# ------------read data---------------
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")
my_data <- readxl::read_excel(file.choose()) # df_tilt.xlsx



# -----------remove participants who were not able to do the task--------
failed_list <- c("502", "505", "507", "510", "513", "516", "517", "518", "528")


my_data <- my_data %>%
  filter(!SubID %in% failed_list)
