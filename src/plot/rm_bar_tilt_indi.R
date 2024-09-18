# ---------------libraries------------------------
library(dplyr)
library(ggplot2)


# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")

# read data
my_data <- readxl::read_excel(file.choose()) # data_tilt.xlsx


# factors
my_data <- my_data %>% 
  filter(RM %in% c('RM', "Correct"))
my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct"))


my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct", "Overestimate"))


# list all participants
unique_subids <- unique(my_data$SubID)


# -------------some descriptive stats---------------

# RM task---------------

# summarize the counts of RM under different oriented line locations
rm_distribution_by_pp <- my_data %>%
  group_by(SubID, oriented_line_location, RM, correct_num) %>%
  summarise(count = n()) %>%
  ungroup()

print(rm_distribution_by_pp)

# plot RM distribution under different oriented line locations
ggplot(rm_distribution_by_pp, aes(x = oriented_line_location, y = count, fill = RM)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(correct_num ~ SubID, scales = "free_y") + 
  labs(title = "Distribution of RM by Oriented Line Location", 
       x = "Oriented Line Location", 
       y = "Count of RM Responses") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") 


# ori task---------------

# Summarize ori_correct distribution by SubID, oriented_line_location, and ori
my_data$ori_correct <- as.numeric(as.character(my_data$ori_correct))

ori_correct_distribution <- my_data %>%
  group_by(SubID, oriented_line_location, ori, correct_num, RM) %>%
  summarise(proportion_correct = mean(ori_correct)) %>%
  ungroup()


# Plotting the proportion of correct responses for each participant
ggplot(ori_correct_distribution, aes(x = ori, 
                                     y = proportion_correct, 
                                     fill = as.factor(ori))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ SubID + oriented_line_location + correct_num + RM) +  
  labs(title = "Proportion of Correct Responses (ori_correct) by Oriented Line Location and Setsize for Each Participant", 
       x = "Oriented Line Location and Ori", 
       y = "Proportion of Correct Responses",
       fill = "Ori") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



ori_correct_distribution_across_pp <- my_data %>%
  group_by(oriented_line_location, ori, correct_num, RM) %>%
  summarise(
    proportion_correct = mean(as.numeric(ori_correct), na.rm = TRUE),
    n = n(),  
    SEM = sqrt((proportion_correct * (1 - proportion_correct)) / n),
    ci_lower = proportion_correct - 1.96 * SEM,
    ci_upper = proportion_correct + 1.96 * SEM 
  ) %>%
  ungroup()


ggplot(ori_correct_distribution_across_pp, aes(x = ori, 
                                     y = proportion_correct,
                                     yim = proportion_correct - ci_lower,
                                     ymax = proportion_correct + ci_upper,
                                     fill = as.factor(ori))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~oriented_line_location + correct_num + RM) +  
  labs(title = "Proportion of Correct Responses (ori_correct) by Oriented Line Location and Setsize for Each Participant", 
       x = "Oriented Line Location and Ori", 
       y = "Proportion of Correct Responses",
       fill = "Ori") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


