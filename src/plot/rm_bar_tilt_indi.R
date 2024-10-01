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
#my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct", "Overestimate"))


# list all participants
unique_subids <- unique(my_data$SubID)


# -------------some descriptive stats---------------

# -------------------------RM task---------------

# summarize the counts of RM under different oriented line locations
rm_distribution_by_pp <- my_data %>%
  group_by(SubID, oriented_line_location, RM, correct_num) %>%
  summarise(count = n()) %>%
  ungroup()


print(rm_distribution_by_pp)

# plot RM distribution under different oriented line locations
my_plot <- ggplot() +
  geom_bar(
    data = rm_distribution_by_pp,
    aes(
      x = oriented_line_location, 
      y = count, 
      fill = RM
      ),
    stat = "identity", 
    position = "dodge"
  ) +
  
  labs(title = "Distribution of RM by Oriented Line Location", 
       x = "Oriented Line Location", 
       y = "Count of RM Responses") +
  
  scale_fill_manual(
    labels = c( "RM", "Correct"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  facet_wrap(correct_num ~ SubID, scales = "free_y") + 
  theme_minimal()


my_plot

# -------------------ori task------------------

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


#------------------fit CDF for each pp--------------------------

setsize = 3

my_data <- my_data %>% 
  filter(correct_num == setsize)

df_to_fit_by_pp <- my_data %>%
  dplyr::group_by(ori, RM, oriented_line_location, SubID) %>%
  dplyr::summarize(
    total_responses = dplyr::n(),
    right_responses = sum(rotation_response.keys == 'right'),
    percentage_right = (right_responses / total_responses) * 100,
    .groups = 'drop'
  )


col_to_keep <- c("ori", "RM", "oriented_line_location", "SubID", "percentage_right")
df_to_fit_by_pp <- df_to_fit_by_pp[, col_to_keep]



# for each pp, intical try not converged; try other initial estimates
fit_cumulative_gaussian_by_pp <- function(data) {
  tryCatch({
    minpack.lm::nlsLM(
      percentage_right ~ pnorm(ori, mean, sd) * 100,
      data = data,
      start = list(mean = median(data$ori, na.rm = TRUE), sd = sd(data$ori, na.rm = TRUE)),
      control = minpack.lm::nls.lm.control(maxiter = 1000)
    )
  }, error = function(e) NULL)
}

fit_cumulative_gaussian_safe <- purrr::possibly(fit_cumulative_gaussian_by_pp, NULL)

fit_res_by_pp <- df_to_fit_by_pp %>%
  dplyr::group_by(SubID, RM, oriented_line_location) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    fit = purrr::map(data, fit_cumulative_gaussian_safe),
    augmented = purrr::map2(data, fit, ~ if (!is.null(.y)) broom::augment(.y, newdata = .x) else NULL),
    pse = purrr::map_dbl(fit, ~ if (!is.null(.x) && length(coef(.x)) > 0) coef(.x)["mean"] else NA_real_),
    sd = purrr::map_dbl(fit, ~ if (!is.null(.x) && length(coef(.x)) > 0) coef(.x)["sd"] else NA_real_)
  )


fitted_data_by_pp <- fit_res_by_pp %>%
  dplyr::select(SubID, RM, oriented_line_location, augmented) %>%
  tidyr::unnest(augmented, keep_empty = TRUE)


my_plot_each_pp <- ggplot() +
  geom_point(
    data = df_to_fit_by_pp, 
    aes(x = ori, 
        y = percentage_right, 
        color = RM,
        group = RM),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8
  ) +
  
  geom_line(
    data = fitted_data_by_pp, 
    aes(x = ori, y = .fitted,
        color = RM,
        group = RM),
    linetype = "dashed") +
  
  facet_wrap(~ SubID + oriented_line_location) +
  
  labs(
    x = "Orientation",
    y = "Percentage 'right' responses",
    color = "Trial types"
  ) +
  
  geom_text(
    data = df_to_fit_by_pp,
    aes(x = ori, 
        y = percentage_right, 
        label = total_responses),
    position = position_dodge(0.8), 
    vjust = -0.5, 
    size = 3,  
    color = "black"
  ) +
  
  scale_x_continuous(breaks = c(-10, -4, -2, 2, 4, 10),
                     labels = c("-10", "-4", "-2", "2", "4", "10"), limits = c(-10.5, 10.5))+
  
  scale_y_continuous(limits = c(0, 120)) + 
  
  
  scale_color_manual(
    labels = c( "RM", "Correct"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

my_plot_each_pp

# ----------------------group participants----------------------------

df2<- my_data %>%
  dplyr::group_by(ori, RM, oriented_line_location, SubID) %>%
  dplyr::summarize(
    total_responses = dplyr::n(),
    .groups = 'drop'
  )

# get the abs ori
df2$ori <- abs(df2$ori)

# Group the data and put total responses for RM and Correct trials

rm_total <- df2 %>%
  filter(RM == 'RM') %>%
  group_by(SubID, ori, oriented_line_location) %>%
  summarise(RM_total = sum(total_responses, na.rm = TRUE))

correct_total <- df2 %>%
  filter(RM == 'Correct') %>%
  group_by(SubID, ori, oriented_line_location) %>%
  summarise(Correct_total = sum(total_responses, na.rm = TRUE))

# merge
merged_df <- full_join(rm_total, correct_total, by = c("SubID", "ori", "oriented_line_location"))


# diff number of RM trials and number of correct trials

merged_df <- merged_df %>%
  mutate(Difference = RM_total - Correct_total)


ggplot(merged_df, aes(x = Difference, y = as.factor(SubID), color = as.factor(ori), shape = as.factor(ori))) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Clustering of SubID Based on RM - Correct Total Responses",
       x = "Total Responses Difference (number of RM trials - Correct trials)", 
       y = "SubID") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))+
  theme_minimal() +
  theme(legend.title = element_blank()) +
  facet_wrap(~ oriented_line_location)  

