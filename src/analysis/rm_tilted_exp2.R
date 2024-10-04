# ------------library-----------------
library(dplyr)
library(ggplot2)


# ------------read data---------------
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")
my_data <- readxl::read_excel(file.choose()) # df_tilt.xlsx


# -----------remove participants who were not able to do the task--------
failed_list <- c("507","516", "517", "518", "525")


# list all participants
unique_subids <- unique(my_data$SubID)

my_data <- my_data %>%
  filter(!SubID %in% failed_list)


# factors and format
str(my_data)
my_data$ori_correct <- as.numeric(as.character(my_data$ori_correct))

my_data <- my_data %>% 
  filter(RM %in% c('RM', "Correct"))

my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct"))
#my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct", "Overestimate"))
my_data$abs_ori <- abs(my_data$ori_deg)




# proportion_correct 
data_by_participant <- my_data %>% 
  dplyr::group_by(SubID,abs_ori, oriented_line_location, RM, correct_num) %>% 
  summarise(
    n = n(),
    corr_ori_resp = sum(ori_correct ==1),
    proportion_correct = corr_ori_resp / n
  )

ggplot(data_by_participant, aes(x = abs_ori, 
                                     y = proportion_correct, 
                                     fill = as.factor(abs_ori))) +
  geom_bar(stat = "identity", position = "dodge") +
  
  facet_wrap(~ SubID + oriented_line_location + correct_num + RM) +  
  labs(title = "Proportion of Correct Responses (ori_correct) by Oriented Line Location and Setsize for Each Participant", 
       x = "Abs(Ori)", 
       y = "Proportion of Correct Responses",
       fill = "Ori") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data_by_participant <- my_data %>% 
  dplyr::group_by(SubID, abs_ori, RM) %>% 
  summarise(
    n = n(),
    corr_ori_resp = sum(ori_correct ==1),
    proportion_correct = corr_ori_resp / n
  )

ggplot(data_by_participant, aes(x = abs_ori, 
                                y = proportion_correct, 
                                fill = as.factor(abs_ori))) +
  geom_bar(stat = "identity", position = "dodge") +
  
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  
  facet_wrap(~ SubID + RM) +  
  labs(title = "Proportion of Correct Responses (ori_correct) by Trial Types(RM vs. correct)", 
       x = "Abs(Ori) ", 
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
  dplyr::group_by(abs_ori, RM, oriented_line_location, SubID) %>%
  dplyr::summarize(
    total_responses = dplyr::n(),
    corr_ori_resp = sum(ori_correct == 1),
    proportion_correct = corr_ori_resp / total_responses, 
    .groups = 'drop'
  )


my_plot_check_total_resp <- ggplot() +
  geom_point(
    data = df_to_fit_by_pp,
    aes(
      x = abs_ori,
      y = proportion_correct,
      color = RM,
      group = RM
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8
  ) +
  
  geom_text(
    data = df_to_fit_by_pp,
    aes(x = abs_ori, 
        y = proportion_correct, 
        label = total_responses),
    position = position_dodge(0.8), 
    vjust = -0.5, 
    size = 3,  
    color = "black"
  ) +

  
  scale_x_continuous(breaks = c( 2, 4, 6, 8, 10),
                     labels = c("2", "4", "6", "8","10"), limits = c(1, 10.5))+
  
  # scale_y_continuous(limits = c(0, 120)) + 
  
  
  scale_color_manual(
    labels = c( "RM", "Correct"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  facet_wrap( ~SubID + oriented_line_location, scales = "free_y") + 
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



my_plot_check_total_resp


df_to_fit_by_pp <- df_to_fit_by_pp %>% 
  filter(total_responses >= 5)
  

col_to_keep <- c("abs_ori", "RM", "oriented_line_location", "SubID", "total_responses","proportion_correct")
df_to_fit_by_pp <- df_to_fit_by_pp[, col_to_keep]


# for each pp, intical try not converged; try other initial estimates
fit_cumulative_gaussian_by_pp <- function(data) {
  tryCatch({
    minpack.lm::nlsLM(
      proportion_correct ~ pnorm(abs_ori, mean, sd),
      data = data,
      start = list(mean = 6, sd = sd(data$abs_ori, na.rm = TRUE)),
      lower = c(2, 0), 
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
    threshold_50 = purrr::map_dbl(fit, ~ if (!is.null(.x) && length(coef(.x)) > 0) coef(.x)["mean"] else NA_real_),
    sd = purrr::map_dbl(fit, ~ if (!is.null(.x) && length(coef(.x)) > 0) coef(.x)["sd"] else NA_real_),
    
    # threshold for 75% correct: mean + 0.674 * sd
    threshold = purrr::map2_dbl(threshold_50, sd, ~ if (!is.na(.x) && !is.na(.y)) .x + 0.674 * .y else NA_real_)
  )
  
# filter out rows where data is NULL
fit_res_by_pp_clean <- fit_res_by_pp %>%
  filter(purrr::map_lgl(fit, ~ !is.null(.)))

# filter out rows where the nested data has fewer than 5 rows (no enough data point to fit)
fit_res_by_pp_clean <- fit_res_by_pp_clean %>%
  filter(purrr::map_int(data, ~ sum(!is.na(.x$abs_ori))) >= 5)


fit_res_by_pp_clean <- fit_res_by_pp_clean %>%
  dplyr::select(SubID, RM, oriented_line_location,threshold, threshold_50, sd, augmented) %>%
  tidyr::unnest(augmented, keep_empty = TRUE)


# cal the residuals
fit_res_by_pp_clean <- fit_res_by_pp_clean %>%
  dplyr::mutate(
    .resid = proportion_correct - .fitted  
  )


# Plot fitted values vs. residuals
ggplot(fit_res_by_pp_clean, aes(x = .fitted, y = .resid)) +
  geom_point(color = "blue") +  # Plot the residuals as points
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
  labs(title = "Fitted Values vs. Residuals", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


my_plot_each_pp <- ggplot() +
  geom_point(
    data = fit_res_by_pp_clean, 
    aes(x = abs_ori, 
        y = proportion_correct, 
        color = RM,
        group = RM),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8
  ) +
  
  geom_line(
    data = fit_res_by_pp_clean, 
    aes(x = abs_ori, y = .fitted,
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
    data = fit_res_by_pp_clean,
    aes(x = abs_ori, 
        y = proportion_correct, 
        label = total_responses),
    position = position_dodge(0.8), 
    vjust = -0.5, 
    size = 3,  
    color = "black"
  ) +
  
  scale_x_continuous(breaks = c( 2, 4, 6, 8, 10),
                     labels = c("2", "4", "6", "8" ,"10"), limits = c(1, 10.5))+
  
  # scale_y_continuous(limits = c(0, 120)) + 
  
  
  scale_color_manual(
    labels = c( "RM", "Correct"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

my_plot_each_pp



# # fit logistic
# 
# logistic_function <- function(x, threshold, slope) {
#   return(1 / (1 + exp(-(x - threshold) / slope)))
# }
# 
# 
# fit_logistic_by_pp <- function(data) {
#   tryCatch({
#     minpack.lm::nlsLM(
#       proportion_correct ~ logistic_function(abs_ori, threshold, slope),
#       data = data,
#       start = list(threshold = median(data$abs_ori, na.rm = TRUE), slope = 1),
#       control = minpack.lm::nls.lm.control(maxiter = 1000)
#     )
#   }, error = function(e) NULL)
# }
# 
# fit_logistic_safe <- purrr::possibly(fit_logistic_by_pp, NULL)
# 
# 
# fit_res_by_pp <- df_to_fit_by_pp %>%
#   dplyr::group_by(SubID, RM, oriented_line_location) %>%
#   tidyr::nest() %>%
#   dplyr::mutate(
#     fit = purrr::map(data, fit_logistic_safe),
#     augmented = purrr::map2(data, fit, ~ if (!is.null(.y)) broom::augment(.y, newdata = .x) else NULL),  
#     threshold = purrr::map_dbl(fit, ~ if (!is.null(.x)) coef(.x)["threshold"] else NA_real_),  
#     slope = purrr::map_dbl(fit, ~ if (!is.null(.x)) coef(.x)["slope"] else NA_real_)  
#     
#   )
#   
# 
# 
# fitted_data_by_pp <- fit_res_by_pp %>%
#   dplyr::select(SubID, RM, oriented_line_location,threshold, slope, augmented) %>%
#   tidyr::unnest(augmented, keep_empty = TRUE)



#----------threshold-------------------------------

#fitted_data_by_pp <- fitted_data_by_pp[!is.na(fitted_data_by_pp$threshold),]


data_threshold <- fit_res_by_pp_clean %>% 
  group_by(SubID, oriented_line_location, RM) %>% 
  summarize(
    n = n(),
    threshold = mean(threshold, na.rm = TRUE)
  )

data_threshold_across_pp <- data_threshold %>% 
  group_by(oriented_line_location, RM) %>% 
  summarise(
    n = n(),
    threshold_mean = mean(threshold),
    threshold_sd= sd(threshold, na.rm = TRUE)
  ) %>% 
  mutate(
    threshold_SEM = threshold_sd / sqrt(n),
    threshold_CI = threshold_SEM* qt((1 - 0.05) / 2 + .5, n - 1)
    )

my_plot_threshold <- ggplot() +
  geom_point(
    data = data_threshold_across_pp,
    aes(
      x = oriented_line_location,
      y = threshold_mean,
      color= RM,
      group = RM
    ),
    position = position_dodge(1),
    alpha = 0.8
  ) +
  
  
  geom_errorbar(
    data = data_threshold_across_pp,
    aes(
      x = oriented_line_location,
      y = threshold_mean,
      color = RM,
      group = RM,
      ymin = threshold_mean - threshold_CI,
      ymax = threshold_mean + threshold_CI
    ),
    
    linewidth  = 0.5,
    width = .00,
    alpha = 0.8,
    position = position_dodge(1)
  ) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  
  labs(y = "Threshold", x = "Tilted line location") +
  
  
  scale_color_manual(
    labels = c( "RM trials", "Correct trials"),
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
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) 


my_plot_threshold


data_threshold2 <- fit_res_by_pp_clean %>% 
  group_by(SubID, RM) %>% 
  summarize(
    n = n(),
    threshold = mean(threshold, na.rm = TRUE)
  )

data_threshold_across_pp2 <- data_threshold2 %>% 
  group_by( RM) %>% 
  summarise(
    n = n(),
    threshold_mean = mean(threshold),
    threshold_sd= sd(threshold, na.rm = TRUE)
  ) %>% 
  mutate(
    threshold_SEM = threshold_sd / sqrt(n),
    threshold_CI = threshold_SEM* qt((1 - 0.05) / 2 + .5, n - 1)
  )

my_plot_threshold2 <- ggplot() +
  geom_point(
    data = data_threshold_across_pp2,
    aes(
      x = RM,
      y = threshold_mean,
      color= RM,
      group = RM
    ),
    position = position_dodge(1),
    alpha = 0.8
  ) +
  
  
  geom_errorbar(
    data = data_threshold_across_pp2,
    aes(
      x = RM,
      y = threshold_mean,
      color = RM,
      group = RM,
      ymin = threshold_mean - threshold_CI,
      ymax = threshold_mean + threshold_CI
    ),
    
    linewidth  = 0.5,
    width = .00,
    alpha = 0.8,
    position = position_dodge(1)
  ) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  
  labs(y = "Threshold", x = "Tilted line location") +
  
  
  scale_color_manual(
    labels = c( "RM trials", "Correct trials"),
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
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold")
  ) 


my_plot_threshold2


ggplot(data_threshold, aes(x = RM, y = threshold, color = SubID)) +
  geom_point(size = 3) +
  geom_line(aes(group = SubID)) + 
  facet_wrap(~oriented_line_location) +
  labs(title = "Thresholds by Condition", x = "Condition (RM)", y = "Threshold") +
  theme_minimal()

 # LMM threshold

contrasts(fit_res_by_pp_clean$RM) <- matrix(c(0.5, -0.5), ncol = 1)

levels(fit_res_by_pp_clean$RM)

m1 <- lme4::lmer(threshold ~ RM + oriented_line_location  + (1|SubID),
                 data = fit_res_by_pp_clean)

m2 <- lme4::lmer(threshold ~ RM * oriented_line_location  + (1|SubID),
                 data = fit_res_by_pp_clean)

m3 <- lme4::lmer(threshold ~ oriented_line_location  + (1|SubID),
                 data = fit_res_by_pp_clean)



anova(m1, m3)

anova(m1, m2)


summary(m1)

sjPlot::tab_model(
  m1,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 

# pairwise comparisons
emms <- emmeans::emmeans(m2,
                         list(pairwise ~ RM | oriented_line_location),
                         adjust = "tukey")

summary(emms, infer = TRUE)


# ------------------clustering participants------------------

df_clustering_pp<- my_data %>%
  dplyr::group_by(abs_ori, RM, SubID) %>%
  dplyr::summarize(
    total_responses = dplyr::n(),
    .groups = 'drop'
  )

# Group the data and put total responses for RM and Correct trials
rm_total <- df_clustering_pp %>%
  filter(RM == 'RM') %>%
  group_by(SubID, abs_ori) %>%
  summarise(RM_total = sum(total_responses, na.rm = TRUE))

correct_total <- df_clustering_pp %>%
  filter(RM == 'Correct') %>%
  group_by(SubID, abs_ori) %>%
  summarise(Correct_total = sum(total_responses, na.rm = TRUE))

# merge
merged_df <- full_join(rm_total, correct_total, by = c("SubID", "abs_ori"))

merged_df <- merged_df %>%
  mutate(Difference = RM_total - Correct_total)

merged_df <- merged_df %>% 
  filter(!is.na(Difference))

ggplot(merged_df, aes(x = Difference, y = as.factor(SubID), color = as.factor(abs_ori), shape = as.factor(abs_ori))) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Clustering of SubID Based on RM - Correct Total Responses",
       x = "Total Responses Difference (number of RM trials - Correct trials)", 
       y = "SubID") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))+
  theme_minimal() +
  theme(legend.title = element_blank()) 



merged_df_across_ori <- merged_df %>% 
  group_by(SubID) %>% 
  summarise(
    n = n(),
    mean_response_difference = mean(Difference),
    sd = sd(Difference)
  ) %>% 
  mutate(
    sem = sd / sqrt(n),
    CI = sem * qt((1 - 0.05) / 2 + .5, n - 1),
  )

my_plot_clustering_pp <- ggplot() +
  geom_point(
    data = merged_df_across_ori,
    aes(
      x = mean_response_difference, 
      y = as.factor(SubID)
      )
  ) +
  
  geom_point(
    data = merged_df,
    aes(
      x = Difference, 
      y = as.factor(SubID),
      color = as.factor(abs_ori), 
      shape = as.factor(abs_ori)
    )
  )+ 
 scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))+
  
  geom_errorbar(
    data = merged_df_across_ori, 
    aes(
      x = mean_response_difference,
      y = as.factor(SubID),
      xmin = mean_response_difference - CI,
      xmax = mean_response_difference + CI),
    color = "black",
    size  = 0.8,
    width = .00) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Clustering of SubID Based on RM - Correct Total Responses",
       x = "Total Responses Difference (number of RM trials - Correct trials)", 
       y = "SubID") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

my_plot_clustering_pp
