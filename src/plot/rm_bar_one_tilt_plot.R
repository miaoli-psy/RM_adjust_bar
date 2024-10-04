# ---------------libraries------------------------
library(dplyr)
library(ggplot2)
# install.packages('minpack.lm')
library(minpack.lm)


# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/")

# read data
my_data <- readxl::read_excel(file.choose()) # data_tilt.xlsx


failed_list <- c("502", "507", "511", "516", "517", "518", "525")


my_data <- my_data %>%
  filter(!SubID %in% failed_list)

# factors
my_data <- my_data %>% 
  filter(RM %in% c('RM', "Correct"))

my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct"))
my_data$RM <- factor(my_data$RM , levels = c("RM", "Correct", "Overestimate"))


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


# plot for either set size 3 or set size 4
setsize = 4 

my_data <- my_data %>% 
  filter(correct_num == setsize)

data_by_pp <- my_data %>%
  group_by(ori, RM, oriented_line_location, SubID) %>%
  summarize(
    total_responses = n(),
    correct_responses = sum(rotation_response.keys == direction),
    percentage_correct = (correct_responses / total_responses) * 100,
    right_responses = sum(rotation_response.keys == "right"),
    percentage_right = (right_responses / total_responses) * 100
  )


data_across_pp <- data_by_pp %>%
  group_by(ori, RM, oriented_line_location) %>%
  summarize(
    n = n(),
    percentage_correct_mean = mean(percentage_correct),
    percentage_correct_sd= sd(percentage_correct),
    percentage_right_mean = mean(percentage_right),
    percentage_right_sd= sd(percentage_right)
  ) %>% 
  mutate(
    percentage_correct_SEM = percentage_correct_sd / sqrt(n),
    percentage_correct_CI = percentage_correct_SEM* qt((1 - 0.05) / 2 + .5, n - 1),
    percentage_right_SEM = percentage_right_sd / sqrt(n),
    percentage_right_CI = percentage_right_SEM* qt((1 - 0.05) / 2 + .5, n - 1)
  )

# data to fit
grand_mean_df <- data_by_pp %>%
  group_by(ori, RM, oriented_line_location) %>%
  summarize(
    mean_percentage_right = mean(percentage_right, na.rm = TRUE),
    .groups = 'drop'
  )

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

# fit a cumulative Gaussian CDF
fit_cumulative_gaussian <- function(data) {
  nlsLM(
    mean_percentage_right ~ pnorm(ori, mean, sd) * 100,
    data = data,
    start = list(mean = 0, sd = 1),
    control = nls.lm.control(maxiter = 1000)
  )
}

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


# now do the fitting on grand mean
fit_res <- grand_mean_df %>% 
  dplyr::group_by(RM, oriented_line_location) %>%
  tidyr::nest() %>%
  dplyr::mutate(fit = purrr::map(data, fit_cumulative_gaussian),
                augmented = purrr::map2(data, fit, ~ broom::augment(.y, newdata = .x)),
                pse = purrr::map_dbl(fit, ~ coef(.x)["mean"]),
                sd = purrr::map_dbl(fit, ~ coef(.x)["sd"]))

# unnest plotting
fitted_data <- fit_res %>%
  dplyr::select(RM, oriented_line_location, augmented) %>%
  tidyr::unnest(augmented)


# PSE and threshold (75% response level)
threshold_res <- fit_res %>%
  dplyr::mutate(threshold = pse + qnorm(0.75) * sd) %>%
  dplyr::select(RM, oriented_line_location, pse, threshold)



# now do the fitting on each participant

# fit robust to failure
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

threshold_results_by_pp <- fit_res_by_pp %>%
  dplyr::mutate(threshold = ifelse(!is.na(pse) & !is.na(sd), pse + qnorm(0.75) * sd, NA_real_)) %>%
  dplyr::select(SubID, RM, oriented_line_location, pse, threshold, sd)

# remove NA
threshold_results_by_pp <- threshold_results_by_pp[!is.na(threshold_results_by_pp$threshold), ]
# remove outliers
mean_threshold <- mean(threshold_results_by_pp$threshold, na.rm = TRUE)
std_threshold <- sd(threshold_results_by_pp$threshold, na.rm = TRUE)
upper_bound <- mean_threshold + 3 * std_threshold
lower_bound <- mean_threshold - 3 * std_threshold
threshold_results_by_pp <- threshold_results_by_pp[threshold_results_by_pp$threshold >= lower_bound & threshold_results_by_pp$threshold <= upper_bound, ]


# check CDF for each participant------------------

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
  
  scale_x_continuous(breaks = c(-10, -4, -2, 2, 4, 10),
                     labels = c("-10", "-4", "-2", "2", "4", "10"), limits = c(-10.5, 10.5))+
  
  scale_color_manual(
    labels = c( "RM trials", "Correct trials"),
    values = c( "#800074", "#298C8C"),
    name = ""
  ) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

my_plot_each_pp

# Percentage right response - ori, fit CDF-------------
my_plot <- ggplot() +
  geom_point(
    data = data_across_pp,
    aes(
      x = ori,
      y = percentage_right_mean,
      color = RM,
      group = RM
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8
  ) +
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = ori,
      y = percentage_right_mean,
      color = RM,
      group = RM,
      ymin = percentage_right_mean - percentage_right_CI,
      ymax = percentage_right_mean + percentage_right_CI
    ),
    
    linewidth  = 0.5,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.8)
  ) +
  
  geom_line(data = fitted_data,
            aes(x = ori, y = .fitted,
            color = RM,
            group = RM),
            linetype = "dashed") +
  
  geom_hline(yintercept = 50, linetype = "dashed") +
  
  labs(y = "Percentage 'right' response", x = "Orientation") +
  
  scale_x_continuous(breaks = c(-10, -8, -6,  -4, -2, 2, 4, 6, 8, 10),
                     labels = c("-10","-8", "-6",  "-4", "-2", "2", "4","6", "8", "10"), limits = c(-10.5, 10.5))+
  
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
  )   +
  
  facet_wrap(~ oriented_line_location, labeller = labeller(
    oriented_line_location =
      c(
        "inner" = "Inner",
        "middle" = "Middle",
        "outer" = "Outer"
      ))
  )

my_plot


# threshold-------------------------------------------------
threshold_results_across_pp <- threshold_results_by_pp %>% 
  group_by(RM, oriented_line_location) %>% 
  summarize(
    n = n(),
    threshold_mean = mean(threshold, na.rm = TRUE),
    threshold_sd= sd(threshold, na.rm = TRUE),
    sd_mean = mean(sd, na.rm = TRUE),
    sd_sd= sd(sd, na.rm = TRUE)
  ) %>% 
  mutate(
    threshold_SEM = threshold_sd / sqrt(n),
    threshold_CI = threshold_SEM* qt((1 - 0.05) / 2 + .5, n - 1),
    sd_SEM = sd_sd / sqrt(n),
    sd_CI = sd_SEM* qt((1 - 0.05) / 2 + .5, n - 1)
  )


my_plot2 <- ggplot() +
  geom_point(
    data = threshold_results_across_pp,
    aes(
      x = oriented_line_location,
      y = threshold_mean,
      color= RM,
      group = RM
    ),
    position = position_dodge(1),
    alpha = 0.8
  ) +
  
  
  geom_point(
    data = threshold_results_by_pp,
    aes(
      x = oriented_line_location,
      y = threshold,
      color = RM,
      group = RM
    ),
    position = position_dodge(1),
    alpha = 0.2
  ) +
   
  geom_errorbar(
    data = threshold_results_across_pp,
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
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  
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


my_plot2

# JND
my_plot2.1 <- ggplot() +
  geom_point(
    data = threshold_results_across_pp,
    aes(
      x = oriented_line_location,
      y = sd_mean,
      color= RM,
      group = RM
    ),
    position = position_dodge(1),
    alpha = 0.8
  ) +
  
  
  geom_point(
    data = threshold_results_by_pp,
    aes(
      x = oriented_line_location,
      y = sd,
      color = RM,
      group = RM
    ),
    position = position_dodge(1),
    alpha = 0.2
  ) +
  
  geom_errorbar(
    data = threshold_results_across_pp,
    aes(
      x = oriented_line_location,
      y = sd_mean,
      color = RM,
      group = RM,
      ymin = sd_mean - sd_CI,
      ymax = sd_mean + sd_CI
    ),
    
    linewidth  = 0.5,
    width = .00,
    alpha = 0.8,
    position = position_dodge(1)
  ) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "JND", x = "Tilted line location") +
  
  
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


my_plot2.1
# some infer stats
contrasts(threshold_results_by_pp$RM) <- matrix(c(-0.5, 0.5), ncol = 1)

model <- lme4::lmer(
  threshold ~ RM * oriented_line_location + (1|SubID), data = threshold_results_by_pp
)

summary(model)

# model details
sjPlot::tab_model(
  model,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)



