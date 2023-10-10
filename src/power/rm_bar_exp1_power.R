# ---------------libraries------------------------
library(dplyr)
library(tidyverse)
library(lme4)
library(mixedpower)

# ---------------read data------------------------

exp <- "exp1"

# set working path
setwd("D:/OneDrive/projects/RM_adjust_bar/data/exp1/")
path <- "D:/OneDrive/projects/RM_adjust_bar/data/exp1/"

# cols
col_rm_bar_exp1 <- c(
  "amount",
  "center_to_center",
  "edge_to_edge",
  "w",
  "stim_length",
  "trial_type",
  "blocks.thisN",
  "response",
  "starting_width_deg",
  "starting_space_deg",
  "response_width_degree",
  "response_spacing_degree",
  "participant",
  "age",
  "gender"
)

# list to store
dfs <-list()

list_csv_files <- list.files(path = path, pattern = ".csv")

# loop each file in the 
for (file in list_csv_files) {
  # read csv
  df <- read.csv(paste0(path, file), stringsAsFactors =  FALSE)
  
  # get subset based on kept cols
  df_subset <- df[, col_rm_bar_exp1, drop = FALSE]
  
  # add to the list
  dfs <- append(dfs, list(df_subset))
}

my_data <- do.call(rbind, dfs)

# -------------------------preprocess--------------------------------
# drop rows that contains NA or empty

my_data <- my_data[!is.na(my_data$blocks.thisN), ]
my_data <- my_data[!is.na(my_data$amount), ]


# convert response to num
my_data$response_num <- as.numeric(str_extract(my_data$response, "\\d+"))

# add DV
my_data$num_DV <- my_data$response_num - my_data$amount
my_data$width_DV <- my_data$response_width_degree - my_data$w

# add trial type

my_data$is_RM <- ifelse(my_data$num_DV < 0, "RM", "non_RM")

# -----------------------LMM----------------------------------------------

my_data$is_RM <- as.factor(my_data$is_RM)
my_data$amount <- as.factor(my_data$amount)

model <-
  lmer(width_DV ~ is_RM * amount +  (1 | participant), data = my_data)

model2 <-
  lmer(width_DV ~ is_RM + amount +  (1 | participant), data = my_data)

anova(model, model2)

# ----------------------power---------------------------------------------

power <- mixedpower(model = model, data = my_data,
                    fixed_effects = c("is_RM", "amount"),
                    simvar = "participant", steps = c(20, 25, 30, 40, 50),
                    critical_value = 2, n_sim = 1000)


power2 <- mixedpower(model = model2, data = my_data,
                    fixed_effects = c("is_RM", "amount"),
                    simvar = "participant", steps = c(20, 25, 30, 40, 50),
                    critical_value = 2, n_sim = 1000)
