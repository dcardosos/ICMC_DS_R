library(tidyverse)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv"
da <- readr::read_csv(url)

# how many bird we are in the urban_rural

da %>% 
  dplyr::count(urban_rural)

# top birds na

top_birds <- 
  da %>% 
  dplyr::filter(is.na(urban_rural)) %>% 
  dplyr::arrange(-bird_count) %>% 
  dplyr::slice_max(bird_count, n = 15) %>% 
  dplyr::pull(bird_type)


# eda

bird_parsed <-
  da %>% 
  dplyr::filter(!is.na(urban_rural),
                bird_type %in% top_birds) %>% 
  dplyr::group_by(urban_rural, bird_type) %>% 
  dplyr::summarise(bird_count = mean(bird_count), .groups = "drop")

p1 <- 
  bird_parsed %>% 
  ggplot(aes(bird_count, bird_type)) +
  geom_segment(
    data = bird_parsed %>% pivot_wider(names_from = urban_rural,
                                       values_from = bird_count),
    aes(x = Rural, xend = Urban, y = bird_type, yend = bird_type),
    alpha = 0.7, color = "gray70", size = 1.5
  ) +
  geom_point(aes(color = urban_rural), size = 3) +
  scale_x_continuous(labels  = scales::percent) +
  theme_minimal() +
  labs(x = "Probability of seeing bird", y = NULL, color = NULL)


# data for modeling

bird_df <-
  da %>% 
  filter(!is.na(urban_rural),
         bird_type %in% top_birds) %>% 
  mutate(bird_count = if_else(bird_count > 0, "bird", "no bird")) %>% 
  mutate_if(is.character, as.factor)



# build a first model -----------------------------------------------------
library(tidymodels)

set.seed(123)

# training and testing set - prop 3/4
bird_split <- initial_split(bird_df, strata = bird_count)

bird_train <- training(bird_split)
bird_test <- testing(bird_split)

set.seed(234)
bird_folds <- vfold_cv(bird_train, strata = bird_count)
bird_folds

# logistic regression
glm_spec <- logistic_reg()

# feature engineering
rec_basic <- 
  recipe(bird_count ~ urban_rural + bird_type, data = bird_train) %>% 
  step_dummy(all_nominal_predictors()) 

# workflow
wf_basic <- workflow(rec_basic, glm_spec)

# fitting
doParallel::registerDoParallel()
ctrl_preds <- control_resamples(save_pred = TRUE)
rs_basic <- fit_resamples(wf_basic, bird_folds, control = ctrl_preds)


# augment
augment(rs_basic) %>% 
  roc_curve(bird_count, .pred_bird) %>% 
  autoplot()

# add interactions --------------------------------------------------------

rec_interact <- 
  rec_basic %>%
  step_interact( ~ starts_with("urban_rural"):starts_with("bird_type")) 


wf_interact <- workflow(rec_interact, glm_spec)
rs_interact <- fit_resamples(wf_interact, bird_folds, control = ctrl_preds)

augment(rs_interact) %>% 
  roc_curve(bird_count, .pred_bird) %>% 
  autoplot()


# evaluate model on test data ---------------------------------------------

bird_fit <- fit(wf_interact, bird_train)

## on new data
new_bird_data <- 
  tibble(bird_type = top_birds) %>% 
  crossing(urban_rural = c("Urban", "Rural"))

bird_preds <- 
  augment(bird_fit, new_bird_data) %>% 
  bind_cols(
    predict(bird_fit, new_bird_data, type = "conf_int") 
  )

p2 <- 
  bird_preds %>% 
  ggplot(aes(.pred_bird, bird_type, color = urban_rural)) +
  geom_errorbar(
    aes(xmin = .pred_lower_bird,
        xmax = .pred_upper_bird),
    width = 0.4, size = 1.2, alpha = 0.5
  ) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels  = scales::percent) +
  theme_minimal() +
  labs(x = "Predicted probability of seeing bird", y = NULL, color = NULL)


library(patchwork)

p1 + p2















