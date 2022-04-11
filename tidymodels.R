library(tidymodels)

episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv") %>%
  filter(!is.na(uk_viewers))

episodes %>%
  ggplot(aes(first_aired, uk_viewers)) +
  geom_line(alpha = 0.8, size = 1.2, color = "midnightblue") +
  labs(x = NULL)

set.seed(123)
folds <- bootstraps(episodes, times = 100, strata = uk_viewers)
folds

who_rec <-
  recipe(uk_viewers ~ first_aired, data = episodes) %>%
  step_date(first_aired, features = "year") %>%
  step_holiday(first_aired,
               holidays = c("NewYearsDay", "ChristmasDay"),
               keep_original_cols = FALSE
  )

## not needed for modeling, but just to check how things are going:
prep(who_rec) %>% bake(new_data = NULL)

who_wf <- workflow(who_rec, linear_reg())
who_wf

fit_resamples(who_wf, folds)

ctrl_extract <- control_resamples(extract = extract_fit_engine)

doParallel::registerDoParallel()
set.seed(234)
who_rs <- fit_resamples(who_wf, folds, control = ctrl_extract)
who_rs

who_rs %>%
  select(id, .extracts) %>%
  unnest(.extracts) %>%
  mutate(coefs = map(.extracts, tidy)) %>%
  unnest(coefs) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(estimate, fill = term)) +
  geom_histogram(alpha = 0.8, bins = 12, show.legend = FALSE) +
  facet_wrap(vars(term), scales = "free")