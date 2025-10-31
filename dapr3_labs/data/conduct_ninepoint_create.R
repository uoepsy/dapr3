# Elizabeth converts unexplained data from w08 lab into something resembling
# nine-point rating scale data

library(tidyverse)
dat <- read_csv('https://uoepsy.github.io/data/conduct_probs_scale.csv')

# Round the data to the nearest integer, so that it's plausibly rating scale values.
likert_dat <- dat |>
  mutate(across(breaking_curfew:threatening_others, c(function(x) round(x)+5 ))) |>
  select(id, breaking_curfew_1:threatening_others_1)

# Remove uniquifying number from colnames.
names(likert_dat) <- lapply(
  names(likert_dat),
  function(x) str_replace(x, '_1', '')
) |> unlist()

likert_dat |> 
  summary()

# Plot to sense check.
likert_dat |>
  pivot_longer(cols = breaking_curfew:threatening_others) |>
  ggplot(aes(x = value)) +
  geom_bar() +
  facet_wrap(~ name)

# Save new data.
write_csv(likert_dat, 'conduct_ninepoint.csv')

