# woo clap stuff
library(tidyverse)

## w2 ----
df = tibble(
  group = paste0("group_",1:10),
  munro = sample(c("ben nevis","ben lomond","ben chonzie","ben vorlich","ben macdui","creag meagaidh"),10,replace=T,prob = c(.3,.2,.1,.2,.1,.1)),
  group_size = replicate(10,rbinom(1,20,.15)+1),
  minutes_taken = round(rnorm(10, 110+group_size*14,30)/5)*5
)
gt::gt(df)

tibble(
  participant = rep(paste0("ppt_",1:4),e=6),
  statement = rep(c(LETTERS[1:6],rev(LETTERS[1:6])),2),
  condition = rep(rep(c("human","AI"),e=3),4),
  trust = round(rnorm(24, 40+(condition=="human")*5, 5))
)  |> group_by(participant) |>
  mutate(
    order = sample(1:6)
  )|> arrange(participant, order) |> ungroup() |>
  select(-order) |> gt::gt()

tibble(
  participant = paste0("ppt:",replicate(10, paste0(sample(letters,4,T),collapse=""))),
  city = sample(c("glasgow","edinburgh","birmingham","manchester","liverpool","london"),
                10, replace = T, prob = c(1,1,2,2,2,4)),
  type = sample(c("tee-totaller","occasional","regular","binge"), 10, T, prob=c(2,5,3,2)),
  GAD7 = replicate(10, sum(rbinom(7,5,prob=.3)))
) |> gt::gt()


tibble(
  school = c("SchoolA"),
  ppt = rep(c("P01","P02"),e=5),
  age = rep(11:15,2),
  selfconf = round(age*.2 + (ppt=="P01")*3 + rnorm(10))
)





