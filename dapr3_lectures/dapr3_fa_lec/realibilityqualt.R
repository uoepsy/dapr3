library(qualtRics)
library(tidyverse)
dotdat <- fetch_survey("SV_2nRRjGfjCSy80uO")

testretest <- dotdat |> select(starts_with("s1d"),starts_with("s2d")) |>
  mutate(id = 1:n()) |>
  pivot_longer(-id) |>
  filter(!is.na(value)) |>
  mutate(
    retest = grepl("retest",name)*1,
    name = substr(name,1,4),
    scale = substr(name,2,2),
  ) |> filter(grepl("d4", name)) |>
  select(scale,id,value,retest) |>
  pivot_wider(names_from=retest,values_from=value,names_prefix = "t")

testretest1 <- testretest |> filter(scale==1)
testretest2 <- testretest |> filter(scale==2)

hdat <- dotdat |> select(starts_with("Q22")) |>
  mutate(id=1:n()) |> 
  pivot_longer(-id) |>
  mutate(value = case_match(value,
                            "Almost None (Almost forgotten)" ~ 1, 
                            "Minor Local Impact (Only influenced a small area/field)" ~ 2,
                            "Notable Regional Impact (Known by most historians)" ~ 3,
                            "Moderate Global Impact (Changed one aspect of global culture)" ~ 4,
                            "High Global Impact (Fundamental shift in history)" ~ 5,
                            "Profound Global Impact (Redefined human civilization)" ~ 6, 
                            "World-Altering Significance (Legacy is undeniable and vast)" ~ 7
                            ),
         name = case_match(name,
                           "Q22_1" ~ "j caesar", "Q22_2" ~ "m gandhi", "Q22_3" ~ "g khan",
                           "Q22_4" ~ "m l king jr", "Q22_5" ~ "m curie", "Q22_6" ~ "s squarepants",
                           "Q22_7" ~ "n mandela", "Q22_8" ~ "confucius", "Q22_9" ~ "s bolivar",
                           "Q22_10" ~ "m yousafzai", "Q22_11" ~ "r kertezc", "Q22_12" ~ "a lincoln")
         ) |>
  pivot_wider(names_from=id,values_from=value,names_prefix = "rater_")






