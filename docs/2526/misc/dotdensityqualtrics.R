library(tidyverse)
library(psych)
# you can use this package to read in data straight from Qualtrics, but it gets linked to your
# specific qualtrics account, so I can't share it with you. 
# if you're interested doing this for your own projects, see 
# https://cran.r-project.org/web/packages/qualtRics/vignettes/qualtRics.html
# library(qualtRics)
# dotdat <- fetch_survey("SV_2nRRjGfjCSy80uO")

# i've put the data here for you to access:  
dotdat <- read_csv("https://uoepsy.github.io/dapr3/2526/misc/dotdensityqualtrics.csv")


# here's the test retest stuff:  
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

# group info
# scale1 = no increments on the scale, just the far left and far right
# scale2 = 7 increments along the scale

testretest1 <- testretest |> filter(scale==1)
testretest2 <- testretest |> filter(scale==2)

# reliability
ICC(testretest1[,3:4])
ICC(testretest2[,3:4])

plot(testretest1[,3:4])
plot(testretest2[,3:4])

# this is the complete opposite of what i expected! i was expecting the scale with the labelled
# increments to be more reliable! gahh, ah well. 
# more evidence that we should aways assess reliability of measures whenever we use them!!  


# here's the inter-rater reliability

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

# ICC is quite low - 0.3
ICC(hdat[,2:ncol(hdat)])


# note that if we DIDN'T have raymond kertezc in the sample, then 
# our reliability is even lower..
psych::ICC(hdat[-c(11),2:ncol(hdat)])

# this actually makes sense. the scale is more reliable for measuring the 
# historical significance of people like raymond kertezc, because essentially you
# all correctly identified that he didn't do anything (we just made him up!)
hdat |> pivot_longer(2:last_col(), names_to="rater", values_to="rating") |>
  ggplot(aes(x=name,y=rating))+
  geom_boxplot()+
  coord_flip()





