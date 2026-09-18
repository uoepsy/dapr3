
library(lavaan)
library(tidyverse)
library(semPlot)

#eseed=round(runif(1,1,1e6))
set.seed(159661)

wqitems = c(
  "How would you rate your quality of life?",
  "How satisfied are you with your health?",
  "To what extent do you feel that (physical) pain prevents you from doing what you need to do? ",
"How much do you need any medical treatment to function in your daily life?",
"How much do you enjoy life?",
"To what extent do you feel your life to be meaningful?",
"How well are you able to concentrate?",
"How safe do you feel in your daily life?",
"How healthy is your physical environment?",
"Do you have enough energy for everyday life?",
"Are you able to accept your bodily appearance?",
"Have you enough money to meet your needs?",
"How available to you is the information that you need in your day-to-day life?",
"To what extent do you have the opportunity for leisure activities?",
"How well are you able to get around?",
"How satisfied are you with your sleep?",
"How satisfied are you with your ability to perform your daily living activities?",
"How satisfied are you with your capacity for work?",
"How satisfied are you with yourself?",
"How satisfied are you with your personal relationships?",
"How satisfied are you with your sex life?",
"How satisfied are you with the support you get from your friends?",
"How satisfied are you with the conditions of your living place?",
"How satisfied are you with your access to health services?",
"How satisfied are you with your transport?",
"How often do you have negative feelings such as blue mood, despair, anxiety, depression?")
  
wqit <- tibble(
  q = paste0("q",1:26),
  domain = c(NA,NA,"Physical","Physical","Psychological","Psychological","Psychological",
             "Environmental","Environmental","Physical","Psychological","Environmental",
             "Environmental","Environmental","Physical","Physical","Physical","Physical",
             "Psychological","Social","Social","Social","Environmental","Environmental",
             "Environmental","Psychological"),
  wording = wqitems
)

# 
# 
# 
# 
# whoqol_pop_adults <- '
#   Phys =~ 0.62*q3 + 0.58*q4 + 0.65*q10 + 0.60*q15 + 0.58*q16 + 0.64*q17 + 0.68*q18
#   Psych =~ 0.65*q5 + 0.68*q6 + 0.60*q7 + 0.55*q11 + 0.64*q19 + 0.58*q26
#   Social =~ 0.68*q20 + 0.60*q21 + 0.65*q22
#   Env =~ 0.68*q8 + 0.55*q9 + 0.62*q12 + 0.58*q13 + 0.60*q14 + 0.65*q23 + 0.58*q24 + 0.55*q25
#   Phys ~~ 0.55*Psych
#   Phys ~~ 0.40*Social
#   Phys ~~ 0.50*Env
#   Psych ~~ 0.48*Social
#   Psych ~~ 0.52*Env
#   Social ~~ 0.45*Env
#   q3 ~~ .25*q4
#   q4 ~~ .25*q26
#   q3 ~~ .25*q26
# '
# 
# df_adults_raw <- simulateData(whoqol_pop_adults, standardized = TRUE, sample.nobs = 300)
# 
# df_adults <- df_adults_raw |>
#   mutate(across(everything(), ~ as.numeric(cut(.x, breaks = 5, labels = 1:5)))) |>
#   mutate(across(c("q3", "q4", "q26"), ~ 6 - .x))
# 
# # q18, q21, q25, q12
# whoqol_pop_older <- '
#   Phys =~ 0.60*q3 + 0.55*q4 + 0.62*q10 + 0.65*q15 + 0.55*q16 + 0.62*q17 + 0.15*q18
#   Psych =~ 0.62*q5 + 0.70*q6 + 0.52*q7 + 0.50*q11 + 0.60*q19 + 0.55*q26
#   Social =~ 0.72*q20 + 0.20*q21 + 0.68*q22
#   Env =~ 0.62*q8 + 0.58*q9 + 0.32*q12 + 0.55*q13 + 0.52*q14 + 0.62*q23 + 0.60*q24 + 0.75*q25
#   Phys ~~ 0.50*Psych
#   Phys ~~ 0.35*Social
#   Phys ~~ 0.58*Env
#   Psych ~~ 0.42*Social
#   Psych ~~ 0.48*Env
#   Social ~~ 0.40*Env
#   q3 ~~ .25*q4
#   q4 ~~ .25*q26
#   q3 ~~ .25*q26
# '
# 
# df_older_raw <- simulateData(whoqol_pop_older, standardized = TRUE, sample.nobs = 300)
# df_older <- df_older_raw |>
#   mutate(across(everything(), ~ as.numeric(cut(.x, breaks = 5, labels = 1:5)))) |>
#   mutate(across(c("q3", "q4", "q26"), ~ 6 - .x))
# 
# 
# whoqol_mod <- '
#   Phys =~ q3 + q4 + q10 + q15 + q16 + q17 + q18
#   Psych =~ q5 + q6 + q7 + q11 + q19 + q26
#   Social =~ q20 + q21 + q22
#   Env =~ q8 + q9 + q12 + q13 + q14 + q23 + q24 + q25
# '
# 
# # adults
# fit_adults <- cfa(whoqol_mod, data = df_adults)
# fitmeasures(fit_adults)[c("rmsea","srmr","cfi","tli")]
# modindices(fit_adults,sort=T) |> head(4)
# 
# # older
# fit_older <- cfa(whoqol_mod, data = df_older)
# fitmeasures(fit_older)[c("rmsea","srmr","cfi","tli")]
# modindices(fit_older,sort=T) |> head(4)
# 
# library(semPlot)
# semPaths(fit_adults,whatLabels="std",rotation=2)
# semPaths(fit_older,whatLabels="std",rotation=2)
# 
# # AVE
# standardizedsolution(fit_adults) |>
#   filter(op=="=~") |>
#   mutate(r2 = est.std^2) |>
#   group_by(lhs) |>
#   summarise(
#     mean(r2)
#   )
# standardizedsolution(fit_older) |>
#   filter(op=="=~") |>
#   mutate(r2 = est.std^2) |>
#   group_by(lhs) |>
#   summarise(
#     mean(r2)
#   )
# 
# dff <- bind_rows(adult = df_adults,older = df_older,.id="sample")
# modfull <- cfa(whoqol_mod, dff ,std.lv =TRUE, group="sample")
# modfullm <- cfa(whoqol_mod, dff ,std.lv =TRUE, group="sample",
#                 group.equal = "loadings")
# anova(modfull,modfullm)
# semTools::compareFit(modfull,modfullm) |> summary()
# 
# 
# 
# lavPredict(fit_adults) |>
#    as.data.frame() |>
#   mutate(
#     q1_latent = 0.40*Phys + 0.35*Psych + 0.15*Social + 0.20*Env + rnorm(n(), mean = 0, sd = 0.60),
#     q1 = as.numeric(cut(q1_latent, breaks = 5, labels = 1:5))
#   ) |>
#   pull(q1) -> q1
# 
# 
# whqb_a <- df_adults |> mutate(q1 = q1, .before=q3)
# whqb_b <- df_older

# 
# write_csv(whqb_a, file="../../data/whoqolbref-sampleA.csv")
# write_csv(whqb_b, file="../../data/whoqolbref-sampleB.csv")


