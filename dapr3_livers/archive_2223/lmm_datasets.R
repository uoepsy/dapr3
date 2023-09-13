library(tidyverse)

#####
# LABS
#####

# BBALL HRV (rpt measures)
bball <-
  left_join(
    read_csv("https://uoepsy.github.io/data/basketballconditions.csv"),
    read_csv("https://uoepsy.github.io/data/bballhrv.csv") %>%
      pivot_longer(trial_1:trial_20, names_to = "trial_no", values_to = "hrv")
  ) %>%
  mutate(sub = factor(sub))
# LAA WELLBEING (cross sectional)
read_csv("https://uoepsy.github.io/data/LAAwellbeing.csv")
# WELLBEING WORK (longitudinal)
load(url("https://uoepsy.github.io/data/wellbeingwork3.rda"))

# BNT MONOLINGUAL (nested ranef)
bnt <- read_csv("https://uoepsy.github.io/data/bntmono.csv")
# TEST ENHANCED LEARNING (crossed ranef)
load(url("https://uoepsy.github.io/data/testenhancedlearning.RData"))

# HANGRY (centering)
hangry<-read_csv("https://uoepsy.github.io/data/hangry.csv")
# MEMORY TAPPING (logistic)
memtap <- read_csv("https://uoepsy.github.io/data/memorytap.csv")


##########
# LECTURES
##########

# CRQ EDS (cross sectional)
crq <- read_csv("https://uoepsy.github.io/data/crqdata.csv")
# COG TIME (longitudinal)
cogtime <- read_csv("https://uoepsy.github.io/data/cogtimerpm.csv")
nursedf <- read_csv("https://uoepsy.github.io/data/nurse_stress.csv")
# NURSES (cross sectional)
nursedf <- read_csv("https://uoepsy.github.io/data/nurse_stress.csv")
# BIG FISH (centering)
bflp <- read_csv("https://uoepsy.github.io/data/bflp.csv")
# Alc Anx (centering)
alcgad <- read_csv("https://uoepsy.github.io/data/alcgad.csv") %>% mutate(interv=group)
# CRQ DETENTION (logistic)
crq_detention <- read_csv("https://uoepsy.github.io/data/crqdetentiondata.csv")
# Agrssv Behav Int 
absint<-read_csv("https://uoepsy.github.io/data//abs_intervention.csv")


######
# MISC
######

worms_data <- read_csv("https://uoepsy.github.io/data/worms.csv")
toys_read <- read_csv("https://uoepsy.github.io/data/toyexample.csv")
load(url("https://uoepsy.github.io/data/WeightMaintain3.rda"))


