# LMMs: Practice analysis
# Example code 

# PHASE 1 ----

## 1a: Set up code and data ----

# Load required R packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(stats)
library(HLMdiag)
library(effects)

# Read in data
eds_data <- read_csv("https://uoepsy.github.io/data/crqeds.csv")

# Tidy data -- looks fine
glimpse(eds_data)
is.na(eds_data) |> table()


## 1b: Set up fixed effects ----

# Set up categorical predictors: schooltype.
# Convert schooltype to factor and check ref level.
eds_data <- eds_data |>
  mutate(
    schooltype = factor(schooltype)
  )
contrasts(eds_data$schooltype)  # ref level = Private

# Set up continuous predictors: CRQ.
# CRQ ranges from 0 to 7, so it's justifiable to leave it as-is.
# NB: Mean-centering would also be justifiable, since no observations = 0.
eds_data$CRQ |> sort()

# Explore patterns by plotting outcome and predictor variables together.
eds_data |>
  ggplot(aes(x = CRQ, y = EDS, colour = schooltype)) +
  geom_point()


## 1c: Set up random effects ----

# Identify grouping variables that contribute random variability:
# The only remaining variable is schoolid, so check if it is a
# grouping variable -- and it is.
eds_data |>
  group_by(schoolid) |>
  count()

# Identify random slopes.
# Does each level of schoolid appear with at least one value of CRQ? Yes.
xtabs(~ schoolid + CRQ, data = eds_data)
# A trick from the flash cards to count the non-zero values: 
(xtabs(~ schoolid + CRQ, data = eds_data)  != 0) |> rowSums()

# Does each level of schoolid appear with at least one value of schooltype? No.
xtabs(~ schoolid + schooltype, data = eds_data)

# PHASE 2 ----

# Try fitting maximal model.
eds_m1 <- lmer(
  EDS ~ schooltype + CRQ + (1 + CRQ | schoolid),
  data = eds_data
)

# No warnings, but check variance components for sneaky singularities.
VarCorr(eds_m1)
# Looks fine!


# PHASE 3 ----


## 3a: Check assumptions and diagnostics ----

# Check model assumptions:

# Linearity of association:
plot(eds_m1, 
     type=c(
       "p",      # includes points representing Pearson residuals
       "smooth"  # includes smoothed line representing mean of residuals
     )
)

# Normality of errors:
qqnorm(resid(eds_m1)); qqline(resid(eds_m1))

# Revisit residuals-vs-fitted plot for equal variance of errors: looks fine.

# Normality of random effects:
qqnorm(ranef(eds_m1)$schoolid[,1]); qqline(ranef(eds_m1)$schoolid[,1])
qqnorm(ranef(eds_m1)$schoolid[,2]); qqline(ranef(eds_m1)$schoolid[,2])


# Check influence of observations and schools
inf_obs      <- hlm_influence(model = eds_mod, level = 1, approx = TRUE)
inf_schoolid <- hlm_influence(model = eds_mod, level = "schoolid", approx = TRUE)

dotplot_diag(inf_obs$cooksd, cutoff = "internal")
dotplot_diag(inf_schoolid$cooksd, index = inf_schoolid$schoolid, cutoff = "internal")



## 3b: Plot and interpret model estimates ----

# Plot model-fitted values:
effect(term = c("CRQ", 'schooltype'), mod = eds_m1) |> as.data.frame()
# CRQ is spaced weird, so we'll manually define a range from 0 to 7
effect(term = c("CRQ", 'schooltype'), mod = eds_m1, xlevels = list(CRQ = 0:7)) |>
  as.data.frame() 
# better. now plot it:
effect(term = c("CRQ", 'schooltype'), mod = eds_m1, xlevels = list(CRQ = 0:7)) |>
  as.data.frame() |>
  ggplot(aes(x = CRQ, y = fit)) +
  geom_line(aes(colour = schooltype)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = schooltype), alpha = 0.3) +
  labs(
    y = 'Emotion Dysregulation Score (EDS)',
    x = 'Childhood Routines Questionnaire (CRQ) score'
  )

# Interpret fixed effects:
fixef(eds_m1) |> round(2)
  
# Plot random effects:
dotplot.ranef.mer(ranef(eds_m1))

# Interpret SD of random effects relative to fixed effects.
VarCorr(eds_m1)
