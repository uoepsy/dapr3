## ---- message=FALSE, warning=FALSE-------------------------------------------------------------------
library(backports)     # to revive the isFALSE() function for sim_slopes()
library(effects)       # for probing interactions
library(ggplot2)       # for data visualization
library(interactions)  # for probing/plotting interactions
library(lme4)          # for multilevel models
library(lmerTest)      # for p-values
library(psych)         # for describing the data
library(plyr)          # for data manipulation


## ---- message=FALSE----------------------------------------------------------------------------------
#set filepath for data file
filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/AMIBshare_persons_2019_0501.csv"

#read in the .csv file using the url() function
AMIB_persons <- read.csv(file=url(filepath),header=TRUE)

#subsetting to variables of interest
AMIB_persons <- AMIB_persons[ ,c("id","bfi_n")]


## ---- message=FALSE----------------------------------------------------------------------------------
#set filepath for data file
filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/AMIBshare_daily_2019_0501.csv"

#read in the .csv file using the url() function
AMIB_daily <- read.csv(file=url(filepath),header=TRUE)

#subsetting to variables of interest
AMIB_daily <- AMIB_daily[ ,c("id","day","negaff","pss")]


## ---- warning=FALSE, message = FALSE-----------------------------------------------------------------
#reverse coding the pss variable into a new stress variable
AMIB_daily$stress <- 4 - AMIB_daily$pss

#describing new variable
describe(AMIB_daily$stress)

#histogram
ggplot(data=AMIB_daily, aes(x=stress)) +
  geom_histogram(fill="white", color="black",bins=20) +
  labs(x = "Stress (high = more stressed)")


## ---- warning=FALSE, message = FALSE-----------------------------------------------------------------
#calculating intraindividual means
#Alternative approach using dplyr
    #AMIB_imeans <- AMIB_daily %>% 
    #               group_by(id) %>% 
    #               summarize(stress_trait=mean(stress, na.rm=TRUE))

AMIB_imeans <- ddply(AMIB_daily, "id", summarize,
                       stress_trait = mean(stress, na.rm=TRUE),
                       negaff_trait = mean(negaff, na.rm=TRUE))
describe(AMIB_imeans)

#merging into person-level file
AMIB_persons <- merge(AMIB_persons, AMIB_imeans, by="id")   

#make centered versions of the person-level scores
AMIB_persons$bfi_n_c <- scale(AMIB_persons$bfi_n,center=TRUE,scale=FALSE)
AMIB_persons$stress_trait_c <- scale(AMIB_persons$stress_trait,center=TRUE,scale=FALSE)

#describe person-level data
describe(AMIB_persons)


## ----------------------------------------------------------------------------------------------------
#merging person-level data into daily data
daily_long <- merge(AMIB_daily,AMIB_persons,by="id")

#calculating state variables
daily_long$stress_state <- daily_long$stress - daily_long$stress_trait
daily_long$negaff_state <- daily_long$negaff - daily_long$negaff_trait

#describing data
describe(daily_long)


## ---- warning=FALSE, message = FALSE-----------------------------------------------------------------
#faceted plot
ggplot(data=daily_long[which(daily_long$id <= 125),], aes(x=stress_state,y=negaff)) +
  geom_point() +
  stat_smooth(method="lm", fullrange=TRUE) +
  xlab("Stress State") + ylab("Negative Affect (Continuous)") + 
  facet_wrap( ~ id) +
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        strip.text=element_text(size=14))


## ---- warning=FALSE, message = FALSE-----------------------------------------------------------------
#unconditional means model
model0_fit <- lmer(formula = negaff ~ 1 + (1|id), 
              data=daily_long,
              na.action=na.exclude)
summary(model0_fit)


## ----------------------------------------------------------------------------------------------------
VarCorr(model0_fit)


## ----------------------------------------------------------------------------------------------------
RandomEffects <- as.data.frame(VarCorr(model0_fit))
RandomEffects


## ----------------------------------------------------------------------------------------------------
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between


## ---- warning=FALSE, message = FALSE-----------------------------------------------------------------
# fit model
model1_fit <- lmer(formula = negaff ~ 1 + day + stress_trait_c + 
                      stress_state + stress_state:stress_trait_c + 
                      (1 + stress_state|id), 
                    data=daily_long,
                    na.action=na.exclude)
summary(model1_fit)

# save predicted scores
daily_long$pred_m1 <- predict(model1_fit)


## ----------------------------------------------------------------------------------------------------
# Get confidence intervals for both fixed and random effects
confint(model1_fit)


## ----------------------------------------------------------------------------------------------------
ggplot(data=AMIB_imeans, aes(x=stress_trait, y=negaff_trait, group=factor(id)), legend=FALSE) +
  geom_point(colour="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="blue") +
  xlab("Trait Stress") + ylab("Trait Negative Affect") +
  theme_classic() +
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=12),
        plot.title=element_text(size=16, hjust=.5)) +
  ggtitle("Between-Person Association Plot\nTrait Stress & Negative Affect")


## ----------------------------------------------------------------------------------------------------
head(daily_long)
ggplot(data=daily_long, aes(x=stress_state, y=negaff_state, group=factor(id), colour="gray"), legend=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40")


## ---- warning=FALSE, message = FALSE-----------------------------------------------------------------
# fit model
model2_fit <- lmer(formula = negaff ~ 1 + day + stress_trait_c + 
                      bfi_n_c + stress_trait_c:bfi_n_c +
                      stress_state + stress_state:stress_trait_c + 
                      stress_state:bfi_n_c + stress_state:stress_trait_c:bfi_n_c + 
                      (1 + stress_state|id),
                    data=daily_long,
                    na.action=na.exclude)
#Look at results
summary(model2_fit)

# save predicted scores
daily_long$pred_m2 <- predict(model2_fit)


## ----------------------------------------------------------------------------------------------------
#xlevels = is the list of values that we want to evaluate at.
#obtaining the standard deviation of the between-person moderator
describe(daily_long$bfi_n_c)

#obtaining the standard deviation of the time-varying predictor
describe(daily_long$stress_state)

#calculate effect
effects_model2 <- effect(term="bfi_n_c*stress_state", mod=model2_fit, 
                         xlevels=list(bfi_n_c=c(-0.96, +0.96), stress_state=c(-0.49,+0.49)))
summary(effects_model2)


## ----------------------------------------------------------------------------------------------------
#convert to dataframe
effectsdata <- as.data.frame(effects_model2)
#plotting the effect evaluation (with standard error ribbon)
ggplot(data=effectsdata, aes(x=stress_state, y=fit, group=bfi_n_c), legend=FALSE) + 
  geom_point() +
  geom_line() +
  #geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15) +
  xlab("Stress State") + xlim(-2,2) +
  ylab("Predicted Negative Affect") + ylim(1,7) +
  ggtitle("Differences in Stress Reactivity across Neuroticism")


## ----------------------------------------------------------------------------------------------------
#check for embedded matrices
str(daily_long)

#fixing a few variables that were matrices to be vectors
daily_long$bfi_n_c <- as.vector(daily_long$bfi_n_c)
daily_long$stress_trait_c <- as.vector(daily_long$stress_trait_c)


## ----------------------------------------------------------------------------------------------------
# fit model
model2a_fit <- lme4::lmer(formula = negaff ~ 1 + day + stress_trait_c + 
                      bfi_n_c + stress_trait_c:bfi_n_c +
                      stress_state + stress_state:stress_trait_c + 
                      stress_state:bfi_n_c + stress_state:stress_trait_c:bfi_n_c + 
                      (1 + stress_state|id),
                    data=daily_long,
                    na.action=na.exclude)

# Look at results
summary(model2a_fit)
  
# Get confidence intervals for both fixed and random effects
#confint(model2a_fit)
  
# Save predicted scores
daily_long$pred_m2a <- predict(model2a_fit)

# Fit statistics  
AIC(logLik(model2a_fit)) 
BIC(logLik(model2a_fit))
logLik(logLik(model2a_fit))


## ----------------------------------------------------------------------------------------------------
#probing 2-way interaction
johnson_neyman(model=model2a_fit, pred=stress_state, modx=bfi_n_c)


## ----------------------------------------------------------------------------------------------------
#obtaining the standard deviation of the 2nd moderator 
describe(daily_long$stress_trait_c)

#probing 3-way interaction
# simpleslopes_model2a <- sim_slopes(model=model2a_fit, pred=stress_state, modx=bfi_n_c,
#                                   mod2=stress_trait_c,mod2.values = c(-0.47,+0.47))
# plot(simpleslopes_model2a)
#ran, but did not knit

