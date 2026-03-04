library(tidyverse)
filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/AMIBshare_persons_2019_0501.csv"
AMIB_persons <- read.csv(file=url(filepath),header=TRUE)
AMIB_persons <- AMIB_persons[ ,c("id","bfi_n")]

filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/AMIBshare_daily_2019_0501.csv"
AMIB_daily <- read.csv(file=url(filepath),header=TRUE)
AMIB_daily <- AMIB_daily[ ,c("id","day","negaff","pss")]
#reverse coding the pss variable into a new stress variable
AMIB_daily$stress <- 4 - AMIB_daily$pss
#histogram
ggplot(data=AMIB_daily, aes(x=stress)) +
  geom_histogram(fill="white", color="black",bins=20) +
  labs(x = "Stress (high = more stressed)")

# id means
AMIB_imeans <- plyr::ddply(AMIB_daily, "id", summarize,
                     stress_trait = mean(stress, na.rm=TRUE),
                     negaff_trait = mean(negaff, na.rm=TRUE))
#merging into person-level file
AMIB_persons <- merge(AMIB_persons, AMIB_imeans, by="id")   
#make centered versions of the person-level scores
AMIB_persons$bfi_n_c <- scale(AMIB_persons$bfi_n,center=TRUE,scale=FALSE)
AMIB_persons$stress_trait_c <- scale(AMIB_persons$stress_trait,center=TRUE,scale=FALSE)

#merging person-level data into daily data
daily_long <- merge(AMIB_daily,AMIB_persons,by="id")

#calculating state variables
daily_long$stress_state <- daily_long$stress - daily_long$stress_trait
daily_long$negaff_state <- daily_long$negaff - daily_long$negaff_trait

#faceted plot
ggplot(data=daily_long[which(daily_long$id <= 125),], aes(x=stress_state,y=negaff)) +
  geom_point() +
  stat_smooth(method="lm", fullrange=TRUE) +
  xlab("Stress State") + ylab("Negative Affect (Continuous)") + 
  facet_wrap( ~ id) +
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        strip.text=element_text(size=14))
