library(tidyverse)
library(qualtRics)
# qualtrics_api_credentials(
#   api_key = # INSERT JOSIAH'S API KEY HERE!,
#   base_url = "fra1.qualtrics.com",
#   install = TRUE
# )
qdata <- fetch_survey(surveyID = "SV_5BCIFyTms4yZSoC")[-c(7,10),] |> 
  janitor::clean_names() 

# ratings
ratings <- 
  qdata |> 
  mutate(pid = 1:n(),
         pid = ifelse(is.na(q33),pid,q33)) |>
  select(pid,m1_1:s10_1) |>
  pivot_longer(-pid, values_to="rating") |> 
  filter(!is.na(rating)) |>
  mutate(
    size = as.numeric(gsub("_1","", substr(name,2,nchar(name)))),
    condition = substr(name,1,1)
  )

# item orders
orders <- 
  qdata |>
  mutate(pid = 1:n(),
         pid = ifelse(is.na(q33),pid,q33)) |>
  select(pid,mammals_do_m1:last_col()) |>
  pivot_longer(-pid, values_to="order") |>
  filter(!is.na(order)) |>
  mutate(name = gsub("birds|mammals|sea_creatures|_do_","",name),
         size = as.numeric(substr(name,2,nchar(name))),
         condition = substr(name,1,1))
  
cdat <- full_join(
  orders |> select(-name), 
  ratings |> select(-name)
  ) |>
  mutate(
    condition = factor(condition, levels = c("m","b","s"),
                       labels = c("mammals","birds","sea_creatures"))
  )

ggplot(cdat, aes(x=size, y = rating)) + 
  geom_point(size=3)+
  stat_smooth(method=lm,se=F,aes(group=pid))+
  geom_line(aes(group=pid))+
  facet_wrap(~condition)+
  ylim(-10,10)+
  theme_minimal()

cdat |> 
  filter(pid %in% unique(cdat$pid)[1:16]) |>
  ggplot(aes(x=size, y = rating, col = condition)) + 
  geom_point(size=3)+
  stat_smooth(method=lm,se=F,aes(group=pid))+
  geom_line(aes(group=pid))+
  facet_wrap(~pid)+
  ylim(-10,10)+
  theme_minimal()

cdat |>
  filter(pid == "chisquareatops") |>
  ggplot(aes(x=size, y = rating)) + 
  geom_point(size=3)+geom_line()+
  ylim(-10,10)+
  theme_minimal()


library(lme4)
mod <- lmer(rating ~ 1 + size*condition+ (1+size| pid), data = cdat)

broom.mixed::augment(mod) |>
  filter(pid %in% unique(cdat$pid)[1:16]) |>
  ggplot(aes(x=size)) +
  geom_line(aes(y=rating))+
  stat_smooth(aes(y=rating),method=lm,se=F)+
  geom_line(aes(y=.fitted))+
  facet_wrap(~pid)+
  ylim(-10,10)+
  theme_minimal()
  
  




