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

# write_csv(cdat, "../../docs/2526/misc/cutespec.csv")
# 
ggplot(cdat, aes(x=factor(size), y = rating)) +
  #geom_point(size=3)+
  stat_summary(geom="pointrange")+
  stat_summary(geom="line",aes(group=1))+
  #geom_line(aes(group=pid))+
  facet_wrap(~condition)+
  #facet_wrap(~pid)+
  ylim(-10,10)+
  theme_minimal()

# people_to_remove <- 
#   cdat |> group_by(pid) |>
#   summarise(
#     n10 = sum(rating==10),
#     n0 = sum(rating==-10)
#   ) |> filter(n10 == 10 | n0 == 10) |>
#   pull(pid)
# 
# cdat <- cdat |> filter(!pid %in% people_to_remove)
toadd <- tibble(
  pid = rep(c("hello","waah","erhdf","iamtired",
              "skibidi","pingu","covfefe","meh",
              "what","daa","3993","pplllll"),e=10),
  order = unlist(lapply(1:12,\(x) sample(1:10))),
  condition = rep(sample(unique(cdat$condition)),e=40),
  size = unlist(lapply(1:12,\(x) sample(1:10))),
  rating = pmin(10,pmax(-10,round(-5+1*size + rnorm(120,0,5))))
)

cdat <- bind_rows(cdat,toadd)

mod <- lmer(rating ~ 1 + size*condition + (1+size| pid), data = cdat)
summary(mod)



cdat |>
  #filter(pid %in% unique(cdat$pid)[88:94]) |>
  ggplot(aes(x=size, y = rating, col = condition)) +
  geom_point(size=3,alpha=.2)+
  stat_smooth(method=lm,se=F,aes(group=pid))+
  facet_wrap(~condition)+
  ylim(-10,10)+
  theme_minimal()


library(lme4)

# let's ignore condition for a minute.

mod <- lmer(rating ~ 1 + size*condition + (1+size| pid), data = cdat)
summary(mod)


broom.mixed::augment(mod) |>
  filter(pid %in% unique(cdat$pid)[1:16]) |>
  ggplot(aes(x=size)) +
  geom_line(aes(y=rating))+
  stat_smooth(aes(y=rating),method=lm,se=F)+
  geom_line(aes(y=.fitted))+
  facet_wrap(~pid)+
  ylim(-10,10)+
  theme_minimal()

  
rm(orders,qdata,ratings)




