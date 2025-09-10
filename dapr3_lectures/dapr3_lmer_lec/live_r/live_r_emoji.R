library(tidyverse)
library(qualtRics)
# qualtrics_api_credentials(
#   api_key = # INSERT JOSIAH'S API KEY HERE!,
#   base_url = "fra1.qualtrics.com",
#   install = TRUE
# )
qdata <- fetch_survey(surveyID = "SV_0UI3AkMccldZeJw") |> 
  janitor::clean_names() 

# ratings
ratings <- 
  qdata |> select(n_1_1:f_9_1) |>
  mutate(pid = 1:n()) |>
  pivot_longer(-pid, values_to="rating") |> 
  filter(!is.na(rating)) |>
  separate(name, into=c("condition","item"), sep = "_", convert = TRUE)

# item orders
orders <- 
  qdata |> select(neutral_do_n_8:last_col()) |>
  mutate(pid = 1:n()) |>
  pivot_longer(-pid, values_to="order") |>
  filter(!is.na(order)) |>
  mutate(name = gsub("forward_do_|neutral_do_|backward_do_","",name)) |>
  separate(name, into=c("condition","item"), sep = "_", convert = TRUE)

emdat <- full_join(orders, ratings) |>
  mutate(
    item = as.numeric(item)
  )

ryg_scale <- c("#FF0000", "#FF3300", "#FF6600", "#FF9900", 
               "#FFFF00", 
               "#CCFF00", "#99FF00", "#66FF00", "#00FF00")

em_cols <- c(ryg_scale,
            rev(ryg_scale),
            rep("#FFF000",9))

names(em_cols) <- lapply(c("f","b","n"), \(x) paste(x, 1:9, sep=".")) |> unlist()

ggplot(emdat, aes(x=item-5, y = rating, col = interaction(condition,item))) + 
  geom_point(size=3)+
  stat_smooth(method=lm,se=F,aes(group=pid))+
  geom_line(aes(group=pid))+
  scale_color_manual(values=em_cols)+
  facet_wrap(~condition)+
  theme_dark()+
  guides(col="none")



lmer(rating ~ 1 +I(item-5)*condition+ (1+I(item-5) | pid), data = emdat) |>
  summary()

emdat |> group_by(item) |> summarise(m=mean(rating,na.rm=T),s=sd(rating, na.rm=T))
