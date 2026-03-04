library(tidyverse)
library(lme4)

makd <- function(){
  tibble(
    x1 = rnorm(500,0,1),
    x2 = rnorm(500,x1,1),
    x2c = scale(cut(x2, 20, labels=F), scale=T),
  ) %>% mutate(
    y = 10 + x1*x2c + 2*x2 + rnorm(500,0,1),
    x2c = factor(x2c)) -> df
  
  #ggplot(df, aes(x=x1,y=y,col=x2c))+geom_smooth(method="lm",se=F)+geom_point()
  
  df %>% count(x2c) %>% arrange(n) %>%
    filter(n<=10) %>% pull(x2c) -> smallgr
  df %>% filter(!(x2c %in% smallgr)) %>% droplevels() -> df
  
  #ggplot(df, aes(x=x1,y=y,col=x2c))+geom_smooth(method="lm",se=F)+geom_point()
  
  df <- 
    df %>% 
    mutate(y_grand = y - mean(y)) %>% 
    group_by(x2c) %>% 
    mutate(y_group = y - mean(y),
           x1_group = mean(x1)) %>% 
    ungroup()
  
  df
}

mod_plot <- function(m, dat){
  rns <- ranef(m)$x2c
  rns$x2c <- row.names(rns)
  if(any(grepl("x1",names(rns)))){
    rns <- rns %>% select(-x1)
  }
  left_join(dat, rns)
}



tibble(
  f = c("y ~ x1 + (1|x2c)",
        "y ~ x1 + x1_group + (1|x2c)",
        "y ~ x1 + (1 + x1|x2c)",
        "y ~ x1 + x1_group + (1 + x1|x2c)")
) %>%
  mutate(
    mod = map(f, ~lmer(as.formula(.),data=df)),
    t = map(mod, ~broom.mixed::tidy(.)),
    estx1 = map_dbl(t, ~filter(., effect=="fixed", term=="x1") %>% pull(estimate)),
    rns = map(mod, ~mod_plot(., df))
  ) %>% unnest(rns) %>%
  ggplot(.,aes(x=x1, y=`(Intercept)`)) +
  geom_point()+
  geom_smooth()+
  facet_wrap(~f)



expand_grid(
  s = 1:1000,
  f = c("y ~ x1 + (1|x2c)",
        "y ~ x1 + x1_group + (1|x2c)",
        "y ~ x1 + (1 + x1|x2c)",
        "y ~ x1 + x1_group + (1 + x1|x2c)")
) -> mods

sims <- tibble(
  s = 1:1000,
  d = map(s, ~makd())
)

left_join(sims, mods) %>%
  mutate(
    mod = map2(f,d, ~lmer(as.formula(.x),data=.y)),
    t = map(mod, ~broom.mixed::tidy(.)),
    estx1 = map_dbl(t, ~filter(., effect=="fixed", term=="x1") %>% pull(estimate)),
    rns = map(mod, ~mod_plot(., df))
  ) -> sims

sims %>% group_by(f) %>% summarise(m = mean(estx1), s = sd(estx1))
ggplot(sims,aes(x=estx1))+geom_histogram()+facet_wrap(~f)




df<-makd()
lmer(y ~ x1 + (1|x2c), df) %>% summary
lmer(y ~ x1 + x1_group + (1|x2c), df) %>% summary
library(robustlmm)
rlmer(y ~ x1 + (1|x2c), df) %>% summary

