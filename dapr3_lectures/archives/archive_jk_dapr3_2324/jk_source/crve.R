library(tidyverse)

iccgen <- function(j,n,e,icc){
  v=(icc*e)/(1-icc)
  npj = n/j
  tibble(
    j = letters[1:j],
    zeta_j = rnorm(j,0,sqrt(v))
  ) %>%
    mutate(
      e_ij = list(rnorm(npj, 0, sqrt(e)))
    ) %>% unnest() %>%
    mutate(
      x = rnorm(n, 10, 5),
      y = .5 + zeta_j + e_ij
    )
}

df <- iccgen(j=5,n=100,e=10,icc=.2)
ggplot(df, aes(x=x,y=y,col=j))+
  geom_point()
  #geom_path()
m0 <- lm(y~1,df)

m1 <- lm(y~1+j,df,contrasts=list(j="contr.sum"))

df %>% group_by(j) %>%
  mutate(
    jm = mean(y),
    y = y - mean(y),
    x = x - mean(x)
  ) %>% 
lm(y~1,.) -> m2


summary(m0)$coefficients %>% round(3)
summary(m1)$coefficients %>% round(3)
summary(m2)$coefficients %>% round(3)




library(plm)
clm <- plm(y ~ 1+x, data=df, model='pooling', index='j')
sqrt(diag(vcovHC(clm, method='arellano', cluster='group')))
summary(clm)



library(geepack)
library(gee)
mg <- geeglm(y~1, id=j, data=df, corstr = "unstructured")
summary(mg)
