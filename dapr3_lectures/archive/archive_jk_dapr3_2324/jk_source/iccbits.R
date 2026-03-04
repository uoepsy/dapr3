iccgen <- function(j,n,e,icc,coef=0){
  v = (icc*e)/(1-icc)
  es = e/(v+e)
  v = if(is.infinite(v)){v=e}else{v/(v+e)}
  npj = n/j
  tibble(
    j = letters[1:j],
    zeta_j = rnorm(j,0,sqrt(v))
  ) %>%
    mutate(
      e_ij = map(j, ~rnorm(npj, 0, sqrt(es)))
    ) %>% unnest() %>%
    mutate(
      x = rnorm(n, 10, 5),
      y = 5 + coef*x + zeta_j + e_ij
    )
}


sims = map_dfr(set_names(c(0.01, 0.5, 0.95)), 
        ~iccgen(j=10,n=100,e=1,icc=.,coef=0), .id="icc")

ggplot(sims, aes(x=0, y=y))+
  geom_jitter(height=0, size=2,aes(col=j))+
  facet_wrap(~icc, scales="free_y")+
  scale_x_continuous(NULL, labels=NULL)+
  stat_summary(geom="segment",aes(y=mean(y),yend=mean(y),x=-.5,xend=.5))+
  themedapr3()


sims = map_dfr(set_names(seq(0.01, 0.5, 0.95)), 
               ~iccgen(j=20,n=1000,e=5,icc=.), .id="icc")

ggplot(sims,aes(x=x,y=y,col=j))+
  #geom_point()+
  geom_smooth(method="lm",se=F)+
  facet_wrap(~icc)






library(tidyverse)
iccgen2 <- function(j,n,e,icc,xb=0,xm=0,xs=1){
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
      x = rnorm(n, xm, xs),
      y = 5 + xb*x + zeta_j + e_ij
      #y = 5 + zeta_j + e_ij
    )
}


draws <- 
  expand_grid(
    xbeta = -5:5,
    er = 1:5,
    #xs = 1:5,
    n = c(1000, 2000, 5000),
  ) %>% 
  mutate(icc = list(seq(0.05,.99,0.1))) %>% 
  unnest(icc) %>% 
  mutate(
    sims = pmap(list(a=xbeta, b=er, f=n, y=icc), function(a,b,f,y) iccgen2(j=20,n=f,e=b,icc=y,xb=a)),
    models = map(sims, ~ lme4::lmer(y ~ x + (1 | j), data = .x)),
    sing = map_lgl(models, ~lme4::isSingular(.))
  ) %>% filter(sing != TRUE)
         
draws %>% 
  mutate(icc_est = map(models, ~ 
                         tibble(
                           adj_icc = sjstats::icc(.x)$ICC_adjusted,
                           cond_icc = sjstats::icc(.x)$ICC_conditional,
                         ))) %>% 
  unnest(icc_est) %>% 
  select(xbeta, er, n, icc, adj_icc, cond_icc) %>% 
  gather(icc_est, est_val, adj_icc:cond_icc) %>% 
  mutate(icc_gap = icc - est_val) -> plotdat

ggplot(plotdat, aes(x=icc, y=abs(icc_gap), col=factor(xbeta)))+
  stat_summary(geom="line")+
  #guides(col=FALSE)+
  facet_grid(icc_est~n)
