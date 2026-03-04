crq %>% rename(
  y = emot_dysreg, 
  x = crq,
  group = schoolid
) -> my_data
m =lmer(y ~ x + (1 + x | group), my_data)
summary(m)


v = vcov(m)
e = fixef(m)
ne = e
ne[ne!=0]<-0
sims  = as_tibble(MASS::mvrnorm(n=1e4, mu = ne, Sigma = v))

theme_dens <- function(){
  theme_classic() + 
    theme(axis.title.y = element_blank(), 
          axis.line.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=24),
          axis.title.x = element_text(size=24))
}


f1 = ggplot(sims, aes(x=`(Intercept)`))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[1,2]),
                alpha=.1,fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=(2*summary(m)$coefficients[1,2]),
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[1,2]),
                            NA),
                alpha=.2,fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=summary(m)$coefficients[1,2],
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[1,2]),
                            NA),
                alpha=.4,fill="green")+
  geom_errorbarh(aes(xmin=0,xmax=summary(m)$coefficients[1,2],y=1e-1),height=4e-4, col="green4",lwd=1)+
  geom_errorbarh(aes(xmin=0,xmax=2*summary(m)$coefficients[1,2],y=0),height=4e-4, col="green4", lwd=1)+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0,0,summary(m)$coefficients[1,2])),lty="dotted")+

  geom_vline(xintercept = summary(m)$coefficients[1,1],col="blue", lwd=2)+
  labs(x=expression(paste(gamma[0], " (Fixed Intercept)")))+
  theme_dens()

fx = ggplot(sims, aes(x=x))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                alpha=.1, fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=(2*summary(m)$coefficients[2,2]),
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                            NA),
                alpha=.2,fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=summary(m)$coefficients[2,2],
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                            NA),
                alpha=.4,fill="green")+
  geom_errorbarh(aes(xmin=0,xmax=summary(m)$coefficients[2,2],y=1e-1),height=4e-4, col="green4",lwd=1)+
  geom_errorbarh(aes(xmin=0,xmax=2*summary(m)$coefficients[2,2],y=0),height=4e-4, col="green4", lwd=1)+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0,0,summary(m)$coefficients[2,2])),lty="dotted")+

  geom_vline(xintercept = summary(m)$coefficients[2,1],col="blue", lwd=2)+
  labs(x=expression(paste(gamma[x], " (Fixed effect of x)")))+
  theme_dens()



### ranef plots
ranvar = as.data.frame(VarCorr(m)) %>% mutate(tau = sqrt(vcov))


r1 = ggplot(sims, aes(x=`(Intercept)`))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=fixef(m)[1],sd=ranvar$tau[1]),
                alpha=.2,fill="grey")+
  geom_vline(xintercept = summary(m)$coefficients[1,1],col="blue", lwd=2)+
  geom_segment(data = tibble(x = coef(m)$group[,1]), aes(x=x,xend=x,y=0,yend=.2))+
  labs(x=expression(paste(zeta[0], " (Group-level Intercepts)")))+
  theme_dens() + 
  xlim(-2,2.3)

rx = ggplot(sims, aes(x=x))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=fixef(m)[2],sd=ranvar$tau[2]),
                alpha=.2,fill="grey")+
  geom_vline(xintercept = summary(m)$coefficients[2,1],col="blue", lwd=2)+
  geom_segment(data = tibble(x = coef(m)$group[,2]), aes(x=x,xend=x,y=0,yend=.2))+
  labs(x=expression(paste(zeta[x], " (Group-level effects of x)")))+
  theme_dens() + 
  xlim(-2,1)


library(patchwork)
(f1 + fx ) / (r1 + rx)




ggplot(sims, aes(x=`(Intercept)`))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                alpha=.1, fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=(2*summary(m)$coefficients[2,2]),
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                            NA),
                alpha=.2,fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=summary(m)$coefficients[2,2],
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                            NA),
                alpha=.4,fill="green")+
  geom_errorbarh(aes(xmin=0,xmax=summary(m)$coefficients[2,2],y=1e-1),height=4e-4, col="green4",lwd=1)+
  geom_errorbarh(aes(xmin=0,xmax=2*summary(m)$coefficients[2,2],y=0),height=4e-4, col="green4", lwd=1)+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0,0,summary(m)$coefficients[2,2])),lty="dotted")+
  
  
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=fixef(m)[1],sd=summary(m)$coefficients[2,2]),
                alpha=.1, fill="blue")+
  
  
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~(-dnorm(.x, mean=fixef(m)[1],sd=ranvar$tau[1])),
                alpha=.2,fill="grey")+
  geom_segment(data = tibble(x = coef(m)$group[,1]), aes(x=x,xend=x,y=0,yend=-.2))+
  
  geom_vline(xintercept = summary(m)$coefficients[1,1],col="blue", lwd=2)+

  #labs(x=expression(paste(zeta[0], " (Group-level Intercepts)")))+
  theme_dens() + 
  xlim(-2,2.3)



ggplot(sims, aes(x=`(Intercept)`))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=ranvar$tau[1]),
                alpha=.2,fill="grey")+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0, mean=0,sd=ranvar$tau[1])),lty="dotted")+
  geom_segment(data = tibble(x = ranef(m)$group[,1]), aes(x=x,xend=x,y=0,yend=.2))+
  labs(x=expression(paste(zeta[0], " (Group-level Intercepts)")))+
  theme_dens() + 
  xlim(-2,2.3)



  

ggplot(sims, aes(x=`(Intercept)`))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=ranvar$tau[1]),
                alpha=.2,fill="grey")+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0, mean=0,sd=ranvar$tau[1])),lty="dotted")+
  geom_segment(data = tibble(group = row.names(ranef(m)$group), x = ranef(m)$group[,1]), aes(x=x,xend=x,y=0,yend=.2, group=group))+
  #geom_curve(data = ddf, aes(x=y,xend=interc,y=0,yend=0))+
  labs(x=expression(paste(zeta[0], " (Group-level Intercept Deviations)")))+
  theme_dens() +
  gghighlight::gghighlight(group=="school5")+
  xlim(-2,2.3)

library(gghighlight)
tibble(group = row.names(coef(m)$group), x = coef(m)$group[,1]) %>%
  ggplot(., aes(x=x)) + 
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=fixef(m)[1],sd=ranvar$tau[1]),
                alpha=.2,fill="grey") + 
  geom_segment(aes(x=fixef(m)[1],xend=fixef(m)[1],y=0,yend=dnorm(0, mean=0,sd=ranvar$tau[1])),lty="dotted")+
  geom_segment(aes(x=x,xend=x,y=0,yend=.2))+
  geom_segment(aes(x=x,xend=x,y=0,yend=.2))+
  theme_dens() +
  gghighlight(group%in%c("school12"),label_key=group)+
  xlim(-2,2.3) + 
  labs(x = "group level intercept") -> r1

tibble(group = row.names(coef(m)$group), x = coef(m)$group[,2]) %>%
  ggplot(., aes(x=x)) + 
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=fixef(m)[2],sd=ranvar$tau[2]),
                alpha=.2,fill="grey") + 
  geom_segment(aes(x=fixef(m)[2],xend=fixef(m)[2],y=0,yend=dnorm(0, mean=0,sd=ranvar$tau[2])),lty="dotted")+
  geom_segment(aes(x=x,xend=x,y=0,yend=.2))+
  geom_segment(aes(x=x,xend=x,y=0,yend=.2))+
  theme_dens() +
  gghighlight(group%in%c("school12"),label_key=group)+
  xlim(-2,2.3) + 
  labs(x = "group level slope of x") -> rx
  

r1 + rx

tibble(
  interc = ranef(m)$group[,1],
  slo = ranef(m)$group[,2],
  group = row.names(ranef(m)$group),
  e = y - interc
) %>% left_join(my_data, .) -> ddf


ggplot(my_data, aes(x=x,y=y))+
  geom_point()

ddf %>% filter(group=="school2") %>%
  ggplot(.,aes(x=y-interc)) + 
  geom_segment(aes(x=y-interc,xend=y-interc,y=0,yend=.1))+
  ylim(0,1)+
  geom_vline(aes(xintercept=0),lty="dotted")+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=ranvar$tau[4]),
                alpha=.2,fill="grey") + 
  theme_dens()





crq %>% rename(
  y = emot_dysreg, 
  x = crq,
  group = schoolid
) -> my_data
m =lmer(y ~ x + (1 + x | group), my_data)
summary(m)


coefs <- tibble(
  interc = coef(m)$group[,1],
  slo = coef(m)$group[,2],
  group = row.names(coef(m)$group),
)


broom.mixed::augment(m) %>% 
  ggplot(., aes(x=x,y=y,col=group)) + 
  geom_point() + 
  geom_line(aes(y=.fitted))+
  geom_smooth(aes(y=.fitted), method="lm",se=F,lty="dotted",fullrange=T, lwd=.5)+
  geom_abline(intercept=fixef(m)[1], slope=fixef(m)[2])+
  geom_segment(data = coefs, aes(x=-.05, xend=0.05, y = interc, yend=interc))+
  guides(col=FALSE)



p1 = 
  broom.mixed::augment(m) %>% 
  ggplot(., aes(x=x,y=y,col=group)) + 
  geom_abline(intercept=fixef(m)[1], slope=fixef(m)[2])+
  #geom_abline(intercept=coefs$interc, slope=fixef(m)[2], alpha=.2)+
  geom_point(alpha=.5)+
  geom_line(aes(y=.fitted), alpha=.5)+
  geom_smooth(aes(y=.fitted), method="lm",se=F,lty="dotted",fullrange=T, lwd=.5)+
  guides(col=FALSE) + 
  #geom_point(data = coefs, aes(x=0, y=interc), shape = 4)+
  themedapr3()
p1

p2 = 
  broom.mixed::augment(m) %>% 
  ggplot(., aes(x=x,y=y,col=group)) + 
  geom_abline(intercept=fixef(m)[1], slope=fixef(m)[2])+
  #geom_abline(intercept=coefs$interc, slope=fixef(m)[2], alpha=.2)+
  with_blur(geom_point(alpha=.3), sigma = unit(0.5, 'mm')) + 
  with_blur(geom_line(aes(y=.fitted), alpha=.3),sigma = unit(0.5, 'mm'))+
  with_blur(geom_smooth(aes(y=.fitted), method="lm",se=F,lty="dotted",fullrange=T, lwd=.5),sigma = unit(0.5, 'mm')) + 
  guides(col=FALSE) + 
  geom_point(data = coefs, aes(x=0, y=interc), shape = 4, size=2)+
  themedapr3()
p2

ints = tibble(
  y = seq(-.5,max(coef(m)$group[1])+.1,length = 100),
  x = dnorm(y,fixef(m)[1],attr(VarCorr(m)$group, "stddev")[1])
)

p2 + geom_path(data=ints, inherit.aes=F, aes(x=x,y=y))




p2 + 
  geom_point(data=broom.mixed::augment(m) %>% filter(group%in% c("school9","school12")), aes(y=y))+
  geom_line(data=broom.mixed::augment(m) %>% filter(group%in% c("school9","school12")),aes(y=.fitted))+
  geom_segment(data=broom.mixed::augment(m) %>% filter(group%in% c("school9","school12")),aes(x=x,xend=x,y=.fitted, yend=y))
  

