library(tidyverse)
set.seed(46)
tibble(
  x = rnorm(100, 50, 20),
  x2 = rnorm(100, 20, 30),
  y = 1.5*x + 0.3*x2 + rnorm(100, 0, 50)
) -> df

m =  lm(y~x+x2,df)
v = vcov(m)
e = coef(m)
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


## COEF PLOTS

ggplot(sims, aes(x=`(Intercept)`))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[1,2]),
                alpha=.4,fill="grey")+
  geom_vline(xintercept = summary(m)$coefficients[1,1],col="blue", lwd=2)+
  labs(x=expression(paste(beta[0], " (Intercept)")))+
  theme_dens() 

ggplot(sims, aes(x=x))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[2,2]),
                alpha=.4, fill="grey")+
  geom_vline(xintercept = summary(m)$coefficients[2,1],col="blue", lwd=2)+
  labs(x=expression(paste(beta[x], " (coefficient for x)")))+
  theme_dens() 

ggplot(sims, aes(x=x2))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[3,2]),
                alpha=.4, fill="grey")+
  geom_vline(xintercept = summary(m)$coefficients[3,1],col="blue",lwd=2)+
  labs(x=expression(paste(beta[x2], " (coefficient for x2)")))+
  theme_dens()



## SE PLOTS

ggplot(sims, aes(x=`(Intercept)`))+
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
  geom_errorbarh(aes(xmin=0,xmax=summary(m)$coefficients[1,2],y=1e-3),height=4e-4, col="green4",lwd=1)+
  geom_errorbarh(aes(xmin=0,xmax=2*summary(m)$coefficients[1,2],y=0),height=4e-4, col="green4", lwd=1)+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0,0,summary(m)$coefficients[1,2])),lty="dotted")+

  geom_vline(xintercept = summary(m)$coefficients[1,1],col="blue", lwd=2)+
  labs(x=expression(paste(beta[0], " (Intercept)")))+
  theme_dens() 

ggplot(sims, aes(x=x))+
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
  geom_errorbarh(aes(xmin=0,xmax=summary(m)$coefficients[2,2],y=5e-2),height=4e-2, col="green4",lwd=1)+
  geom_errorbarh(aes(xmin=0,xmax=2*summary(m)$coefficients[2,2],y=0),height=4e-2, col="green4", lwd=1)+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0,0,summary(m)$coefficients[2,2])),lty="dotted")+
  
  geom_vline(xintercept = summary(m)$coefficients[2,1],col="blue", lwd=2)+
  labs(x=expression(paste(beta[x], " (coefficient for x)")))+
  theme_dens() 

ggplot(sims, aes(x=x2))+
  stat_function(aes(ymin=0, ymax=..y..),
                geom="ribbon",fun=~dnorm(.x, mean=0,sd=summary(m)$coefficients[3,2]),
                alpha=.2, fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=(2*summary(m)$coefficients[3,2]),
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[3,2]),
                            NA),
                alpha=.2,fill="green")+
  stat_function(aes(ymin=0, ymax=..y..),geom="ribbon",
                fun=~ifelse(abs(.x)<=summary(m)$coefficients[3,2],
                            dnorm(.x, mean=0,sd=summary(m)$coefficients[3,2]),
                            NA),
                alpha=.4,fill="green")+
  geom_errorbarh(aes(xmin=0,xmax=summary(m)$coefficients[3,2],y=1e-1),height=7e-2, col="green4",lwd=1)+
  geom_errorbarh(aes(xmin=0,xmax=2*summary(m)$coefficients[3,2],y=0),height=7e-2, col="green4", lwd=1)+
  geom_segment(aes(x=0,xend=0,y=0,yend=dnorm(0,0,summary(m)$coefficients[3,2])),lty="dotted")+
  
  geom_vline(xintercept = summary(m)$coefficients[3,1],col="blue",lwd=2)+
  labs(x=expression(paste(beta[x2], " (coefficient for x2)")))+
  theme_dens()



summary(m)
s<-summary(m)$coefficients
s[3,4]
2*pt(abs(s[1,1]/s[1,2]),lower.tail=F,df=100-3-1)
2*pt(abs(s[2,1]/s[2,2]),lower.tail=F,df=100-3-1)
2*pt(abs(s[3,1]/s[3,2]),lower.tail=F,df=100-3-1)

