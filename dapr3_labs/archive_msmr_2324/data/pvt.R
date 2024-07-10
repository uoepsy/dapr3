require(tidyverse)
require(lme4)

simm2<-function(seed=NULL,b0=0,b1=1,b2=-.2,z0=1,z1=1,e=1){
  if(!is.null(seed)){
    set.seed(seed)
  }
  n_groups = round(runif(1,4,30))
  npg = 7
  g = rep(1:n_groups, e = 7)      # the group identifier
  x = rep(0:6,n_groups)
  b = rbinom(n_groups,1,prob=.4)
  b = b[g]
  re0 = rnorm(n_groups, sd = z0)  # random intercepts
  re  = re0[g]
  rex = rnorm(n_groups, sd = z1)  # random effects
  re_x  = rex[g]
  rex2 = rnorm(n_groups, sd = z1)  # random effects
  re_x2  = rex2[g]
  
  
  
  lp = (b0 + re) + (b1 + re_x)*poly(x,3)[,1] + (b2+re_x2)*poly(x,3)[,2] + (0)*poly(x,3)[,3] +
    b*poly(x,3)[,2]*rnorm(1,3,.2) + b*poly(x,3)[,3]*-2 + b*rnorm(1,-.2,.1)
  y = rnorm(length(g), mean = lp, sd = e) # create a continuous target variable
  # y_bin = rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable
  data.frame(x, b, g=factor(g), y)
}

while(TRUE){
  effseed=round(runif(1,1e3,1e6))
  set.seed(373391)
  #set.seed(effseed)
  big = tibble(
    school = 1:30,
    #b = rep(0:1,e=15),
    int = rnorm(30),
    sl = rnorm(30,7,1),
    qu = rnorm(30,-2.4,.3),
    z0 = runif(30,.5,2),
    z1 = runif(30,.5,2),
    e = runif(30,.5,2)
  )
  big = big |> mutate(
    #int= ifelse(b==1,int-1,int),
    #qu = ifelse(b==1,qu+3,qu),
    #cu = ifelse(b==0,0,cu),
    data = pmap(list(int,sl,qu,z0,z1,e), ~simm2(b0=..1,b1=..2,b2=..3,z0=..4,z1=..5,e=..6))
  ) |> unnest(data)
  
  big = big |> mutate(
    y = case_when(x==0 & b==1 ~ y-.2,
                  TRUE~y)
  )
  m = lmer(y~poly(x,3)*b+(1+poly(x,1)+b|school)+(1+poly(x,2)|school:g),big,
           control=lmerControl(optimizer="bobyqa"))
  if(is.null(m@optinfo$conv$lme4$messages)){ break }
}

  fm = broom::augment(m)
  fm$x = round(fm$`poly(x, 3)`[,1],3)
  fm |> ggplot(aes(x=x,y=.fitted,col=factor(b)))+
    stat_summary(geom="line")
  # 
  # sjPlot::plot_model(m,type="pred",terms=c("x [all]","b"))
  
childnames = randomNames::randomNames(1e4,which.names="first")
bigwrite = big |> transmute(
  child = childnames[as.numeric(paste0(school,g))],
  school=paste0("School ", school),
  isBilingual = b,
  age = x+4,
  #PVTU = scale(y)[,1]*9.75 + 27.4,
  PVT = round(pmin(60,pmax(0,scale(y)[,1]*9.75 + 27.4)))
)

# lmer(PVT~poly(year,3)*isBilingual+(1+poly(year,1)|school)+(1+poly(year,2)|school:child),bigwrite) |>
#   summary()

# hist(bigwrite$PVT)
write_csv(bigwrite,"../../data/pvt_bilingual.csv")

