

# 24 ppts
# unnec_agg ~ character * mode + (1 + character | id)
# unnec_agg ~ character * mode * (P + M + N) + (1 + character | id)
# ss = round(runif(1,1e3,1e6))
set.seed(269699)
n_groups = 80
npg = rep(20, n_groups)
g = rep(1:n_groups,e=20)
char = rep(1:2,n_groups*10)
level = rep(rep(1:10,e=2),n_groups)
mode = rep(1:2,e=(n_groups/2))[g]
S = c(.65,.61,.63)
R = matrix(c(1,.5,.4,
             .5,1,.2,
             .4,.2,1),nrow=3)
SD3 = MASS::mvrnorm(length(g), mu = c(2.9,2.9,2.1),Sigma=diag(S)%*%R%*%diag(S))
dtM = pmax(1,pmin(5,SD3[,1]))[g]
dtN = pmax(1,pmin(5,SD3[,2]))[g]
dtP = pmax(1,pmin(5,SD3[,3]))[g]


re0 = rnorm(n_groups, sd = 3)[g]
rex1 = rnorm(n_groups, sd = 2)[g]
#rex2 = rnorm(n_groups, sd = 1)[g]

lp = (0 + re0) + (-4 + rex1)*(char==2) + 
  -1.4*(mode==2) +
  -.6*(char==2 & mode==2) + 
  2.28*dtM + 2.1*dtP + 1*dtP*(char==2) +
  .77*dtM*(char==2) + 
  .7*(level==3) + -.7*(level==5) + .6*(level==2 & mode==2) +
  -.8*(level==2 & mode==1)
  
y = rnorm(length(g), mean=lp, sd = 2.56)

df = data.frame(g,level, char=factor(char), mode=factor(mode),dtP,dtM,dtN,y)

lmer(y ~char*(mode+dtP+dtN+dtM) + (1+char|g)+(1+mode|level),df) |> summary()
#lmer(y ~ char*mode*(dtP+dtN+dtM) + (1+char|g),df) |> sjPlot::plot_model(type="int")


df |> transmute(
  PID = paste0("ppt_",g),
  age = round(runif(1e3,18,48)[g]),
  level = paste0("level",level),
  character = factor(char, labels=c("cartoon","realistic")),
  mode = factor(mode, labels=c("Screen","VR")),
  P = round(dtP),
  N = round(dtN),
  M = round(dtM),
  NGV = pmax(0,round(scale(y)[,1]*6.4+22)/2)
) |> slice_sample(prop=1) |> arrange(PID) -> df

df |> ggplot(aes(x=character,y=NGV,col=mode))+geom_jitter(height=0)

# lmer(NGV ~ character*(mode+P+M+N) + (1+character|PID) + (1+mode|level),df) |> sjPlot::tab_model()
#lmer(NGV ~ character*mode*(P+M+N) + (1+character|PID) + (1+mode|level),df) |> sjPlot::plot_model(type="int")

ngv <- df

# impossible
ngv$P[ngv$PID== sample(unique(ngv$PID), 1)] <- 0
ngv$N[ngv$PID== sample(unique(ngv$PID), 1)] <- 7
ngv$age[ngv$PID %in% sample(unique(ngv$PID), 3)] <- -99

# psycho
psycho = sample(unique(ngv$PID), 1)
ngv[ngv$PID == psycho, c("P","M","N")] <- c(5,5,5)
ngv[ngv$PID == psycho, "NGV"] <- ngv[ngv$PID == psycho, "NGV"]*2


set.seed(3433)
# calm
ngv$NGV[ngv$PID== sample(unique(ngv$PID), 1)] <- rbinom(20,1,.05)
# unclear
ngv$NGV[ngv$PID== sample(unique(ngv$PID), 1)] <- 0



ngv <- ngv |> 
  filter(N>=1 & N<=5) |>
  filter(P>=1) |>
  filter(age>0)


#ngv <- read_csv("../../data/ngv.csv")
m1 = lmer(NGV ~ character * (mode + P + M + N) + 
            (1 + character | PID) + 
            (1 + mode | level), data = ngv)
summary(m1,correlation=F)
sjPlot::tab_model(m1)

write_csv(ngv,"../../data/NGV.csv")