simt <- function(){
  
  df = junk::sim_basicrs(b1=0)
  m0 = lmer(y~1+(1+x|g),df,REML=F)
  m1 = lmer(y~1+x+(1+x|g),df,REML=F)
  c(
    anova(m0,m1)[2,8],
    summary(lmerTest::lmer(y~1+x+(1+x|g),df,REML=T))$coefficients[2,5]
  )
}
res = replicate(100, simt())
plot(res[1,],res[2,])
hist(res[1,]-res[2,])


df = junk::sim_basicrs(b1=0,z0 = .2)
df = df[-sample(1:nrow(df),20),]

library(pbkrtest)
library(parameters)

m0 = lmer(y~1+(1+x|g),df,REML=T)
m1 = lmer(y~1+x+(1+x|g),df,REML=T)


# WALD ----
parameters::model_parameters(m1, ci_method="wald", ci_random=FALSE)

# Likelihood ----
anova(m0,m1,test="Chisq") # refits with ML, differ fixef, same ranef
drop1(m1,test="Chisq")
parameters::model_parameters(m1,ci_method="profile", ci_random=FALSE) 
# CIs computed via REML, estimates, t, p not unless initial model fitted with REML
confint(m1, method="profile") # refits with ML whatever

# ddf ----
SATmodcomp(m1,m0) # refits with REML
KRmodcomp(m1,m0) # refits with REML

parameters::model_parameters(m1, ci_method="kr", ci_random=FALSE)
# CIs computed via REML, estimates, t, p not unless initial model fitted with REML
parameters::model_parameters(m1,ci_method="sat", ci_random=FALSE)
# CIs computed via REML, estimates, t, p not unless initial model fitted with REML

# Bootstrap ----
PBmodcomp(m1,m0,nsim=10) # refits with ML?

library(lmeresampler)
bb=bootstrap(m1,.f=extract_parameters,type="parametric",B=10)
confint(bb,type="basic")


library(lmeresampler)
m1 = lmer(y~1+x+(1+x|g),df,REML=T)
bb=bootstrap(m1,.f=extract_parameters,type="parametric", B=10)
confint(bb,type="basic")

bb = bootstrap(m1,.f=extract_parameters,type="case",B=500,resample=c(FALSE,TRUE))
confint(bb,type="basic")

