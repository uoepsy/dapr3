library(simstandard)

m = "
jobsat ~ -0.8*age + .58*autonomy + .47*income + .2*x2
income ~ .22*autonomy + .57*age + .6*x3
autonomy ~ .28*age + .2*x1 + .3*x2
"

df <- sim_standardized(m, n = 200)
df<-df[,1:4]
#df<- map_dfc(df, ~.+rnorm(nrow(df),0,1))
round(cor(df),2)
m = "
jobsat ~ age + autonomy + income
income ~ autonomy + age
autonomy ~ age
"
mod <- sem(m, data=df)
round(mod@implied$cov[[1]],2)
round(cov(df),2)
