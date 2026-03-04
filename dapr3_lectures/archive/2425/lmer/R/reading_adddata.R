# create reading2 from reading

load('R/reading.Rdata')


stat <- 1
while(stat > 0) {
  s1 <- sample(c(rep('private',13),rep('state',12)))
  s2 <- sample(c(rep('private',12),rep('state',13)))
  reading2 <- reading |> arrange(method)
  reading2 <- reading2 |> mutate(school=c(s1,s2))
  modl <- lm(R_AGE~method*school,data=reading2)
  pvals <- summary(modl)$coefficients[,4]
  if(pvals[2]<.05 & pvals[3]<.05 & pvals[4]<.05) {
    stat <- 0
  }
}
