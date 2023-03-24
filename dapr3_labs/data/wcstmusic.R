
# wcst ~ music*headph + (1 + music | ppt)

n_groups = 60
N = n_groups*3*5
g = rep(1:n_groups, e = N/n_groups)

w = rep(rep(letters[1:3],5),n_groups)
w1 = model.matrix(lm(rnorm(N)~w))[,2]
w2 = model.matrix(lm(rnorm(N)~w))[,2]

b = rep(0:1, e = N/2)

re0 = rnorm(n_groups, sd = 1)[g]
re_w1  = rnorm(n_groups, sd = 1)[g]
re_w2  = rnorm(n_groups, sd = 1)[g]

lp = (0 + re0) + 
  (2)*b + 
  (3 + re_w1)*w1 +
  (1 + re_w2)*w2 + 
  (2)*b*w1
  
y = rnorm(N, mean = lp, sd = 1) # create a continuous target variable

df <- data.frame(w, g=factor(g), y)

library(lme4)
m = lmer(y ~ 1 + b * w + (1 + w | g), df)
summary(m)
