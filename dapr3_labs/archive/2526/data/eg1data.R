

# https://m-clark.github.io/docs/mixedModels/mixedModels.html 
set.seed(87346)
N = 600                                  # total sample size
n_groups = 30                          # number of groups
g = rep(1:n_groups, e = N/n_groups)      # the group identifier
x = rnorm(N)                             # an observation level continuous variable
b = rbinom(n_groups, size = 1, prob=.5)  # a cluster level categorical variable
b = b[g]
jx = rbinom(N, 1, .8)

sd_g = 1.5     # standard deviation for the random effect
sigma = .8     # standard deviation for the observation
sd_x = .4

re0 = rnorm(n_groups, sd = sd_g)  # random effects
re  = re0[g]
rex = rnorm(n_groups, sd = sd_x)  # random effects
re_x  = rex[g]
lp = (2 + re) + (-.2 + re_x)*x + 1*b + 0*jx + (-.5*b*x)

y = rnorm(N, mean = lp, sd = sigma)               # create a continuous target variable
y_bin = rbinom(N, size = 1, prob = plogis(lp))    #- create a binary target variable

d = tibble(x, b, y, y_bin, g = factor(g), jx)

ggplot(d,aes(x=x,y=y,col=b))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  guides(col=FALSE)+
  facet_wrap(~g)

library(lme4)
summary(lmer(y~x*b+(1+x|g),d))

library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_state_schools_in_Scotland_(city_council_areas)"
schools <- url %>%
  read_html() %>%
  html_nodes("ul") %>%
  html_text(trim = TRUE) %>%
  strsplit(split = "\n") %>%
  unlist()
schools <- schools[211:622]