library(tidyverse)
library(psych)
library(lavaan)
set.seed(346)
d1 <- simulateData("
f=~ 1*y1+1*y2+1*y3+1*y4+1*y5+1*y6
y1 ~~ 0*y1
y2 ~~ 0*y2
y3 ~~ 0*y3
y4 ~~ 0*y4
y5 ~~ 0*y5
y6 ~~ 0*y6
") |> apply(2,\(x) cut(x,7,labels=FALSE)) |> as.data.frame()

d2 <- simulateData("f=~ .8*y1+.8*y2+.8*y3+.8*y4+.8*y5+.8*y6") |> 
  apply(2,\(x) cut(x,7,labels=FALSE)) |> as.data.frame()



## 1 we get people to answer 6 questions rating how much they like:
# digestives, hobnobs, rich tea, oreos, bourbon, choc-leibniz
# for each Q, people rate it 1-7

## 2 the idea of this is:
# - these variables all measure the same thing (love of biscuits)
# - can we express this information more efficiently 
#     - without having to use all 6 variables. 

# add unmeansured column


## 3. if the *only* reason people differ on these variables is because of "how much they like biscuits", and each Q is a perfect measure of that, then we might see:
slice_sample(d1,n=6)
cor(d1)
plot(d1)

## "people either score 1 on all 6 Qs, or 2 on all 6 Qs, or 3.." << one dimension, captures everything
scree(d1)
## - if we used pca here, 1 compnent would capture 100% of the variance
## - if we used efa, the 1-factor model would be preferred - all loadings on the first factor would be 1

## 4. in reality, measures aren't perfect. liking hobnobs isn't a perfect measure of "liking biscuits (in general)".
bind_rows(
  d2 |> mutate(f1 = rowMeans(pick(y1:y6))) |> 
    arrange(f1) |> select(-f1) |> slice(1:2),
  slice_sample(d2, n=2),
  d2 |> mutate(f1 = rowMeans(pick(y1:y6))) |> 
    arrange(desc(f1)) |> select(-f1) |> slice(1:2),
)

# but if there's only one thing that these items all share, and all the other little deviations are completely unique to that question, then we can still 
# express the information more efficiently:
## "people tend to be high on all 6 Qs, medium on all 6, low on all 6" << one dimension, captures a lot of "why people score high" on individual questions

## 5 other dimensions that don't explain anything shared across questions
## "some people have a specific preference/dislike for digestives" << 
## "some people have a specific preference/dislike for hobnobs" << 
## "some people have a specific preference/dislike for rich tea" << 
## "some people have a specific preference/dislike for oreos" << 
scree(d2)
# all the variables correlate at a similar level with all others
cor(d2) |> round(2)

## - if we used pca here, 1 component would capture quite a lot of the variance
    # scree plot is fairly unambiguous
## - if we used efa, the 1-factor model would be preferred - all loadings on the first factor would be quite high
    # when we try to add more factors, we just end up with factors that have 1 item. 
    # this isn't helpful - these factors don't give us something 'shared'. they just give us the bourbon specific variation. not reducing anything.   
fa(d2,nfactors=2,rotate="oblimin")$loadings |> print(cutoff=.3)


set.seed(533612)
d3 <- simulateData("f=~ .8*y1+.8*y2+.8*y3
                   f1=~.8*y4+.8*y5+.8*y6
                   f~~.6*f1") |> 
  apply(2,\(x) cut(x,7,labels=FALSE)) |> as.data.frame()
d3a <- d3 |> mutate(f1 = rowMeans(pick(y1:y3)),
                    f2 = rowMeans(pick(y4:y6))) |>
  mutate(across(f1:f2,~cut(.,5,labels=F)))


## 6 what if
# there's another dimension to how the scores vary across the questions. 
# or in efa language: what if there's not just 1 thing driving 'why people score high/low on these questions'? 
bind_rows(
  d3a |> arrange(f1,f2) |> select(-f1,-f2) |> slice(1:2),
  d3a |> arrange(f1,desc(f2)) |> select(-f1,-f2) |> slice(1:2),
  d3a |> arrange(desc(f1),f2) |> select(-f1,-f2) |> slice(1:2),
  d3a |> arrange(desc(f1),desc(f2)) |> select(-f1,-f2) |> slice(1:2)
)

## 7 the correlation matrix captures this:
round(cor(d3),2)
heatmap(cor(d3))

scree(d3)

## 8 doing efa:
# just 1 factor explains 24%
fa(d3, nfactors = 1, rotate = "oblimin")
# 2 factors explains 36%
fa(d3, nfactors = 2, rotate = "oblimin")
# allowing them to correlate, we get a nice clean pattern:
# why do people differ on these 6 Qs?
  # 1. partly depends on whether they like/dislike non-chocolatey biscuits
  # 1. partly depends on whether they like/dislike chocolatey biscuits
  # 1. a and b are correlated - if you like non-chocolatey, typically oyu like chocolatey
fa(d3, nfactors = 2, rotate = "oblimin")

# because the 2 factors correlate, their combination explains variance.
# if we force orthogonal/no rotation (i.e. zero correlation), this variance gets shoved into the loadings
# results in cross loadings and makes the factors more difficult to define. 
fa(d3, nfactors = 2, rotate = "varimax")$loadings |> print(cutoff=.1)










