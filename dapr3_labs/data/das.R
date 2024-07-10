# DAS
# effseed=round(runif(1,1e3,1e6))
# set.seed(effseed)
set.seed(722674)

qnames = c("I need a bit of encouragement to get things started","I contact my friends","I express my emotions","I think of new things to do during the day","I am concerned about how my family feel","I find myself staring in to space","Before I do something I think about how others would feel about it","I plan my days activities in advance","When I receive bad news I feel bad about it","I am unable to focus on a task until it is finished","I lack motivation","I struggle to empathise with other people","I set goals for myself","I try new things","I am unconcerned about how others feel about my behaviour","I act on things I have thought about during the day","When doing a demanding task, I have difficulty working out what I have to do","I keep myself busy","I get easily confused when doing several things at once","I become emotional easily when watching something happy or sad on TV","I find it difficult to keep my mind on things","I am spontaneous","I am easily distracted","I feel indifferent to what is going on around me")

revitems = c(10,3,5,7,9,20,2,4,8,13,14,16,18,22)

Exitems = c(1,6,10,11,17,19,21,23)
Emitems = c(3,5,7,9,12,15,20,24)
BCIitems = c(2,4,8,13,14,16,18,22)

mm = paste0(
  paste0("Ex =~ ",paste0(runif(7,.3,.95),"*",paste0("y",Exitems), collapse="+"),"\n"),
  paste0("Em =~ ",paste0(runif(7,.3,.95),"*",paste0("y",Emitems), collapse="+"),"\n"),
  paste0("BCI =~ ",paste0(runif(7,.3,.95),"*",paste0("y",BCIitems), collapse="+"),"\n"),
  paste0("BCI ~~ .648*Ex\nEx ~~ .091*Em\nBCI ~~ .236*Em\ny1 ~~ .3*y3\ny1~~.3*y6\ny3~~.4*y6")
)
library(lavaan)
df = simulateData(mm, sample.nobs = 250)
df = as.data.frame(apply(df,2,function(x) as.numeric(cut(x,6))))

df2 <- df 
df2[,paste0("y",revitems)] <- apply(df2[,paste0("y",revitems)], 2, function(x) 7-x)

df2 <- df2[,order(names(df2))]
df2 <- df2[,c(1,12,18:24,2:11,13:17)]
df2 <- as.data.frame(apply(df2, 2, function(x) dplyr::case_match(x,
                                           1 ~ "Never",
                                           2 ~ "Hardly Ever",
                                           3 ~ "Occasionally",
                                           4 ~ "Often",
                                           5 ~ "Almost Always",
                                           6 ~"Always"
                                           )))

names(df2) <- qnames


# mod = "
# Em =~ y1 + y6 + y10 + y11 + y17 + y19 + y23
# Ex =~ y3 + y5 + y7 + y9 + y12 + y15 + y20 + y24
# BCI =~ y2 + y4 + y8 + y13 + y14 + y16 + y18 + y22
# Em ~~ Ex
# Em ~~ BCI
# Ex ~~ BCI
# "
# mest = cfa(mod, df)
# fitmeasures(mest)[c("srmr","rmsea","tli","cfi")]
# summary(mest)
# semPlot::semPaths(mest, rotation=2)


qnames[Exitems]
qnames[Emitems]
qnames[BCIitems]
image(is.na(df2))
for(i in 1:23){
  df2[sample(1:nrow(df2),1),sample(1:ncol(df2),sample(1:5,1))] <- NA
}
for(i in 1:11){
  df2[sample(1:nrow(df2),1),sample(1:ncol(df2),1)] <- "[NO ENTRY]"
}
image(is.na(df2))
write_csv(df2,"../../data/radakovic_das.csv")







rdas<-df2
rdas_dict <- tibble(
  variable = paste0("q",1:24),
  item = names(rdas)
)
head(rdas_dict)
names(rdas) <- paste0("q",1:24)
rdas <- rdas |> 
  mutate(across(q1:q24, ~case_match(.,
                                    "Never" ~ 1,
                                    "Hardly Ever" ~ 2,
                                    "Occasionally" ~ 3,
                                    "Often" ~ 4,
                                    "Almost Always" ~ 5,
                                    "Always" ~ 6
  )))

rdasc <- na.omit(rdas)

dasmod = "
Em =~ q1 + q6 + q10 + q11 + q17 + q19 + q23
Ex =~ q3 + q5 + q7 + q9 + q12 + q15 + q20 + q24
BCI =~ q2 + q4 + q8 + q13 + q14 + q16 + q18 + q22
Em ~~ Ex
Em ~~ BCI
Ex ~~ BCI
"
psych::multi.hist(rdasc)
dasmod.est = cfa(dasmod, rdasc, std.lv=TRUE)
fitmeasures(dasmod.est)[c("rmsea","srmr","tli","cfi")]

