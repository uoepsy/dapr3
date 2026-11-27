library(qualtRics)
library(tidyverse)
df <- fetch_survey("SV_9n2jX22KLvNdVtQ") |>
  mutate(across(Q2_1:Q2_24, 
                ~case_match(.,
                            "Strongly disagree" ~ 1,
                            "Disagree" ~ 2,
                            "Somewhat disagree" ~ 3,
                            "Neither agree nor disagree" ~ 4,
                            "Somewhat agree" ~ 5,
                            "Agree" ~ 6,
                            "Strongly agree" ~ 7
                            )
                ))

df <- df |> select(Q2_1:Q2_24) |> na.omit()

library(psych)
ff = fa(df,nfactors=8,rotate="oblimin")
fs = factor.scores(df,ff)$scores
newfs = slice_sample(as.data.frame(fs), n = 200, replace = TRUE)

L <- as.matrix(ff$loadings)
uniques <- ff$uniquenesses
F_mat <- as.matrix(newfs)
common_part <- F_mat %*% t(L)
n_new <- nrow(newfs)
error_matrix <- sapply(uniques, function(u) rnorm(n_new, mean = 0, sd = sqrt(u)))
sim_df <- as.data.frame(common_part + error_matrix)
colnames(sim_df) <- colnames(df)
# sim_df <- apply(sim_df, 2, \(x) cut(x,7,labels=FALSE)) |> as.data.frame()

orig_means <- sapply(df, mean, na.rm = TRUE)
orig_sds   <- sapply(df, sd, na.rm = TRUE)
final_df <- sweep(sim_df, 2, orig_sds, `*`)
final_df <- sweep(final_df, 2, orig_means, `+`)

final_df <- final_df |> mutate(across(everything(), ~pmax(1,pmin(7,round(.)))))

dfair <- bind_rows(df,final_df)

airs_doms <- rep(c("HUM","ACC","EASE","EFF","ETH","SOC"),e=4)
airs_doms <- paste0(airs_doms,"_",rep(1:4,6))

names(dfair) = airs_doms

pheatmap::pheatmap(cor(dfair))
scree(dfair)
write_csv(df, file="uoepsy_courses/dapr3/docs/2627/misc/dfairs.csv")



fa(dfair, 
   nfactors=5, rotate="oblimin")$loadings |>
    print(cutoff=.3, sort=TRUE)




airs_items = c("AI understands concepts better and more quickly than humans",
               "AI threatens human creativity and agency ",
               "AI possesses broader knowledge than any individual human",
               "AI produces more original and creative ideas than I can",
               "AI consistently provides reliable and factually correct information",
               "reliance on AI will often lead to making incorrect decisions or judgements",
               "I frequently double-check AI outputs because they can be misleading",
               "I trust AI tools to give me accurate explanations when I am learning complex concepts",
               "I find interacting with AI simple, intuitive and straightforward",
               "It takes very little effort for me to get the results I want from an AI tool",
               "Learning how to use AI tools effectively is frustrating and complicated",
               "I feel confident in my ability to write clear prompts for AI systems",
               "Using AI improves my productivity and the overall quality of my work",
               "Using AI saves me a significant amount of effort on routine tasks",
               "I don't have time to complete all my work without AI ",
               "I find it hard to do all the work myself without the use of AI",
               "I am worried that relying on AI will introduce serious risks to society",
               "The environmental cost of training and running AI systems makes me reluctant to use them",
               "I am concerned about the power and motives of the corporations developing AI",
               "Concerns about the environmental and ethical risks of AI are largely overhyped",
               "People whose opinions I respect expect me to use AI",
               "If I don't get used to using AI I will be left behind",
               "My labour-value upon graduating depends on me knowing how to use AI ",
               "Using AI has become the standard expectation among my peers and colleagues")

