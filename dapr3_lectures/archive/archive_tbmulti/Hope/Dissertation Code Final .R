DissData <- read.csv("~/Downloads/Dissertation Data Edit2.csv")
is.na(DissData)
install.packages("tidyr")
library(tidyr)
install.packages("psych")
library(psych)
install.packages("multicon")
library(multicon)
install.packages("QuantPsyc")
library(QuantPsyc)
install.packages("tidyverse")
library(tidyverse)

## DEMOGRAPHICS ##

Age <- data.frame(DissData$Age)
is.numeric(Age)
is.na(Age)
mean(data.matrix(DissData$Age)) #Mean Age 27.7
sd(data.matrix(DissData$Age)) #SD 10.97

## WIDE TO LONG ##

# calculate the average scores for the questionnaire measures
# . carries through the data set, names in [] are the ones we want to have the average of
DissData2 <- 
  DissData %>%
  mutate(TII_score = rowMeans(.[c("Q1.TII", "Q2.TII", "Q3.TII", "Q4.TII", "Q5.TII", "Q6.TII", "Q7.TII", "Q8.TII", "Q9.TII", "Q10.TII","Q11.TII", "Q12.TII", "Q13.TII", "Q14.TII", "Q15.TII", "Q16.TII", "Q17.TII", "Q18.TII", "Q19.TII", "Q20.TII", "Q21.TII", "Q22.TII", "Q23.TII", "Q24.TII", "Q25.TII", "Q26.TII", "Q27.TII", "Q28.TII", "Q29.TII", "Q30.TII" )]))


# always check the data at different stages
DissData2

DissData3 <- 
  DissData2 %>%
  mutate(MA_score = rowMeans(.[c("Q1.MO", "Q2.MO", "Q3.MO", "Q4.MO", "Q5.MO", "Q6.MO", "Q7.MO", "Q8.MO", "Q8.MO.1")]))

# always check the data at different stages
DissData3


DissData4 <- 
  DissData3 %>%
  mutate(
    current.size = recode(Current.Size, 6,8,10,12,14,16),
    ideal.size = recode(Ideal.Size, 6,8,10,12,14,16)
  )

DissData4

##TOPS

DissData5 <-  DissData4 %>%
  mutate(Top_S6 = rowMeans(.[c( "Top_6", "Top_6.1", "Top_6.2")]))
DissData5

DissData6 <-  DissData5 %>%
  mutate(Top_S8 = rowMeans(.[c("Top_8", "Top_8.1", "Top_8.2")]))
DissData6

DissData7 <-  DissData6 %>%
  mutate(Top_S10 = rowMeans(.[c( "Top_10", "Top_10.1", "Top_10.2")]))
DissData7

DissData8 <-  DissData7 %>%
  mutate(Top_S12 = rowMeans(.[c( "Top_12", "Top_12.1", "Top_12.2")]))
DissData8

DissData9 <-  DissData8 %>%
  mutate(Top_S14 = rowMeans(.[c( "Top_14", "Top_14.1", "Top_14.2")]))
DissData9

DissData10 <-  DissData9 %>%
  mutate(Top_S16 = rowMeans(.[c( "Top_16", "Top_16.1", "Top_16.2")]))
DissData10

##BOTTOMS

DissData11 <-  DissData10 %>%
  mutate(Bottom_S6 = rowMeans(.[c( "Bottom_6", "Bottom_6.1", "Bottom_6.2")]))
DissData11

DissData12 <-  DissData11 %>%
  mutate(Bottom_S8 = rowMeans(.[c( "Bottom_8", "Bottom_8.1", "Bottom_8.2")]))
DissData12

DissData13 <-  DissData12 %>%
  mutate(Bottom_S10 = rowMeans(.[c( "Bottom_10", "Bottom_10.1", "Bottom_10.2")]))
DissData13

DissData14 <-  DissData13 %>%
  mutate(Bottom_S12 = rowMeans(.[c( "Bottom_12", "Bottom_12.1", "Bottom_12.2")]))
DissData14

DissData15 <-  DissData14 %>%
  mutate(Bottom_S14 = rowMeans(.[c( "Bottom_14", "Bottom_14.1", "Bottom_14.2")]))
DissData15

DissData16 <-  DissData15 %>%
  mutate(Bottom_S16 = rowMeans(.[c( "Bottom_16", "Bottom_16.1", "Bottom_16.2")]))
DissData16

##DRESSES

DissData17 <-  DissData16 %>%
  mutate(Dress_S6 = rowMeans(.[c( "Dress_6", "Dress_6.1", "Dress_6.2")]))
DissData17

DissData18 <-  DissData17 %>%
  mutate(Dress_S8 = rowMeans(.[c( "Dress_8", "Dress_8.1", "Dress_8.2")]))
DissData18

DissData19 <-  DissData18 %>%
  mutate(Dress_S10 = rowMeans(.[c( "Dress_10", "Dress_10.1", "Dress_10.2")]))
DissData19

DissData20 <-  DissData19 %>%
  mutate(Dress_S12 = rowMeans(.[c( "Dress_12", "Dress_12.1", "Dress_12.2")]))
DissData20

DissData21 <-  DissData20 %>%
  mutate(Dress_S14 = rowMeans(.[c( "Dress_14", "Dress_14.1", "Dress_14.2")]))
DissData21

DissData22 <-  DissData21 %>%
  mutate(Dress_S16 = rowMeans(.[c( "Dress_16", "Dress_16.1", "Dress_16.2")]))
DissData22

# gather up the responses to all items and create a Type and Price variable
# Last argument = list of variable names

DataLong <- 
  DissData22 %>%
  gather(key = Type, 
         value = Price, 
         Top_S6:Dress_S16)

DataLong

# Type will contain things like Dress_S12, which we want in two variables "item" and "size
# This splits.
# "_" says that the coding of item and size information is split by a dash -> Changed to "_S"
Data <- 
  DataLong %>%
  separate(Type, c("Item", "Size"), "_S")

Data

##Code to Binary for current and Ideal Match
Data.long <- 
  Data %>%
  mutate(
    current.match = if_else(current.size == Size, 1, 0),
    ideal.match = if_else(ideal.size == Size, 1, 0)
  )

Data.long

Data.long$Size <- factor(Data.long$Size, levels = c("6", "8", "10", "12", "14", "16"))


Data.long$Size <- as.factor(Data.long$Size)
contrasts(Data.long$Size) <- contr.sum(6)

str(Data.long$Size)

Data.long$Item <- as.factor(Data.long$Item)
contrasts(Data.long$Item) <- contr.sum(3)

str(Data.long$Item)

## ANALYSIS ##

install.packages("lme4")
library(lme4)

lm <- lmer(data = Data.long, Price ~ 1+ Item + Size + current.match + ideal.match + TII_score + MA_score 
           + (1|Response.ID) + (1|Item))
summary(lm)

MICC<- lmer(data= Data.long, Price ~ 1 + (1|Response.ID))
summary(MICC)
60.88/(60.88+47.94) # 55.94% variation is to do with differncecs within indiviudals for price paid.

MICC2 <- lmer(data= Data.long, Price ~ 1 + (1|Item))
summary(MICC2)
15.95/(15.95+96.85) # 14.14% explained by Item


M0 <- lmer(data= Data.long, Price ~ 1 + Item + (1|Response.ID) + (1|Item))
summary(M0)

M1 <- lmer(data= Data.long, Price ~ 1 + Item + Size + (1|Response.ID) + (1|Item))
summary(M1)

M2 <- lmer(data= Data.long, Price ~ 1 + Item + Size + current.match + (1|Response.ID) + (1|Item))
summary(M2)

M3 <- lmer(data= Data.long, Price ~ 1 + Item + Size + current.match + ideal.match +(1|Response.ID) + (1|Item))
summary(M3)

M4 <- lmer(data= Data.long, Price ~ 1 + Item + Size + current.match + ideal.match + TII_score +(1|Response.ID) + (1|Item))
summary(M4)

M5 <- lmer(data= Data.long, Price ~ 1 + Item + Size + current.match + ideal.match + TII_score + MA_score +(1|Response.ID) + (1|Item))
summary(M5)

anova(M0, M1, M2, M3, M4, M5)

