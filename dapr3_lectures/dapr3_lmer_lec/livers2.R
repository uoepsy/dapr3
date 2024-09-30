#- how many breeds?
#  - how many dogs (puppies / adults) per breed

new_trick_dogbreeds |>
  count(BREED, isOld, DOG) |>
  select(BREED, isOld) |> table()

new_trick_dogbreeds |> 
  select(BREED,DOG) |>
  table()

ggplot(new_trick_dogbreeds, aes(x=trainingdays,y=tricktime,col=isOld))+
  stat_smooth(method=lm)+
  facet_wrap(~BREED)

mm = lmer(tricktime ~ trainingdays * isOld + 
       (1 + trainingdays * isOld | BREED),
     data = new_trick_dogbreeds)

broom.mixed::augment(mm) |>
ggplot(aes(x=trainingdays,y=.fitted,col=isOld))+
  stat_smooth(method=lm)+
  facet_wrap(~BREED)




lmer(tricktime ~ trainingdays * isOld + 
       (1 + trainingdays + isOld | BREED) + 
       (1 + trainingdays | DOG:BREED),
     data = new_trick_dogbreeds) |> summary()


# many tricks

many_new_tricks |>
  select(trick, DOG) |>
  table()

ggplot(many_new_tricks, aes(x=trainingdays,y=tricktime,col=trick))+
  geom_point()+
  facet_wrap(~DOG)
