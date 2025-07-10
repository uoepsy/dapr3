load(url("https://uoepsy.github.io/data/wellbeingwork3.rda"))

qs = sample(wellbeingwork3$ID,12)

pptplots = 
  ggplot(wellbeingwork3 |> filter(ID %in% qs),
       aes(x = TimePoint, y = Wellbeing, col = Condition))+
  geom_point(size=3,alpha=.4)+
  guides(col="none")+
  facet_wrap(~ID)

lm_mod = lm(Wellbeing ~ TimePoint, data = wellbeingwork3)

pptplots + 
  geom_line(aes(y = fitted(lm_mod)[wellbeingwork3$ID %in% qs]),
            col="blue")

ri_mod = lmer(Wellbeing ~ TimePoint + (1 | ID), data = wellbeingwork3)

pptplots + 
  geom_line(aes(y = fitted(ri_mod)[wellbeingwork3$ID %in% qs]),
            col="red")


rs_mod = lmer(Wellbeing ~ TimePoint + (1 + TimePoint | ID), data = wellbeingwork3)

pptplots + 
  geom_line(aes(y = fitted(rs_mod)[wellbeingwork3$ID %in% qs]),
            col="orange")


fe_mod = lm(Wellbeing ~ TimePoint * ID, data = wellbeingwork3)
pptplots + 
  geom_line(aes(y = fitted(fe_mod)[wellbeingwork3$ID %in% qs]),
            col="black") + 
  geom_line(aes(y = fitted(rs_mod)[wellbeingwork3$ID %in% qs]),
            col="orange")

cli_mod = lmer(Wellbeing ~ TimePoint * Condition + (1 + TimePoint | ID), data = wellbeingwork3)

VarCorr(rs_mod)
VarCorr(cli_mod)

pptplots + 
  geom_line(aes(y = fitted(cli_mod)[wellbeingwork3$ID %in% qs],col=Condition))

broom.mixed::augment(ri_mod) |>
  ggplot(aes(x=TimePoint,y=.fitted,group=ID))+
  geom_line(alpha=.3)+
  geom_abline(intercept=fixef(ri_mod)[1],slope=fixef(ri_mod)[2],col="blue",
              size=1)


broom.mixed::augment(rs_mod) |>
  ggplot(aes(x=TimePoint,y=.fitted,group=ID))+
  geom_line(alpha=.3)+
  geom_abline(intercept=fixef(rs_mod)[1],slope=fixef(rs_mod)[2],col="blue",
              size=1)


broom.mixed::augment(cli_mod) |>
  ggplot(aes(x=TimePoint,y=.fitted,group=ID,col=Condition))+
  geom_line(alpha=.5)+
  geom_abline(intercept=fixef(cli_mod)[1],slope=fixef(cli_mod)[2],col="red",size=1) + 
  geom_abline(intercept=sum(fixef(cli_mod)[c(1,3)]),slope=sum(fixef(cli_mod)[c(2,5)]),col="green",size=1)+
  geom_abline(intercept=sum(fixef(cli_mod)[c(1,4)]),slope=sum(fixef(cli_mod)[c(2,6)]),col="blue",size=1)
