set.seed(987)
tibble(
  id = letters[1:10],
  start = seq(5,20,length.out=10),
  end = seq(15,30,length.out=10),
  coef = rnorm(10,-2)
) %>% mutate(
  x = map2(start,end, ~runif(10,.x,.y)),
  y = map2(x, coef,~.x*.y)
) %>% unnest %>%
  mutate(y=y+10*start+rnorm(n(),0,3)) %>%
  ggplot(.,aes(x=x,y=y,col=id))+geom_point()
