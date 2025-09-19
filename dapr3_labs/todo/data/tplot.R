as.data.frame(ranef(rsmod)$schoolid) |>
  rownames_to_column() |>
  ggplot(aes(x=motiv))+
  geom_area(stat = "function", fun = dnorm,args=list(mean=0,sd=sqrt(VarCorr(rsmod)[[1]][2,2])),fill="#a41ae4",xlim=c(-6,6),alpha=.3)+
  with_blur(geom_rug(alpha=.7,aes(col=rowname),lwd=1,
                     length = unit(0.05, "npc")),sigma=1) +
  geom_segment(x=0,
               xend=sqrt(VarCorr(rsmod)[[1]][2,2]),
               y=0.009,yend=0.009,
               col="darkorange3",lwd=1)+
  annotate("text",label=expression(sigma["1"]),size=5,
           x=1,y=0.021,col="darkorange3")+
  geom_vline(xintercept=0,col="#a41ae4")+
  annotate("text",label=expression(gamma["11"]),size=5,
           x=0,y=0.08,col="#a41ae4",hjust=-.2)+
  scale_y_continuous(NULL,breaks=NULL)+
  scale_x_continuous(expression(zeta["1i"]))+
  guides(col="none")+
  annotate("text",label="Hutchesons'\nGrammar School",
           x=-3.5,y=.03,col="green4",vjust=0)+
  geom_segment(x=-3.02,xend=-3.5,y=0,yend=0.03,
               col="green4")


library(ggside)

as.data.frame(ranef(rsmod)$schoolid) |>
  rownames_to_column() |>
  mutate(int=`(Intercept)`) |>
  ggplot(aes(x=int,y=motiv)) +
  geom_point() + 
  geom_density2d() +
  geom_xsidedensity(fill = "blue", alpha = .3) +
  geom_ysidedensity(fill = "blue", alpha = .3) +
  stat_xsidefunction(fun = dweibull, args = list(shape = 0, scale = sqrt(VarCorr(rsmod)[[1]][1,1])), 
                     colour = "#a41ae4") +
  stat_ysidefunction(fun = dweibull, args = list(shape = 0, scale = sqrt(VarCorr(rsmod)[[1]][2,2])), 
                                                 colour = "#a41ae4")


