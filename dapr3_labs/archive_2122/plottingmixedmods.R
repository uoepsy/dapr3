require(lme4)
require(tidyverse)
dat<-read.csv("data/caff.csv")[,-1] %>%
  rename(outcome="reaction",x1="caffeine",group="coffee_drinker") %>%
  mutate(subject=paste0("sub_",subject))


mod1<-lm(outcome~x1,data=dat)
mod2<-lmer(outcome~x1+(1|subject),data=dat)
mod2pooled<-lm(outcome~x1+subject,data=dat)
mod3<-lmer(outcome~x1+(1+x1|subject),data=dat)



subsannotate<-function(mod){
  if(class(mod)=="lm"){
    x<-enframe(coef(mod1)) %>% pivot_wider %>%
      mutate(
        label1 = paste0("Intercept: ", round(`(Intercept)`,2),
                        "\nSlope: ",round(x1, 2))
      )
  } else {
    x<-coef(mod)$subject
    x %>% mutate(
      subject = row.names(x),
      label1 = paste0("Intercept: ", round(`(Intercept)`,2),
                      "\nSlope: ",round(x1, 2)),
    ) -> x
  }
  geom_text(data=x,aes(label=label1), x=90,y=600, hjust=1,size=3)
}




plot_data<-
  ggplot(dat, aes(x=x1,y=outcome))+
  geom_point(alpha=0.5,col="tomato1")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The data - ", y="y", x="x")+
  NULL


plot_lm <-
  ggplot(dat, aes(x=x1,y=outcome))+
  geom_point(alpha=0.5,col="tomato1")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The simple regression model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod1),col="lm(y~x)"),lwd=1)+
  scale_color_manual(NULL, breaks = c("lm(y~x)","lmer(y~x+(1|subject))",
                                      "lm(y~x+subject)","lmer(y~x+(1+x|subject))"),
                     values=c("lm(y~x)"="#1C86EE",
                              "lmer(y~x+(1|subject))"="#00CD66",
                              "lm(y~x+subject)"="#B000CF",
                              "lmer(y~x+(1+x|subject))"="#008B45"))+
  NULL

plot_lm_fac <-
  ggplot(dat, aes(x=x1,y=outcome))+
  facet_wrap(~subject)+
  geom_point(alpha=0.5,col="tomato1")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The simple regression model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod1),col="lm(y~x)"),lwd=1)+
  scale_color_manual(NULL, breaks = c("lm(y~x)","lmer(y~x+(1|subject))",
                                      "lm(y~x+subject)","lmer(y~x+(1+x|subject))"),
                     values=c("lm(y~x)"="#1C86EE",
                              "lmer(y~x+(1|subject))"="#00CD66",
                              "lm(y~x+subject)"="#B000CF",
                              "lmer(y~x+(1+x|subject))"="#008B45"))+
  subsannotate(mod1)+
  NULL

plot_ri_fac <-
  ggplot(dat, aes(x=x1,y=outcome))+
  facet_wrap(~subject)+
  geom_point(alpha=0.5,col="tomato1")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The random intercept model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod2),col="lmer(y~x+(1|subject))"),lwd=1)+
  scale_color_manual(NULL, breaks = c("lm(y~x)","lmer(y~x+(1|subject))",
                                      "lm(y~x+subject)","lmer(y~x+(1+x|subject))"),
                     values=c("lm(y~x)"="#1C86EE",
                              "lmer(y~x+(1|subject))"="#00CD66",
                              "lm(y~x+subject)"="#B000CF",
                              "lmer(y~x+(1+x|subject))"="#008B45"))+
  subsannotate(mod2)+
  NULL

plot_rs_fac <-
  ggplot(dat, aes(x=x1,y=outcome))+
  facet_wrap(~subject)+
  geom_point(alpha=0.5,col="tomato1")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The random slope model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod3), col="lmer(y~x+(1+x|subject))"),lwd=1)+
  scale_color_manual(NULL, breaks = c("lm(y~x)","lmer(y~x+(1|subject))",
                                      "lm(y~x+subject)","lmer(y~x+(1+x|subject))"),
                     values=c("lm(y~x)"="#1C86EE",
                              "lmer(y~x+(1|subject))"="#00CD66",
                              "lm(y~x+subject)"="#B000CF",
                              "lmer(y~x+(1+x|subject))"="#008B45"))+
  subsannotate(mod3)+
  NULL


plot_shrinkage <-
  dat %>% mutate(fit1=fitted(mod2pooled),fit2=fitted(mod2)) %>%
  filter(str_detect(subject,"352|369|373|374")) %>%
  ggplot(., aes(x=x1,y=outcome))+
  facet_wrap(~subject)+
  geom_point(alpha=0.5,col="tomato1")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(y="y", x="x")+
  geom_line(aes(y=fit1, color="lm(y~x+subject)"), lwd=1)+
  geom_line(aes(y=fit2, color="lmer(y~x+(1|subject))"),lwd=1)+
  scale_color_manual(NULL, breaks = c("lm(y~x)","lmer(y~x+(1|subject))",
                                      "lm(y~x+subject)","lmer(y~x+(1+x|subject))"),
                     values=c("lm(y~x)"="#1C86EE",
                              "lmer(y~x+(1|subject))"="#00CD66",
                              "lm(y~x+subject)"="#B000CF",
                              "lmer(y~x+(1+x|subject))"="#008B45"))+
  NULL

plot_lm2 <-
  ggplot(dat, aes(x=x1,y=outcome))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The simple regression model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod1)),col="#1C86EE",lwd=2)+
  theme(legend.position = "none")+
  NULL

plot_ri <-
  ggplot(dat, aes(x=x1,y=outcome))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The random intercept model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod2), group=subject, col=subject),alpha=0.3,lwd=1)+
  geom_abline(intercept=fixef(mod2)[1],slope=fixef(mod2)[2],lwd=2,col="#00CD66")+
  NULL


plot_rs <-
  ggplot(dat, aes(x=x1,y=outcome))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
  labs(title="- The random slope model - ", y="y", x="x")+
  geom_line(aes(y=fitted(mod3),group=subject,col=subject),alpha=0.3,lwd=1)+
  geom_abline(intercept=fixef(mod3)[1],slope=fixef(mod3)[2],lwd=2, col="#008B45")+
  NULL


# require(patchwork)
# plot_data
# plot_lm
# plot_lm_fac
# plot_lm_fac + plot_ri_fac
# #plot_lmvsri
# plot_shrinkage
#
# plot_rs_fac
#
# plot_lm2 + plot_ri + plot_rs






########
# shufnest<-function(df, i){df %>% filter(!str_detect(subject,"373|374")) %>%
#     mutate(outcome=outcome+rnorm(n(),0,70)) %>%
#     separate(subject,c("sub","sn"),sep="_", convert=TRUE) %>%
#     mutate(subject = paste0("sub_",sn+({{i}}*100))) %>%
#     select(-sub,-sn) %>%
#     mutate(grouping = paste0("group_",{{i}}))
#     }
# shufcross<-function(df, i){df %>% filter(!str_detect(subject,"373|374")) %>%
#     mutate(outcome=outcome+rnorm(n(),0,70)) %>%
#     mutate(grouping = paste0("group_",{{i}}))
# }
#
# nested<-map_dfr(1:3, ~shufnest(dat, .))
#
# crossed<-map_dfr(1:3, ~shufcross(dat, .))
#
# ggplot(nested, aes(x=x1,y=outcome))+
#   geom_point(alpha=0.5,aes(col=subject))+
#   geom_path(alpha=0.5,aes(col=subject))+
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))+
#   scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
#   labs(title="- The data - ", y="y", x="x")+
#   facet_wrap(~grouping)+
#   NULL
#
# ggplot(crossed, aes(x=x1,y=outcome))+
#   geom_point(alpha=0.5,aes(col=subject))+
#   geom_path(alpha=0.5,aes(col=subject))+
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))+
#   scale_y_continuous(breaks=NULL)+scale_x_continuous(breaks=NULL)+
#   labs(title="- The data - ", y="y", x="x")+
#   facet_wrap(~grouping)+
#   NULL


rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}

