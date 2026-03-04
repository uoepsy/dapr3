require(tidyverse)
require(lme4)
doit<-1
while (doit) {
    Ngroup2s = 9
    dd2<-MASS::mvrnorm(n=Ngroup2s, mu = c(0,0), Sigma = matrix(c(1,0,0,1),byrow = T, nrow=2))
    cor(dd2)
    
    df<-as.data.frame(c())
    for(i in 1:Ngroup2s){
        Ngroups = round(rnorm(1,10,2))
        Nxgroups = 3
        nrep = 5
        NperGroup = rep(Nxgroups*nrep,Ngroups)
        N = sum(NperGroup)
        groups = map(seq_along(NperGroup), ~rep(.,NperGroup[.])) %>% unlist() %>% factor
        
        dd<-MASS::mvrnorm(n=Ngroups, mu = c(0,0), Sigma = matrix(c(1,0,0,1),byrow = T, nrow=2))
        ddx<-MASS::mvrnorm(n=Nxgroups, mu = c(0,0), Sigma = matrix(c(1,0,0,1),byrow = T, nrow=2))
        
        igs = map(seq_along(NperGroup), ~rep(.,NperGroup[.])) %>% unlist
        xgs = map(1:Ngroups, ~rep(1:Nxgroups,each = nrep)) %>% unlist
        xl = map(1:Ngroups, ~rnorm(nrep*Nxgroups, ., 3))
        xm = map(xl, ~rep(mean(.), nrep*Nxgroups)) %>% unlist()
        xlc = map(xl, ~.-mean(.)) %>% unlist
        x = unlist(xl)
        
        xx = map(1:Ngroups, ~rep(rdunif(1,0,10), NperGroup[.]))%>% unlist
        l3p = i%%2
        e = rnorm(N, sd = .1)
        
        y = 0 + dd[igs,1]+dd2[i,1]+ddx[xgs, 1] + 
            0*xlc +
            0*xm+
            dd[igs,2]*xlc +
            ddx[xgs, 2]*xm + 
            dd2[i,2]*xm +
            xx + 
            l3p +
            10*l3p * xm +
            e
        d = data.frame(y,x,xx,igs,xgs,i, l3p)
        ggplot(d,aes(x=x,y=y,group=factor(igs)))+facet_wrap(~xgs)+geom_path()
        d$ng2 = i
        df<-rbind(df,d)
    }
    # ggplot(df[df$xgs==1,],aes(x=x,y=y))+facet_wrap(~ng2)+
    #     geom_path(aes(group=factor(igs)),alpha=.3)+
    #     #geom_smooth(method="lm",se=F)+
    #     NULL
    # 
    # df %>% filter(xgs==1) %>%
    #     group_by(ng2,igs) %>%
    #     mutate(x=scale(x,scale=F,center=T)) %>% ungroup %>%
    #     ggplot(.,aes(x=x,y=y))+facet_wrap(~ng2)+
    #     geom_path(aes(group=factor(igs)),alpha=.3)+
    #     #geom_smooth(method="lm",se=F)+
    #     NULL
    df %>% filter(xgs==1) %>% group_by(igs) %>% mutate(xm = mean(x), xc = x-xm) %>% ungroup %>%
        lmer(y ~ xc + xm*l3p + (1+xc|igs:ng2) + (1+xm|ng2), data = . ,control=lmerControl(optimizer = "bobyqa")) -> m
        print(VarCorr(m))

        t1 = attributes(VarCorr(m)[[1]])$stddev
        t2 = attributes(VarCorr(m)[[1]])$correlation
        t3 = attributes(VarCorr(m)[[2]])$stddev
        t4 = attributes(VarCorr(m)[[2]])$correlation

        if(!isSingular(m) & all(t1 != 0) & !(t2[lower.tri(t2)] %in% c(0,1,-1)) & all(t3 != 0) & !(t4[lower.tri(t4)] %in% c(0,1,-1)) ){
            doit <- 0
        }
}

summary(m)
ggplot(df[df$xgs==1,],aes(x=x,y=y,col=factor(l3p)))+facet_wrap(xgs~ng2)+
        geom_path(aes(group=factor(igs)),alpha=.3)+
        #geom_smooth(method="lm",se=F)+
        NULL

summary(m)
