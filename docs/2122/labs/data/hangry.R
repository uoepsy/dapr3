require(tidyverse)
require(lme4)
set.seed(356)
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
        e = rnorm(N, sd = 3)
        
        y = 0 + dd[igs,1]+dd2[i,1]+ddx[xgs, 1] + 
            1*xlc +
            0*xm+
            dd[igs,2]*xlc +
            xx + 
            .5*l3p*xlc +
            1*l3p*xm+
            e
        d = data.frame(y,x,xx,xm,xlc,igs,xgs,i, l3p)
        ggplot(d,aes(x=x,y=y,group=factor(igs)))+facet_wrap(~xgs)+geom_path()
        d$ng2 = i
        df<-rbind(df,d)
    }
    
    df %>% filter(xgs==1) %>% group_by(igs) %>% mutate(xm = mean(x), xc = x-xm) %>% ungroup %>%
        lmer(y ~ (xc + xm)*l3p + (1+xc|igs:ng2) + (1+xm|ng2), data = . ,control=lmerControl(optimizer = "bobyqa")) -> m
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


df %>% filter(xgs==1) %>%
    transmute(
        q_irritability = pmax(0,round((scale(y)[,1]*12.42)+23.5872)),
        q_hunger = xm+xlc,
        q_hunger = round(47.38+scale(q_hunger)[,1]*16.423),
        nutritionist = paste0("N",ng2),
        ppt = paste0("N",ng2,"p",igs),
        fivetwo = l3p
) %>% write.csv("../../data/hangry.csv",row.names=F)
    ggplot(.,aes(x=q_hunger,y=q_irritability))+
    geom_line(aes(group=ppt))+
    facet_wrap(~fivetwo)


