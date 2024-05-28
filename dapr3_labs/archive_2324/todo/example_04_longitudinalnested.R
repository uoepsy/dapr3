





sim1 <- function(){
  z0 = runif(1,.3,2)
  z1 = runif(1,.1,.6)
  b0 = runif(1,.5,1)
  b1 = runif(1, -.2,-.1)
  b2 = runif(1,-1.5,-.7)
  b3 = runif(1,.1,.3)
  n_groups = round(runif(1,12,30))
  N = n_groups*10*5
  g = rep(1:n_groups, e = N/n_groups)      # ppt id
  x = rep(rep(1:10,5), n_groups) # time
  d = rep(rep(1:5,e=10), n_groups) # domain
  piq = rep(rnorm(n_groups), e = N/n_groups)
  re0 = rnorm(n_groups, sd = z0)  # random intercepts
  re  = re0[g]
  rex = rnorm(n_groups, sd = z1)  # random effects
  re_x  = rex[g]
  lp = (b0 + re) + ((b1 + re_x)*x) + (b2*(d==5)) + (b3*x*(d==5)) + runif(1,.2,.9)*piq
  y = rnorm(N, mean = lp, sd = 2) # create a continuous target variable
  data.frame(x, d=factor(d),piq=piq, g=g, y)
}

set.seed(136)
df <- purrr::map_dfr(1:17,~sim1(),.id="c")

lab_ppts = unique(replicate(1e4, paste0("PPT_",paste(sample(letters,4),collapse=""))))
lab_cs = paste0("Cn",1:17)

df |> 
  mutate(
    domain = case_when(
      d == 1 ~ 'processing_speed',
      d == 2 ~ 'spatial_visualisation',
      d == 3 ~ 'memory',
      d == 4 ~ 'reasoning',
      d == 5 ~ 'vocabulary'
    ),
    age = 45+(3*x),
    c = as.numeric(c),
    pptID = map2_chr(g,c,~lab_ppts[(..2*100 + ..1)]),
    cID = paste0("Cn",c),
    baselineIQ = round(piq,3),
    educ = round(16+scale(baselineIQ)[,1]*2),
    y = round(y,3)
  ) |> 
  select(cID,pptID,educ,age,domain,y) |>
  pivot_wider(names_from="domain",values_from="y") |>
  mutate(
    age = round(age + rnorm(n(),0,.5))
  ) -> dff

write_csv(dff, "../../data/cogdecline.csv")


dflong <- dff |> pivot_longer(5:9)
dflong$agez = dflong$age-min(dflong$age)
dflong$name = factor(dflong$name)
m = lmer(value ~ educ + agez*name + 
           (1+educ|cID) + 
           (1+agez|pptID),
         dflong,control=lmerControl(optimizer="bobyqa"))  
summary(m)
sjPlot::plot_model(m,type="int")



# These data are simulated to represent a large scale international study of cognitive aging, for which data from 17 research centers has been combined. The study team are interested in whether different cognitive domains have different trajectories over age. Each of the 17 research centers recruited a minimum of 14 participants (Median = 21, Range 14-29) at age 45, and recorded their IQ at the baseline visit. Participants were then tested on 5 cognitive domains: processing speed, spatial visualisation, memory, reasoning, and vocabulary. Participants were contacted for follow-up on a further 9 occasions (resulting in 10 datapoints for each participant), and at every follow-up they were tested on the same 5 cognitive domains. Follow-ups were on average 3 years apart (Mean = 3, SD = 0.8). 

# The researchers 

