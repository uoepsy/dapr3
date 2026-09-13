library(tidyverse)
library(patchwork)
df <- tibble(h = round(rnorm(100,162,12),1))
p1 <- ggplot(df,aes(x=h))+
  geom_dotplot()+
  labs(title="EU study",x="Height (cm)")

df2 <- df |> mutate(
  h2 = h/30.48)
p2 <- ggplot(df2,aes(x=h2))+
  geom_dotplot()+
  labs(title="US study",x="Height (feet)")



# which has greater variance?  
p1 + p2
ggsave("woo/woo_measure1.png")
# if i standardise both variables, which will have the greater variance? 


df3 <- df2 |> mutate(
  ss = round(2*(41.3+scale(rnorm(100, .6*h, 10))[,1]*1.9))/2,
  ss2 = ss-33
)

p3 <- ggplot(df3, aes(x=h,y=ss))+
  geom_point(size=5)+
  labs(title="EU study",x="Height (cm)",
       y="Shoe Size (EU)")
  

p4 <- ggplot(df3, aes(x=h2,y=ss2))+
  geom_point(size=5)+
  labs(title="US study",x="Height (feet)",
       y="Shoe Size (US)")

p3 + p4 
ggsave("woo/woo_measure2.png")

# which is true?
# covariance will be positive, bigger for plot C
# covariance will be negative, bigger for plot D
# covariance will be positive, bigger for plot C
# covariance will be negative, bigger for plot D


# if i standardise all variables, which will have the greater covariance? 


