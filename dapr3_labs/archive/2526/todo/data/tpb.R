set.seed(235)
m = "
att =~ 0.689*attitude1+0.726*attitude2+0.689*attitude3+0.719*attitude4
SN =~ 0.661*SN1+0.651*SN2+0.616*SN3+0.638*SN4
PBC =~ 0.799*PBC1+0.772*PBC2+0.756*PBC3+0.773*PBC4
intent =~ 0.584*int1+0.646*int2+0.625*int3+0.597*int4+0.6*int5
beh =~ 0.649*beh1+0.599*beh2+0.588*beh3+-0.605*beh4

int2~~.4*int4
int1~~.08*int3
SN3~~.2*SN4
PBC1~~.1*PBC2
beh2~~.1*beh3

beh ~ 0.506*intent + 0.185*PBC
intent ~ 0.359*att + 0.196*SN + 0.49*PBC

att~~0.32*SN
att~~0.253*PBC
SN~~0.196*PBC
"
df = simulateData(m, sample.nobs = 890)
df = as.data.frame(apply(df, 2, function(x) as.numeric(cut(x,7,labels=1:7))))
TPB_data<-df







