load('MR.Rdata')
library(lubridate)
library(ggplot2)
##Time transfer
MR$YYYY=make_datetime(as.numeric(MR$YYYY),as.numeric(MR$MM),as.numeric(MR$DD),as.numeric(MR$hh))
MR=MR[,-c(2:4)]
MR1=MR[MR$WTMP<99,]
MR2=MR[MR$WTMP>98,]
##
x=0
y=4
z=0
for(i in 1988:2015){
  x[i-1987]=mean(as.numeric(MR1$WTMP[date(MR1$YYYY)>make_date(i)&date(MR1$YYYY)<make_date(i+1)]))
  z[i-1987]=dim(MR2[date(MR2$YYYY)>make_date(i)&date(MR2$YYYY)<make_date(i+1),])[1]
}

D1=data.frame(Time=1988:2015,TMP=x)
D2=data.frame(Time=1988:2015,FREQ=z)
ggplot(D1,aes(Time,TMP))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)
D2