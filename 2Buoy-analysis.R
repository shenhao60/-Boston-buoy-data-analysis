load('Buoy.Rdata')
library(lubridate)
library(ggplot2)
###Time transfer
Buoy$YYYY=make_datetime(Buoy$YY,Buoy$MM,Buoy$DD,Buoy$hh)
Buoy=Buoy[,-c(2:4)]
###Analysis using 'Water Temperature'
##Delete those lines with 999.0 and 99.0
Buoy_W=Buoy[Buoy$WTMP<99,]
##
x=0
y=4
z=0
for(i in 1988:2015){
  x[i-1987]=mean(as.numeric(Buoy1$WTMP[date(Buoy1$YYYY)>make_date(i)&date(Buoy1$YYYY)<make_date(i+1)]))
  z[i-1987]=dim(Buoy2[date(Buoy2$YYYY)>make_date(i)&date(Buoy2$YYYY)<make_date(i+1),])[1]
}

D1=data.frame(Time=1988:2015,TMP=x)
D2=data.frame(Time=1988:2015,FREQ=z)
ggplot(D1,aes(Time,TMP))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)
D2
###Analysis using 'Air Temperature'
##Delete those lines with 999.0 and 99.0
Buoy_A=Buoy[Buoy$ATMP<99,]
##
x=0
y=4
z=0
for(i in 1988:2015){
  x[i-1987]=mean(as.numeric(Buoy1$WTMP[date(Buoy1$YYYY)>make_date(i)&date(Buoy1$YYYY)<make_date(i+1)]))
  z[i-1987]=dim(Buoy2[date(Buoy2$YYYY)>make_date(i)&date(Buoy2$YYYY)<make_date(i+1),])[1]
}

D1=data.frame(Time=1988:2015,TMP=x)
D2=data.frame(Time=1988:2015,FREQ=z)
ggplot(D1,aes(Time,TMP))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)
D2