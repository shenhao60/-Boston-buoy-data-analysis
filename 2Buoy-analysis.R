library(lubridate)
library(ggplot2)
library(ggpubr)
load('Buoy.Rdata')

###Time transfer
Buoy$DT=make_datetime(Buoy$YY,Buoy$MM,Buoy$DD,Buoy$hh)
Buoy=Buoy[,-c(2:4)]

###Analysis using 'Water Temperature'
##Delete those lines with 999.0 and 99.0
Buoy_W=Buoy[Buoy$WTMP<99,]
##Trend of average temperature per year
Y_W=0
for(i in 1988:2015){
  Y_W[i-1987]=mean(Buoy_W$WTMP
                   [date(Buoy_W$DT)>=make_date(i)&
                       date(Buoy_W$DT)<make_date(i+1)]
                   )
}
D_W=data.frame(Time=1988:2015,TMP=Y_W)
P_W=ggplot(D_W,aes(Time,TMP))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)
P_W
##Trend of monthly average temperature per year
M_W=0
P_WM_name=str_c('P_W',1:12,sep='')
for(j in 1:12){
  for(i in 1988:2015){
    M_W[i-1987]=mean(Buoy_W$WTMP
                     [date(Buoy_W$DT)>=
                         make_date(i,j)&
                         date(Buoy_W$DT)<
                         make_date(ifelse(j==12,i+1,i),
                                   ifelse(j==12,1,j+1)
                                   )
                      ]
                     )
  }
  D_W=data.frame(Time=1988:2015,TMP=M_W)
  assign(P_WM_name[j],
           ggplot(D_W,aes(Time,TMP))
            +geom_point()
            +geom_smooth(method="lm",formula=y~x)
         )
}
ggarrange(P_W1,P_W2,P_W3,P_W4,P_W5,P_W6,
          P_W7,P_W8,P_W9,P_W10,P_W11,P_W12,
          ncol=2,nrow=6,
          labels=c('Jan','Feb','Mar','Apr',
                    'May','Jun','Jul','Aug',
                    'Sep','Oct','Nov','Dec')
          )
###Analysis using 'Air Temperature'
##Delete those lines with 999.0 and 99.0
Buoy_A=Buoy[Buoy$ATMP<99,]
##Trend of average temperature per year
Y_A=0
for(i in 1988:2015){
  Y_A[i-1987]=mean(Buoy_A$ATMP
                   [date(Buoy_A$DT)>=make_date(i)&
                       date(Buoy_A$DT)<make_date(i+1)]
  )
}
D_A=data.frame(Time=1988:2015,TMP=Y_A)
P_A=ggplot(D_A,aes(Time,TMP))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)
P_A
##Trend of monthly average temperature per year
M_A=0
P_AM_name=str_c('P_A',1:12,sep='')
for(j in 1:12){
  for(i in 1988:2015){
    M_A[i-1987]=mean(Buoy_A$ATMP
                     [date(Buoy_A$DT)>=
                         make_date(i,j)&
                         date(Buoy_A$DT)<
                         make_date(ifelse(j==12,i+1,i),
                                   ifelse(j==12,1,j+1)
                         )
                     ]
    )
  }
  D_A=data.frame(Time=1988:2015,TMP=M_A)
  assign(P_AM_name[j],
         ggplot(D_A,aes(Time,TMP))
         +geom_point()
         +geom_smooth(method="lm",formula=y~x)
  )
}
ggarrange(P_A1,P_A2,P_A3,P_A4,P_A5,P_A6,
          P_A7,P_A8,P_A9,P_A10,P_A11,P_A12,
          ncol=2,nrow=6,
          labels=c('Jan','Feb','Mar','Apr',
                   'May','Jun','Jul','Aug',
                   'Sep','Oct','Nov','Dec')
)