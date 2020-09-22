###Library packages
library(tidyverse)
library(stringr)

###Make URLs and read Data
##Make URLs
url1="http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2=".txt.gz&dir=data/historical/stdmet/"
years=c(1987:2016)
urls=paste0(url1,years,url2)
Dnames=paste0('D',years)
##Read the data from the URLs
for(i in years) assign(Dnames[i-years[1]+1],read_table2(urls[i-years[1]+1]))
coln=colnames(get(Dnames[1]))

###Combine the data into a frame
for(i in years){
  D=get(Dnames[i-years[1]+1])
  ##From Y2000 to Y2016, delete an additional variable of 'TIDE'
  if(i %in% 2000:2016){D=select(D,-TIDE)}
  ##From Y2005 to Y2016, delete an additional variable of 'mm'
  if(i %in% 2005:2016){D=select(D,-mm)}
  ##From Y2007 to Y2016, delete first row of units
  if(i %in% 2007:2016){D=D[-1,]}
  ##Check and unify col names and set data type as 'numeric'
  if(ncol(D)==length(coln)){colnames(D)=coln}
  D=sapply(D, as.numeric)
  ##From Y1987 to Y1999, transfer the Year from 'XX' to '19XX'
  D[,1][D[,1]<100]=D[,1][D[,1]<100]+1900
  ##Create and combine to form final data set Buoy
  if(i==years[1]){Buoy=D}
  else{Buoy=rbind.data.frame(Buoy,D)}
}

###Save data
save(Buoy,file='Buoy.Rdata')