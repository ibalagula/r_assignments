## In a recent mythical poll in Scotland, voters were asked if they preferred Cullen skink over Partan bree.

## Questions:
## Is Cullen skink more popular in Scotland than Partan bree?
## What percentage of population of each city prefer Cullen skink over Partan bree?
## What percentage of people age 16-24 prefer Cullen skink over Partan bree?
## 

library(tidyr)
library(dplyr)
library(plyr)
df <- data.frame(answer = c("Yes", "No"),
Edinburgh.16_24= c(80100,35900),
Edinburgh.25_plus = c(143000,214800),
Glasgow.16_24= c(99400,43000),
Glasgow.25_plus = c(150400,207000))

df1 <- df %>% gather(pop_category,total_cnt, Edinburgh.16_24:Edinburgh.25_plus:Glasgow.16_24:Glasgow.25_plus)
df2 <- df1 %>% separate(pop_category, into = c("city", "age"), sep = "\\.")

## Is Cullen skink more popular in Scotland than Partan bree?
ddply(df2,~answer,function(x) mean(x[,which(colnames(x)=="total_cnt")]))

## What percentage of population of each city prefer Cullen skink over Partan bree? 
## Could not figure out how to do it with plyr
ddply(df2,~city,function(x) sum(x[x$answer=="Yes" ,which(colnames(x)=="total_cnt")])/sum(x[ ,which(colnames(x)=="total_cnt")])) 
ddply(df2,c("city","age"),function(x) sum(x[x$answer=="Yes" ,which(colnames(x)=="total_cnt")])/sum(x[ ,which(colnames(x)=="total_cnt")])) 

## Answering the same question using basic R functions
df3<-merge(x=aggregate(total_cnt ~ answer + city, df2,sum),y=aggregate(total_cnt ~ city, df2,sum),by.x=c("city"),by.y=c("city"))
df3$pct<-df3$total_cnt.x/df3$total_cnt.y
df3

## What percentage of people age 16-24 prefer Cullen skink over Partan bree?
## Answering using basic R functions
df4<-merge(x=aggregate(total_cnt ~ answer + age, df2,sum),y=aggregate(total_cnt ~ age, df2,sum),by.x=c("age"),by.y=c("age"))
df4$pct<-df4$total_cnt.x/df4$total_cnt.y
df4

## I didn't find plyr package to be convenient after data were normalized. I could not figure out how to answer Question 2 using plyr but was able to 
## get results with the basic R easier and faster. But it could be due to my lack of experience for plyr.
  