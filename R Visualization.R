setwd('C:/Users/Nithin/Downloads/R - Visualization case study/R case study 3 (Visualization)')

#**************************************************************************************************************#

#########################
#-->Required Packages<--#
#########################
require(dplyr)
require(ggplot2)
require(lubridate)
require(tidyr)
require(scales)
require(plotrix)

#**************************************************************************************************************#

################
#-->Datasets<--#
################

sales <- read.csv('SalesData.csv')

#**************************************************************************************************************#

#1.Compare Sales by region for 2016 with 2015 using bar chart

df <- sales[,c('Region','Sales2015','Sales2016')]
df2 <- gather(df,key = 'Year',value = 'sales',2:3)
df3 <- df2 %>% dplyr::group_by(Region,Year) %>% summarise(Tot_sale = sum(sales,na.rm = T))

ggplot2::ggplot(df3) + aes(y = Tot_sale,x =Region,fill = Year) + geom_bar(stat = 'identity',position = 'dodge')+geom_text(aes(label=Tot_sale))+
  scale_y_continuous(labels = scales::comma)

#**************************************************************************************************************#

#2.Pie charts for sales for each region in 2016

df4 <- sales %>% group_by(Region) %>% summarise(tot_sale = sum(Sales2016,na.rm = T))

pie(df4$tot_sale,paste0(df4$Region,' ',round(df4$tot_sale/sum(df4$tot_sale)*100,2),' %'))

pie3D(df4$tot_sale,labels = paste0(df4$Region,' ',round(df4$tot_sale/sum(df4$tot_sale)*100,2),' %'),explode = 0.05)

#**************************************************************************************************************#

#3.Compare sales of 2015 and 2016 with Region and Tiers

df5 <- sales[,c('Region','Tier','Sales2015','Sales2016')]
df5 <- gather(df5,key = 'salesyr',value = 'tot_sale',3:4)
df6 <- df5 %>% dplyr::group_by(Region,Tier,salesyr) %>% summarise(tot_sale = round(sum(tot_sale,na.rm = T),2))

ggplot2::ggplot(df6) + aes(x = Tier,y = tot_sale,fill = salesyr) + geom_col() + facet_grid(.~Region)

#**************************************************************************************************************#

#4.In East region, which state registered a decline in 2016 as compared to 2015?

df7 <- sales[sales$Region == 'East',c('State','Sales2015','Sales2016')]
df8 <- gather(df7,key = 'saleyr','tot_sale',2:3)
df9 <- df8 %>% group_by(State,saleyr) %>% summarise('tot_sale' = sum(tot_sale))

ggplot2::ggplot(df9) + aes(x = State,y = tot_sale , fill = saleyr) + geom_bar(stat = 'identity',position = 'dodge') + 
  scale_y_continuous(labels = scales::comma)

#**************************************************************************************************************#

#5.In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015?
df10 <- sales[sales$Tier == 'High',c('Division','Units2015','Units2016')]
df11 <- gather(df10,'saleyr','tot_ut',2:3)
df12 <- df11 %>% group_by(Division,saleyr) %>% summarise('tot_ut' = sum(tot_ut))

ggplot(df12) + aes(x = Division,y = tot_ut,fill = saleyr) + geom_bar(stat = 'identity',position = 'dodge')

#**************************************************************************************************************#

#6.Create a new column Qtr -
#   .	Jan - Mar : Q1
#   .	Apr - Jun : Q2
#   .	Jul - Sep : Q3
#   .	Oct - Dec : Q4
sales['Qtr'] <- ifelse(sales$Month %in% c('Jan','Feb','Mar') , 'Q1',
                if_else(sales$Month %in% c('Apr','May','Jun'),'Q2',
                if_else(sales$Month %in% c('Jul','Aug','Sep'),'Q3','Q4'
                )))  
       
#**************************************************************************************************************#

#7.Compare Qtr wise sales in 2015 and 2016 in a bar plot
df13 <- sales[,c('Qtr','Sales2015','Sales2016')]
df14 <- gather(df13,key = 'saleyr',value = 'tot_sale',2:3)
df15 <- df14 %>% group_by(Qtr,saleyr) %>% summarise('tot_sale' = sum(tot_sale) )

ggplot(df15) + aes(x = Qtr,y =tot_sale,fill = saleyr) + geom_col(position = 'dodge') + 
  scale_y_continuous(labels = scales::comma)

#**************************************************************************************************************#

#8.Determine the composition of Qtr wise sales in and 2015 with regards to all the Tiers in a pie chart.
#  (Draw 4 pie charts representing a Quarter for each Tier)

df16 <- sales %>% group_by(Qtr,Tier)%>% filter(Qtr == 'Q1') %>% summarise('tot_sale'= sum(Sales2015))
df17 <- sales %>% group_by(Qtr,Tier)%>% filter(Qtr == 'Q2') %>% summarise('tot_sale'= sum(Sales2015))
df18 <- sales %>% group_by(Qtr,Tier)%>% filter(Qtr == 'Q3') %>% summarise('tot_sale'= sum(Sales2015))
df19 <- sales %>% group_by(Qtr,Tier)%>% filter(Qtr == 'Q4') %>% summarise('tot_sale'= sum(Sales2015))

par(mfrow = c(2,2)) 
pie(df16$tot_sale,labels = df16$Tier,radius = 1,main = 'Qtr 1')
pie(df17$tot_sale,labels = df17$Tier,radius = 1,main = 'Qtr 2')
pie(df18$tot_sale,labels = df18$Tier,radius = 1,main = 'Qtr 3')
pie(df19$tot_sale,labels = df19$Tier,radius = 1,main = 'Qtr 4')

#**************************************************************************************************************#

