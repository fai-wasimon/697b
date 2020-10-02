setwd("C:/Users/tosur/OneDrive/Desktop/POL640/DataHW/AllData")
#library(haven)  # library for read dta files in R
install.packages("tidyverse") 
library(dplyr) # in tidyverse for merging
library(readxl) # to read .xls file  

#### Loading datasets ####
## HIEF
HIEF <- read.csv("HIEF_data.csv") #from 1945-2013

## WDI 
WDI <- read.csv("WDI-selected-longform.csv") # from 1960-2020
names(WDI)[names(WDI) == "ï..Time"] <- "Year"
names(WDI)[names(WDI) == "Country.Name"] <- "Country"


## DD revisited
list.files(path = "C:/Users/tosur/OneDrive/Desktop/POL640/DataHW/AllData")
DD <- read_excel("ddrevisited_data_v1.xls")
View(DD)
names(DD)[names(DD) == "ctryname"] <- "Country"
names(DD)[names(DD) == "year"] <- "Year"

## 


#### Putting two IV datasets together####
##only from 1960-2013 
summary(HIEF$Year)
HIEFsmol <- HIEF %>% filter(Year > 1959 & Year < 2014)
summary(HIEFsmol$Year)

typeof(WDI$Year) #It's integer
summary(WDI$Year) 
WDI$Year <- as.numeric(WDI$Year)+1958 #Make it numeric but 1960 -> 2 so we add 1958
WDIsmol <- WDI %>% filter(Year > 1959 & Year < 2014)
summary(WDIsmol$Year)
View(WDIsmol)

nrow(HIEFsmol) #7608
nrow(WDIsmol) #14256 -- I am curious to why this is about twice as many as the HIEF data set. This is something that I should into later if I actually have to use the dataset for non-casual analysis.

IV = merge(HIEFsmol, WDIsmol, by.x=c("Year", "Country"), by.y=c("Year", "Country"))

nrow(IV) #6447 --> This might be that the merging command drop some countries whose names don't match between two data sets. Again this is something I should look into if I want to use the data for future analysis

#### Regress IV on democracy in DD revisited ####
#### ** Merge
df1 = merge(DD, IV, by.x=c("Year", "Country"), by.y=c("Year", "Country"))
View(df1)
nrow(df1) #5676 --> Again this drop quite a bit of data. I will have to come back later to check what happened.

probitDD <- glm(democracy~EFindex, df1, family=binomial("probit"))
summary(probitDD)
nd=data.frame(EFindex=seq(from=0,to=1,by=0.01))
pred=predict(probitDD, nd ) %>% plogis() 
plot.data<-data.frame(x=nd[,1],y=pred)
names(plot.data)
library(ggplot2)
plt <- ggplot(plot.data, aes(x=x, y=y))+
  geom_line(colour="red", alpha=1)+
  labs(title="Democracy",
       caption="")+
  xlab("EFindex")+
  ylab("Probability") + 
  geom_ribbon(aes(ymin = pred-2*0.06460, ymax = pred+2*0.06460),  alpha = 0.1)
plt

#I got 0.06460 from the Std Error of EFindex prediction