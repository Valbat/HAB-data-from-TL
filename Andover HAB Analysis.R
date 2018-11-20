library(tidyverse)
library(dplyr)
library(ggplot2)

Andover_2018 <-read.csv("Andover_Station_2018.csv")
summary(Andover_2018)
colnames(Andover_2018)


# Look at turbidity and phyco
#multiple regressions(how varilables affect one another)
# what are the best predictors of high Phyco and Cyan

#clean up data
!is.na(Andover_2018)
#brought up a lot of falses, searched the excel and found one false. Remove false and re-read data.


install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")




Andover_2018 <- read.csv("Andover_station_2018.csv",stringsAsFactors = FALSE )


# whay so many falses 11769??


na.omit(Andover_2018$phycocyanin)
na.omit(Andover_2018$chlorophyll)
colnames(Andover_2018)

ggplot(data=Andover_2018,
       aes(x=chlorophyll,turbidity))+
  geom_point()


ggplot(data=Andover_2018,
       aes(x=chlorophyll,y=turbidity))+
  geom_smooth()


ggplot(data=Andover_2018,
       aes(x=phycocyanin, y=turbidity))+
  geom_point()

ggplot(data=dat2017,
       aes(x=phycocyanin, y=ph))+
  geom_point()


ggplot(data=dat2017,
       aes(x=phycocyanin, y=ph))+
  geom_smooth()

ggplot(data=dat2017,
       aes(x=chlorophyll, y=ph))+
  geom_point()

nrow(Andover_2018)
summary(dat2017)
mean(dat2017)

head(dat2017)
dim(dat2017)
ncol(dat2017)
names(dat2017)
rownames(dat2017)
head(rownames(dat2017))
str(dat2017)

na.omit(Andover_2018)

table(dat2017$chlorophyll)

Andoverdat2018<- read.csv(file = "Andover_station_2018.csv", stringsAsFactors = TRUE)

View(Andoverdat2018)

table(Andover_2018$Group)
barplot(Andover_2018$phycocyanin)
barplot(Andover_2018$chlorophyll)
barplot(Andover_2018$ph)
barplot(Andover_2018$turbidity)
barplot(Andover_2018$do)

boxplot(Andover_2018$phycocyanin)
boxplot(Andover_2018$chlorophyll)

boxplot(dat2017$ph)
boxplot(dat2017$do)
boxplot(dat2017$turbidity)
boxplot(dat2017$temp.c)

plot(x=dat2017$phycocyanin,y=dat2017$ph)
plot(x=dat2017$chlorophyll,y=dat2017$ph)




Andoverdat_factors<- read.csv(file = "Andover_station_2018.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(phycocyanin)) %>% 
  filter(!is.na(chlorophyll))%>%
  arrange(desc(phycocyanin),desc(chlorophyll)) %>%
  select(phycocyanin,chlorophyll,turbidity,ph,DO,Temp)         

View(Andoverdat_factors)

hist(Andoverdat_factors$phycocyanin)
hist(Andoverdat_factors$chlorophyll)
hist(Andoverdat_factors$ph)
hist(Andoverdat_factors$turbidity)
hist(Andoverdat_factors$DO)

head(Andoverdat_factors)


str(Andoverdat_factors$turbidity)
summarise(Andoverdat_factors)
summary(Andoverdat_factors)

table(Andoverdat_factors$turbidity)

str(Andoverdat_factors$turbidity)
head(Andoverdat_factors$turbidity)
table(Andoverdat_factors$turbidity)
arrange(desc(Andoverdat_factors$turbidity))

ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin, y=ph))+
  geom_boxplot()

ggplot(data=Andoverdat_factors,
       aes(x=chlorophyll, y=ph))+
  geom_boxplot()

ggplot(data=Andover_factors,
       aes(x=ph, y=chlorophyll))+
  geom_boxplot()


ggplot(data=Andoverdat_factors,
       aes(x=chlorophyll,y=turbidity))+
  geom_boxplot()

ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin, y=turbidity))+
  geom_boxplot()


ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin, y=ph))+
  geom_smooth()
colnames(Andoverdat_factors)


ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin, y=Temp))+
  geom_boxplot()

ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin, y=ph))+
  geom_smooth()

ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin,y=Temp))+
  geom_smooth()

ggplot(data=Andoverdat_factors,
       aes(x=phycocyanin,y=turbidity))+
  geom_smooth()

ggplot(data=Andoverdat_factors,
       aes(x=turbidity,y=phycocyanin))+
  geom_smooth()

ggplot(data=dat2017_factors,
       aes(x=phycocyanin,y=do))+
  geom_smooth()

ggplot(data=Andoverdat_factors,
       aes(x=chlorophyll,y=phycocyanin))+
  geom_smooth()

str(dat2017_factors$phycocyanin)
table(dat2017_factors$phycocyanin)
table(dat2017_factors$chlorophyll)
table(dat2017_factors$turbidity)

dat_turbidity <- filter(dat2017_factors)%>%
  select(date,turbidity)

ggplot(data=dat2017_factors,
       aes(x=phycocyanin,y=chlorophyll))+
  geom_smooth()

#Add a new column with the ratio of phyco over chloro
Andover_2018$phyco_chloro <- Andover_2018$phycocyanin/Andover_2018$chlorophyll
View(Andover_2018)
colnames(Andover_2018)

ggplot(data= Andover_2018,
       aes(x=Date,y=phyco_chloro))+
       geom_smooth()

ggplot(data= Andover_2018,
       aes(x=Date,y=phyco_chloro))+
      geom_point()

#read Lawrence data set
Lawrence_ecoli <- read.csv("Lawrence bacteria Monitoring Data_ecoli.csv")
View(Lawrence_ecoli)
str(Lawrence_ecoli)
sum(Lawrence_ecoli)
colnames(Lawrence_ecoli)
na.omit(Lawrence_ecoli$E.coli..MPN.100mL.)

#Let's look at ecoli over stations
ggplot(data=Lawrence_ecoli,
       aes(x=ï..Site.ID,y=E.coli..MPN.100mL.))+
  geom_point()

hist(Lawrence_ecoli$E.coli..MPN.100mL.)
boxplot(Lawrence_ecoli)
hist(Lawrence_ecoli)

#trying to convert to numbers so I can plot it
as.numeric(Lawrence_ecoli$E.coli..MPN.100mL.) 
ggplot(data=Lawrence_ecoli,
       aes(x=ï..Site.ID,y=E.coli..MPN.100mL.))+
  geom_point()

ecoli_Nos <- levels(Lawrence_ecoli$E.coli..MPN.100mL.)

as.numeric(as.character(ecoli_Nos))
na.omit(ecoli_Nos)

#now let's see if it will plot
 
ggplot(data=Lawrence_ecoli,
       aes(x=ï..Site.ID,y=ecoli_Nos))+
  geom_point()

#error message indicates I have to change the site id too 

Site_id <- levels(Lawrence_ecoli$ï..Site.ID)

as.numeric(as.character(Site_id))
na.omit(Site_id)

Site_id <- as.character(Lawrence_ecoli$ï..Site.ID)


#now let's try tpoplot again
ggplot(data=Lawrence_ecoli,
       aes(x=Site_id, y=ecoli_Nos))+
        geom_point()

plot(Site_id)
#This plot is good
plot(ecoli_Nos)  

sum(Site_id)
str(Site_id)
table(Site_id)

ggplot(data=Lawrence_ecoli,
          aes(x=Site_id, y=ecoli_Nos))+
  geom_point()

? guess
sum(ecoli_Nos)
