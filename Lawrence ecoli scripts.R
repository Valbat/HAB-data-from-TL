library(tidyverse)
library(dplyr)
library(ggplot2)



#read Lawrence data set
Lawrence_ecoli <- read.csv("Lawrence bacteria Monitoring Data_ecoli.csv",stringsAsFactors = FALSE) %>%
  filter(!is.na(E.coli..MPN.100mL.)) %>% 
  filter(!is.na(ï..Site.ID))%>%
  arrange(desc(E.coli..MPN.100mL.))%>%
  select(E.coli..MPN.100mL.,ï..Site.ID, Date)     

View(Lawrence_ecoli)
str(Lawrence_ecoli)
sum(Lawrence_ecoli)
colnames(Lawrence_ecoli)
nrow(Lawrence_ecoli)
na.omit(Lawrence_ecoli$E.coli..MPN.100mL.)
str(Lawrence_ecoli$E.coli..MPN.100mL.)
!is.na(Lawrence_ecoli)

#Let's look at ecoli over stations
ggplot(data=Lawrence_ecoli,
       aes(x=E.coli..MPN.100mL.,y=ï..Site.ID))+
  geom_point()

hist(Lawrence_ecoli$E.coli..MPN.100mL.)
boxplot(Lawrence_ecoli)
hist(Lawrence_ecoli)

#trying to convert to numbers so I can plot it
as.numeric(Lawrence_ecoli$E.coli..MPN.100mL.)%>%
na.omit(Lawrence_ecoli$E.coli..MPN.100mL.)%>%
arrange(Lawrence_ecoli$E.coli..MPN.100mL.)

plot(data=Lawrence_ecoli,
       aes(x=ï..Site.ID,y=E.coli..MPN.100mL.))+
  geom_point()

#This plot works
plot(Lawrence_ecoli$E.coli..MPN.100mL.)

ecoli_Nos <- levels(Lawrence_ecoli$E.coli..MPN.100mL.)
ecoli_Nos
as.numeric(ecoli_Nos)
na.omit(ecoli_Nos)

#now let's see if it will plot through gg plot

ggplot(data=Lawrence_ecoli,
       aes(x=ï..Site.ID,y=ecoli_Nos))+
       geom_point()

#error message indicates I have to change the site id too 

Site_id <- levels(Lawrence_ecoli$ï..Site.ID)

!a.omit(Site_id)



#now let's try tpoplot again
ggplot(data=Lawrence_ecoli,
       aes(x=Site_id, y=ecoli_Nos))+
  geom_point()

plot(Site_id)

sum(Site_id)
str(Site_id)
table(Site_id)
str(ecoli_Nos)
table(ecoli_Nos)

ggplot(data=Lawrence_ecoli,
       aes(x=Site_id, y=ecoli_Nos))+
  geom_point()

? guess
sum(ecoli_Nos)
str(Lawrence_ecoli)


bacteria_data <- filter(Lawrence_ecoli)%>%
       select(E.coli..MPN.100mL.,ï..Site.ID, Date)


arrange(desc(bacteria_data$E.coli..MPN.100mL.))

  bacteria_data
plot(bacteria_data)

boxplot(bacteria_data)
hist(bacteria_data)

ggplot(data=Lawrence_ecoli,
  aes(x=E.coli..MPN.100mL.,y=Date,color=ï..Site.ID))+
  geom_point()
 

ggplot(data=Lawrence_ecoli,
       aes(x=Date,y=E.coli..MPN.100mL.,color=ï..Site.ID))+
  geom_point()

ecoli_Nos <- levels(Lawrence_ecoli$E.coli..MPN.100mL.)%>%
na.omit(ecoli_Nos)%>%
as.numeric(ecoli_Nos)


#This works, but not great
ggplot(data=Lawrence_ecoli,
       aes(x=Date,y=E.coli..MPN.100mL.,color=ï..Site.ID))+
  geom_point()

ggplot(data=Lawrence_ecoli,
       aes(x=ï..Site.ID,y=E.coli..MPN.100mL.,color=ï..Site.ID))+
  geom_point()

summarize(Lawrence_ecoli)
str(Lawrence_ecoli)
sum(Lawrence_ecoli)

dim(Lawrence_ecoli)
str(Lawrence_ecoli)
as.numeric(as.character(bacteria_data$E.coli..MPN.100mL.))%>%
arrange(bacteria_data$E.coli..MPN.100mL.)

