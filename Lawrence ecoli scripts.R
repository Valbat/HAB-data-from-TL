install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

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

#stringer package finds and replaces characters with numbers
#str function
#use lubridate

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
arrange(desc(Lawrence_ecoli$E.coli..MPN.100mL.))

str(Lawrence_ecoli)

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
       aes(x=Date,y=E.coli.MPN.100mL.,color=ï..Site.ID))+
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

# Cleaned the data so hopefully can better analyze it
Lawrence_ecoli_cleandataset <- read.csv("Clean_Ecoli_dataset_Lawrence.csv",stringsAsFactors = FALSE)
View(Lawrence_ecoli_cleandataset)
 
colnames(Lawrence_ecoli_cleandataset)

Lawrence_ecoli_cleandataset
str(Lawrence_ecoli_cleandataset)   
na.omit(Lawrence_ecoli_cleandataset)
arrange(Lawrence_ecoli_cleandataset,E.coli._MPN_100mL)
colnames(Lawrence_ecoli_cleandataset)

ggplot(data=Lawrence_ecoli_cleandataset,
       aes(x=Date,y=E.coli._MPN_100mL,color=ï..Site.ID))+
  geom_point()


ggplot(data=Lawrence_ecoli_cleandataset,
       aes(x=ï..Site.ID,y=E.coli._MPN_100mL,color=ï..Site.ID))+
  geom_histogram()

ggplot(data=Lawrence_ecoli_cleandataset,
       aes(x=E.coli._MPN_100mL,y=ï..Site.ID,color=ï..Site.ID))+
  geom_point()

ggplot(data=Lawrence_ecoli_cleandataset,
       aes(x=ï..Site.ID,y=E.coli._MPN_100mL,color=ï..Site.ID))+
  geom_bar()

hist(Lawrence_ecoli_cleandataset$E.coli._MPN_100mL)
boxplot(Lawrence_ecoli_cleandataset$E.coli._MPN_100mL)

Ecoli_site <-Lawrence_ecoli_cleandataset %>%
  group_by(ï..Site.ID,E.coli._MPN_100mL,Date)


View(Ecoli_site)
View(phyco_state)
chla_gg <- ggplot(data=dat2015, mapping = aes(x=sample_date,y=chla_ugl)) +
  geom_point() +
  facet_wrap(~ state) +
  geom_smooth(method = "lm")