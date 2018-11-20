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