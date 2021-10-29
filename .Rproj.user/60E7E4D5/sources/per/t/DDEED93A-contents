#Linear Regression Script
# compare or decipher a relationship between 2 numerical categories

library(dplyr)
library(ggplot2)
library(readr)
library(knitr)
library(leaflet)


 #Clear R's brain
rm(list = ls())

#Import data
SMR <- read_csv("Master_Database_SMR only .csv")


###

#Graph males and females w/ shedding individuals 

ggplot(SMR, aes(x= Mass , y= Average_MR , shape=Sex, lty = Sex))+
  geom_point(aes(size = 15))+
  geom_smooth(method=(lm), color = "forestgreen", se= FALSE)+
  xlab("Body Mass (g)") + ylab("Oxygen Consumption (mL O2 hour-1)")+
  theme_bw(base_size = 25)+
  theme(legend.position = "none")

#Analyze 
Sex_SMR2 <- lm(Average_MR ~ Mass + Sex, data = SMR)
Sex_anova2 <- anova(Sex_SMR)
summary(Sex_SMR2)

###
ggplot(SMR, aes(x= Basetime , y= Basetemp))+
  geom_point(aes(size = 15))+
  geom_smooth(method=(lm), se= FALSE)+
  xlab("Base Time (minutes)") + ylab("Base Temperature (C)")+
  theme_bw(base_size = 25)+
  theme(legend.position = "none")

blood <- lm(BaseTemp ~ Basetime, data = SMR)
blood_anova <- anova(blood)
summary(blood_anova)


### 

#Shedding and not shedding 
ggplot(SMR, aes(x= Mass , y= Average_MR, shape=Shedding, lty = Shedding))+
  geom_point(aes(size = 15))+
  geom_smooth(method=(lm), color = "forestgreen", se = FALSE)+
  xlab("Body Mass (g)") + ylab("Oxygen Consumption (mL O2 hour-1)")+
  theme_bw(base_size = 25)+
  theme(legend.position = "none")

Shed_SMR <- lm(Average_MR ~ Mass + Shedding, data = SMR)
Shed_anova <- anova(Shed_SMR)
summary(Shed_SMR)

# Nullhypothesis states that there is no difference between x an y
# p<0.05 reject the null; there is a significant difference
# p>0.05 accept the null: there is no significant difference between x and y

###
kable(SMR[1:15, ], format = "pandoc", 
      caption = 'Table 1. A knitr table depicting the first 15 rows of data found 
      in the SMR dataset')

#Table summarizing SMR 
SMRsummary <- summarise(group_by(SMR),
                      n=n(), 
                      mean=mean(Average_MR), 
                      sd=sd(Mass))
kable(SMRsummary, format = "pandoc", 
      caption = 'Table 2. A summary table displaying the number of snakes captured, 
      the average metabolic rate, and standard deviation of mass.')


#Table depticting cort draw speeds with temperature 
BloodSum <- summarise(group_by(SMR),
                     n=n(), 
                     mean=mean(Basetime),
                     sd=sd(Basetime))
kable(BloodSum, format = "pandoc", 
      caption = 'Table 3. A summary table displaying the number of snakes captured, 
      the average blood draw time and standard deviation in blood draw times.')                    

# Map of study area 

leaflet(data=SMR) %>%
    addTiles() %>%
      addMarkers(data = SMR, lat = ~Latitude, lng = ~Longitude)


