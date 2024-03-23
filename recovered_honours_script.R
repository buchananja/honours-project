#Working directory ####
setwd("C:/Users/James/Documents/RData")



#Setup ####
library(readxl) #Load read excel function
library(lme4) #Lmer mixed effects models
library(lmerTest) #Get p-values
library(effects) #Plotting effects
library(ggplot2) #Plot nice graphs
library(jtools) #Simple output for regression analyses (summ(x, digits=))
library(dplyr) #Rename function
library(nortest) #Includes Anderson-Darling test
library(fitdistrplus) #Testing distribution assumptions of models



#Data ####

#Data clean-up
d <- read_excel("FlycatcherData.xlsx", sheet="FCData") #Load data into R environment
d$ring <- as.factor(d$ring)
d$wgt.N <- as.numeric(d$wgt.N)
d$tars.N <- as.numeric(d$tars.N)
d$condition.N <- I(d$wgt.N/d$tars.N) #Add chick condition column
d$feed.freq[d$feed.freq >50] <- NA #Remove feeding frequency values over 50
d.age <- subset(d, !is.na(age)) #Remove individuals with missing ages

#Individual effects preparation
max.age <- aggregate.data.frame(d.age$age, list(d.age$ring), max)
max.age <- rename(max.age, c(ring = Group.1, age.max = x))
d.age <- merge(d.age, max.age, by = c("ring"), all.x = TRUE) #Add a column on max age per individual
rm(max.age)
d.age$age.max.diff <- d.age$age - d.age$age.max #Add age difference column



#Questions####
# Q1 Age related in fitness traits? 
# Q1.a What are the effects at the population level
# Q1.b Can we decompose the effects in demographic effects and individual effects?

# Fitness traits to keep ?  LD / FLEDGE / Chick condition
# Random effects caused singularity > removed and insteas fixed effects models


#Feeding frequency ####
#Population
m0 <- lm(feed.freq ~ age 
          + sex
          #+ age:sex
          + I(age^2) 
          #+ I(age^2):sex
          ,data = d.age)
summ(m0, digits=3)
plot(allEffects(m0))


#Individual
m1 <-  lm(feed.freq ~ age.max.diff 
          + age.max + sex 
          + age.max:age.max.diff 
          + age.max.diff:sex 
          #+ age.max:sex 
          #+ age.max.diff:age.max:sex 
          ,data = d.age)
summary(m1)



#Chick body condition ####

#Population
m2 <- lm(condition.N ~ age 
          + sex 
          + age*sex 
          + I(age^2) 
          #+ I(age^2)*sex
          ,data = d.age)
summ(m2, digits=3)
plot(allEffects(m2))
 

#Individual
m3 <- lm(condition.N ~ age.max.diff #Needs quadratic effect 
          + age.max 
          + sex
          + age.max:sex 
          #+ age.max:age.max.diff 
          #+ age.max:age.max.diff:sex
          ,data = d.age)
summary(m3)
plot(allEffects(m3))


#By feeding frequency
m4 <- lm(condition.N ~ age.max.diff 
          + age.max
          + sex 
          + feed.freq
          #+ feed.freq:sex
          #+ age.max:sex
          #+ age.max:age.max.diff
          #+ age.max:age.max.diff:sex
          ,data = d.age)
summary(m4)
plot(allEffects(m4))


M <- lm(condition.N ~ age.max.diff 
         + age.max
         + sex 
         + feed.freq
         #+ feed.freq:sex
         #+ I(age.max.diff^2)
         #+ I(age.max.diff^2):sex
         #+ I(age.max^2)
         #+ I(age.max^2):sex
         #+ age.max:sex
         #+ age.max:age.max.diff
         #+ age.max:age.max.diff:sex
         ,data = d.age)
summary(M)
plot(allEffects(M))


#Fledging size #### 
#Population
m5 <-  glm(FLEDGE ~ age 
          + sex 
          + age*sex 
          #+ I(age^2) 
          #+ I(age^2)*sex 
          ,poisson, data = d.age)
summ(m5, digits=3)
plot(allEffects(m5))


#Individual
m6 <-  glm(FLEDGE ~ age.max.diff 
          + age.max + sex 
          #+ age.max:age.max.diff 
          #+ age.max.diff:sex 
          #+ age.max:sex 
          #+ age.max.diff:age.max:sex 
          ,poisson, data = d.age)
summary(m6)
plot(allEffects(m6))


#By feeding frequency
m7 <- glm(FLEDGE ~ age.max.diff 
          + age.max
          + sex 
          + feed.freq
          #+ feed.freq:sex
          #+ age.max:sex
          #+ age.max:age.max.diff
          #+ age.max:age.max.diff:sex
          ,poisson, data = d.age)
summary(m7)
plot(allEffects(m7))




