if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, haven, psych, ggplot2, lavaan, semPlot, semTable, correlation, see)


#Import the data

dat <-read_sav("data.sav")

#check the structure & view 
str(dat)
View(dat)

#check for the missingness

sum(is.na(dat))


#descriptives

describe(dat[, c("PP", "CLC", "CNation", "CH", "CNature", "PSB")])

#correlations

cor_results <- correlation(dat[, c("PP", "CLC", "CNation", "CH", "CNature", "PSB")], redundant = T)
summary(cor_results)

#visualize the cors

cor_results%>%
  summary(redundant = TRUE) %>%
  plot()


################################# LINEAR REGRESSIONS using OLS and SEM 

#Model1: multiple regression model where PSB is regressed on PP and CH


#let's first run a multiple linear regression; 

linear <-lm(PSB ~ PP + CH, data = dat)

summary(linear)

# Step 1: Model specification 

model1 <-'
PSB ~PP + CH'

# Step 2: Fit the model

model1.fit <- sem(model1, data=dat)


# Step 3: Summarize the results 

summary(model1.fit, fit.measures = TRUE, standardized=T,rsquare=T)

#Step 4: Diagram 
semPaths(model1.fit, fade =F)

######################################## PART I:  PATH ANALYSIS #################################################################################


model2 <-'
#connectedness to humanity is regressed on honesty-humility

CH ~ a*PP

#prosocial behavior is regressed on connectedness to humanity and honesty-humility
PSB~b*CH + C*PP

#calculate indirect effect
ab := a*b

#calcualate the total effect
total := a*b + C

'

#Fit the model 
model2.fit <- sem(model2, data=dat)

#summarize

summary(model2.fit, fit.measures = TRUE, standardized=T,rsquare=T)


#visualize 

semPaths(model2.fit, fade =F)


#Should we include control variables too?

model3 <-'
#CH regressed on PP 

CH ~ a*PP

#PSB regressed on CH and PP 
PSB~b*CH + C*PP + Age

#indirect effects
ab := a*b
total := (a*b) + C'


#fit the model 
model3.fit <- sem(model3, data=dat)


#summarize

summary(model3.fit, fit.measures = TRUE, standardized=T,rsquare=T)

#visualize 

semPaths(model3.fit, fade =F)

#Include all connectedness variables as mediators between PP and PSB


model4 <-'
# All four mediators regressed on the IV (path a)

CLC ~ a1*PP
CH ~ a2*PP
CNation ~ a3*PP
CNature ~ a4*PP


#PSB regressed on all four mediators and the IV (paths B and C)

PSB~b1*CLC + b2*CH + b3*CNation + b4*CNature + c*PP

#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a3*b4


total := c + (a1 * b1) + (a2 * b2) + (a3 * b3) + (a4 * b4)'


#fit the model 
model4.fit <- sem(model4, data=dat)


#summarize 

summary(model4.fit, fit.measures = TRUE, standardized=T,rsquare=T)



#SHALL WE ALLOW FOR RESIDUAL COVARIANCE BETWEEN THE MEDIATORS?

model5 <-'
#All four mediators regressed on the IV (path a)

CLC ~ a1*PP
CH ~ a2*PP
CNation ~ a3*PP
CNature ~ a4*PP


#PSB regressed on all four mediators and the IV (paths B and C)

PSB~b1*CLC + b2*CH + b3*CNation + b4*CNature + c*PP

#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a3*b4

#total effect
total := c + (a1 * b1) + (a2 * b2) + (a3 * b3) + (a4 * b4)


#residual covariances

CLC ~~ CH
CLC ~~ CNation
CLC ~~ CNature
CH ~~ CNation
CH ~~ CNature
CNation  ~~ CNature
'


#fit the model 
model5.fit <- sem(model5, data=dat)

#summarize the model 

summary(model5.fit, fit.measures = TRUE, standardized=T,rsquare=T)




#compare model 4 and model 5 (with the residual covars)

models_paths <- list(model4.fit, model5.fit)
compareLavaan(models_paths, fitmeas = c("chisq", "df", "pvalue", "rmsea",
                                  "cfi", "tli", "srmr", "aic", "bic"), nesting = NULL, scaled = TRUE)



############################### PART 2: CONFIRMATORY FACTOR ANALYSIS ################################


#Honesty-humility

#start with the simplest model 

#But first reverse-score some items 


reverse_columns <-c("HonestyHumility1", "HonestyHumility9", "HonestyHumility2", "HonestyHumility6", "HonestyHumility14",
                    "HonestyHumility7", "HonestyHumility11", "HonestyHumility15", "HonestyHumility12", "HonestyHumility16")



dat[ , reverse_columns] = 6 - dat[ , reverse_columns]




#------------------------ HONESTY HUMILITY SUB-SCALE ITEMS----------------------------------------------------

#SINCERITY FACTOR: HonestyHumility1 + HonestyHumility5 + HonestyHumility9 + HonestyHumility13 


#FAIRNESS FACTOR: HonestyHumility2 + HonestyHumility6 + HonestyHumility10 + HonestyHumility14 

#GREED-AVOIDANCE FACTOR: HonestyHumility3 + HonestyHumility7 + HonestyHumility11 + HonestyHumility15 

#MODESTY FACTOR: HonestyHumility4 + HonestyHumility8 + HonestyHumility12 + HonestyHumility16 #4th factor = Modesty

#NOTE: we can model honesty-humility as (a) unidimensional, (b) two-dimensional, (c) four-dimensional construct. 

#unidimensional --> all 16 items load on honesty-humility construct

#two dimensional --> honesty and humility are two separate factors. Honesty consits of sincerity + fairness factors
# humility consists of greed-avoidance and modesty factors. 

#four-dimensional --> sincerity, fairness, greed-avoidance, and modesty as four separate dimensions.

#please also check a word document where all items of honesty-humility as well as sub-dimensions are provided.
#define the measurement model. Here we believe that all 16 items are predicted by a single honesty-humility trait


hh1 <-' unifactorial =~ HonestyHumility1 + HonestyHumility2 + HonestyHumility3 + 
HonestyHumility4 + HonestyHumility5 + HonestyHumility6 + HonestyHumility7 + HonestyHumility8 + HonestyHumility9 + HonestyHumility10 + 
HonestyHumility11 + HonestyHumility12 + HonestyHumility13 + HonestyHumility14 + HonestyHumility15 + HonestyHumility16'


#fit the model 

hh1.fit <-cfa(hh1, data = dat)

#summarize the model 
summary(hh1.fit, fit.measures = TRUE, standardized=T,rsquare=T)

#visualize 


semPaths(hh1.fit, fade =F)

#terrible fit. What shall we do? (hint: what does theory say)


#STRUCTURE OF THE HH: Sincerity, Fairness, Greed Avoidance, and Modesty. Sincerity and Fairness = Honesty; the latter two = humility


#two factorial approach: honesty and humility 


hh2 <-'honesty =~ HonestyHumility1 + HonestyHumility5 + HonestyHumility9 + HonestyHumility13 + HonestyHumility2 + 
        HonestyHumility6 + HonestyHumility10 + HonestyHumility14

       humility =~ HonestyHumility3 + HonestyHumility7 + HonestyHumility11 + HonestyHumility15 + 
       HonestyHumility4 + HonestyHumility8 + HonestyHumility12 + HonestyHumility16
'




#fit the model 

hh2.fit <-cfa(hh2, data = dat)

#summarize the model 


summary(hh2.fit, fit.measures = TRUE, standardized=T,rsquare=T)


#visualize

semPaths(hh2.fit, fade =F)

#a bit better but still terible 


#four factorial approach: Sincerity, Fairness, greed avoidance, and Modesty

hh3 <-'sincerity =~ HonestyHumility1 + HonestyHumility5 + HonestyHumility9 + HonestyHumility13 # 1st factor = sincerity

Fairness =~ HonestyHumility2 + HonestyHumility6 + HonestyHumility10 + HonestyHumility14 #2nd factor = Fairness

Greedavoid =~ HonestyHumility3 + HonestyHumility7 + HonestyHumility11 + HonestyHumility15 # 3rd factor = Greed avoidance

Modesty =~ HonestyHumility4 + HonestyHumility8 + HonestyHumility12 + HonestyHumility16 #4th factor = Modesty'


#fit the model 
hh3.fit <-cfa(hh3, data = dat)

#summarize the model 
summary(hh3.fit, fit.measures = T, standardized = T)

#visualize 

semPaths(hh3.fit, edge.width = 0.5, fade =F)


#compare the models 

hh_model_comparisons <- list(hh1.fit, hh2.fit, hh3.fit)
compareLavaan(hh_model_comparisons, fitmeas = c("chisq", "df", "pvalue", "rmsea",
                                        "cfi", "tli", "srmr", "aic", "bic"), nesting = NULL, scaled = TRUE)

#What about the outcome variable?

psb <-'Prosocial =~ ProsocialBehaviour1 + ProsocialBehaviour2 + ProsocialBehaviour3 + ProsocialBehaviour4 + 
ProsocialBehaviour5 + ProsocialBehaviour6 + ProsocialBehaviour7'

#fit the model 

psb1.fit <-cfa(psb, data = dat)

#summarize the model 

summary(psb1.fit, fit.measures = T, standardized = T)

#look into modification indices

modificationindices(psb1.fit, sort = T, maximum.number = 5)


#update the model to allow for the residual covariance between 1st and 2nd items



psb2 <-'Prosocial =~ ProsocialBehaviour1 + ProsocialBehaviour2 + ProsocialBehaviour3 + ProsocialBehaviour4 + 
ProsocialBehaviour5 + ProsocialBehaviour6 + ProsocialBehaviour7


#residual covariance
ProsocialBehaviour1 ~~ ProsocialBehaviour2
'

#fit the model 

psb2.fit <-cfa(psb2, data = dat)

#summarize the model 

summary(psb2.fit, fit.measures = T, standardized = T)


#compare the models
psb_model_comparisons <- list(psb1.fit, psb2.fit)
compareLavaan(psb_model_comparisons, fitmeas = c("chisq", "df", "pvalue", "rmsea",
                                                "cfi", "tli", "srmr", "aic", "bic"), nesting = NULL, scaled = TRUE)



#build a FULL SEM MODEL 


full_sem <-' #SPECIFY THE MEASUREMENT MODEL 


sincerity =~ HonestyHumility1 + HonestyHumility5 + HonestyHumility9 + HonestyHumility13 # 1st factor = sincerity

Fairness =~ HonestyHumility2 + HonestyHumility6 + HonestyHumility10 + HonestyHumility14 #2nd factor = Fairness

Greedavoid =~ HonestyHumility3 + HonestyHumility7 + HonestyHumility11 + HonestyHumility15 # 3rd factor = Fairness

Modesty =~ HonestyHumility4 + HonestyHumility8 + HonestyHumility12 + HonestyHumility16 #4th factor = Modesty


Prosocial =~ ProsocialBehaviour1 + ProsocialBehaviour2 + ProsocialBehaviour3 + ProsocialBehaviour4 + 
  ProsocialBehaviour5 + ProsocialBehaviour6 + ProsocialBehaviour7

#SPECIFY THE STRUCTURAL Part 

Prosocial ~ c1*sincerity + c2*Fairness + c3*Greedavoid + c4*Modesty  +  b1*CH 

#mediator regressed on DV
CH ~ a1*sincerity + a2*Fairness  + a3*Greedavoid + a4*Modesty 


#indirect effects

indirect1 := a1*b1
indirect2 := a2*b1
indirect3 := a3*b1
indirect4:= a4*b1


#total effects
total := c1 + c2 + c3 + c4 + (a1 * b1) + (a2 * b1) + (a3 * b1) + (a4 * b1)

ProsocialBehaviour1 ~~ ProsocialBehaviour2

'


#fit the model 

full_sem.fit <-cfa(full_sem, dat)

#summarize the model 
summary(full_sem.fit, fit.measures = T, standardized = T, rsquare = T)

#visualize the model 

semPaths(full_sem.fit, "std",edge.label.cex=0.5, curvePivot = TRUE)






















