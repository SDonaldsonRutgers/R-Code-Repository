####JWA Plots####

setwd("~/Desktop/JWA/R")
#Download Packages
library(lavaan)
library(semPlot)
library(psych)
library(haven)

#Upload Data
DPF_Criterion <- read_sav("DPF_Criterion3.sav")
attach(DPF_Criterion)
data = DPF_Criterion

 
#CFA MODEL Testing#
#Mahal Outlier Test
library(mice)
data2 = data[, -c(1:139)]
names(data2)
Mahal = mahalanobis(data2, colMeans(data2, na.rm = TRUE), cov(data2, use = "pairwise.complete.obs"))
summary(Mahal)
cuttoff = qchisq(.999, ncol(data2))
summary(Mahal < cuttoff)
FinalData = data[Mahal < cuttoff , ]


-----------------
attach(FinalData)
  #FirstOrder model
First.Order <-  '
P =~ PE_1 + PE_2 + PE_3 
E =~ PEN_1 + PEN_2 + PEN_3
R =~ PREL_1 + PREL_2 + PREL_3 + PREL_4 
M  =~ PMEAN_1 + PMEAN_2 + PMEAN_3
A  =~ PACCOM_1 + PACCOM_2 + PACCOM_3
Health =~ PPHealth_1 + PPHealth_2 + PPHealth_3 + PPHealth_4
Mind =~ PMIND_1 + PMIND_2 + PMIND_3
Enviro =~ PPWE_1 + PPWE_2 + PPWE_3
Econ  =~ PECON_1 + PECON_2 + PECON_3
'

Higher.Order <-  
'
P =~ PE_1 + PE_2 + PE_3 
E =~ PEN_1 + PEN_2 + PEN_3
R =~ PREL_1 + PREL_2 + PREL_3 + PREL_4 
M  =~ PMEAN_1 + PMEAN_2 + PMEAN_3
A  =~ PACCOM_1 + PACCOM_2 + PACCOM_3
Health =~ PPHealth_1 + PPHealth_2 + PPHealth_3 + PPHealth_4
Mind =~ PMIND_1 + PMIND_2 + PMIND_3
Enviro =~ PPWE_1 + PPWE_2 + PPWE_3
Econ  =~ PECON_1 + PECON_2 + PECON_3
global =~ P + E + R + M + A + Health + Mind + Enviro + Econ
'

BiFactor <- 
  '
P =~ b*PE_1 + b*PE_2 + b*PE_3
E =~ c*PEN_1 + c*PEN_2 + c*PEN_3
R =~ a*PREL_1 + a*PREL_2 + a*PREL_3 + a*PREL_4
M  =~ d*PMEAN_1 + d*PMEAN_2 + d*PMEAN_3
A  =~ e*PACCOM_1 + e*PACCOM_2 + e*PACCOM_3
Health =~ g*PPHealth_1 + g*PPHealth_2 + g*PPHealth_3 + g*PPHealth_4
Mind =~ h*PMIND_1 + h*PMIND_2 + h*PMIND_3
Enviro =~ i*PPWE_1 + i*PPWE_2 + i*PPWE_3
Econ  =~ f*PECON_1 + f*PECON_2 + f*PECON_3
PFW =~ b*PE_1 + b*PE_2 + b*PE_3 + c*PEN_1 + c*PEN_2 + c*PEN_3 + a*PREL_1 + a*PREL_2 + a*PREL_3 + a*PREL_4 + d*PMEAN_1 + d*PMEAN_2 + d*PMEAN_3 + e*PACCOM_1 + e*PACCOM_2 + e*PACCOM_3 + g*PPHealth_1 + g*PPHealth_2 + g*PPHealth_3 + g*PPHealth_4 + h*PMIND_1 + h*PMIND_2 + h*PMIND_3 + i*PPWE_1 + i*PPWE_2 + i*PPWE_3 + f*PECON_1 + f*PECON_2 + f*PECON_3
' 

OneFactor <- 
  '
Global =~ PREL_1 + PREL_2 + PREL_3 + PREL_4 + PE_1 + PE_2 + PE_3 + PEN_1 + PEN_2 + PEN_3 + PMEAN_1 + PMEAN_2 + PMEAN_3 + PACCOM_1 + PACCOM_2 + PACCOM_3 + PECON_1 + PECON_2 + PECON_3 + PPHealth_1 + PPHealth_2 + PPHealth_3 + PPHealth_4 + PMIND_1 + PMIND_2 + PMIND_3 + PPWE_1 + PPWE_2 + PPWE_3
'

#Fit First Order CFA
first.fit <- cfa(First.Order, data = FinalData)
Higher.Order.fit <- cfa(Higher.Order, data = FinalData)
Bifactor.fit <- cfa(BiFactor, data = FinalData, estimator = "ML", orthogonal=TRUE)
OneFactor.fit <- cfa(OneFactor, data = FinalData)
 

#Create Pictures
library(semPlot)

#First Order Plot
semPaths(first.fit, whatLabels = "std" , layout = "tree")
PFlabels <- c("P1" , "P2" , "P3",
              "E1", "E1", "E3",
              "R1", "R2", "R3", "R4", 
              "M1" , "M2", "M3",
              "A1","A2", "A3", 
              "H1", "H2", "H3", "H4",
              "MI1", "MI2", "MI3",
              "EN1", "EN2", "EN3",
              "EC1", "EC2", "EC3", 
              "P" , "E", "R", "M","A", "Health", "Mind", "Enviro", "Econ")
semPaths(first.fit, nodeLabels =  PFlabels, style = "lisrel", curve = 0.8, nCharNodes = 0, 
         sizeLat = 5, sizeLat2 = 5, sizeMan = 3.5, title = TRUE, mar = c(10,1,10,1), edge.label.cex = 0.5, exoVar = FALSE,
         covAtResiduals = FALSE, exoCov = TRUE)


#Higher-Order Plot
PFlabels <- c("P1" , "P2" , "P3",
              "E1", "E1", "E3",
              "R1", "R2", "R3", "R4", 
              "M1" , "M2", "M3",
              "A1","A2", "A3", 
              "H1", "H2", "H3", "H4",
              "MI1", "MI2", "MI3",
              "EN1", "EN2", "EN3",
              "EC1", "EC2", "EC3", 
              "P" , "E", "R", "M","A", "Health", "Mind", "Enviro", "Econ", "PF-W")
semPaths(Higher.Order.fit, layout = "tree", nodeLabels =  PFlabels, style = "lisrel", curve = 0.8, nCharNodes = 0, 
         sizeLat = 7, sizeLat2 = 5, sizeMan = 3.5, title = TRUE, mar = c(10,1,10,1), edge.label.cex = 0.5, exoVar = FALSE,
         covAtResiduals = FALSE, exoCov = FALSE)
      

#Bifactor Plot
PFlabels <- c("P1" , "P2" , "P3",
              "E1", "E1", "E3",
              "R1", "R2", "R3", "R4", 
              "M1" , "M2", "M3",
              "A1","A2", "A3", 
              "H1", "H2", "H3", "H4",
              "MI1", "MI2", "MI3",
              "EN1", "EN2", "EN3",
              "EC1", "EC2", "EC3", 
              "P" , "E", "R", "M","A", "Health", "Mind", "Enviro", "Econ", "PFW")
semPaths(Bifactor.fit,  layout = "tree2", nodeLabels = PFlabels, style = "lisrel", curve = 0.8, nCharNodes = 0, sizeLat = 5, sizeLat2 = 4, sizeMan = 3, title = TRUE, mar = c(10,1,10,1), edge.label.cex = 0.5, exoVar = FALSE, covAtResiduals = FALSE, exoCov = FALSE, bifactor = "PFW", rotation = 1)

#One-Factor Plot
PFlabels <- c("P1" , "P2" , "P3",
              "E1", "E1", "E3",
              "R1", "R2", "R3", "R4",
              "M1" , "M2", "M3",
              "A1","A2", "A3", 
              "H1", "H2", "H3", "H4",
              "MI1", "MI2", "MI3",
              "EN1", "EN2", "EN3",
              "EC1", "EC2", "EC3",
              "PF-W")
semPaths(OneFactor.fit, layout = "tree", nodeLabels =  PFlabels, style = "lisrel", curve = 0.8, nCharNodes = 0, 
         sizeLat = 7, sizeLat2 = 5, sizeMan = 3, title = TRUE, mar = c(10,1,10,1), edge.label.cex = 0.5, exoVar = FALSE, covAtResiduals = FALSE, exoCov = TRUE)



