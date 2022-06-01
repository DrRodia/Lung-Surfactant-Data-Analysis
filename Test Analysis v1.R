#Contingency Analysis by Test v1
################################################################################################
#load the necessary library

library(tidyverse)#delivers plenty of functions 
library(magrittr)
library(ggplot2) #Used in plotting
library(cowplot) #used to set multiple plots into one page
library(svDialogs) #to create pop up window
library("gridExtra") #to save tables into pdf format

#install.packages("gt")
#install.packages("webshot")
library(gt) #to create nice dataframes saving can be made but on HTML format
####################################################################################

#Min ST

SetWD_Input<-setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Test Analysis/")
#Calling the Datas
Min_ST_test<-read.table(file.choose(new=TRUE),header = TRUE,sep = ",",dec = ".")
Min_ST_test %>% remove_rownames %>% column_to_rownames(var="X")

Min_ST_True_Poz<-Min_ST_test$TOXIC[1] #TP
Min_ST_False_Neg<-Min_ST_test$TOXIC[2] #FP
Min_ST_False_Poz<-Min_ST_test$SAFE[1] #TN
Min_ST_True_Neg<-Min_ST_test$SAFE[2] #FN
##################################################################################
#Prevalence of the event = TP + FN/ Total of population
Min_ST_Prevalence<-(Min_ST_True_Poz+Min_ST_False_Neg)/Min_ST_test$Total[3]
Min_ST_Prevalence
##################################################################################
#Sensibility

# Sensibility of a test = TP/(TP+FN)

Min_ST_Sensibility<-Min_ST_True_Poz/(Min_ST_True_Poz+Min_ST_False_Neg)
Min_ST_Sensibility

#False Neg fraction = 1- Sensibility
Min_ST_False_Neg_Fraction<-1-Min_ST_Sensibility
Min_ST_False_Neg_Fraction

#####################################################################################
#Specificity

#Specificity of a test = TN/(TN+FP)

Min_ST_Specificity<-Min_ST_True_Neg/(Min_ST_True_Neg+Min_ST_False_Poz)
Min_ST_Specificity

#Min_ST_False Poz Fraction= 1- Specificity
Min_ST_False_Poz_Fraction<-1-Min_ST_Specificity
Min_ST_False_Poz_Fraction

####################################################################################
#False positive Rate

#FPR= FP/(FP+TN)

Min_ST_False_Poz_Rate<-Min_ST_False_Poz/(Min_ST_False_Poz+Min_ST_True_Neg)
Min_ST_False_Poz_Rate

####################################################################################
#False Negative Rate

#FNR= FN/(FN+TP)
Min_ST_False_Neg_Rate<- Min_ST_False_Neg/(Min_ST_False_Neg+Min_ST_True_Poz)
Min_ST_False_Neg_Rate

###################################################################################
#Positive Predictive Value
#PPV<- TP/(TP+FP) or
#VPP=Sensibilité x Prévalence / [(Sensibilité x Prévalence + (1-Spécificité)(1- Prévalence)]
#as the prevalence is independant of the test quality, it would be better to use the second one

Min_ST_Poz_Predictive_Value<-(Min_ST_Sensibility*Min_ST_Prevalence)/(Min_ST_Sensibility*Min_ST_Prevalence+(1-Min_ST_Specificity)*(1-Min_ST_Prevalence))
Min_ST_Poz_Predictive_Value

####################################################################################
#Negative Predictive Value
#NPV<- TN/(TN+FN) or 
#PP= Spécificité(1- Prévalence) / [Spécificité(1- Prévalence) + (1- Sensibilité)Prévalence]
#as the prevalence is independant of the test quality, it would be better to use the second one

Min_ST_Neg_Predictive_Value<-(Min_ST_Specificity*(1-Min_ST_Prevalence))/(Min_ST_Specificity*(1-Min_ST_Prevalence)+(1-Min_ST_Sensibility)*Min_ST_Prevalence)
Min_ST_Neg_Predictive_Value

###########################################################################################
#Positive Likekihood Ratio
#PLR = Sensibility/(1-Spécificity)

Min_ST_Poz_Likelihood_Ratio<- Min_ST_Sensibility/(1-Min_ST_Specificity)
Min_ST_Poz_Likelihood_Ratio

#########################################################################################
#Negative Likelihood Ratio
#NLR = (1-Sensibility)/Specificity
Min_ST_Neg_Likelihood_Ratio<-(1-Min_ST_Sensibility)/Min_ST_Specificity
Min_ST_Neg_Likelihood_Ratio
##########################################################################################
#post test proba (+) and post test proba (-) Calculation

Min_ST_Pre_Test_Proba<-Min_ST_Prevalence

Min_ST_Pre_Test_Odds<-Min_ST_Pre_Test_Proba/(1-Min_ST_Pre_Test_Proba)

Min_ST_Post_Test_Odds_Poz<-Min_ST_Pre_Test_Odds*Min_ST_Poz_Likelihood_Ratio
Min_ST_Post_Test_Odds_Neg<-Min_ST_Pre_Test_Odds*Min_ST_Neg_Likelihood_Ratio

Min_ST_Post_Test_Proba_Poz<-Min_ST_Post_Test_Odds_Poz/(Min_ST_Post_Test_Odds_Poz+1)
Min_ST_Post_Test_Proba_Neg<-Min_ST_Post_Test_Odds_Neg/(Min_ST_Post_Test_Odds_Neg+1)

###############################################################################################
#Odds Ratio
#OR = TP*TN/FP*FN
Min_ST_Odds_Ratio<-(Min_ST_True_Poz*Min_ST_True_Neg)/(Min_ST_False_Poz*Min_ST_False_Neg)
Min_ST_Odds_Ratio

###########################################################################################
#Relative Risk
#Relative Risk=(VP/(VP+FP))/(FN/(FN+VN))

Min_ST_Relative_Risk<-(Min_ST_True_Poz/(Min_ST_True_Poz*Min_ST_False_Poz))/(Min_ST_False_Neg/(Min_ST_False_Neg+Min_ST_True_Neg))
Min_ST_Relative_Risk

############################################################################################
#Creating a vector containing all datas
Min_ST_test_Values<-c(Min_ST_Prevalence,
                      Min_ST_Sensibility,
                      Min_ST_Specificity,
                      Min_ST_False_Neg_Fraction,
                      Min_ST_False_Poz_Fraction,
                      Min_ST_False_Neg_Rate,
                      Min_ST_False_Poz_Rate,
                      Min_ST_Neg_Predictive_Value,
                      Min_ST_Poz_Predictive_Value,
                      Min_ST_Poz_Likelihood_Ratio,
                      Min_ST_Post_Test_Proba_Poz,
                      Min_ST_Neg_Likelihood_Ratio,
                      Min_ST_Post_Test_Proba_Neg,
                      Min_ST_Odds_Ratio,
                      Min_ST_Relative_Risk)
###############################################################################################
################################################################################################
#RR ST

#Min ST

SetWD_Input<-setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Test Analysis/")
#Calling the Datas
RR_ST_test<-read.table(file.choose(new=TRUE),header = TRUE,sep = ",",dec = ".")
RR_ST_test %>% remove_rownames %>% column_to_rownames(var="X")

RR_ST_True_Poz<-RR_ST_test$TOXIC[1] #TP
RR_ST_False_Neg<-RR_ST_test$TOXIC[2] #FP
RR_ST_False_Poz<-RR_ST_test$SAFE[1] #TN
RR_ST_True_Neg<-RR_ST_test$SAFE[2] #FN
##################################################################################
#Prevalence of the event = TP + FN/ Total of population
RR_ST_Prevalence<-(RR_ST_True_Poz+RR_ST_False_Neg)/RR_ST_test$Total[3]
RR_ST_Prevalence
##################################################################################
#Sensibility

# Sensibility of a test = TP/(TP+FN)

RR_ST_Sensibility<-RR_ST_True_Poz/(RR_ST_True_Poz+RR_ST_False_Neg)
RR_ST_Sensibility

#False Neg fraction = 1- Sensibility
RR_ST_False_Neg_Fraction<-1-RR_ST_Sensibility
RR_ST_False_Neg_Fraction

#####################################################################################
#Specificity

#Specificity of a test = TN/(TN+FP)

RR_ST_Specificity<-RR_ST_True_Neg/(RR_ST_True_Neg+RR_ST_False_Poz)
RR_ST_Specificity

#RR_ST_False Poz Fraction= 1- Specificity
RR_ST_False_Poz_Fraction<-1-RR_ST_Specificity
RR_ST_False_Poz_Fraction

####################################################################################
#False positive Rate

#FPR= FP/(FP+TN)

RR_ST_False_Poz_Rate<-RR_ST_False_Poz/(RR_ST_False_Poz+RR_ST_True_Neg)
RR_ST_False_Poz_Rate

####################################################################################
#False Negative Rate

#FNR= FN/(FN+TP)
RR_ST_False_Neg_Rate<- RR_ST_False_Neg/(RR_ST_False_Neg+RR_ST_True_Poz)
RR_ST_False_Neg_Rate

###################################################################################
#Positive Predictive Value
#PPV<- TP/(TP+FP) or
#VPP=Sensibilité x Prévalence / [(Sensibilité x Prévalence + (1-Spécificité)(1- Prévalence)]
#as the prevalence is independant of the test quality, it would be better to use the second one

RR_ST_Poz_Predictive_Value<-(RR_ST_Sensibility*RR_ST_Prevalence)/(RR_ST_Sensibility*RR_ST_Prevalence+(1-RR_ST_Specificity)*(1-RR_ST_Prevalence))
RR_ST_Poz_Predictive_Value

####################################################################################
#Negative Predictive Value
#NPV<- TN/(TN+FN) or 
#PP= Spécificité(1- Prévalence) / [Spécificité(1- Prévalence) + (1- Sensibilité)Prévalence]
#as the prevalence is independant of the test quality, it would be better to use the second one

RR_ST_Neg_Predictive_Value<-(RR_ST_Specificity*(1-RR_ST_Prevalence))/(RR_ST_Specificity*(1-RR_ST_Prevalence)+(1-RR_ST_Sensibility)*RR_ST_Prevalence)
RR_ST_Neg_Predictive_Value

###########################################################################################
#Positive Likekihood Ratio
#PLR = Sensibility/(1-Spécificity)

RR_ST_Poz_Likelihood_Ratio<- RR_ST_Sensibility/(1-RR_ST_Specificity)
RR_ST_Poz_Likelihood_Ratio

#########################################################################################
#Negative Likelihood Ratio
#NLR = (1-Sensibility)/Specificity
RR_ST_Neg_Likelihood_Ratio<-(1-RR_ST_Sensibility)/RR_ST_Specificity
RR_ST_Neg_Likelihood_Ratio

##########################################################################################
#post test proba (+) and post test proba (-)

RR_ST_Pre_Test_Proba<-RR_ST_Prevalence

RR_ST_Pre_Test_Odds<-RR_ST_Pre_Test_Proba/(1-RR_ST_Pre_Test_Proba)

RR_ST_Post_Test_Odds_Poz<-RR_ST_Pre_Test_Odds*RR_ST_Poz_Likelihood_Ratio
RR_ST_Post_Test_Odds_Neg<-RR_ST_Pre_Test_Odds*RR_ST_Neg_Likelihood_Ratio

RR_ST_Post_Test_Proba_Poz<-RR_ST_Post_Test_Odds_Poz/(RR_ST_Post_Test_Odds_Poz+1)
RR_ST_Post_Test_Proba_Neg<-RR_ST_Post_Test_Odds_Neg/(RR_ST_Post_Test_Odds_Neg+1)

###############################################################################################
#Odds Ratio
#OR = TP*TN/FP*FN
RR_ST_Odds_Ratio<-(RR_ST_True_Poz*RR_ST_True_Neg)/(RR_ST_False_Poz*RR_ST_False_Neg)
RR_ST_Odds_Ratio

###########################################################################################
#Relative Risk
#Relative Risk=(VP/(VP+FP))/(FN/(FN+VN))

RR_ST_Relative_Risk<-(RR_ST_True_Poz/(RR_ST_True_Poz*RR_ST_False_Poz))/(RR_ST_False_Neg/(RR_ST_False_Neg+RR_ST_True_Neg))
RR_ST_Relative_Risk


############################################################################################
#Creating a vector containing all datas
RR_ST_test_Values<-c(RR_ST_Prevalence,
                     RR_ST_Sensibility,
                     RR_ST_Specificity,
                     RR_ST_False_Neg_Fraction,
                     RR_ST_False_Poz_Fraction,
                     RR_ST_False_Neg_Rate,
                     RR_ST_False_Poz_Rate,
                     RR_ST_Neg_Predictive_Value,
                     RR_ST_Poz_Predictive_Value,
                     RR_ST_Poz_Likelihood_Ratio,
                     RR_ST_Post_Test_Proba_Poz,
                     RR_ST_Neg_Likelihood_Ratio,
                     RR_ST_Post_Test_Proba_Neg,
                     RR_ST_Odds_Ratio,
                     RR_ST_Relative_Risk)
############################################################################################
############################################################################################
#SI of LS

SetWD_Input<-setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Test Analysis/")
#Calling the Datas
SI_test<-read.table(file.choose(new=TRUE),header = TRUE,sep = ",",dec = ".")
SI_test %>% remove_rownames %>% column_to_rownames(var="X")

SI_True_Poz<-SI_test$TOXIC[1] #TP
SI_False_Neg<-SI_test$TOXIC[2] #FP
SI_False_Poz<-SI_test$SAFE[1] #TN
SI_True_Neg<-SI_test$SAFE[2] #FN
##################################################################################
#Prevalence of the event = TP + FN/ Total of population
SI_Prevalence<-(SI_True_Poz+SI_False_Neg)/SI_test$Total[3]
SI_Prevalence
##################################################################################
#Sensibility

# Sensibility of a test = TP/(TP+FN)

SI_Sensibility<-SI_True_Poz/(SI_True_Poz+SI_False_Neg)
SI_Sensibility

#False Neg fraction = 1- Sensibility
SI_False_Neg_Fraction<-1-SI_Sensibility
SI_False_Neg_Fraction

#####################################################################################
#Specificity

#Specificity of a test = TN/(TN+FP)

SI_Specificity<-SI_True_Neg/(SI_True_Neg+SI_False_Poz)
SI_Specificity

#SI_False Poz Fraction= 1- Specificity
SI_False_Poz_Fraction<-1-SI_Specificity
SI_False_Poz_Fraction

####################################################################################
#False positive Rate

#FPR= FP/(FP+TN)

SI_False_Poz_Rate<-SI_False_Poz/(SI_False_Poz+SI_True_Neg)
SI_False_Poz_Rate

####################################################################################
#False Negative Rate

#FNR= FN/(FN+TP)
SI_False_Neg_Rate<- SI_False_Neg/(SI_False_Neg+SI_True_Poz)
SI_False_Neg_Rate

###################################################################################
#Positive Predictive Value
#PPV<- TP/(TP+FP) or
#VPP=Sensibilité x Prévalence / [(Sensibilité x Prévalence + (1-Spécificité)(1- Prévalence)]
#as the prevalence is independant of the test quality, it would be better to use the second one

SI_Poz_Predictive_Value<-(SI_Sensibility*SI_Prevalence)/(SI_Sensibility*SI_Prevalence+(1-SI_Specificity)*(1-SI_Prevalence))
SI_Poz_Predictive_Value

####################################################################################
#Negative Predictive Value
#NPV<- TN/(TN+FN) or 
#PP= Spécificité(1- Prévalence) / [Spécificité(1- Prévalence) + (1- Sensibilité)Prévalence]
#as the prevalence is independant of the test quality, it would be better to use the second one

SI_Neg_Predictive_Value<-(SI_Specificity*(1-SI_Prevalence))/(SI_Specificity*(1-SI_Prevalence)+(1-SI_Sensibility)*SI_Prevalence)
SI_Neg_Predictive_Value

###########################################################################################
#Positive Likekihood Ratio
#PLR = Sensibility/(1-Spécificity)

SI_Poz_Likelihood_Ratio<- SI_Sensibility/(1-SI_Specificity)
SI_Poz_Likelihood_Ratio

#########################################################################################
#Negative Likelihood Ratio
#NLR = (1-Sensibility)/Specificity
SI_Neg_Likelihood_Ratio<-(1-SI_Sensibility)/SI_Specificity
SI_Neg_Likelihood_Ratio
##########################################################################################
#post test proba (+) and post test proba (-)

SI_Pre_Test_Proba<-SI_Prevalence

SI_Pre_Test_Odds<-SI_Pre_Test_Proba/(1-SI_Pre_Test_Proba)

SI_Post_Test_Odds_Poz<-SI_Pre_Test_Odds*SI_Poz_Likelihood_Ratio
SI_Post_Test_Odds_Neg<-SI_Pre_Test_Odds*SI_Neg_Likelihood_Ratio

SI_Post_Test_Proba_Poz<-SI_Post_Test_Odds_Poz/(SI_Post_Test_Odds_Poz+1)
SI_Post_Test_Proba_Neg<-SI_Post_Test_Odds_Neg/(SI_Post_Test_Odds_Neg+1)


###############################################################################################
#Odds Ratio
#OR = TP*TN/FP*FN
SI_Odds_Ratio<-(SI_True_Poz*SI_True_Neg)/(SI_False_Poz*SI_False_Neg)
SI_Odds_Ratio

###########################################################################################
#Relative Risk
#Relative Risk=(VP/(VP+FP))/(FN/(FN+VN))

SI_Relative_Risk<-(SI_True_Poz/(SI_True_Poz*SI_False_Poz))/(SI_False_Neg/(SI_False_Neg+SI_True_Neg))
SI_Relative_Risk

############################################################################################
############################################################################################
#Creating a vector containing all datas
SI_test_Values<-c(SI_Prevalence,
                  SI_Sensibility,
                  SI_Specificity,
                  SI_False_Neg_Fraction,
                  SI_False_Poz_Fraction,
                  SI_False_Neg_Rate,
                  SI_False_Poz_Rate,
                  SI_Neg_Predictive_Value,
                  SI_Poz_Predictive_Value,
                  SI_Poz_Likelihood_Ratio,
                  SI_Post_Test_Proba_Poz,
                  SI_Neg_Likelihood_Ratio,
                  SI_Post_Test_Proba_Neg,
                  SI_Odds_Ratio,
                  SI_Relative_Risk)
###############################################################################################
################################################################################################

#Complilance

SetWD_Input<-setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Test Analysis/")
#Calling the Datas
Compliance_test<-read.table(file.choose(new=TRUE),header = TRUE,sep = ",",dec = ".")
Compliance_test %>% remove_rownames %>% column_to_rownames(var="X")

Compliance_True_Poz<-Compliance_test$TOXIC[1] #TP
Compliance_False_Neg<-Compliance_test$TOXIC[2] #FP
Compliance_False_Poz<-Compliance_test$SAFE[1] #TN
Compliance_True_Neg<-Compliance_test$SAFE[2] #FN
##################################################################################
#Prevalence of the event = TP + FN/ Total of population
Compliance_Prevalence<-(Compliance_True_Poz+Compliance_False_Neg)/Compliance_test$Total[3]
Compliance_Prevalence
##################################################################################
#Sensibility

# Sensibility of a test = TP/(TP+FN)

Compliance_Sensibility<-Compliance_True_Poz/(Compliance_True_Poz+Compliance_False_Neg)
Compliance_Sensibility

#False Neg fraction = 1- Sensibility
Compliance_False_Neg_Fraction<-1-Compliance_Sensibility
Compliance_False_Neg_Fraction

#####################################################################################
#Specificity

#Specificity of a test = TN/(TN+FP)

Compliance_Specificity<-Compliance_True_Neg/(Compliance_True_Neg+Compliance_False_Poz)
Compliance_Specificity

#Compliance_False Poz Fraction= 1- Specificity
Compliance_False_Poz_Fraction<-1-Compliance_Specificity
Compliance_False_Poz_Fraction

####################################################################################
#False positive Rate

#FPR= FP/(FP+TN)

Compliance_False_Poz_Rate<-Compliance_False_Poz/(Compliance_False_Poz+Compliance_True_Neg)
Compliance_False_Poz_Rate

####################################################################################
#False Negative Rate

#FNR= FN/(FN+TP)
Compliance_False_Neg_Rate<- Compliance_False_Neg/(Compliance_False_Neg+Compliance_True_Poz)
Compliance_False_Neg_Rate

###################################################################################
#Positive Predictive Value
#PPV<- TP/(TP+FP) or
#VPP=Sensibilité x Prévalence / [(Sensibilité x Prévalence + (1-Spécificité)(1- Prévalence)]
#as the prevalence is independant of the test quality, it would be better to use the second one

Compliance_Poz_Predictive_Value<-(Compliance_Sensibility*Compliance_Prevalence)/(Compliance_Sensibility*Compliance_Prevalence+(1-Compliance_Specificity)*(1-Compliance_Prevalence))
Compliance_Poz_Predictive_Value

####################################################################################
#Negative Predictive Value
#NPV<- TN/(TN+FN) or 
#PP= Spécificité(1- Prévalence) / [Spécificité(1- Prévalence) + (1- Sensibilité)Prévalence]
#as the prevalence is independant of the test quality, it would be better to use the second one

Compliance_Neg_Predictive_Value<-(Compliance_Specificity*(1-Compliance_Prevalence))/(Compliance_Specificity*(1-Compliance_Prevalence)+(1-Compliance_Sensibility)*Compliance_Prevalence)
Compliance_Neg_Predictive_Value

###########################################################################################
#Positive Likekihood Ratio
#PLR = Sensibility/(1-Spécificity)

Compliance_Poz_Likelihood_Ratio<- Compliance_Sensibility/(1-Compliance_Specificity)
Compliance_Poz_Likelihood_Ratio

#########################################################################################
#Negative Likelihood Ratio
#NLR = (1-Sensibility)/Specificity
Compliance_Neg_Likelihood_Ratio<-(1-Compliance_Sensibility)/Compliance_Specificity
Compliance_Neg_Likelihood_Ratio
##########################################################################################
#post test proba (+) and post test proba (-)

Compliance_Pre_Test_Proba<-Compliance_Prevalence

Compliance_Pre_Test_Odds<-Compliance_Pre_Test_Proba/(1-Compliance_Pre_Test_Proba)

Compliance_Post_Test_Odds_Poz<-Compliance_Pre_Test_Odds*Compliance_Poz_Likelihood_Ratio
Compliance_Post_Test_Odds_Neg<-Compliance_Pre_Test_Odds*Compliance_Neg_Likelihood_Ratio

Compliance_Post_Test_Proba_Poz<-Compliance_Post_Test_Odds_Poz/(Compliance_Post_Test_Odds_Poz+1)
Compliance_Post_Test_Proba_Neg<-Compliance_Post_Test_Odds_Neg/(Compliance_Post_Test_Odds_Neg+1)


###############################################################################################
#Odds Ratio
#OR = TP*TN/FP*FN
Compliance_Odds_Ratio<-(Compliance_True_Poz*Compliance_True_Neg)/(Compliance_False_Poz*Compliance_False_Neg)
Compliance_Odds_Ratio

###########################################################################################
#Relative Risk
#Relative Risk=(VP/(VP+FP))/(FN/(FN+VN))

Compliance_Relative_Risk<-(Compliance_True_Poz/(Compliance_True_Poz*Compliance_False_Poz))/(Compliance_False_Neg/(Compliance_False_Neg+Compliance_True_Neg))
Compliance_Relative_Risk

############################################################################################
############################################################################################
#Creating a vector containing all datas
Compliance_test_Values<-c(Compliance_Prevalence,
                          Compliance_Sensibility,
                          Compliance_Specificity,
                          Compliance_False_Neg_Fraction,
                          Compliance_False_Poz_Fraction,
                          Compliance_False_Neg_Rate,
                          Compliance_False_Poz_Rate,
                          Compliance_Neg_Predictive_Value,
                          Compliance_Poz_Predictive_Value,
                          Compliance_Poz_Likelihood_Ratio,
                          Compliance_Post_Test_Proba_Poz,
                          Compliance_Neg_Likelihood_Ratio,
                          Compliance_Post_Test_Proba_Neg,
                          Compliance_Odds_Ratio,
                          Compliance_Relative_Risk)
###############################################################################################
###############################################################################################
#HLA

SetWD_Input<-setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Test Analysis/")
#Calling the Datas
HLA_test<-read.table(file.choose(new=TRUE),header = TRUE,sep = ",",dec = ".")
HLA_test %>% remove_rownames %>% column_to_rownames(var="X")

HLA_True_Poz<-HLA_test$TOXIC[1] #TP
HLA_False_Neg<-HLA_test$TOXIC[2] #FP
HLA_False_Poz<-HLA_test$SAFE[1] #TN
HLA_True_Neg<-HLA_test$SAFE[2] #FN
##################################################################################
#Prevalence of the event = TP + FN/ Total of population
HLA_Prevalence<-(HLA_True_Poz+HLA_False_Neg)/HLA_test$Total[3]
HLA_Prevalence
##################################################################################
#Sensibility

# Sensibility of a test = TP/(TP+FN)

HLA_Sensibility<-HLA_True_Poz/(HLA_True_Poz+HLA_False_Neg)
HLA_Sensibility

#False Neg fraction = 1- Sensibility
HLA_False_Neg_Fraction<-1-HLA_Sensibility
HLA_False_Neg_Fraction

#####################################################################################
#Specificity

#Specificity of a test = TN/(TN+FP)

HLA_Specificity<-HLA_True_Neg/(HLA_True_Neg+HLA_False_Poz)
HLA_Specificity

#HLA_False Poz Fraction= 1- Specificity
HLA_False_Poz_Fraction<-1-HLA_Specificity
HLA_False_Poz_Fraction

####################################################################################
#False positive Rate

#FPR= FP/(FP+TN)

HLA_False_Poz_Rate<-HLA_False_Poz/(HLA_False_Poz+HLA_True_Neg)
HLA_False_Poz_Rate

####################################################################################
#False Negative Rate

#FNR= FN/(FN+TP)
HLA_False_Neg_Rate<- HLA_False_Neg/(HLA_False_Neg+HLA_True_Poz)
HLA_False_Neg_Rate

###################################################################################
#Positive Predictive Value
#PPV<- TP/(TP+FP) or
#VPP=Sensibilité x Prévalence / [(Sensibilité x Prévalence + (1-Spécificité)(1- Prévalence)]
#as the prevalence is independant of the test quality, it would be better to use the second one

HLA_Poz_Predictive_Value<-(HLA_Sensibility*HLA_Prevalence)/(HLA_Sensibility*HLA_Prevalence+(1-HLA_Specificity)*(1-HLA_Prevalence))
HLA_Poz_Predictive_Value

####################################################################################
#Negative Predictive Value
#NPV<- TN/(TN+FN) or 
#PP= Spécificité(1- Prévalence) / [Spécificité(1- Prévalence) + (1- Sensibilité)Prévalence]
#as the prevalence is independant of the test quality, it would be better to use the second one

HLA_Neg_Predictive_Value<-(HLA_Specificity*(1-HLA_Prevalence))/(HLA_Specificity*(1-HLA_Prevalence)+(1-HLA_Sensibility)*HLA_Prevalence)
HLA_Neg_Predictive_Value

###########################################################################################
#Positive Likekihood Ratio
#PLR = Sensibility/(1-Spécificity)

HLA_Poz_Likelihood_Ratio<- HLA_Sensibility/(1-HLA_Specificity)
HLA_Poz_Likelihood_Ratio

#########################################################################################
#Negative Likelihood Ratio
#NLR = (1-Sensibility)/Specificity
HLA_Neg_Likelihood_Ratio<-(1-HLA_Sensibility)/HLA_Specificity
HLA_Neg_Likelihood_Ratio

##########################################################################################
##########################################################################################
#post test proba (+) and post test proba (-)

HLA_Pre_Test_Proba<-HLA_Prevalence

HLA_Pre_Test_Odds<-HLA_Pre_Test_Proba/(1-HLA_Pre_Test_Proba)

HLA_Post_Test_Odds_Poz<-HLA_Pre_Test_Odds*HLA_Poz_Likelihood_Ratio
HLA_Post_Test_Odds_Neg<-HLA_Pre_Test_Odds*HLA_Neg_Likelihood_Ratio

HLA_Post_Test_Proba_Poz<-HLA_Post_Test_Odds_Poz/(HLA_Post_Test_Odds_Poz+1)
HLA_Post_Test_Proba_Neg<-HLA_Post_Test_Odds_Neg/(HLA_Post_Test_Odds_Neg+1)


###############################################################################################
#Odds Ratio
#OR = TP*TN/FP*FN
HLA_Odds_Ratio<-(HLA_True_Poz*HLA_True_Neg)/(HLA_False_Poz*HLA_False_Neg)
HLA_Odds_Ratio

###########################################################################################
#Relative Risk
#Relative Risk=(VP/(VP+FP))/(FN/(FN+VN))

HLA_Relative_Risk<-(HLA_True_Poz/(HLA_True_Poz*HLA_False_Poz))/(HLA_False_Neg/(HLA_False_Neg+HLA_True_Neg))
HLA_Relative_Risk

############################################################################################
############################################################################################
#Creating a vector containing all datas
HLA_test_Values<-c(HLA_Prevalence,
                   HLA_Sensibility,
                   HLA_Specificity,
                   HLA_False_Neg_Fraction,
                   HLA_False_Poz_Fraction,
                   HLA_False_Neg_Rate,
                   HLA_False_Poz_Rate,
                   HLA_Neg_Predictive_Value,
                   HLA_Poz_Predictive_Value,
                   HLA_Poz_Likelihood_Ratio,
                   HLA_Post_Test_Proba_Poz,
                   HLA_Neg_Likelihood_Ratio,
                   HLA_Post_Test_Proba_Neg,
                   HLA_Odds_Ratio,
                   HLA_Relative_Risk)


#############################################################################################
#Let's then create a single dataframe containing all those values
Row_Name_All_Results_Df<-c("Prevalence (%)",
                           "Sensitivity (%)",
                           "Specificity (%)",
                           "False Negative Fraction (%)",
                           "False Positive Fraction (%)",
                           "False Negative Rate",
                           "False Positive Rate",
                           "Negative Predictive Value (%)",
                           "Positive Predictive Value (%)",
                           "Positive Likelihood Ratio",
                           "Positive Post-Test Probability (%)",
                           "Negative Likelihood Ratio",
                           "Negative Post-Test Probability (%)",
                           "Odds Ratio",
                           "Relative Risk")

All_Results_Df_Percentage<-data.frame(Row_Name_All_Results_Df,
                      round(100*Min_ST_test_Values,digits=1),
                      round(100*RR_ST_test_Values,digits=1),
                      round(100*SI_test_Values,digits=1),
                      round(100*Compliance_test_Values,digits=1),
                      round(100*HLA_test_Values,digits=1))

Col_Names_All_Results_Df<-c("Performace Indexes",
                            "Min of ST",
                            "Relative Reduction",
                            "Stability Index",
                            "Compliance",
                            "Hysteresis Loop Area")
colnames(All_Results_Df_Percentage)<-Col_Names_All_Results_Df


row.names(All_Results_Df_Percentage)<-Row_Name_All_Results_Df

All_Results_Df_Percentage
##############################################################################################
Min_ST_NaN_In_Poz_Proba<-All_Results_Df_Percentage$`Min of ST`%>%
  is.nan()%>%
  which()
RR_ST_NaN_In_Poz_Proba<-All_Results_Df_Percentage$`Relative Reduction`%>%
  is.nan()%>%
  which()
SI_NaN_In_Poz_Proba<-All_Results_Df_Percentage$`Stability Index`%>%
  is.nan()%>%
  which()
Compliance_NaN_In_Poz_Proba<-All_Results_Df_Percentage$Compliance%>%
  is.nan()%>%
  which()
HLA_NaN_In_Poz_Proba<-All_Results_Df_Percentage$`Hysteresis Loop Area`%>%
  is.nan()%>%
  which()

#lets create an empty vector

First_Note_position<-c()
ifelse(Min_ST_NaN_In_Poz_Proba>0,First_Note_position[1]<-1,First_Note_position[1]<-NA)
ifelse(RR_ST_NaN_In_Poz_Proba>0,First_Note_position[2]<-2,First_Note_position[2]<-NA)
ifelse(SI_NaN_In_Poz_Proba>0,First_Note_position[3]<-3,First_Note_position[3]<-NA)
ifelse(Compliance_NaN_In_Poz_Proba>0,First_Note_position[4]<-4,First_Note_position[4]<-NA)
ifelse(HLA_NaN_In_Poz_Proba>0,First_Note_position[5]<-5,First_Note_position[5]<-NA)
First_Note_position
#############################################################################
#Ok let's make that a beautiful table
### Try and make a nice table to export


#All_Results_Df that's the df i'm trying to make beautiful
All_Results_Final<-All_Results_Df_Percentage %>%
  gt(rowname_col = "Performace Indexes") %>%
  tab_header(title = "Test Performance Analysis",
             subtitle = "All 5 tests used to assess the Toxicity of the compound were analyzed") %>%
  tab_stubhead(label = "Performace Indexes") %>%
  tab_footnote(footnote = "Not A Number : Can be considered = 100",
               locations = cells_body(columns = Col_Names_All_Results_Df[c((na.exclude(First_Note_position))+1)],
                                      rows=c("Positive Post-Test Probability (%)")))%>%
  tab_source_note(source_note = "Inf = Infinity")%>%
  tab_source_note(source_note = "Analysis based on the results of the AB/K/U/L/Y/Z experiments.      Total experiments: N = 30  (5 experiments by Compound)")

All_Results_Final
gtsave(All_Results_Final,"Tests Performance Analysis.html")

