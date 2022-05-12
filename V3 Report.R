#You first need to install the package and call the library for the program to work

#     install.packages("tidyverse")
#     install.packages("svDialogs")

library(tidyverse)#delivers plenty of functions 
library(magrittr)
library(ggplot2) #Used in plotting
library(svDialogs) #to create pop up windows

#Calling the Datas
#LS_Df = Lung Surfactant Dataframe

setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Rapport 6 chemical/")
LS_Df<-read.csv(file.choose(new=TRUE),header = FALSE,sep = "",dec = ".")
names(LS_Df)<-c("Time (s)",
                "Surface Tension (nN/m)",
                "Surface area (mm2)",
                "Drop Volume (cm3)",
                "Curvature (1/cm)",
                "Contact angle (degrees)")

#All units can be convert into International units (IU) for better future conversions
#IU_Names<-c("Time (s)",
            #"Surface Tension (N/m)",
            #"Surface area (m2)",
            #"Drop Volume (m3)",
            #"Curvature (1/m)",
            #"Contact angle (degrees)"
            #)
#names(LS_Df)<-IU_Names

#LS_Df$`Surface Tension (N/m)`<-c((LS_Df$`Surface Tension (N/m)`)*10^9)

#LS_Df$`Surface area (m2)`<-c((LS_Df$`Surface area (m2)`)*10^-6)

#LS_Df$`Drop Volume (m3)`<-c((LS_Df$`Drop Volume (m3)`)*10^-6)

#LS_Df$`Curvature (1/cm)`<-c((LS_Df$`Curvature (1/m)`*100))

##################################################################################################################

#The program will then extract the index entry points of each cycle

ST_Sup_20<-LS_Df[LS_Df$`Surface Tension (nN/m)`>20]
ST_Sup_20_Index<-which(LS_Df$`Surface Tension (nN/m)`>20)

Diff_Index_ST_Sup_20<-ST_Sup_20_Index %>%
  diff() 

Index_Cycle_Points_ST_Sup_20<- which(Diff_Index_ST_Sup_20>1)
RealIndex_Cycle_Points_ST<-ST_Sup_20_Index[Index_Cycle_Points_ST_Sup_20]

##################################################################################################################
#Slicing the Dataframe into cycles



LS_Df_Splitted_by_Cycle<-split(LS_Df,
                               cut(seq_along(LS_Df$`Surface Tension (nN/m)`),
                                   length(RealIndex_Cycle_Points_ST),
                                   labels =  seq(length(RealIndex_Cycle_Points_ST))

                                                                  ))
#########################################################################################################################
#Period exctraction

Mean_Period<-RealIndex_Cycle_Points_ST %>%
  diff() %>%
  mean()

Mean_Period_Seconds<-Mean_Period/10

Mean_Frequency<-1/Mean_Period_Seconds

Print_The_Mean_Period<-print(paste("The Mean Period of each cycle is:",round(Mean_Period_Seconds, digits=1),"seconds")
                             ,quote = FALSE)

###########################################################################################################################
#Extracting Min and max values

      #ST Min() and Max()

Min_ST<-c()

for(i in 1:length(LS_Df_Splitted_by_Cycle)) {Min_ST[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface Tension (nN/m)` %>%
  extract() %>%
  min()}

Max_ST<-c()

for(i in 1:length(LS_Df_Splitted_by_Cycle)) {Max_ST[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface Tension (nN/m)` %>%
  extract() %>%
  max()}

        #SA Min() and Max()
Min_SA<-c()


for(i in 1:length(LS_Df_Splitted_by_Cycle)) {Min_SA[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface area (mm2)` %>%
  extract() %>%
  min()}

Max_SA<-c()

for(i in 1:length(LS_Df_Splitted_by_Cycle)) {Max_SA[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface area (mm2)` %>%
  extract() %>%
  max()}

#The Min and Max can also be extracted for the other parameters
######################################################################################################################

#this could be used to look at their Relative Reduction
#Calculation of the Relative Reduction for ST, SA 
RR_ST<-c((Max_ST-Min_ST)/Max_ST);
RR_SA<-c((Max_SA-Min_SA)/Max_SA);

#Calculation of The RR for the Other Parameter is possible if asked

#######################################################################################################################
#Toxicity determination on ST

#ST analysis
#10 mN/m limit

ST_Min_Sup_10<-Min_ST>10

ST_Min_Sup_10_Index <- ST_Min_Sup_10 %>%
  which()

ST_Sup_10_Diff_Index<- ST_Min_Sup_10 %>%
  which() %>%
  diff()

ST_Sup_10_Consecutive_Index <- which(ST_Sup_10_Diff_Index == 1)

ST_Sup_10_RealConsecutive_Index <- ST_Min_Sup_10_Index[ST_Sup_10_Consecutive_Index]

Print_Number_of_Cons_Min<-print(length(ST_Sup_10_RealConsecutive_Index))

#Relative Reduction of ST Analysis
#0.8 limit, if lower, then this shows inhibition
RR_ST_Low_Index<-which(RR_ST<0.8) 
Diff_RR_ST_Low_Index <- RR_ST_Low_Index %>%
  diff()

RR_ST_Low_Consecutive_Index <- which(Diff_RR_ST_Low_Index==1)

RR_ST_Low_Real_Consecutive_Index <- RR_ST_Low_Index[Diff_RR_ST_Low_Index]

print(length(RR_ST_Low_Real_Consecutive_Index))

#########################################################################################################
#Stability Index Analysis
#SI=2(STmax-STmin)/(STmax+STmin)
#Values below 1 shows Inhibition of the Surfactant
SI_ST<-c()
for (i in 1:length(LS_Df_Splitted_by_Cycle)) {SI_ST[i]<-2*(Max_ST[i]-Min_ST[i])/(Max_ST[i]+Min_ST[i])}

SI_ST_Low_Index<-which(SI_ST<1) 
Diff_SI_ST_Low_Index <- SI_ST_Low_Index %>%
  diff()

SI_ST_Low_Consecutive_Index <- which(Diff_SI_ST_Low_Index==1)

SI_ST_Low_Real_Consecutive_Index <- SI_ST_Low_Index[Diff_SI_ST_Low_Index]

print(length(SI_ST_Low_Real_Consecutive_Index))


#SI Linear Regression

x <- seq(length(LS_Df_Splitted_by_Cycle))

SI_Linear_Regression<-lm(SI_ST~x);


SI_Linear_Regression_Coeffs<-c(SI_Linear_Regression$coefficients);


SI_If_Else_Evolution<-ifelse(SI_Linear_Regression_Coeffs[2]<0,
                             print(paste("The Stability Index is following a linear decrease with a slope of",
                                         round(SI_Linear_Regression_Coeffs[2],digits=4),
                                         "by cycle."),quote=FALSE),
                             print(paste("The Stability Index is following a linear evolution with a slope of",
                                         round(SI_Linear_Regression_Coeffs[2],digits=4),
                                         "by cycle, that may be insignificant."),quote=FALSE));

#############################################################################################################################
#Toxic/Safe print for the end you can change it as you please

Toxic_Print<-c("THIS COMPOUND INDUCES THE INHIBITION OF THE LUNG SURFACTANT");
Safe_Print<-c("THIS COMPOUND DOES NOT INDUCE THE INHIBITION OF THE LUNG SURFACTANT")

ST_If_else_Toxic<-ifelse(length(ST_Sup_10_RealConsecutive_Index) >= 5, 
                            print(Toxic_Print, quote=FALSE),
                            print(Safe_Print, quote=FALSE))

RR_ST_If_else_Toxic<-ifelse(length(RR_ST_Low_Real_Consecutive_Index)>=5, 
                            print(Toxic_Print,quote=FALSE),
                            print(Safe_Print,quote = FALSE))

SI_ST_If_else_Toxic<-ifelse(length(SI_ST_Low_Real_Consecutive_Index)>5,
                            print(Toxic_Print,quote=FALSE),
                            print(Safe_Print,quote=FALSE))

#############################################################################################################################

#SA Evolution Analysis using the Squeeze Theorem

SA_Min_Linear_Regression<-lm(Min_SA~x);
SA_Max_Linear_Regression<-lm(Max_SA~x);

SA_Min_Linear_Regression_Coeffs<-c(SA_Min_Linear_Regression$coefficients);
SA_Max_Linear_Regression_Coeffs<-c(SA_Max_Linear_Regression$coefficients)

SA_Tendency_Linear_Regression_Coeffs<-c((SA_Min_Linear_Regression_Coeffs+SA_Max_Linear_Regression_Coeffs)/2)


SA_Func_Matrix<-rbind(SA_Min_Linear_Regression_Coeffs,
                      SA_Max_Linear_Regression_Coeffs,
                      SA_Tendency_Linear_Regression_Coeffs);

colnames(SA_Func_Matrix)<-c("Intercept","Slope");
row.names(SA_Func_Matrix)<-c("Min SA","Max SA","Tendency SA");
print(SA_Func_Matrix)

SA_If_Else_Evolution<-ifelse(SA_Func_Matrix[3,2]<0,
                             print(paste("The SA is following a linear decrease with a slope of",round(SA_Func_Matrix[3,2],digits=4),"m2/cycle."),
                                   quote=FALSE),
                             print(paste("The SA is following a linear evolution with a slope of",round(SA_Func_Matrix[3,2],digits=4),"m2/cycle, that may be insignificant."),
                                   quote=FALSE))

############################################################################################################################################################################################################

#Compliance analysis
#Lung Surfactant helps in reducing surface tension and thereby increases compliance. 
#An absence of the surfactant leads to a decrease in pulmonary compliance, 
#This condition is called newborn respiratory distress syndrome.

#Compliance=diff(DropVolume)/Diff(Surface Tension)

#I've chose to use the SURFACE AREA instead of the DROP VOLUME 
#as changes in paterns are more noticeables
#Thus it is not exactly the Compliance but a created value which is proportionnal to the Compliance

Compliance<-c()
Compliance<-c(LS_Df$`Surface area (mm2)`/LS_Df$`Surface Tension (nN/m)`)
Rounded_Compliance<-round(Compliance,5)

#For now, we'll take 0.04 at a base limit
#Let's consider that if the maximum of a cycle drops below that value, it means LS inhibition
#The best would be to take a limit which suits the 5 mN/m limit that we put earlier
#First The Programm will split the compliance values into cycles just as before

Compliance_Splitted_by_Cycle<-split(Compliance,
                               cut(seq_along(LS_Df$`Surface Tension (nN/m)`),
                                   length(RealIndex_Cycle_Points_ST),
                                   labels =  seq(length(RealIndex_Cycle_Points_ST))
                               ))


Max_Compliances<-c()

for (l in seq(length(Compliance_Splitted_by_Cycle))) 
  {Max_Compliances[l]<-Compliance_Splitted_by_Cycle[[l]] %>%
    extract() %>%
    max()}


Compliance_Low_Index<-which(Max_Compliances<0.04) 
Diff_Compliance_Low_Index <- Compliance_Low_Index %>%
  diff()

Compliance_Low_Consecutive_Index <- which(Diff_Compliance_Low_Index==1)

Compliance_Low_Real_Consecutive_Index <- Compliance_Low_Index[Diff_Compliance_Low_Index]
Number_of_Cons_Compliance_low<-length(Compliance_Low_Real_Consecutive_Index) %>%
  print()

#plot(seq(LS_Df$`Surface Tension (nN/m)`),Compliance,type = "l")

#plot(seq(length(Max_Compliances)),Max_Compliances,type="b",
#     xlab="Cycle Number",
#     ylab="Max Compliance",
#     main = "Max Compliance by Cycle")


#####################################################################################################################################
#Hysteresis Loop Analysis

SA_ST_Isolated_Splitted<-vector("list",length(LS_Df_Splitted_by_Cycle))

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{SA_ST_Isolated_Splitted[[i]]<-LS_Df_Splitted_by_Cycle[[i]] %>%
  select("Surface area (mm2)","Surface Tension (nN/m)")}

#The program will split each cycle into Inspiration values and expiration values
# on cherche l'expiration, compris entre le max et le min
#puis on considère que toutes les autres valeurs font partie de l'expiration
#utiliser la fonction sort, pour que l'expiration augmente
#attention il faut les memes valeurs/index que SA, donc la fonction sort maybe sur la toute fin

ST_Insp<-vector("list",length(LS_Df_Splitted_by_Cycle))
ST_Exp<-vector("list",length(LS_Df_Splitted_by_Cycle))

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{ST_Exp[[i]]<-c(SA_ST_Isolated_Splitted[[i]][["Surface Tension (nN/m)"]][
  which.max(SA_ST_Isolated_Splitted[[i]][["Surface Tension (nN/m)"]]):
    which.min(SA_ST_Isolated_Splitted[[i]][["Surface Tension (nN/m)"]])
  ])} 

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{ST_Insp[[i]]<-c(SA_ST_Isolated_Splitted[[i]][["Surface Tension (nN/m)"]][-(
  which.max(SA_ST_Isolated_Splitted[[i]][["Surface Tension (nN/m)"]]):
    which.min(SA_ST_Isolated_Splitted[[i]][["Surface Tension (nN/m)"]])
)])} 


SA_Insp<-vector("list",length(LS_Df_Splitted_by_Cycle))
SA_Exp<-vector("list",length(LS_Df_Splitted_by_Cycle))

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{SA_Exp[[i]]<-c(SA_ST_Isolated_Splitted[[i]][["Surface area (mm2)"]][
  which.max(SA_ST_Isolated_Splitted[[i]][["Surface area (mm2)"]]):
    which.min(SA_ST_Isolated_Splitted[[i]][["Surface area (mm2)"]])
])} 

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{SA_Insp[[i]]<-c(SA_ST_Isolated_Splitted[[i]][["Surface area (mm2)"]][-(
  which.max(SA_ST_Isolated_Splitted[[i]][["Surface area (mm2)"]]):
    which.min(SA_ST_Isolated_Splitted[[i]][["Surface area (mm2)"]])
)])} 

##########################################################################################

#Hysteresis Loop Area
#It uses the trapezoidal theorem to get a round value of the HLA
#AUC=Area Under the Curve

AUC_Exp<-vector("list",(length(LS_Df_Splitted_by_Cycle)-1))
for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{AUC_Exp[[i]]<-abs(((SA_Exp[[i]]+SA_Exp[[i+1]])*(ST_Exp[[i+1]]-ST_Exp[[(i)]])/2))}

AUC_Insp<-vector("list",(length(LS_Df_Splitted_by_Cycle)-1))
for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
  {AUC_Insp[[i]]<-abs(((SA_Insp[[i]]+SA_Insp[[i+1]])*(ST_Insp[[i+1]]-ST_Insp[[i]]))/2)}

#Then it will sum all the elements of each vector, as it forms the whole area under the curve from the first value to the last
Sum_AUC_Exp<-c()
for(k in seq(length(LS_Df_Splitted_by_Cycle)))
  {Sum_AUC_Exp[k]<-sum(AUC_Exp[[k]],na.rm = TRUE)}

Sum_AUC_Insp<-c()
for(k in seq(length(LS_Df_Splitted_by_Cycle)))
  {Sum_AUC_Insp[k]<-sum(AUC_Insp[[k]],na.rm = TRUE)}

#ici,valeurs different en fonctiion de si valeur absolue ou non
#guetter les courbes pour comprendre
Hysteresis_Loop_Area_by_Cycle<-abs(Sum_AUC_Exp-Sum_AUC_Insp)

plot(seq(length(Hysteresis_Loop_Area_by_Cycle)),Hysteresis_Loop_Area_by_Cycle,
     xlab = "Cycle Number",
     ylab = "Hysteresis Loop Area",
     type = "b")
Reg <- lm(Hysteresis_Loop_Area_by_Cycle~seq(length(Hysteresis_Loop_Area_by_Cycle)))
abline(Reg,col="red")

#C'est parf mgl, quand Reg decroit c'est que c'est toxique
#quand reg croit c'est non toxique
#Rajouter les ifelse pour la compliance et l'hysteresis loop area
#attention aux eventuelles coquilles dans le sens ou il peut y avoir des faux positifs
#d'ou l'importance d'un script de stat, 
#il faudra aussi reunir dans une liste toutes les valeurs à extraires pour le script stat
#egalement inclure des pop up box pour save et renommer
#trouver comment sauvegarder le total des graphs + la final list avec tous le recap
#trouver un moyen de faire le code avec plusieurs composés d'un coup
#At the end of the end of the end, a "capture.ouput(Final_List, file="xxx.txt") must be added so the end resutlts are saved in a recap list
#Don't hesitate to reload the work directory
#The file for now shall be renamed each time, same goes for the plots
#A way of not having to do that each time, or use popups to do it may be a better solution
#there might be packages designed for that