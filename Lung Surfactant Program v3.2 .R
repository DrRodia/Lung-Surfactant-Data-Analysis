#You first need to install some packages and call them from the library for the program to work
#If any problem occurs be sure to have an updated version of them by checking the "Tools" button in the utiliity bar

install.packages("tidyverse")
install.packages("svDialogs")
install.packages("cowplot")
install.packages("gridExtra")   

library(tidyverse)#delivers plenty of functions 
library(magrittr)
library(ggplot2) #Used in plotting
library(cowplot) #used to set multiple plots into one page
library(svDialogs) #to create pop up windows
library(gridExtra) #to save tables into pdf format

#LS_Df = Lung Surfactant Dataframe

###########################################################################
#Setting your Work Directory
#it would be a place where you'll find the raw results of the surfactometer
      #it fully works with .dat files

SetWD_Input<-setwd("C:/Users/Niz/Desktop/STAGE TOX LS/Rapport 6 chemical/")
#Calling the Datas
LS_Df<-read.table(file.choose(new=TRUE),header = FALSE,sep = "",dec = ".")

#Enter the name of the Compound that was selected
#It would be used to save the end results in a selected folder
User_Input_File_Name <- dlg_input("What was the Coumpound's name?  (Letter+Number)")$res

########################################################################

names(LS_Df)<-c("Time (s)",
                "Surface Tension (mN/m)",
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

ST_Sup_20<-LS_Df[LS_Df$`Surface Tension (mN/m)`>20]
ST_Sup_20_Index<-which(LS_Df$`Surface Tension (mN/m)`>20)

Diff_Index_ST_Sup_20<-ST_Sup_20_Index %>%
  diff() 

Index_Cycle_Points_ST_Sup_20<- which(Diff_Index_ST_Sup_20>1)
RealIndex_Cycle_Points_ST<-ST_Sup_20_Index[Index_Cycle_Points_ST_Sup_20]

##################################################################################################################
#Slicing the Dataframe into cycles



LS_Df_Splitted_by_Cycle<-split(LS_Df,
                               cut(seq_along(LS_Df$`Surface Tension (mN/m)`),
                                   length(RealIndex_Cycle_Points_ST),
                                   labels =  seq(length(RealIndex_Cycle_Points_ST))))
Cycle_Number<-seq(length(LS_Df_Splitted_by_Cycle))
                                                                  
#########################################################################################################################
#Period extraction
   #It should be approximately 3 seconds as you told me 

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

for(i in 1:length(LS_Df_Splitted_by_Cycle)) 
  {Min_ST[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface Tension (mN/m)` %>%
  extract() %>%
  min()}

Max_ST<-c()

for(i in 1:length(LS_Df_Splitted_by_Cycle)) 
  {Max_ST[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface Tension (mN/m)` %>%
  extract() %>%
  max()}


###################################################################################
        #SA Min() and Max()
Min_SA<-c()


for(i in 1:length(LS_Df_Splitted_by_Cycle)) 
  {Min_SA[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface area (mm2)` %>%
  extract() %>%
  min()}

Max_SA<-c()

for(i in 1:length(LS_Df_Splitted_by_Cycle)) 
  {Max_SA[i]<-LS_Df_Splitted_by_Cycle[[i]]$`Surface area (mm2)` %>%
  extract() %>%
  max()}

#The Min and Max can also be extracted for the other parameters
#this could be used to look at their Relative Reduction
######################################################################################################################
#Calculation of the Relative Reduction for ST, SA 
RR_ST<-c((Max_ST-Min_ST)/Max_ST);
RR_SA<-c((Max_SA-Min_SA)/Max_SA);

#Calculation of The RR for the Other Parameter is possible if asked

#######################################################################################################################
#Toxicity determination on ST

#ST analysis
#Tox_Limit mN/m limit

ST_Tox_Limit<-dlg_input("Enter the value of min(ST) considered to be the base limit for LS inhibition (mN/m):")$res %>%
  as.numeric()

ST_Min_Sup_Tox_Limit<-Min_ST>ST_Tox_Limit

ST_Min_Sup_Tox_Limit_Index <- ST_Min_Sup_Tox_Limit %>%
  which()

ST_Sup_Tox_Limit_Diff_Index<- ST_Min_Sup_Tox_Limit %>%
  which() %>%
  diff()

ST_Sup_Tox_Limit_Consecutive_Index <- which(ST_Sup_Tox_Limit_Diff_Index == 1)

ST_Sup_Tox_Limit_RealConsecutive_Index <- ST_Min_Sup_Tox_Limit_Index[ST_Sup_Tox_Limit_Consecutive_Index]

Print_Number_of_Cons_Min<-print(length(ST_Sup_Tox_Limit_RealConsecutive_Index))
##########################################################################################
#Relative Reduction of ST Analysis
#RR_ST_Toxic limit, if lower, then this shows inhibition
RR_ST_Toxic<-((mean(Max_ST[seq(4)])-ST_Tox_Limit)/mean(Max_ST[seq(4)]))
  
RR_ST_Low_Index<-which(RR_ST<RR_ST_Toxic) 
Diff_RR_ST_Low_Index <- RR_ST_Low_Index %>%
  diff()

RR_ST_Low_Consecutive_Index <- which(Diff_RR_ST_Low_Index==1)

RR_ST_Low_Real_Consecutive_Index <- RR_ST_Low_Index[RR_ST_Low_Consecutive_Index]

print(length(RR_ST_Low_Real_Consecutive_Index))

#RR GAM Analysis
GAM2<-stat_smooth(method = "gam",
                  x=Cycle_Number,
                  y=RR_ST)
RR_ST_GAM_Index_inhibition_Time_By_Cycle<-which(GAM2$aes_params$y<RR_ST_Toxic)
RR_ST_Theorical_Inhibition_Start_Index_By_Cycle<-GAM2$aes_params$x[RR_ST_GAM_Index_inhibition_Time_By_Cycle[1]]
RR_ST_Theorical_Inhibition_Start_By_Seconds<-((RR_ST_Theorical_Inhibition_Start_Index_By_Cycle*Mean_Period_Seconds))
RR_ST_Theorical_Inhibition_Start_Index_By_Cycle
RR_ST_Theorical_Inhibition_Start_By_Seconds

#########################################################################################################
#Stability Index Analysis
#SI=2(STmax-STmin)/(STmax+STmin)
#The litterature shows that values below 1 shows Inhibition of the Surfactant
SI_ST<-c()
for (i in 1:length(LS_Df_Splitted_by_Cycle)) {SI_ST[i]<-2*(Max_ST[i]-Min_ST[i])/(Max_ST[i]+Min_ST[i])}

SI_ST_Low_Index<-which(SI_ST<1) 
Diff_SI_ST_Low_Index <- SI_ST_Low_Index %>%
  diff()

SI_ST_Low_Consecutive_Index <- which(Diff_SI_ST_Low_Index==1)

SI_ST_Low_Real_Consecutive_Index <- SI_ST_Low_Index[SI_ST_Low_Consecutive_Index]

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

ST_If_else_Toxic<-ifelse(length(ST_Sup_Tox_Limit_RealConsecutive_Index) >= 5, 
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
                             print(paste("The SA is following a linear decrease with a slope of",round(SA_Func_Matrix[3,2],digits=4),"mm2/cycle."),
                                   quote=FALSE),
                             print(paste("The SA is following a linear evolution with a slope of",round(SA_Func_Matrix[3,2],digits=4),"mm2/cycle, IT IS CONSIDERED STABLE."),
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
Compliance<-c(LS_Df$`Surface area (mm2)`/LS_Df$`Surface Tension (mN/m)`)
Rounded_Compliance<-round(Compliance,5)

#For now, we'll take 0.04 at a base limit
#Let's consider that if the maximum of a cycle drops below that value, it means LS inhibition
#The best would be to take a limit which suits the 5 mN/m limit that we put earlier
#First The Programm will split the compliance values into cycles just as before

Compliance_Splitted_by_Cycle<-split(Compliance,
                               cut(seq_along(LS_Df$`Surface Tension (mN/m)`),
                                   length(RealIndex_Cycle_Points_ST),
                                   labels =  seq(length(RealIndex_Cycle_Points_ST))
                               ))


Max_Compliances<-c()

for (l in seq(length(Compliance_Splitted_by_Cycle))) 
  {Max_Compliances[l]<-Compliance_Splitted_by_Cycle[[l]] %>%
    extract() %>%
    max()}


Min_Compliances<-c()

for (l in seq(length(Compliance_Splitted_by_Cycle))) 
{Min_Compliances[l]<-Compliance_Splitted_by_Cycle[[l]] %>%
  extract() %>%
  min()}

Compliance_Low_Index<-which(Max_Compliances<0.02) 
Diff_Compliance_Low_Index <- Compliance_Low_Index %>%
  diff()

Compliance_Low_Consecutive_Index <- which(Diff_Compliance_Low_Index==1)

Compliance_Low_Real_Consecutive_Index <- Compliance_Low_Index[Diff_Compliance_Low_Index]
Number_of_Cons_Compliance_low<-length(Compliance_Low_Real_Consecutive_Index[!is.na(Compliance_Low_Real_Consecutive_Index)]) 

Compliance_If_else_Toxic<-ifelse(Number_of_Cons_Compliance_low >= 5, 
                         print(Toxic_Print, quote=FALSE),
                         print(Safe_Print, quote=FALSE))

#Loess Analysis on the max(Compliance), would be able with it to gain the theorical time before inhibition

GAM3<-stat_smooth(method = "gam",
                  x=Cycle_Number,
                  y=Max_Compliances)
Max_Compliance_GAM_Index_inhibition_Time_By_Cycle<-which(GAM3$aes_params$y<0.02)
Max_Compliance_Theorical_Inhibition_Start_Index_By_Cycle<-GAM3$aes_params$x[Max_Compliance_GAM_Index_inhibition_Time_By_Cycle[1]]
Max_Compliance_Theorical_Inhibition_Start_By_Seconds<-((Max_Compliance_Theorical_Inhibition_Start_Index_By_Cycle*Mean_Period_Seconds))
Max_Compliance_Theorical_Inhibition_Start_Index_By_Cycle
Max_Compliance_Theorical_Inhibition_Start_By_Seconds
###########################################################################################
#RR of Compliance analysis
RR_Compliance<-c((Max_Compliances-Min_Compliances)/Max_Compliances)

RR_Compliance_Toxic<-RR_ST_Toxic

RR_Compliance_Low_Index<-which(RR_Compliance<RR_Compliance_Toxic) 
Diff_RR_Compliance_Low_Index <- RR_Compliance_Low_Index %>%
  diff()

RR_Compliance_Low_Consecutive_Index <- which(Diff_RR_Compliance_Low_Index==1)

RR_Compliance_Low_Real_Consecutive_Index <- RR_Compliance_Low_Index[Diff_RR_Compliance_Low_Index]
Number_of_Cons_RR_Compliance_low<-length(RR_Compliance_Low_Real_Consecutive_Index[!is.na(RR_Compliance_Low_Real_Consecutive_Index)]) 

RR_Compliance_If_else_Toxic<-ifelse(Number_of_Cons_RR_Compliance_low >= 5, 
                                 print(Toxic_Print, quote=FALSE),
                                 print(Safe_Print, quote=FALSE))
#####################################################################################################################################
#Hysteresis Loop Analysis
#HL= ST/SA ou ST=f(SA)0

SA_ST_Isolated_Splitted<-vector("list",length(LS_Df_Splitted_by_Cycle))

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{SA_ST_Isolated_Splitted[[i]]<-LS_Df_Splitted_by_Cycle[[i]] %>%
  select("Surface area (mm2)","Surface Tension (mN/m)")}

#The program will split each cycle into Inspiration values and expiration values
# on cherche l'expiration, compris entre le max et le min
#puis on considère que toutes les autres valeurs font partie de l'expiration
#utiliser la fonction sort, pour que l'expiration augmente
#attention il faut les memes valeurs/index que SA, donc la fonction sort maybe sur la toute fin

ST_Insp<-vector("list",length(LS_Df_Splitted_by_Cycle))
ST_Exp<-vector("list",length(LS_Df_Splitted_by_Cycle))

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{ST_Exp[[i]]<-c(SA_ST_Isolated_Splitted[[i]][["Surface Tension (mN/m)"]][
  which.max(SA_ST_Isolated_Splitted[[i]][["Surface Tension (mN/m)"]]):
    which.min(SA_ST_Isolated_Splitted[[i]][["Surface Tension (mN/m)"]])
  ])} 

for (i in seq(length(LS_Df_Splitted_by_Cycle))) 
{ST_Insp[[i]]<-c(SA_ST_Isolated_Splitted[[i]][["Surface Tension (mN/m)"]][-(
  which.max(SA_ST_Isolated_Splitted[[i]][["Surface Tension (mN/m)"]]):
    which.min(SA_ST_Isolated_Splitted[[i]][["Surface Tension (mN/m)"]])
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
#here the program uses the absolute values so the results are all above 0 
Hysteresis_Loop_Area_by_Cycle<-abs(Sum_AUC_Exp-Sum_AUC_Insp)

Reg_HLA <- lm(Hysteresis_Loop_Area_by_Cycle~seq(length(Hysteresis_Loop_Area_by_Cycle)))
HLA_Reg_Coeffs<-c(Reg_HLA$coefficients)
names(HLA_Reg_Coeffs)<-c("Intercept","Slope")
HLA_Slope<-unname(HLA_Reg_Coeffs[2])
#When the Coeff of the LR is negative, it means the HL A is decreasing each cycle, 
#meaning a decrease in the LUNG surfactant ability to maintain a stable state

If_Else_HLA<-ifelse(HLA_Slope<0,print(Toxic_Print,quote=FALSE),
                    print(Safe_Print,quote=FALSE))
                    
###########################################################################################################
#Setting all Data by cycle in a cycle dataframe

Values_by_Cycle_Df<-data.frame(col1=Min_ST, 
                               col2= Max_ST, 
                               col3=RR_ST,
                               col3_bis=SI_ST,
                               col4=Min_SA, 
                               col5=Max_SA,RR_SA,
                               col6=Max_Compliances,
                               col7=c(Hysteresis_Loop_Area_by_Cycle,
                                      rep(NA,
                                          length(LS_Df_Splitted_by_Cycle)-length(Hysteresis_Loop_Area_by_Cycle))))

names_final_df<-c("Minimum of ST",
                  "Maximum of ST",
                  "Relative Reduction of ST",
                  "Stability Index of ST",
                  "Minimum of SA", 
                  "Maximum of SA", 
                  "Relative Reduction of SA",
                  "Maximum of Compliance",
                  "Hysteresis Loop Area")

names(Values_by_Cycle_Df)<-names_final_df
Cycle_Number<- c(seq(length(LS_Df_Splitted_by_Cycle)))

Values_by_Cycle_Df
##########################################################################################################
#GAM smoothing analysis

#Min ST GAM Analysis
GAM1<-stat_smooth(method = "gam",
                  x=Cycle_Number,
                  y=Values_by_Cycle_Df$`Minimum of ST`)
Min_ST_GAM_Index_inhibition_Time_By_Cycle<-which(GAM1$aes_params$y>ST_Tox_Limit)
Min_ST_Theorical_Inhibition_Start_Index_By_Cycle<-GAM1$aes_params$x[Min_ST_GAM_Index_inhibition_Time_By_Cycle[1]]
Min_ST_Theorical_Inhibition_Start_By_Seconds<-((Min_ST_Theorical_Inhibition_Start_Index_By_Cycle*Mean_Period_Seconds))
Min_ST_Theorical_Inhibition_Start_Index_By_Cycle
Min_ST_Theorical_Inhibition_Start_By_Seconds

#RR GAM Analysis
GAM2<-stat_smooth(method = "gam",
                  x=Cycle_Number,
                  y=RR_ST)
RR_ST_GAM_Index_inhibition_Time_By_Cycle<-which(GAM2$aes_params$y<RR_ST_Toxic)
RR_ST_Theorical_Inhibition_Start_Index_By_Cycle<-GAM2$aes_params$x[RR_ST_GAM_Index_inhibition_Time_By_Cycle[1]]
RR_ST_Theorical_Inhibition_Start_By_Seconds<-((RR_ST_Theorical_Inhibition_Start_Index_By_Cycle*Mean_Period_Seconds))
RR_ST_Theorical_Inhibition_Start_Index_By_Cycle
RR_ST_Theorical_Inhibition_Start_By_Seconds

#GAM Analysis of SI

GAM3<-stat_smooth(method = "gam",
                  x=Cycle_Number,
                  y=SI_ST)
SI_GAM_Index_inhibition_Time_By_Cycle<-which(GAM3$aes_params$y<1)
SI_Theorical_Inhibition_Start_Index_By_Cycle<-GAM3$aes_params$x[SI_GAM_Index_inhibition_Time_By_Cycle[1]]
SI_Theorical_Inhibition_Start_By_Seconds<-((SI_Theorical_Inhibition_Start_Index_By_Cycle*Mean_Period_Seconds))
SI_Theorical_Inhibition_Start_Index_By_Cycle
SI_Theorical_Inhibition_Start_By_Seconds

Mean_ST_Theorical_Inhibition_Start_By_Seconds=mean(c(Min_ST_Theorical_Inhibition_Start_By_Seconds,
                                                     RR_ST_Theorical_Inhibition_Start_By_Seconds,
                                                     SI_Theorical_Inhibition_Start_By_Seconds))



Mean_ST_Theorical_Inhibition_Start_By_Seconds

#Loess Analysis on the max(Compliance), would be able with it to gain the theorical time before inhibition

GAM4<-stat_smooth(method = "gam",
                  x=Cycle_Number,
                  y=Max_Compliances)
Max_Compliance_GAM_Index_inhibition_Time_By_Cycle<-which(GAM4$aes_params$y<0.02)
Max_Compliance_Theorical_Inhibition_Start_Index_By_Cycle<-GAM4$aes_params$x[Max_Compliance_GAM_Index_inhibition_Time_By_Cycle[1]]
Max_Compliance_Theorical_Inhibition_Start_By_Seconds<-((Max_Compliance_Theorical_Inhibition_Start_Index_By_Cycle*Mean_Period_Seconds))
Max_Compliance_Theorical_Inhibition_Start_Index_By_Cycle
Max_Compliance_Theorical_Inhibition_Start_By_Seconds

##########################################################################################################
#Plot Part

Values_by_Cycle_Df<-data.frame(Min_ST,
                               Max_ST, 
                               RR_ST,
                               SI_ST,
                               Min_SA, 
                               Max_SA,
                               RR_SA,
                               Max_Compliances,
                               RR_Compliance,
                               c(Hysteresis_Loop_Area_by_Cycle,
                                      rep(NA,
                                          length(LS_Df_Splitted_by_Cycle)-length(Hysteresis_Loop_Area_by_Cycle))))

names_final_df<-c("Minimum of ST",
                  "Maximum of ST",
                  "Relative Reduction of ST",
                  "Stability Index of ST",
                  "Minimum of SA", 
                  "Maximum of SA", 
                  "Relative Reduction of SA",
                  "Maximum of Compliance",
                  "Relative Reduction of Compliance",
                  "Hysteresis Loop Area")

names(Values_by_Cycle_Df)<-names_final_df
Cycle_Number<- c(seq(length(LS_Df_Splitted_by_Cycle)))

Values_by_Cycle_Df

#color, size ... NE SONT PAS DANS AES, IL FAUT FERMER PARENTHESE


#Nice
##################################################################################"
#Plot ST

P1<-ggplot(LS_Df)+
  #geom_point(aes(`Time (s)`,`Surface Tension (mN/m)`),
  #          color="black")+
  geom_line(aes(x=`Time (s)`,
                y=`Surface Tension (mN/m)`)
            ,color="black")+
  geom_hline(yintercept = ST_Tox_Limit, color="red")+
  labs(title = "Surface Tension = f(Time)",
       subtitle="ST (mN/m), Time (s)", 
       caption = paste("Minimum of Cycle < ",ST_Tox_Limit," mN/m : Inhibited Lung Surfactant")) +
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5,
                                    color= "red"))


#Plot Min and Max ST
P1_BIS<-ggplot(data=Values_by_Cycle_Df) +
  geom_point(aes(x=Cycle_Number,
                 y=`Minimum of ST`),
             color=ifelse(Values_by_Cycle_Df$`Minimum of ST`>ST_Tox_Limit,
                          "red",
                          "green")) +
  geom_smooth(aes(x=Cycle_Number,
                  y=`Minimum of ST`),
              method = "gam",
              color="darkblue")+
  
  geom_point(aes(x=Cycle_Number,
                 y=`Maximum of ST`),
             color="black") +
  geom_hline(yintercept = ST_Tox_Limit,
             color="red")+
  
  labs(title  ="Min and Max of Surface Tension = f(Cycle Number)",
       subtitle = paste("Toxicity can be assest using the", ST_Tox_Limit ,"mN/m limit on the Min of ST"),
       caption = c(paste("Min(ST) <", ST_Tox_Limit, "mN/m = Functional LS"),paste("Min(ST) >", ST_Tox_Limit, "mN/m = Inhibited LS"))) +
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5,
                                  size=11),
        plot.subtitle =  element_text(color = "darkblue"),
        plot.caption = element_text(hjust =c(0,1),color = c("darkgreen","red") )) + 
  
  xlab(label="Cycle Number") + 
  ylab(label = "Surface Tension of the LS (mN/m)")


#Plot RR_ST
P1_TER <-ggplot(data=Values_by_Cycle_Df) +
  geom_point(aes(x=Cycle_Number,
                 y=`Relative Reduction of ST`),
             color=ifelse(Values_by_Cycle_Df$`Relative Reduction of ST`<RR_ST_Toxic,
                          "red",
                          "green")) +
  geom_smooth(aes(x = Cycle_Number,
                  y=`Relative Reduction of ST`),
              method="gam",
              color="darkblue")+
  geom_hline(yintercept = RR_ST_Toxic,
             color="red")+
  labs(title = "Relative Reduction of Surface Tension = f(Cycle Number)",
       subtitle = paste("RR[i] = (Max[i] - Min[i])/Max[i]) , with a Toxic baseline of", round(RR_ST_Toxic,digits=2)) ,
       caption=c(paste("RR(ST) > ",round(RR_ST_Toxic,digits=2)," : Functional LS"),paste("RR(ST) < ",round(RR_ST_Toxic,digits=2)," : Inhibited LS"))) +
  xlab("Cycle Number") + 
  ylab("Relative Reduction of ST")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0,color = "darkblue" ),
        plot.caption = element_text(hjust =c(0,1), color=c("darkgreen","red")))

#######################################################
#Plot STability Index
#SI=2(STmax-STmin)/(STmax+STmin)

P2<-ggplot(Values_by_Cycle_Df)+
  geom_point(aes(x = Cycle_Number,
                 y=`Stability Index of ST`),
             color=ifelse(Values_by_Cycle_Df$`Stability Index of ST`<1,
                          "red",
                          "green"))+
  geom_smooth(aes(x=Cycle_Number,
                  y=`Stability Index of ST`),
              method = "gam",
              color="darkblue")+
  geom_hline(yintercept = 1,
             color="red")+
  
  labs(title = "Stability Index of the Lung Surfactant = f(Cycle Number)",
       subtitle = "SI[i] = (Max[i] - Min[i])/(Max[i]+Min[i]) , with a Toxic baseline of 1" ,
       caption=c("Stability Index >1 : Functional LS","Stability Index <1 : Inhibited LS"))+
  xlab("Cycle Number") +
  ylab("Stability Index ")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0,color = "darkblue" ),
        plot.caption = element_text(hjust =c(0,1),color = c("darkgreen","red" )))

####################################################################
#Plot SA

P3<-ggplot(LS_Df)+
  geom_line(aes(x =`Time (s)`,
                y = `Surface area (mm2)`)) +
  labs(title = "Surface Area = f(Time)",
       subtitle="SA (mm2), Time (s)") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_light()

P3_BIS<-ggplot(data=Values_by_Cycle_Df) +
  geom_point(aes(x=Cycle_Number,
                 y=`Minimum of SA`),
             color="black") +
  
  geom_point(aes(x=Cycle_Number,
                 y=`Maximum of SA`),
             color="black") + 
  geom_smooth(aes(x = Cycle_Number,
                  y =(`Maximum of SA` + `Minimum of SA`)/2 ),
              method =  "lm",
              color="blue") +
  xlab(label = "Cycle Number") +
  ylab(label = "Surface Area (mm2)") +
  labs(title = "Min and Max Values of SA =f(Cycle Number)",
       subtitle = "SA's Tendency curve",
       caption = "Obtained using the Squeeze Theorem on the Min & Max Linear Regression Coefficients") +
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0,color = "blue" ),
        plot.caption = element_text(hjust = 0,color = "darkblue" ))

#############################################################
#Plot Max Compliance (why not plotting the compliance=f(t))

P4<-ggplot()+
  geom_line(aes(x=LS_Df$`Time (s)`,
                y=Compliance),
            color="black")+
  geom_hline(yintercept = 0.02,
             color="Red")+
  xlab(label="Time (s)")+
  ylab(label="Compliance")+
  labs(title = "Compliance=f(Time)",
       subtitle = "Toxicity can be assest by looking at the Maximum of each Cycle",
       caption = "Max(Compliance) < 0.02 : Inhibited LS")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0,color = "darkblue" ),
        plot.caption = element_text(hjust =0.5,color = "red" ))


P4_BIS<-ggplot(data=Values_by_Cycle_Df)+
  geom_point(aes(x=Cycle_Number,
                 y=`Max_Compliances`),
             color= ifelse(Values_by_Cycle_Df$`Maximum of Compliance`<0.02,
                           "red",
                           "green"))+
  
  geom_smooth(aes(x=Cycle_Number,
                  y=`Maximum of Compliance`),
              method = "gam",
              color=c("darkblue"))+
  
  geom_hline(yintercept = 0.02,
             color="red")+
  xlab(label= "Cycle Number") +
  ylab(label = "Maximum of Compliance")+
  labs(title="Maximum of Compliance = f(Cycle)",
       subtitle = "Curve obtained by gam Smoothing",
       caption = c("Max(Compliance) > 0.02 : Functional LS","Max(Compliance) < 0.02 : Inhibited LS"))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5,color="black"),
        plot.subtitle = element_text(hjust = 0,color = "darkblue" ),
        plot.caption = element_text(hjust =c(0,1),color = c("darkgreen","red")))

P4_TER <-ggplot(data=Values_by_Cycle_Df) +
  geom_point(aes(x=Cycle_Number,
                 y=`Relative Reduction of Compliance`),
             color=ifelse(Values_by_Cycle_Df$`Relative Reduction of Compliance`<RR_Compliance_Toxic,
                          "red",
                          "green")) +
  geom_smooth(aes(x = Cycle_Number,
                  y=`Relative Reduction of Compliance`),
              method="gam",
              color="darkblue")+
  geom_hline(yintercept = round(RR_ST_Toxic,digits = 2),
             color="red")+
  labs(title = "Relative Reduction of Compliance = f(Cycle Number)",
       subtitle = paste("RR[i] = (Max[i] - Min[i])/Max[i]) , with a Toxic baseline of ", round(RR_Compliance_Toxic,digits=2)) ,
       caption=c(paste("RR( Compliance) > ", round(RR_Compliance_Toxic, digits=2) ," : Functional LS"),paste("RR(Compliance) < ",round(RR_Compliance_Toxic,digits=2)," : Inhibited LS"))) +
  xlab("Cycle Number") + 
  ylab("Relative Reduction of Compliance")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0,color = "darkblue" ),
        plot.caption = element_text(hjust =c(0,1), color=c("darkgreen","red")))

###############################################################################################
#Plot Hysteresis Loop Area by Cycle

P5<-ggplot(data = Values_by_Cycle_Df)+
  geom_point(aes(x = Cycle_Number,
                y=`Hysteresis Loop Area`))+
  geom_smooth(aes(x = Cycle_Number,
                  y=`Hysteresis Loop Area`),
              method = "lm",
              color= ifelse(HLA_Slope>0,"green","red"))+
  xlab(label= "Cycle Number") +
  ylab(label = "Hysteresis Loop Area")+
  labs(title="Hysteresis Loop Area = f(Cycle)",
       subtitle = "Curve obtained by Linear Regression Smoothing",
       caption = c(paste("The HLA's evolution has a rate of",round(HLA_Slope,digits = 2),"Unit by cycle"),ifelse(HLA_Slope>0,
                                                                                                                 "Functional LS",
                                                                                                                 "Inhibited LS")))+
  
  
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5,color="black"),
        plot.subtitle = element_text(hjust = 0,color = "darkblue" ),
        plot.caption = element_text(hjust = c(0,1),color = c("darkblue",ifelse(HLA_Slope>0,"darkgreen","red")) ))

###############################################################################################
PG1<-plot_grid(P1,P1_BIS,P1_TER,P2, labels = c("1","1'","1''","2"))
PG2<-plot_grid(P3,P3_BIS,P5,labels = c("3","3'","4"))
PG3<-plot_grid(P4,P4_BIS,P4_TER,labels = c("5","5'","5''"))

PG1
PG2
PG3
##########################################################################################################
#Final List
Final_Summary_List<-vector("list",15)

Final_Summary_List[[1]]<-paste("The mean period of a cycle is",round(Mean_Period_Seconds,digits = 1),"seconds")
Final_Summary_List[[2]]<-Print_Number_of_Cons_Min
Final_Summary_List[[3]]<-ST_If_else_Toxic
Final_Summary_List[[4]]<-length(RR_ST_Low_Real_Consecutive_Index)
Final_Summary_List[[5]]<-RR_ST_If_else_Toxic
Final_Summary_List[[6]]<-SI_If_Else_Evolution
Final_Summary_List[[7]]<-length(SI_ST_Low_Real_Consecutive_Index)
Final_Summary_List[[8]]<-SI_ST_If_else_Toxic
Final_Summary_List[[9]]<-SA_Func_Matrix
Final_Summary_List[[10]]<-SA_If_Else_Evolution
Final_Summary_List[[11]]<-Number_of_Cons_Compliance_low
Final_Summary_List[[12]]<-Compliance_If_else_Toxic
Final_Summary_List[[13]]<-Number_of_Cons_RR_Compliance_low
Final_Summary_List[[14]]<-RR_Compliance_If_else_Toxic
Final_Summary_List[[15]]<-HLA_Reg_Coeffs
Final_Summary_List[[16]]<-If_Else_HLA

Final_Summary_List_Titles<-c("Cycle Analysis Result",
                             paste("Number of Consecutive cycles w/ a Min of ST >",ST_Tox_Limit, "mN/m"),
                             paste("Conclusion on Toxicity based on the", ST_Tox_Limit," mN/m limit"),
                             paste("Number of Consecutive cycles w/ a low Relative Reduction of ST (<",round(RR_ST_Toxic,digits = 2),")"),
                             "Conclusion on Toxicity based on the low Relative Reduction of ST limit",
                             "Evolution of ST'Stablity Index by Cycle using the Linear Regression method",
                             "Number of Consecutive cycles w/ a low Stability Index (<1)",
                             "Conclusion on Toxicity based on the Stability Index Analysis",
                             "Linear Regression Coefficients of The Surface Area using the Squeeze Theorem",
                             "Conclusion on the Surface Area's evolution",
                             "Number of Consecutive cycles w/ a Max of 'pseudo'Compliance < 0.02",
                             "Conclusion on Toxicity based on the Max of 'pseudo'Compliance limit (<0.02)",
                             paste("Number of Consecutive cycles w/ a low Relative reduction of Compliance >",round(RR_Compliance_Toxic,digits=2)),
                             "Conclusion on Toxicity based on the low Relative Reduction of Compliance limit",
                             "Linear Regression Coefficients of the Hysteresis Loop Area by Cycle",
                             "Conclusion on toxicity based on the HLA evolution: Stable = SAFE / decreasing = TOXIC")

names(Final_Summary_List)<-Final_Summary_List_Titles

#Save the final report as a txt document,
#you'll have here to define the results folder path
#That folder would be where the plots and the final text would be stored
#It is properly named thanks to the compound name you have entered at the very beginning
#the result would be :2 files 
#1)"Compound + Number Summary.txt" <- with the text summary
#2)"Compound + Number Poly Graph.pdf <- with all the graphs

Setwd_Output<-choose.dir(default = "",
                         caption = "Choose a folder to put your results in")%>%
  setwd
capture.output(Final_Summary_List,file=paste(User_Input_File_Name,"Summay.txt"))

#To auto-save the poly-graph as a UNIQUE pdf file
pdf(file=paste(User_Input_File_Name,"Poly Graph",".pdf"), onefile=T,paper='a4r',
    width=17, height=23)
PG1
PG2
PG3
dev.off()

###############################################################################################################################################