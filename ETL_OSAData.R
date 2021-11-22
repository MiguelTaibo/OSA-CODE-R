########################################
# OSA Use Case
#
# Simple ETL process on a single Excel file
#

# Clear the working space
rm(list=ls())

Input_file <- "Info_BDApnea_QuironMalaga.xlsx"
Output_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "~/MLLB/DATA/"

library(readxl)

df_tmp <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_tmp = as.data.frame(df_tmp)

### Convert to factor or numerical the different attributes of the dataset
sapply(df_tmp, class)

df_tmp1 <- df_tmp %>%
  transform(Patient = as.factor(Patient)) %>%
  transform(Comentarios = as.factor(Comentarios)) %>%
  transform(Audios.tumbado = as.factor(Audios.tumbado)) %>%
  transform(Fotos = as.factor(Fotos)) %>%
  transform(Audio.fs.KHz = as.factor(Audio.fs.KHz)) %>%
  transform(Gender = as.factor(Gender)) %>%
  transform(IAH.Supino = as.numeric(IAH.Supino)) %>%
  transform(IAH.Lateral = as.numeric(IAH.Lateral)) %>%
  transform(Peso = as.numeric(Peso)) %>%
  transform(Fumador = as.factor(Fumador)) %>%
  transform(Roncador = as.factor(Roncador)) %>%
  transform(Enfermedades = as.factor(Enfermedades)) %>%
  transform(Sala.Ruidos = as.factor(Sala.Ruidos)) %>%
  transform(Imagen = as.factor(Imagen)) %>%
  transform(Dialecto = as.factor(Dialecto)) %>%
  transform(DIST.BARB.LOB = as.numeric(DIST.BARB.LOB)) %>%
  transform(Cansancio = as.factor(Cansancio)) %>%
  transform(Concentrarse = as.factor(Concentrarse)) %>%
  transform(PerdRespNoche = as.factor(PerdRespNoche)) %>%
  transform(HiperT = as.factor(HiperT))%>%
  transform(EstHOSP = as.factor(EstHOSP))


library(visdat)
vis_dat(df_tmp1)


### Remove the unwanted attributes
library(dplyr)
df_tmp2 <- df_tmp1 %>%
  select(Patient, Gender, Fumador, Roncador, IAH, Peso, Talla, Edad, PerCervical)
vis_dat(df_tmp2)
# BEFORE CONTINUING EXECUTING NEXT lines of code
#        BE SURE TO FIX the Excel removing comments in Peso

##### QUESTIONS: #############################################

### HOW MANY PATIENTS?
count(df_tmp2)
### HOW MANY MALE /FEMALE
count(df_tmp2 %>% filter (Gender=="hombre"))
count(df_tmp2 %>% filter (Gender=="mujer"))


library(naniar)
df_tmp3 <- df_tmp2 %>% replace_with_na(replace = list(
    Peso=-1, Talla=-1, Edad=-1, PerCervical=-1
  ) )
vis_dat(df_tmp3)
library(tidyr)
df_final <- df_tmp3 %>% drop_na()

##### QUESTIONS: #############################################
#####   AFTER CLEANING:
#####
#####       HOW MANY PATIENTS?
#####       HOW MANY MALE /FEMALE?
count(df_final) #637
count(df_final %>% filter (Gender=="hombre")) #455
count(df_final %>% filter (Gender=="mujer")) #182


df_final <- df_final %>% 
  rename(Weight = Peso, Height = Talla,
         Smoker = Fumador, Snorer = Roncador,
         Age = Edad, Cervical = PerCervical,
         AHI = IAH) 

## Boxplots for numerical attrs
par(mfrow=c(1,5))
boxplot(df_final$AHI, main="AHI")
boxplot(df_final$Weight, main="Weight")
boxplot(df_final$Height, main="Height")
boxplot(df_final$Age, main="Age")
boxplot(df_final$Cervical, main="Cervical")

## Recompute the body mass index
df_final <- df_final %>%
  mutate(BMI = Weight/(Height/100)^2, .after = Height)

## Recode factor levels
df_final$Gender <- df_final$Gender %>% 
  recode_factor(hombre = "male", mujer = "female")
df_final$Smoker <- df_final$Smoker %>% 
  recode_factor(antiguo = "old", poco = "yes", si = "yes", "si (poco)"="yes")
df_final$Snorer <- df_final$Snorer %>% 
  recode_factor("no con CPAD" = "no with CPAD", poco = "yes", si="yes", "si sin CPAP"="yes", "si´(protesis boca para dormir)"="yes")

## histograms for factorial attrs.
par(mfrow=c(1,3))
barplot(summary(df_final$Gender), main="Gender")
barplot(summary(df_final$Smoker), main="Smoker")
barplot(summary(df_final$Snorer), main="Snorer")

## NAs visualizarion
vis_dat(df_tmp3)
vis_dat(df_final)

### Classification Preparation
df_final <- df_final %>%
  mutate(OSA= ifelse(AHI <5, "Healthy", ifelse(AHI<15, "Mild", ifelse(AHI<30,"Moderate", "Severe")))) %>%
  transform(OSA = as.factor(OSA))

## TABLE 1
summary(df_final %>% filter(Gender=="male") %>% select(Gender, Weight, Height, BMI, Age, Cervical))
summary(df_final %>% filter(Gender=="female") %>% select(Gender, Weight, Height, BMI, Age, Cervical))

## TABLE 2
summary(df_final %>% filter(OSA=="Healthy"))$Gender

df_table2 <- df_final %>% group_by(OSA) %>% 
  summarise(males=sum(Gender=="male"), females = sum(Gender=="female"),
            mean_Weight=mean(Weight), std_Weight=sd(Weight), max_Weight=max(Weight), min_Weight=min(Weight),
            mean_Height=mean(Height), std_Height=sd(Height), max_Height=max(Height), min_Height=min(Height),
            mean_BMI=mean(BMI), std_BMI=sd(BMI), max_BMI=max(BMI), min_BMI=min(BMI),
            mean_Age=mean(Age), std_Age=sd(Age), max_Age=max(Age), min_Age=min(Age),
            mean_Cervical=mean(Cervical), std_Cervical=sd(Cervical), max_Cervical=max(Cervical), min_Cervical=min(Cervical))

to_table <- function(df, n_row, n_digits) {
  res <- paste("\begin{tabular}{c}", 
    "mean=",as.character(round(df[n_row,1],n_digits)),"\\",
    "std=",as.character(round(df[n_row,2],n_digits)),"\\",
    "min=",as.character(round(df[n_row,3],n_digits)), "\\",
    "max=",as.character(round(df[n_row,4],n_digits)),"\\","\ end{tabular}", sep="")
  return(res)
}

to_hole_table <- function(df, n_row,n_digits) {
  res <- paste(df %>% select(mean_Weight, std_Weight, max_Weight, min_Weight) %>% to_table(1,1), "&",
               df %>% select(mean_Height, std_Height, max_Height, min_Height) %>% to_table(1,1), "&",
               df %>% select(mean_BMI, std_BMI, max_BMI, min_BMI) %>% to_table(1,1), "&", 
               df %>% select(mean_Age, std_Age, max_Age, min_Age) %>% to_table(1,1), "&", 
               df %>% select(mean_Cervical, std_Cervical, max_Cervical, min_Cervical) %>% to_table(1,1), "\\")
  return(res)
}

to_hole_table(df_table2,1,1)

df %>% select(mean_Weight, std_Weight, max_Weight, min_Weight) %>% to_table(1,1)
df %>% select(mean_Height, std_Height, max_Height, min_Height) %>% to_table(1,1)
df %>% select(mean_BMI, std_BMI, max_BMI, min_BMI) %>% to_table(1,1)
df %>% select(mean_Age, std_Age, max_Age, min_Age) %>% to_table(1,1)
df %>% select(mean_Cervical, std_Cervical, max_Cervical, min_Cervical) %>% to_table(1,1)

## PLOT
library(ggplot2)
dens <- density(df_final$AHI)
df <- data.frame(x=dens$x, y=dens$y)
thresholds <- c(5,15,30)
df$quant <- factor(findInterval(df$x,thresholds))
ggplot(df, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant),) +
  scale_fill_brewer(labels=c("Healthy","Mild","Moderate","Severe")) + 
  xlim(0, 120) + theme(legend.position=c(0.8,0.6))
  


######### SAVING CLEAN DATA ##################################
# Write the clean data into Output_file
# you can install writexl package

library(writexl)
write_xlsx(df_final, paste(Data_Directory, Output_file, sep = ""))




