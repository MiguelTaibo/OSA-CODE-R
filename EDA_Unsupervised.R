library(factoextra)
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(Rtsne)

rm(list=ls())

Input_file <- "OSA_DB_UPM.xlsx"
Data_Directory <- "~/MLLB/DATA/"
df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

summary(df_OSA)
df_OSA <- df_OSA %>%
  transform(Patient = as.factor(Patient)) %>%
  transform(Gender = as.factor(Gender)) %>%
  transform(Smoker = as.factor(Smoker)) %>%
  transform(Snorer = as.factor(Snorer)) %>%
  transform(OSA = as.factor(OSA))

df_OSA <- df_OSA %>%
  transform(Patient = as.numeric(Patient)) %>%
  transform(Gender = as.numeric(Gender)) %>%
  transform(Smoker = as.numeric(Smoker)) %>%
  transform(Snorer = as.numeric(Snorer))
summary(df_OSA)

df_OSA_rm <- df_OSA %>% filter(OSA %in% c("Healthy", "Severe"))
df_OSA_rm$OSA <- droplevels(df_OSA_rm$OSA)
summary(df_OSA_rm)

################################################################
########################## PCA #################################
################################################################

OSA.pca <- df_OSA %>% 
  select(Gender, Smoker, Snorer, Weight, Height, BMI, Age, Cervical) %>%
  prcomp(scale=TRUE) 
OSA_rm.pca <- df_OSA_rm %>% select(Gender, Smoker, Snorer, Weight, Height, BMI, Age, Cervical) %>%
  prcomp(scale=TRUE)

fviz_eig(OSA.pca,addlabels = TRUE, ylim = c(0, 40))+labs(title="", x="PC", y="Eigenvalue")
fviz_eig(OSA_rm.pca,addlabels = TRUE, ylim = c(0, 40))+labs(title="", x="PC", y="Eigenvalue")

fviz_pca_var(OSA.pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) + 
  labs(title="", x="PC1", y="PC2")
fviz_pca_var(OSA_rm.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+labs(title="", x="PC1", y="PC2")


p1 <- fviz_contrib(OSA.pca, choice = "var", axes = 1)+labs(title="PC1")
p2 <- fviz_contrib(OSA.pca, choice = "var", axes = 2)+labs(title="PC2")
p3 <- fviz_contrib(OSA.pca, choice = "var", axes = 3)+labs(title="PC3")
p4 <- fviz_contrib(OSA.pca, choice = "var", axes = 4)+labs(title="PC4")
p5 <- fviz_contrib(OSA.pca, choice = "var", axes = 5)+labs(title="PC5")
p6 <- fviz_contrib(OSA.pca, choice = "var", axes = 6)+labs(title="PC6")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2)

fviz_pca_ind( OSA.pca, geom="point", 
              habillage = df_OSA$OSA,
              addEllipses=TRUE, 
              ellipse.level=0.7) + 
  labs(title="", x="PC1", y="PC2")


##################################################
###################### tSNE ######################
##################################################
df_OSA_temp <- df_OSA %>% 
  select(-Patient) %>% select(-AHI) %>%
  mutate(ID=row_number())


tSNE_fit <- df_OSA_temp %>%
  select(where(is.numeric)) %>%
  column_to_rownames("ID") %>%
  scale() %>% 
  Rtsne(check_duplicates=FALSE)

tSNE_df <- tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(ID=row_number()) %>%
  inner_join(df_OSA_temp %>% select(OSA, ID), by="ID") %>%
  select(tSNE1, tSNE2, OSA)

tSNE_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = OSA))+
  geom_point()+
  theme(legend.position="bottom")


df_OSA_rm_temp <- df_OSA_rm %>% 
  select(-Patient) %>% select(-AHI) %>%
  mutate(ID=row_number())

  
tSNE_fit <- df_OSA_rm_temp %>%
  select(where(is.numeric)) %>%
  column_to_rownames("ID") %>%
  scale() %>% 
  Rtsne(check_duplicates=FALSE)

tSNE_df <- tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(ID=row_number()) %>%
  inner_join(df_OSA_rm_temp %>% select(OSA, ID), by="ID") %>%
  select(tSNE1, tSNE2, OSA)

tSNE_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = OSA))+
  geom_point()+
  theme(legend.position="bottom")


df_OSA_PCA <- df_OSA_rm %>% 
  mutate(PC1=Gender*OSA.pca$rotation[1,1]+
           Smoker*OSA.pca$rotation[2,1]+
           Snorer*OSA.pca$rotation[3,1]+
           Weight*OSA.pca$rotation[4,1]+
           Height*OSA.pca$rotation[5,1]+
           BMI*OSA.pca$rotation[6,1]+
           Age*OSA.pca$rotation[7,1]+
           Cervical*OSA.pca$rotation[8,1]) %>%
  mutate(PC2=Gender*OSA.pca$rotation[1,2]+
           Smoker*OSA.pca$rotation[2,2]+
           Snorer*OSA.pca$rotation[3,2]+
           Weight*OSA.pca$rotation[4,2]+
           Height*OSA.pca$rotation[5,2]+
           BMI*OSA.pca$rotation[6,2]+
           Age*OSA.pca$rotation[7,2]+
           Cervical*OSA.pca$rotation[8,2]) %>%
  mutate(PC3=Gender*OSA.pca$rotation[1,3]+
           Smoker*OSA.pca$rotation[2,3]+
           Snorer*OSA.pca$rotation[3,3]+
           Weight*OSA.pca$rotation[4,3]+
           Height*OSA.pca$rotation[5,3]+
           BMI*OSA.pca$rotation[6,3]+
           Age*OSA.pca$rotation[7,3]+
           Cervical*OSA.pca$rotation[8,3]) %>%
  mutate(PC4=Gender*OSA.pca$rotation[1,4]+
           Smoker*OSA.pca$rotation[2,4]+
           Snorer*OSA.pca$rotation[3,4]+
           Weight*OSA.pca$rotation[4,4]+
           Height*OSA.pca$rotation[5,4]+
           BMI*OSA.pca$rotation[6,4]+
           Age*OSA.pca$rotation[7,4]+
           Cervical*OSA.pca$rotation[8,4]) %>%
  mutate(PC5=Gender*OSA.pca$rotation[1,5]+
           Smoker*OSA.pca$rotation[2,5]+
           Snorer*OSA.pca$rotation[3,5]+
           Weight*OSA.pca$rotation[4,5]+
           Height*OSA.pca$rotation[5,5]+
           BMI*OSA.pca$rotation[6,5]+
           Age*OSA.pca$rotation[7,5]+
           Cervical*OSA.pca$rotation[8,5]) %>%
  mutate(PC6=Gender*OSA.pca$rotation[1,6]+
           Smoker*OSA.pca$rotation[2,6]+
           Snorer*OSA.pca$rotation[3,6]+
           Weight*OSA.pca$rotation[4,6]+
           Height*OSA.pca$rotation[5,6]+
           BMI*OSA.pca$rotation[6,6]+
           Age*OSA.pca$rotation[7,6]+
           Cervical*OSA.pca$rotation[8,6]) %>%
  select(PC1, PC2, PC3, PC4, PC5, PC6, OSA) %>% 
  mutate(ID=row_number())

tSNE_fit <- df_OSA_PCA %>%
  select(where(is.numeric)) %>%
  column_to_rownames("ID") %>%
  scale() %>% 
  Rtsne(check_duplicates=FALSE)

tSNE_df <- tSNE_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(ID=row_number()) %>%
  inner_join(df_OSA_PCA %>% select(OSA, ID), by="ID") %>%
  select(tSNE1, tSNE2, OSA)

tSNE_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = OSA))+
  geom_point()+
  theme(legend.position="bottom")

