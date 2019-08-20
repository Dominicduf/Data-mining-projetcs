setwd("D:/R projects/Projet IND6212") 

library(readr) # Pour lire les donn√©es
library(plyr) #Pour manipuler les donn√©es 
library(arules) # Pour les r√®gles d'association
library(DT) # Pour les tableaux interactifs 
library(tidyr) # Pour manipuler les donn√©es 
library(stringr) # Pour manipuler les donn√©es 
library(arulesViz) # Pour visualiser les r√®gles
library(plotly) # Pour visualiser les r√®gles
library(xlsx)

#Lire les fichiers CSV de la flotte
FicheVehicule <- read_csv("FicheVehicule.csv")
ListeActivites <- read_csv("ListeActivites.csv")

#Supprime les lignes avec NA dans la colonne TextCodePartObj
ListeActivites <- ListeActivites[complete.cases(ListeActivites[ , 5]),]

#Joindre les activitÈs avec le type de vÈhicule
data_flotte <- merge(x = ListeActivites, y = FicheVehicule, by = "Equipement", all = TRUE)

#Concatener l'activitÈ et l'entretien
data_flotte$ListeEntretien <- paste(data_flotte$TexteCodeActiv.,"-",data_flotte$TextCodePartObj)

# Permet d'avoir une BD avec seulement les code V.VEHIC1
data_flotte[["Titre_V.VEHIC1"]] <- grepl("V.VEHIC1", data_flotte[["Groupedecodes"]])
dataflotte_VVEHIC1 = subset(data_flotte, Titre_V.VEHIC1 == TRUE)

# Permet d'avoir une BD avec seulement les code V2
data_flotte[["Titre_V2"]] <- grepl("V2", data_flotte[["Groupedecodes"]])
dataflotte_V2 = subset(data_flotte, Titre_V2 == TRUE)

# Permet d'avoir une BD avec seulement les code VV
data_flotte[["Titre_VV"]] <- grepl("VV", data_flotte[["Groupedecodes"]])
dataflotte_VV = subset(data_flotte, Titre_VV == TRUE)

#Permet d'avoir une nouvelle BD sans avoir les entretiens prÈventif
data_flotte[["SansEntretienPrev"]] <- grepl("Entretien PrÈventif", data_flotte[["ListeEntretien"]])
dataflotte_sansprev = subset(data_flotte,SansEntretienPrev == FALSE)

# Garder seulement les camions de construction dans les codes VV
dataflotte_VV[["Titre_Camion_Construction"]] <- grepl("Camion Construction", dataflotte_VV[["Modex - CatÈgorie cy"]])
dataflotte_VV_CamionConstruction = subset(dataflotte_VV, Titre_Camion_Construction == TRUE)

# Permet de concatener les lignes ‡ la colonne TextCodePartObj qui ont le mÍme numÈro d'avis et de date de crÈation
dataflotte_basket_nomodif <- ddply(data_flotte,c("Avis","CrÈÈle"), function(df1)paste(df1$ListeEntretien, collapse = ","))
dataflotte_basket_VVEHIC1 <- ddply(dataflotte_VVEHIC1,c("Avis","CrÈÈle"), function(df1)paste(df1$ListeEntretien, collapse = ","))
dataflotte_basket_V2 <- ddply(dataflotte_V2,c("Avis","CrÈÈle"), function(df1)paste(df1$ListeEntretien, collapse = ","))
dataflotte_basket_VV <- ddply(dataflotte_VV,c("Avis","CrÈÈle"), function(df1)paste(df1$ListeEntretien, collapse = ","))
dataflotte_basket_VV_CamionConstruction <- ddply(dataflotte_VV_CamionConstruction,c("Avis","CrÈÈle"), function(df1)paste(df1$ListeEntretien, collapse = ","))
dataflotte_basket_sansprev <- ddply(dataflotte_sansprev,c("Avis","CrÈÈle"), function(df1)paste(df1$ListeEntretien, collapse = ","))

# Met la table en format transaction et Èlimine les colonnes inutiles
dataflotte_basket_nomodif$Entretien <- as.factor(dataflotte_basket_nomodif$V1)
dataflotte_basket_nomodif$CrÈÈle <- NULL
dataflotte_basket_nomodif$Avis <- NULL
dataflotte_basket_nomodif$V1 <- NULL

dataflotte_basket_VVEHIC1$Entretien <- as.factor(dataflotte_basket_VVEHIC1$V1)
dataflotte_basket_VVEHIC1$CrÈÈle <- NULL
dataflotte_basket_VVEHIC1$Avis <- NULL
dataflotte_basket_VVEHIC1$V1 <- NULL

dataflotte_basket_V2$Entretien <- as.factor(dataflotte_basket_V2$V1)
dataflotte_basket_V2$CrÈÈle <- NULL
dataflotte_basket_V2$Avis <- NULL
dataflotte_basket_V2$V1 <- NULL

dataflotte_basket_VV$Entretien <- as.factor(dataflotte_basket_VV$V1)
dataflotte_basket_VV$CrÈÈle <- NULL
dataflotte_basket_VV$Avis <- NULL
dataflotte_basket_VV$V1 <- NULL

dataflotte_basket_VV_CamionConstruction$Entretien <- as.factor(dataflotte_basket_VV_CamionConstruction$V1)
dataflotte_basket_VV_CamionConstruction$CrÈÈle <- NULL
dataflotte_basket_VV_CamionConstruction$Avis <- NULL
dataflotte_basket_VV_CamionConstruction$V1 <- NULL

dataflotte_basket_sansprev$Entretien <- as.factor(dataflotte_basket_sansprev$V1)
dataflotte_basket_sansprev$CrÈÈle <- NULL
dataflotte_basket_sansprev$Avis <- NULL
dataflotte_basket_sansprev$V1 <- NULL

#…crie un fichier CSV contenant la liste d'entretien
write.csv(dataflotte_basket_nomodif,"dataflotte_basket_nomodif.csv", quote = FALSE, row.names = TRUE)
write.csv(dataflotte_basket_VVEHIC1,"dataflotte_basket_VVEHIC1.csv", quote = FALSE, row.names = TRUE)
write.csv(dataflotte_basket_V2,"dataflotte_basket_V2.csv", quote = FALSE, row.names = TRUE)
write.csv(dataflotte_basket_VV,"dataflotte_basket_VV.csv", quote = FALSE, row.names = TRUE)
write.csv(dataflotte_basket_VV_CamionConstruction,"dataflotte_basket_VV_CamionConstruction.csv", quote = FALSE, row.names = TRUE)
write.csv(dataflotte_basket_sansprev,"dataflotte_basket_sansprev.csv", quote = FALSE, row.names = TRUE)

# Pour le rapport
dataflotte.df <- as(data_flotte,'data.frame')
write.csv(dataflotte.df,"dataflotte_test.csv", quote = FALSE, row.names = TRUE)



