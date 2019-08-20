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

# Lire le ficher CSV en format transaction
dataflotte_basket_nomodif = read.transactions(file="dataflotte_basket_nomodif.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
dataflotte_basket_V2 = read.transactions(file="dataflotte_basket_V2.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
dataflotte_basket_VV = read.transactions(file="dataflotte_basket_VV.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
dataflotte_basket_VVEHIC1 = read.transactions(file="dataflotte_basket_VVEHIC1.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
dataflotte_basket_VV_CamionConstruction = read.transactions(file="dataflotte_basket_VV_CamionConstruction.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
dataflotte_basket_sansprev = read.transactions(file="dataflotte_basket_sansprev.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)

# Rouler l'aglorithme apriori sur les donnÈes
apriori_nomodif <- apriori(dataflotte_basket_nomodif,parameter = list(support=0.0005, confidence=0.0005,minlen=2,target="rules"))
apriori_V2 <- apriori(dataflotte_basket_V2,parameter = list(support=0.0005, confidence=0.0005,minlen=2,target="rules"))
apriori_VV <- apriori(dataflotte_basket_VV,parameter = list(support=0.0005, confidence=0.0005,minlen=2,target="rules"))
apriori_VVEHIC1 <- apriori(dataflotte_basket_VVEHIC1,parameter = list(support=0.0005, confidence=0.0005,minlen=2,target="rules"))
apriori_VV_CamionConstruction <- apriori(dataflotte_basket_VV_CamionConstruction,parameter = list(support=0.0005, confidence=0.0005,minlen=2,target="rules"))
apriori_sansprev <- apriori(dataflotte_basket_sansprev,parameter = list(support=0.0005, confidence=0.0005,minlen=2,target="rules"))

# Visualisation des donnÈes
itemFrequencyPlot(dataflotte_basket_nomodif,topN = 6)
plot(apriori_nomodif, measure = c("support", "lift"), shading = "confidence")

itemFrequencyPlot(dataflotte_basket_V2,topN = 6)
plot(apriori_V2, measure = c("support", "lift"), shading = "confidence")

itemFrequencyPlot(dataflotte_basket_VV,topN = 6)
plot(apriori_VV, measure = c("support", "lift"), shading = "confidence")

itemFrequencyPlot(dataflotte_basket_VVEHIC1,topN = 6)
plot(apriori_VVEHIC1, measure = c("support", "lift"), shading = "confidence")

itemFrequencyPlot(dataflotte_basket_VVEHIC1,topN = 6)
plot(apriori_VV_CamionConstruction, measure = c("support", "lift"), shading = "confidence")

itemFrequencyPlot(dataflotte_basket_sansprev,topN = 6)
plot(apriori_sansprev, measure = c("support", "lift"), shading = "confidence")

# Mettre les rËgles en format data frame pour mieux les visualiser et les Ècrires dans un fichier XLSX
apriori_nomodif.df <- as(apriori_nomodif,'data.frame')
write.xlsx(apriori_nomodif.df, file = "regles_nomodif.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

apriori_V2.df <- as(apriori_V2,'data.frame')
write.xlsx(apriori_V2.df, file = "regles_V2.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

apriori_VV.df <- as(apriori_VV,'data.frame')
write.xlsx(apriori_VV.df, file = "regles_VV.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

apriori_VVEHIC1.df <- as(apriori_VVEHIC1,'data.frame')
write.xlsx(apriori_VVEHIC1.df, file = "regles_VVEHIC1.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

apriori_VV_CamionConstruction.df <- as(apriori_VV_CamionConstruction,'data.frame')
write.xlsx(apriori_VV_CamionConstruction.df, file = "regles_VV_CamionConstruction.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

apriori_sansprev.df <- as(apriori_sansprev,'data.frame')
write.xlsx(apriori_sansprev.df, file = "regles_sansprev.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)