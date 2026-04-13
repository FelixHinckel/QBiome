####################################################################
################## QBiome : Ajout des menus faune ##################
####################################################################



####################################################################
### 1 : Environnement de travail

# Charger les bibliothèques nécessaires
library(rstudioapi)
library(readr)
library(dplyr)
library(sf)
library(readxl)

# Définir le dossier de travail
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))


####################################################################
### 2 : Importer les données

Repasse <- read.csv("Data/Ajout/Repasse.csv")
Comportement <- read_excel("Data/Ajout/menus_faune.xlsx", sheet = "Comportement")
Stade <- read_excel("Data/Ajout/menus_faune.xlsx", sheet = "Stade de vie")

####################################################################
### 3 : Export des fichiers en GPKG et CSV

# Créer le chemin du fichier
chemin_fichier <- "Output/Referentiels.gpkg"

# Création du dossier si nécessaire
dir.create(dirname(chemin_fichier), recursive = TRUE)

# Écrire la table dans le fichier GPKG
st_write(obj = Repasse, dsn = chemin_fichier, layer = "Repasse", driver = "GPKG", delete_layer = TRUE)
st_write(obj = Comportement, dsn = chemin_fichier, layer = "Comportement", driver = "GPKG", delete_layer = TRUE)
st_write(obj = Stade, dsn = chemin_fichier, layer = "Stade", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(Repasse, "Output/Repasse.csv", row.names = FALSE)
write.csv2(Comportement, "Output/Comportement.csv", row.names = FALSE)
write.csv2(Stade, "Output/Stade.csv", row.names = FALSE)