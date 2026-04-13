####################################################################
########### QBiome : Préparation de la liste d'habitats ############
####################################################################



####################################################################
### 1 : Environnement de travail

# Charger les bibliothèques nécessaires
library(rstudioapi)
library(readr)
library(dplyr)
library(sf)

# Définir le dossier de travail
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

####################################################################
### 2 : Importer les données

# Habref
HABREF <- read.csv("Data/Habref/habref.csv", sep=";")
HABREF_TERR <- read.csv("Data/Habref/habref_terr.csv", sep=";")

####################################################################
### 3 : Sélection des Typologies

hab <- HABREF %>%
  filter(
    CD_TYPO %in% c(7,8,22,18,28))

####################################################################
### 4 : Sélection par territoire

HABFR <- HABREF_TERR %>%
  filter(
    CD_SIG_TERR =="TERFXFR")

hab <- hab %>%
  left_join(HABFR, by = c("CD_HAB" = "CD_HAB"))

hab <- hab %>%
  filter(
    CD_STATUT_PRESENCE =="P")

####################################################################
### 5 : Création de Code + Libellé

hab <- hab %>%
  mutate(CD_LB = trimws(paste(LB_CODE, LB_HAB_FR)),
         HAB = as.factor(CD_HAB),
         TYPO = as.factor(CD_TYPO),
         CD_LB = as.factor(CD_LB),
         CD = as.factor(LB_CODE),
         LB = as.factor(LB_HAB_FR),
         NIVEAU = as.factor(NIVEAU))

####################################################################
### 6 : Sélection et rearrangement des champs

hab <- hab %>%
  select(HAB,TYPO,CD_LB,CD,LB,NIVEAU) %>%
  arrange(TYPO,CD_LB)

####################################################################
### 7 : Nettoyage des noms vides

hab <- hab[!is.na(hab$LB) & hab$LB != "", ]

####################################################################
### 8 : Export

# Créer le chemin du fichier
chemin_fichier <- "Output/Referentiels.gpkg"

# Création du dossier si nécessaire
dir.create(dirname(chemin_fichier), recursive = TRUE)

# Écrire la table dans le fichier GPKG
st_write(obj = hab, dsn = chemin_fichier, layer = "Habitat", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(hab, "Output/Habitat.csv", row.names = FALSE)
