####################################################################
##### QFaune : Préparatioon de la liste d'espèce et de statuts #####
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

# Taxref
taxref <- read_delim("Data/taxref.txt", delim = "\t", na = "null", escape_double = FALSE, trim_ws = TRUE)

# BDC
bdc <- read_csv("Data/bdc_statuts.csv", na = "null")

# Table de correspondance Département/Région/Ancienne région
dep <- read_delim("Data/dep.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Données patch, non exploitables ou non contenues dans la BDC

# Espèces indicatrices de zone humide (article du 24 juin 2008)
ZH <- read.csv("Data/Ajout/ZH.csv", sep=";")

# LRN Nicheurs, Migrateurs et Hivernants (Pas de distinction dans la BDC)
LRNN <- read_delim("Data/Ajout/LRNN.csv", 
                   delim = ";", escape_double = FALSE, na = "null", trim_ws = TRUE)
LRNP <- read_delim("Data/Ajout/LRNP.csv",
                   delim = ";", escape_double = FALSE, na = "null", trim_ws = TRUE)
LRNH <- read_delim("Data/Ajout/LRNH.csv",
                   delim = ";", escape_double = FALSE, na = "null", trim_ws = TRUE)

# LRR PACA Nicheurs, Migrateurs et Hivernants (Pas de distinction dans la BDC)
LRR_PACA_Avifaune <- read_delim("Data/Ajout/LRR_PACA_Avifaune.csv", na = "null")

# LRR AURA Nicheurs, Migrateurs et Hivernants (Pas de distinction dans la BDC)
LRR_AURA_Avifaune <- read_delim("Data/Ajout/LRR_AURA_Avifaune.csv", delim = ";", escape_double = FALSE, na = "null", trim_ws = TRUE)


####################################################################
### 3 : Création de la liste d'espèce

## 3.1 : Filtre des bases

# Sélection Taxref
Liste <- taxref %>%
  filter(# Sélection des noms valides seulement
         CD_REF == CD_NOM, 
         # Sélection pour les versions spécifiques, à # si besoin
         #REGNE == "Plantae", #Flore
         #REGNE == "Animalia", #Faune
         #REGNE == "Fungi", #Fonge
         # Sélection des especes, sous espèces et variétés, mais aussi les familles et les genres
         RANG %in% c('FM','GN','ES','SSES','VAR','SVAR','FO'),
         # Présence en métropole
         FR %in% c('B','C','D','E','I','J','M','P','Q','S'))

# Sélection BDC
statuts_bdc <- bdc %>%
  filter(LB_TYPE_STATUT %in% c("Liste rouge nationale", "Directive Habitat", "Directive Oiseaux", "Protection nationale"),
         LB_ADM_TR %in% c("France métropolitaine","France"))

## 3.2 : Préparation des statuts
# Grouper les entités par CD_REF en récupérant l'information de CODE_STATUT et RQ_STATUT
# Pour les LRN, l'avifaune est considéré à part.

# Liste rouge nationale
LRN <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Liste rouge nationale") %>%
  subset(CLASSE != "Aves") %>%
  group_by(CD_REF) %>%
  summarise(LRN = paste0(CODE_STATUT, collapse = ", "), 
            LRN_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# LRN Nicheurs
LRNN <- LRNN %>%
  left_join(taxref, by = c("CD_REF" = "CD_NOM")) %>%
  mutate(CD_REF = ifelse(!is.na(CD_REF.y), CD_REF.y, CD_REF)) %>%
  select(-CD_REF.y) %>%
  subset(RANG.x != "POP") %>%
  group_by(CD_REF) %>%
  summarise(
    LRN_N = paste0(STATUT, collapse = ", "),
    LRN_N_det = paste0(CRITERES, collapse = ", ")) 

# LRN Hivernants
LRNH <- LRNH %>%
  left_join(taxref, by = c("CD_REF" = "CD_NOM")) %>%
  mutate(CD_REF = ifelse(!is.na(CD_REF.y), CD_REF.y, CD_REF)) %>%
  select(-CD_REF.y) %>%
  subset(RANG.x != "POP") %>%
  group_by(CD_REF) %>%
  summarise(
    LRN_H = paste0(STATUT, collapse = ", "),
    LRN_H_det = paste0(CRITERES, collapse = ", "))

# LRN Passage
LRNP <- LRNP %>%
  left_join(taxref, by = c("CD_REF" = "CD_NOM")) %>%
  mutate(CD_REF = ifelse(!is.na(CD_REF.y), CD_REF.y, CD_REF)) %>%
  select(-CD_REF.y) %>%
  subset(RANG.x != "POP") %>%
  group_by(CD_REF) %>%
  summarise(
    LRN_P = paste0(STATUT, collapse = ", "),
    LRN_P_det = paste0(CRITERES, collapse = ", "))

# Directive Habitat
DH <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Directive Habitat") %>%
  group_by(CD_REF) %>%
  summarise(DH = paste0(unique(CODE_STATUT), collapse = ", "), 
            DH_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# Directive Oiseaux
DO <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Directive Oiseaux") %>%
  group_by(CD_REF) %>%
  summarise(DO = paste0(unique(CODE_STATUT), collapse = ", "),
            DO_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# Protection nationale
PN <- statuts_bdc %>%
  filter(LB_TYPE_STATUT == "Protection nationale") %>%
  group_by(CD_REF) %>%
  summarise(PN = paste0(CODE_STATUT, collapse = ", "), 
            PN_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# Espèces Humides
ZH <- ZH %>%
  left_join(taxref, by = c("CD_REF" = "CD_NOM")) %>%
  mutate(CD_REF = ifelse(!is.na(CD_REF.y), CD_REF.y, CD_REF)) %>%
  select(-CD_REF.y) %>%
  group_by(CD_REF) %>%
  summarise(
    ZH = 'Oui') 

## 3.3 : Finalisation de la Liste 

# Fusionner la Liste avec les statuts nationaux
Liste <- Liste %>%
  left_join(LRN, by = "CD_REF") %>%
  left_join(LRNN, by = "CD_REF") %>%
  left_join(LRNP, by = "CD_REF") %>%
  left_join(LRNH, by = "CD_REF") %>%
  left_join(DH, by = "CD_REF") %>%
  left_join(DO, by = "CD_REF") %>%
  left_join(PN, by = "CD_REF") %>%
  left_join(ZH, by = "CD_REF")

# Gestion des valeurs nulles
Liste <- Liste %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))

# Déclinaison des groupes taxonomiques
Liste <- Liste %>%
  mutate(
    Groupe = case_when(
      REGNE == "Plantae" ~ "Flore",
      ORDRE == "Orthoptera" ~ "Orthoptères",
      CLASSE == "Mammalia" & ORDRE != "Chiroptera" & (HABITAT == 3 | HABITAT == 8) ~ "Mammifères",
      ORDRE == "Coleoptera" ~ "Coléoptères",
      ORDRE == "Odonata" ~ "Odonates",
      CLASSE == "Amphibia" ~ "Amphibiens",
      CLASSE == "Aves" ~ "Avifaune",
      GROUP2_INPN == "Reptiles" ~ "Reptiles",
      ORDRE == "Lepidoptera" ~ "Lépidoptères",
      ORDRE == "Chiroptera" ~ "Chiroptères",
      REGNE == "Fungi" ~ "Fonge",
      GROUP2_INPN == "Poissons" ~ "Poissons",
      GROUP1_INPN == "Mollusques" ~ "Mollusques",
      GROUP2_INPN == "Crustacés" ~ "Crustacés",
      GROUP3_INPN == "Araignées" ~ "Araignées",
      ORDRE == "Ephemeroptera" ~ "Ephémères",
      TRUE ~ NA_character_ # Mettre NA si aucune des conditions n'est remplie
    )
  )

# Ajout du champs de recherche dans QGis
Liste$Nom <- ifelse(
  is.na(Liste$Groupe) | Liste$Groupe == 'Flore',
  Liste$LB_NOM,
  paste0(
    ifelse(
      is.na(Liste$NOM_VERN), 
      " -", 
      gsub("\\s*\\(.*\\)", "", sub(",.*", "", Liste$NOM_VERN))
    ),
    " (",
    Liste$LB_NOM,
    ")"
  )
)

# Réorganiser les colonnes dans l'ordre et factoriser les colonnes pour gagner en place
Liste <- Liste %>%
  select(Groupe, Nom, CD_REF, LB_NOM, NOM_COMPLET, NOM_VERN, CLASSE, ORDRE, FAMILLE, PN, PN_det,
         LRN, LRN_det, LRN_N, LRN_N_det, LRN_H, LRN_H_det, LRN_P, LRN_P_det,DH, DH_det,DO, DO_det, ZH) %>%
  filter(Groupe != "") %>%
  mutate(
    Groupe = as.factor(Groupe),
    CLASSE = as.factor(CLASSE),
    ORDRE = as.factor(ORDRE),
    FAMILLE = as.factor(FAMILLE),
    PN = as.factor(PN),
    PN_det = as.factor(PN_det),
    DH = as.factor(DH),
    DH_det = as.factor(DH_det),
    DO = as.factor(DO),
    DO_det = as.factor(DO_det),
    LRN = as.factor(LRN),
    LRN_det = as.factor(LRN_det),
    LRN_N = as.factor(LRN_N),
    LRN_N_det = as.factor(LRN_N_det),
    LRN_H = as.factor(LRN_H),
    LRN_H_det = as.factor(LRN_H_det),
    LRN_P = as.factor(LRN_P),
    LRN_P_det = as.factor(LRN_P_det),
    ZH = as.factor(ZH)
  )%>%
  arrange(Groupe,LB_NOM)

## 3.4 : Export des fichiers en GPKG et CSV

# Créer le chemin du fichier
chemin_fichier <- "Output/Statuts.gpkg"

# Création du dossier si nécessaire
dir.create(dirname(chemin_fichier), recursive = TRUE)

# Écrire la table dans le fichier GPKG
st_write(obj = Liste, dsn = chemin_fichier, layer = "Liste", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(Liste, "Output/Liste.csv", row.names = FALSE)



####################################################################
### 4 : Statuts régionaux

## 4.1 : Filtre des bases

# Statuts régionaux
statuts_bdc <- bdc %>%
  filter(# Sélection pour les versions spécifiques, à # si besoin
    #REGNE == "Plantae", #Flore
    #REGNE == "Animalia", #Faune
    #REGNE == "Fungi", #Fonge
    LB_TYPE_STATUT %in% c("Liste rouge régionale", "Protection régionale"))

# Sélection des protections départementales pour un traitement séparé
PD_bdc <- bdc %>%
  filter(LB_TYPE_STATUT == "Protection départementale") %>%
  # Jonction des noms de région
  merge(dep, by.x = "LB_ADM_TR", by.y = "dep", all.x = TRUE)

# Sélection des déterminances ZNIEFF pour un traitement séparé
ZNIEFF_bdc <- bdc %>%
  filter(LB_TYPE_STATUT == "ZNIEFF Déterminantes")

## 4.2 Initialisation de la boucle régionale

# Initialiser un data frame vide pour stocker les résultats
Region <- data.frame()

# Boucle à travers chaque ancienne région
for (i in unique(dep$anc_reg)) {
  
  # Sélectionner la région et les départements correspondants
  r <- dep[dep$anc_reg == i,]$reg[1]
  departements <- dep[dep$anc_reg == i,]$dep
  
  ## 4.3 Statuts par région
  
  # Filtrer les statuts pour la région spécifique
  statuts_bdc_i <- statuts_bdc %>%
    filter(LB_ADM_TR %in% c(i, r))
  
  # Filtrer les protections départementales pour la région spécifique
  PD_i <- PD_bdc %>%
    filter(anc_reg == i)
  
  # Filtrer les déterminances ZNIEFF pour la région spécifique
  ZNIEFF_i <- ZNIEFF_bdc %>%
    filter(LB_ADM_TR %in% c(i, r, departements))
  
  # Protection régionale
  PR <- statuts_bdc_i %>%
    filter(LB_TYPE_STATUT == "Protection régionale") %>%
    group_by(CD_REF) %>%
    summarise(PR = paste0(CODE_STATUT, collapse = ", "), 
              PR_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))
  
  # Liste rouge régionale
  LRR <- statuts_bdc_i %>%
    filter(LB_TYPE_STATUT == "Liste rouge régionale") %>%
    subset(CLASSE != "Aves") %>%
    group_by(CD_REF) %>%
    summarise(LRR = paste0(CODE_STATUT, collapse = ", "), 
              LRR_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))
  
  ## LRR Oiseaux : Patch pour AURA et PACA, qui distinguent les Hiv et les Mig
  if (i == "Provence-Alpes-Côte-d'Azur") {
    LRRO <- LRR_PACA_Avifaune %>%
      mutate(CD_NOM = as.double(CD_NOM)) %>%
      left_join(taxref %>% select(CD_REF, CD_NOM), by = "CD_NOM") %>%
      select(-CD_NOM) 
  } else if (r == "Auvergne-Rhône-Alpes") {
    LRRO <- LRR_AURA_Avifaune %>%
      left_join(taxref %>% select(CD_REF, CD_NOM), by = "CD_NOM") %>%
      select(-CD_NOM) 
  } else {
    LRRO <- subset(statuts_bdc_i,LB_TYPE_STATUT == "Liste rouge régionale")%>%
      group_by(CD_REF) %>%
      summarise(LRR_N = paste0(CODE_STATUT, collapse = ", "), 
                LRR_N_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))
    LRRO$LRR_H <- ''
    LRRO$LRR_H_det <- ''
    LRRO$LRR_P <- ''
    LRRO$LRR_P_det <- ''
  }
  
  # Protection départementale
  PD <- PD_i %>%
    #Pour chaque département, récupérer CODE_STATUT et RQ_STATUT par CD_REF
    group_by(LB_ADM_TR, CD_REF) %>%
    summarise(
      Prot = paste0(LB_ADM_TR, " : ", paste0(CODE_STATUT, collapse = " , ")),
      Prot_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "),
      .groups = "drop" ) %>%
    # Concatener sur une région les différents PD pour chaque espèce
    group_by(CD_REF) %>%
    summarise(
      PD = paste0(Prot, collapse = ", "),
      PD_det = paste0(Prot_det[!is.na(Prot_det) & Prot_det != ""], collapse = ", "))
  
  # Déterminance ZNIEFF
  ZNIEFF <- ZNIEFF_i %>%
    group_by(CD_REF) %>%
    summarise(
      ZNIEFF = "Oui", 
      ZNIEFF_det = paste0(unique(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""]), collapse = " ; "))
  
  ## 4.4 Finalisation de la liste régionale
  
  # Fusionner les tables PR, LRR, PD et ZNIEFF
  merged <- PR %>%
    full_join(LRR, by = "CD_REF") %>%
    full_join(LRRO, by = "CD_REF") %>%
    full_join(PD, by = "CD_REF") %>%
    full_join(ZNIEFF, by = "CD_REF")
  
  # Ajouter la colonne pour l'ancienne région
  merged$REGION <- i
  
  # Ajouter cette table régionale à la table globale
  Region <- bind_rows(Region, merged)
}

## 4.5 Finalisation de la table globale

# Réorganisation et factorisation des colonnes
Region <- Region %>%
  select(REGION, CD_REF, PR, PR_det, LRR, LRR_det, LRR_N, LRR_N_det, LRR_H, LRR_H_det, LRR_P, LRR_P_det, PD, PD_det, ZNIEFF, ZNIEFF_det) %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .))) %>%
  mutate(across(everything(), as.factor)) %>%
  arrange(REGION, CD_REF)

## 4.6 : Export des fichiers en GPKG et CSV

# Écrire la table dans le fichier GPKG
st_write(obj = Region, dsn = chemin_fichier, layer = "Region", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(Region, "Output/Region.csv", row.names = FALSE)


####################################################################
### 5 : Nomenclature des textes

Textes <- bdc %>%
  filter(LB_TYPE_STATUT %in% c("Protection nationale", "Protection régionale", "Protection départementale", "Directive Oiseaux", "Directive Habitat"))%>%
  filter(LB_ADM_TR %in% c("France", "France métropolitaine", "Alsace", "Franche-Comté", "Poitou-Charentes", "Nord-Pas-de-Calais", "Auvergne-Rhône-Alpes", "Auvergne", "Midi-Pyrénées", "Languedoc-Roussillon", "Provence-Alpes-Côte-d'Azur", "Bourgogne", "Corse", "Limousin", "Pays-de-la-Loire", "Ile-de-France", "Centre", "Picardie", "Bretagne", "Rhône-Alpes", "Occitanie", "Aquitaine", "Normandie", "Grand Est", "Lorraine", "Champagne-Ardenne", "Haute-Normandie", "Hauts-de-France", "Basse-Normandie", "France", "Alpes-de-Haute-Provence", "Eure-et-Loir", "Lozère", "Cher", "Haute-Garonne", "Gers", "Haute-Savoie", "Lot", "Tarn-et-Garonne", "Moselle", "Ardennes", "Isère", "Meurthe-et-Moselle", "Jura", "Manche", "Loir-et-Cher", "Loire-Atlantique", "Orne", "Ille-et-Vilaine", "Loire", "Drôme", "Dordogne", "Finistère", "Alpes-Maritimes", "Landes", "Pyrénées-Atlantiques", "Ariège", "Aveyron", "Creuse", "Hautes-Pyrénées", "Haute-Vienne", "Gironde", "Mayenne", "Lot-et-Garonne", "Calvados", "Ain", "Tarn", "Loiret", "Marne", "Hautes-Alpes", "Aube", "Meuse", "Vaucluse", "Aisne", "Somme", "Corse-du-Sud", "Haute-Corse", "Yonne", "Nièvre", "Saône-et-Loire", "Côte-d'Or", "Pyrénées-Orientales", "Gard", "Vienne", "Hérault", "Charente", "Corrèze", "Allier", "Savoie", "Haute-Loire", "Aude", "Oise", "Indre-et-Loire", "Deux-Sèvres", "Cantal", "Indre", "Maine-et-Loire", "Sarthe", "Puy-de-Dôme", "Seine-Maritime", "Ardèche", "Rhône", "Seine-Saint-Denis", "Val-d'Oise", "Yvelines", "Val-de-Marne", "Essonne", "Eure", "Pas-de-Calais", "Paris", "Seine-et-Marne", "Hauts-de-Seine", "Territoire de Belfort", "Haute-Saône", "Charente-Maritime", "Vendée", "Doubs", "Morbihan", "Côtes-d'Armor", "Vosges", "Var", "Bourgogne-Franche-Comté", "Bouches-du-Rhône", "Nouvelle-Aquitaine", "Haute-Marne", "Bas-Rhin", "Haut-Rhin")) %>%
  distinct(LB_TYPE_STATUT, CODE_STATUT, LABEL_STATUT) %>%
  arrange(LB_TYPE_STATUT, CODE_STATUT)

# Écrire la table dans le fichier GPKG
st_write(obj = Textes, dsn = chemin_fichier, layer = "Textes", driver = "GPKG", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
write.csv2(Textes, "Output/Textes.csv", row.names = FALSE)
