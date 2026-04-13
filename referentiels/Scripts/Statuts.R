####################################################################
##### QBiome : Préparation de la liste d'espèces et de statuts #####
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

#Définir le projet qui doit être exporté (# les autres lignes)
REGNES_QBIOME <- c("Plantae", "Animalia", "Fungi") #QBiome
#REGNES_QBIOME <- c("Plantae") #QFlore
#REGNES_QBIOME <- c("Animalia") #QFaune
#REGNES_QBIOME <- c("Fungi") #QFonge


####################################################################
### 2 : Importer les données

NA_STRINGS <- c("null", "NULL", "NA", "N/A", "")
ENC <- "UTF-8"

# Taxref (TSV)
taxref <- readr::read_tsv(
  "Data/Taxref/taxref.txt",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)

# Création d'une table de transition CD_NOM vers CD_REF
taxref_map <- taxref %>%
  dplyr::select(CD_NOM, CD_REF)

# BDC (CSV)
bdc <- readr::read_csv(
  "Data/BDC/bdc_statuts.csv",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE
)

# Table de correspondance Département/Région/Ancienne région (CSV ; )
dep <- readr::read_delim(
  "Data/dep.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)

## Données patch, non exploitables ou non contenues dans la BDC

# Espèces indicatrices de zone humide (article du 24 juin 2008)
ZH <- readr::read_delim(
  "Data/Ajout/ZH.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)

# LRN Nicheurs, Migrateurs et Hivernants (Pas de distinction dans la BDC)
LRNN <- readr::read_delim(
  "Data/Ajout/LRNN.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)
LRNP <- readr::read_delim(
  "Data/Ajout/LRNP.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)
LRNH <- readr::read_delim(
  "Data/Ajout/LRNH.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)

# LRR PACA Nicheurs, Migrateurs et Hivernants (Pas de distinction dans la BDC)
LRR_PACA_Avifaune <- readr::read_delim(
  "Data/Ajout/LRR_PACA_Avifaune.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)

# LRR AURA Nicheurs, Migrateurs et Hivernants (Pas de distinction dans la BDC)
LRR_AURA_Avifaune <- readr::read_delim(
  "Data/Ajout/LRR_AURA_Avifaune.csv",
  delim = ";",
  na = NA_STRINGS,
  locale = readr::locale(encoding = ENC),
  show_col_types = FALSE,
  progress = FALSE,
  trim_ws = TRUE
)

####################################################################
### 3 : Création de la liste d'espèce

## 3.1 : Filtre des bases

# Sélection Taxref

RANGS_GARDES  <- c("FM","GN","ES","SSES","VAR","SVAR","FO")
FR_OK         <- c("B","C","D","E","I","J","M","P","Q","S")

Liste <- taxref %>%
  filter(
    CD_REF == CD_NOM, # conservation des noms valides seulement
    REGNE %in% REGNES_QBIOME, #tri des règnes, en fonction des versions
    RANG %in% RANGS_GARDES, # conservation des rangs depuis la famille jusqu'à la sous variété et la forme
    FR %in% FR_OK # conservation des espèces présentes en métropoles seulement
  )

# Sélection BDC

STATUTS_NATIONAUX <- c(
  "Liste rouge nationale",
  "Directive Habitat",
  "Directive Oiseaux",
  "Protection nationale"
)

TERRITOIRES_NATIONAUX <- c(
  "France métropolitaine",
  "France"
)

statuts_bdc <- bdc %>%
  filter(
    REGNE %in% REGNES_QBIOME,
    LB_TYPE_STATUT %in% STATUTS_NATIONAUX,
    LB_ADM_TR %in% TERRITOIRES_NATIONAUX
  )

## 3.2 : Préparation des statuts
# Grouper les entités par CD_REF en récupérant l'information de CODE_STATUT et RQ_STATUT
# Pour les LRN, l'avifaune est considéré à part.

# Liste rouge nationale
LRN <- statuts_bdc %>%
  filter(
    LB_TYPE_STATUT == "Liste rouge nationale",
    is.na(CLASSE) | CLASSE != "Aves"
  ) %>%
  group_by(CD_REF) %>%
  summarise(
    LRN = paste(CODE_STATUT, collapse = ", "),
    LRN_det = paste(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "),
    .groups = "drop"
  )

# Liste rouge nationale Avifaune
prep_lrn_avifaune <- function(df, out_statut, out_det) { #Fontion d'intégration des LRN avifaune
  df %>%
    dplyr::left_join(taxref_map, by = c("CD_REF" = "CD_NOM")) %>%
    dplyr::mutate(
      CD_REF = dplyr::coalesce(CD_REF.y, CD_REF)  # S'assurer de la validité des noms
    ) %>%
    dplyr::select(-CD_REF.y) %>%
    dplyr::filter(RANG != "POP") %>%            
    dplyr::group_by(CD_REF) %>%
    dplyr::summarise( #Récupérer le code LRN pour chaque CD_REF
      "{out_statut}" := paste0(STATUT, collapse = ", "),
      "{out_det}"    := paste0(CRITERES, collapse = ", "),
      .groups = "drop"
    )
}
LRNN <- prep_lrn_avifaune(LRNN, "LRN_N", "LRN_N_det")
LRNH <- prep_lrn_avifaune(LRNH, "LRN_H", "LRN_H_det")
LRNP <- prep_lrn_avifaune(LRNP, "LRN_P", "LRN_P_det")

LRN <- bind_rows( # Intégration de la LRN Nicheur comme LRN par défaut
  LRN %>% transmute(CD_REF, LRN = LRN, LRN_det = LRN_det),
  LRNN %>% transmute(CD_REF, LRN = LRN_N, LRN_det = LRN_N_det)
)

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
  summarise(PN = paste0(unique(CODE_STATUT), collapse = ", "), 
            PN_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))

# Espèces Humides
ZH <- ZH %>%
  left_join(taxref_map, by = c("CD_REF" = "CD_NOM")) %>%
  mutate(CD_REF = dplyr::coalesce(CD_REF.y, CD_REF)) %>%
  select(-CD_REF.y) %>%
  group_by(CD_REF) %>%
  summarise(ZH = "Oui", .groups = "drop")

## 3.3 : Finalisation de la Liste 

# Fusionner la Liste avec les statuts nationaux
Liste <- Liste %>%
  left_join(LRN, by = "CD_REF") %>%
  left_join(LRNP, by = "CD_REF") %>%
  left_join(LRNH, by = "CD_REF") %>%
  left_join(DH, by = "CD_REF") %>%
  left_join(DO, by = "CD_REF") %>%
  left_join(PN, by = "CD_REF") %>%
  left_join(ZH, by = "CD_REF")

# Gestion des valeurs nulles
Liste <- Liste %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), "")))

# Déclinaison des groupes taxonomiques
Liste <- Liste %>%
  mutate(
    Groupe = case_when(
      REGNE == "Fungi" ~ "Fonge",
      REGNE == "Plantae" ~ "Flore",
      ORDRE == "Chiroptera" ~ "Chiroptères",
      CLASSE == "Mammalia" & ORDRE != "Chiroptera" & (HABITAT == 3 | HABITAT == 8) ~ "Mammifères",
      CLASSE == "Aves" ~ "Avifaune",
      CLASSE == "Amphibia" ~ "Amphibiens",
      CLASSE == "Gastropoda" ~ "Gastéropodes",
      GROUP2_INPN == "Poissons" ~ "Poissons",
      GROUP2_INPN == "Crustacés" ~ "Crustacés",
      GROUP3_INPN == "Araignées" ~ "Araignées",
      GROUP2_INPN == "Reptiles" ~ "Reptiles",
      ORDRE == "Orthoptera" ~ "Orthoptères",
      ORDRE == "Coleoptera" ~ "Coléoptères",
      ORDRE == "Odonata" ~ "Odonates",
      ORDRE == "Lepidoptera" ~ "Lépidoptères",
      ORDRE == "Ephemeroptera" ~ "Ephémères",
      CLASSE == "Insecta" ~ "Autres insectes",
      TRUE ~ "Autre faune"
    )
  )

# Ajout du champs de recherche dans QGis
Liste$Nom <- ifelse(
  is.na(Liste$NOM_VERN),
  Liste$LB_NOM,
  paste0(
    Liste$LB_NOM,
    " (",
    gsub("\\s*\\(.*\\)", "", sub(",.*", "", Liste$NOM_VERN)),
    ")"
  )
)


# Réorganiser les colonnes dans l'ordre et factoriser les colonnes pour gagner en place, et ronommage de NOM_COMPLET en NOM_COMPL, pour passer sous les 10 caractères
Liste <- Liste %>%
  select(
    Groupe, Nom, CD_REF, LB_NOM, NOM_COMPL = NOM_COMPLET, NOM_VERN, CLASSE, ORDRE, FAMILLE,
    PN, PN_det,
    LRN, LRN_det,              
    LRN_H, LRN_H_det,
    LRN_P, LRN_P_det,
    DH, DH_det,
    DO, DO_det,
    ZH
  ) %>%
  mutate(
    across(
      c(Groupe, CLASSE, ORDRE, FAMILLE,
        PN, PN_det, DH, DH_det, DO, DO_det,
        LRN, LRN_det, LRN_H, LRN_H_det, LRN_P, LRN_P_det,
        ZH),
      as.factor
    )
  ) %>%
  arrange(Groupe, LB_NOM)

## 3.4 : Export des fichiers en GPKG et CSV

# Créer le chemin du fichier
chemin_fichier <- "Output/Referentiels.gpkg"

# Création du dossier si nécessaire
dir.create(dirname(chemin_fichier), recursive = TRUE)

# Écrire la table dans le fichier GPKG
sf::write_sf(Liste, chemin_fichier, layer = "Liste", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
Liste_csv <- Liste %>% mutate(across(where(is.factor), as.character))
readr::write_excel_csv2(Liste_csv, "Output/Liste.csv")



####################################################################
### 4 : Statuts régionaux

## 4.1 : Filtre des bases

# Statuts régionaux
STATUTS_REGIONAUX <- c(
  "Liste rouge régionale",
  "Protection régionale"
)

statuts_bdc <- bdc %>%
  filter(
    REGNE %in% REGNES_QBIOME,
    LB_TYPE_STATUT %in% STATUTS_REGIONAUX,
  )

# Sélection des protections départementales pour un traitement séparé
PD_bdc <- bdc %>%
  filter(LB_TYPE_STATUT == "Protection départementale") %>%
  left_join(dep, by = c("LB_ADM_TR" = "dep"))

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
    filter(LB_TYPE_STATUT == "Liste rouge régionale", is.na(CLASSE) | CLASSE != "Aves") %>%
    group_by(CD_REF) %>%
    summarise(LRR = paste0(CODE_STATUT, collapse = ", "), 
              LRR_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "))
  
  ## LRR Oiseaux : Patch pour AURA et PACA, qui distinguent les Hiv et les Mig
  if (i == "Provence-Alpes-Côte-d'Azur") {
    LRRO <- LRR_PACA_Avifaune %>%
      left_join(taxref_map, by = c("CD_NOM" = "CD_NOM")) %>%
      select(-CD_NOM) %>%
      transmute(
        CD_REF,
        LRR       = LRR_N,
        LRR_det   = LRR_N_det,
        LRR_H, LRR_H_det,
        LRR_P, LRR_P_det
      )
  } else if (r == "Auvergne-Rhône-Alpes") {
    LRRO <- LRR_AURA_Avifaune %>%
    left_join(taxref_map, by = c("CD_NOM" = "CD_NOM")) %>%
      select(-CD_NOM) %>%
      transmute(
        CD_REF,
        LRR       = LRR_N,
        LRR_det   = LRR_N_det,
        LRR_H, LRR_H_det,
        LRR_P, LRR_P_det
      )
  } else {
    LRRO <- statuts_bdc_i %>%
      filter(LB_TYPE_STATUT == "Liste rouge régionale", CLASSE == "Aves") %>%
      group_by(CD_REF) %>%
      summarise(
        LRR     = paste0(CODE_STATUT, collapse = ", "),
        LRR_det = paste0(RQ_STATUT[!is.na(RQ_STATUT) & RQ_STATUT != ""], collapse = ", "),
        .groups = "drop"
      ) %>%
      mutate(
        LRR_H = NA_character_, LRR_H_det = NA_character_,
        LRR_P = NA_character_, LRR_P_det = NA_character_
      )
  }
  
  # LRR complète
  LRR <- bind_rows(LRR, LRRO)
  
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
  select(
    REGION, CD_REF,
    PR, PR_det,
    LRR, LRR_det,
    LRR_H, LRR_H_det,
    LRR_P, LRR_P_det,
    PD, PD_det,
    ZNIEFF, ZNIEFF_det
  ) %>%
  mutate(across(where(is.character), ~ dplyr::na_if(trimws(.), ""))) %>%
  mutate(
    REGION = as.factor(REGION),
    across(
      c(PR, PR_det, LRR, LRR_det, LRR_H, LRR_H_det, LRR_P, LRR_P_det, PD, PD_det, ZNIEFF, ZNIEFF_det),
      as.factor
    )
  ) %>%
  arrange(REGION, CD_REF)

## 4.6 : Export des fichiers en GPKG et CSV

# Écrire la table dans le fichier GPKG
sf::write_sf(Region, chemin_fichier, layer = "Region", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
Region_csv <- Region %>%mutate(across(where(is.factor), as.character))
readr::write_excel_csv2(Region_csv, "Output/Region.csv")


####################################################################
### 5 : Nomenclature des textes

Textes <- bdc %>%
  filter(LB_TYPE_STATUT %in% c("Protection nationale", "Protection régionale", "Protection départementale", "Directive Oiseaux", "Directive Habitat"))%>%
  distinct(LB_TYPE_STATUT, CODE_STATUT, LABEL_STATUT) %>%
  arrange(LB_TYPE_STATUT, CODE_STATUT)

# Écrire la table dans le fichier GPKG
sf::write_sf(Textes, chemin_fichier, layer = "Textes", delete_layer = TRUE)

#Export en CSV pour lecture hors SIG
Textes_csv <- Textes %>% mutate(across(where(is.factor), as.character))
readr::write_excel_csv2(Textes_csv, "Output/Textes.csv")
