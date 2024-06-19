#################################################
# Analyse des données Ecotaxa
# Emmy STEPHAN 
# DATE 
#################################################

# install.packages("devtools")
devtools::install_github("clatrellu/planktoscopeR")


# Répertoire de travail  --------------------------------------------------

setwd("D:/INTECHMER/STAGE 2e ANNEE/ROSCOFF/RAPPORT/R/BDD_complète_stage_Emmy")

# Librairies  -------------------------------------------------------------

require(tidyverse)
require(planktoscopeR)
require(vegan)
require(data.table)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(dplyr)
require("RColorBrewer")
require(patchwork)
require(cowplot)




---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  
  
  # ANALYSES  ---------------------------------------------------------------


# Importation des données ----------------------------------------------------------------------------------------------------------

# La première étape consiste à importer le fichier tsv dans R. Pour ce faire, nous utilisons la fonction import.data(). 
# Nous décidons d'omettre les catégories "not-ling", "duplicate" et "multiple", que nous considérons comme non pertinentes pour l'analyse.
toy_dataset <- system.file("extdata", "BDD_complète_stage_Emmy.tsv", package = "planktoscopeR")
data <- import.data("BDD_complète_stage_Emmy.tsv",
                    unwanted = c("duplicate","multiple"))
# Ensuite, nous agrégeons les données par catégorie avec la fonction get.counts.and.vol(). Cette fonction consiste essentiellement à compter les objets et à additionner les volumes.
data_distrib <- get.counts.and.vol(data)
# Nous extrayons également les informations relatives aux échantillons avec la fonction `get.info()`.
context <- get.info(data,type="sample")
# Suppression d'un échantillon qui  n'est pas utilisé pour les analyses 
sample_id_to_remove <- "Test comportement HSN_Mystinet_H_60_1250_50_Long_Large"
data_filtered <- data_distrib %>%
  filter(sample_id != "Test comportement HSN_Mystinet_H_60_1250_50_Long_Large")


# Nous pouvons maintenant commencer à analyser les données.


# Création d'un objet regroupant toutes les catégories vivantes -----------

living_categories_to_include <- c(" Bacillariophyceae","Cylindrotheca","Delphineis","Licmophora",
                                  "Pleurosigma","Pseudo-Nitzschia chain","Thalassionema","Cerataulina",
                                  "Chaetoceros < Mediophyceae","Dactyliosolen","Dissoconium","Ditylum","Foraminifera","Guinardia delicatula",
                                  "Guinardia flaccida","Helicotheca tamesis"," Neoceratium","Neoceratium fusus < Neoceratium","Odontella sp.",
                                  "Paralia sulcata < Paralia","Protoperidinium","Rhizosolenia","Rotifera","Skeletonema",
                                  "Tintinnida","copepod sp.","copepoda-cut","multiple organisms","nauplii na",
                                  "spherical","unidentifiable","veliger")

# Ajout d'une colonne LIVING/NOT-LIVING -----------------------------------


# Fonction pour ajouter la nouvelle colonne `living/not-living`
add_living_not_living <- function(data) {
  data %>%
    mutate("living/not-living" = case_when(
      category %in% c("artefact","badfocus<artefact","bubble","cut<artefact","badfocus<other","broken",
                      "cyst<Dinophyceae","detritus","aggregates","dark<detritus","fecal pellets","transparent",
                      "molt","part<Crustacea","rods","spicule") ~ "NOT-LIVING",
      category %in% c(" Bacillariophyceae","Cylindrotheca","Delphineis","Licmophora",
                      "Pleurosigma","Pseudo-Nitzschia chain","Thalassionema","Cerataulina",
                      "Chaetoceros<Mediophyceae","Dactyliosolen","Dissoconium","Ditylum","Foraminifera","Guinardia delicatula",
                      "Guinardia flaccida","Helicotheca tamesis"," Neoceratium","Neoceratium fusus<Neoceratium","Odontella sp.",
                      "Paralia sulcata < Paralia","Protoperidinium","Rhizosolenia","Rotifera","Skeletonema",
                      "Tintinnida","copepod sp.","copepoda-cut","multiple organisms","nauplii na",
                      "spherical","unidentifiable","veliger") ~ "LIVING",
      TRUE ~ "other"
    ))
}



# Appliquer la fonction au jeu de données
data_transformed <- add_living_not_living(data_filtered)

# Sauvegarder le jeu de données transformé
write.csv(data_transformed, "data_transformed_with_living_not_living.csv", row.names = FALSE)

# Charger le jeu de données transformé
data_transformed <- read.csv("data_transformed_with_living_not_living.csv")


# Ajout d'une colonne "dilution" ------------------------------------------

# Fonction pour ajouter la nouvelle colonne `living/not-living`
add_dilution <- function(data) {
  data %>%
    mutate("dilution" = case_when(
      sample_id %in% c("test_collecteur_14_05_24_HSN_Mystinet_H_60_150_50_grand_grande_6_Large de Roscoff",
                       "P7_HSN_Mystinet_60_150_50_petit_6kts_rep1_large Roscoff",
                       "P7_HSN_Corypahenae_40_100_50_petit_6kts_rep1_large Roscoff",
                       "P7_HSN_Mystinet_60_150_50_petit_6kts_rep2_large Roscoff",
                       "P7_HSN_Coryphaenae_40_100_50_petit_6kts_rep2_large Roscoff",
                       "P7_HSN_Mystinet_60_150_50_petit_6kts_rep3_large Roscoff",
                       "P7_HSN_Coryphaenae_40_100_50_petit_6kts_rep3_large Roscoff",
                       "P4_HSN_Mystinet_60_150_50_petit_6kts_rep1_large Roscoff") ~ "2",
      sample_id %in% c("P5_HSN_Mystinet_60_150_50_petit_6kts_rep2_large Roscoff",
                       "P5_HSN_Mystinet_60_150_50_petit_6kts_rep3_large Roscoff",
                       "P5_HSN_Mystinet_60_150_50_grand_grande_6kts_rep1_large Roscoff",
                       "P5_HSN_Mystinet_60_150_50_grand_grande_6kts_rep2_large Roscoff",
                       "P5_HSN_Mystinet_60_150_50_grand_grande_6kts_rep3_large Roscoff",
                       "P4_HSN_Mystinet_60_150_20_petit_6kts_rep1_large Roscoff",
                       "P4_HSN_Mystinet_60_150_20_petit_6kts_rep2_large Roscoff") ~ "0.5",
      sample_id %in% c("test_collecteur_14_05_24_HSN_Mystinet_H_60_150_50_grand_petite_6_large Roscoff",
                       "test_collecteur_14_05_24_HSN_Mystinet_H_60_150_50_petit_6_large Roscoff",
                       "P5_HSN_Coryphaenae_40_100_50_grand_grande_6kts_rep1_large Roscoff",
                       "P5_HSN_Mystinet_60_150_50_petit_6kts_rep1_large Roscoff",
                       "P5_HSN_Coryphaenae_40_100_50_grand_grande_6kts_rep2_large Roscoff",
                       "P5_HSN_Coryphaenae_40_100_50_grand_grande_6kts_rep3_large Roscoff",
                       "P5_HSN_Coryphaenae_40_100_50_petit_6kts_rep1_large Roscoff",
                       "P5_HSN_Coryphaenae_40_100_50_petit_6kts_rep2_large Roscoff",
                       "P5_HSN_Coryphaenae_40_100_50_petit_6kts_rep3_large Roscoff",
                       "P4_HSN_Mystinet_60_150_50_petit_6kts_rep2_large Roscoff",
                       "P4_HSN_Mystinet_60_150_20_petit_6kts_rep3_large Roscoff",
                       "P4_HSN_Mystinet_60_150_50_petit_6kts_rep3_large Roscoff") ~ "1",
      TRUE ~ "2"
    ))
}



# Appliquer la fonction au jeu de données
data_transformed_dilution <- add_dilution(data_transformed)

# Sauvegarder le jeu de données transformé
write.csv(data_transformed_dilution, "data_transformed_dilution.csv", row.names = FALSE)

# Charger le jeu de données transformé
data_transformed_dilution <- read.csv("data_transformed_dilution.csv")

# Modification de la colonne "count" pour prendre en compte les dilutions ------------------------------------------

data_transformed_dilution_count<-data_transformed_dilution %>%
  select(sample_id,category, count,dilution,biovolume,living.not.living) %>%
  mutate(count=count/dilution)


#_______________________________________________________________________________________________________________________________________________




# Composition taxonimique ----------------------------------------------------------------------------------------------------------------------


# PROTOCOLE 2 -------------------------------------------------------------
# Spécifiez les sample_id que vous voulez inclure
sample_ids_to_include_P2 <- c("test_collecteur_14_05_24_HSN_Mystinet_H_60_150_50_grand_grande_6_Large de Roscoff",
                              "test_collecteur_14_05_24_HSN_Mystinet_H_60_150_50_grand_petite_6_Large de Roscoff",
                              "test_collecteur_14_05_24_HSN_Mystinet_H_60_150_50_petit_6_Large de Roscoff")
# Nouvelles étiquettes pour l'axe des abscisses
new_labels <- c("Grand collecteur\ngrandes fenêtres","Grand collecteur\npetites fenêtres","Petit collecteur")


###### 1. Living et not-living

# Filtrer les données pour ne conserver que "living" et "not-living"
P2_data_filtered1 <- data_transformed_dilution_count %>%
  filter(living.not.living %in% c("LIVING", "NOT-LIVING") & sample_id %in% sample_ids_to_include_P2)

# Créer le diagramme à barres COUNT 
ggplot(P2_data_filtered1, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P2: Proportion de LIVING et NOT LIVING par échantillon \n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 9,angle = 45, hjust = 1),  # Modifier la taille et l'orientation du texte de l'axe des abscisses
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))   # Centrer le titre  
# Sauvegarder le graphique
ggsave("P2_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_count.jpg", width = 25, height = 20, units = "cm")


# Créer le diagramme à barres BIOVOLUME 
ggplot(P2_data_filtered1, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P2: Proportion de LIVING et NOT LIVING par échantillon \n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 9,angle = 45, hjust = 1),  # Modifier la taille et l'orientation du texte de l'axe des abscisses
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))   # Centrer le titre  
# Sauvegarder le graphique
ggsave("P2_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_biovolume.jpg", width = 25, height = 20, units = "cm")


#_____________________________

###### 2. Graphique mettant en évidence la composition taxonomique par échantillon en regroupant les NOT LIVING


# Créer une nouvelle colonne category avec les regroupements souhaités
P2_data_transformed2 <- data_transformed_dilution_count %>%
  mutate(category = case_when(
    category %in% c("artefact","badfocus<artefact","bubble","cut<artefact","badfocus<other","broken","cyst<Dinophyceae",
                    "detritus","aggregates","dark<detritus","fecal pellets","transparent",
                    "molt","part<Crustacea","rods","spicule") ~ "NOT-LIVING",TRUE ~ category))
# Créer une palette de 32 couleurs distinctes en combinant différentes palettes
palette_brewer_1 <- brewer.pal(12, "Paired")
palette_brewer_2 <- brewer.pal(8, "Dark2")
palette_brewer_3 <- brewer.pal(8, "Set2")
custom_colors <- c("#D32F2F", "#1976D2", "#388E3C", "#FBC02D")
# Combiner les palettes pour créer une palette de 32 couleurs distinctes
palette_32 <- c(palette_brewer_1, palette_brewer_2, palette_brewer_3, custom_colors)
# Ajouter le noir pour "NOT-LIVING"
palette_33 <- c(palette_32, "#000000")
# Spécifiez les catégories à inclure
P2_categories_to_include2 <- c(living_categories_to_include,"NOT-LIVING") 
# Associer les couleurs aux catégories, avec "NOT-LIVING" en noir
color_mapping <- setNames(palette_33, P2_categories_to_include2)
# Filtrer les données pour ne conserver que "not-living" et "categories_to_include"
P2_data_filtered2 <- P2_data_transformed2 %>%
  filter(category %in% P2_categories_to_include2 & sample_id %in% sample_ids_to_include_P2) %>%
  mutate(category=factor(category, levels=c(living_categories_to_include,"NOT-LIVING")))


# Graphique COUNT
ggplot(P2_data_filtered2, aes(x = factor(sample_id), y = count)) +
  geom_col(aes(fill = category)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  theme_minimal() +
  labs(title = "P2 : Composition taxonomique par échantillon en regroupant les NOT-LIVING\n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +  
  guides(fill = guide_legend(ncol = 1))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_par_échantillon_en_regroupant_les_NOT-LIVING_count.jpg", width = 25, height = 20, units = "cm")

# Graphique COUNT avec les barres à la même hauteur pour facilier la lecture 
ggplot(P2_data_filtered2, aes(x = factor(sample_id), y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  theme_bw() +
  labs(title = "P2 : Composition taxonomique par échantillon à même hauteur en regroupant les NOT-LIVING\n Filet: Mystinet 50 µm",
       x = "ID de l'échantillon",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +  
  guides(fill = guide_legend(ncol = 1))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING.jpg", width = 25, height = 20, units = "cm")

# Graphique biovolume 
ggplot(P2_data_filtered2, aes(x = factor(sample_id), y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  theme_minimal() +
  labs(title = "P2 : Composition taxonomique par échantillon en regroupant les NOT-LIVING\n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +  
  guides(fill = guide_legend(ncol = 2))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_par_échantillon_en_regroupant_les_NOT-LIVING_biovolume.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P2_data_filtered2, aes(x = factor(sample_id), y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  theme_bw() +
  labs(title = "P2 : Composition taxonomique par échantillon à même hauteur en regroupant les NOT-LIVING\n Filet: Mystinet 50 µm",
       x = "ID de l'échantillon",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +  
  guides(fill = guide_legend(ncol = 2))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_biovolume.jpg", width = 25, height = 20, units = "cm")


#_____________________________

###### 3. Graphique mettant en évidence les détritus d'origine vivante par échantillon 


# Choix des catégories à mettre dans le graphique  
P2_categories_to_include3 <- c("spicule","molt","part<Crustacea")  # Spécifiez les catégories à inclure
# Filtrer les données pour ne conserver que "categories_to_include3" et "sample_ids_to_include_P2"
P2_data_filtered3 <- data_transformed_dilution %>%
  filter(category %in% P2_categories_to_include3 & sample_id %in% sample_ids_to_include_P2)
# Créer une palette de 3 couleurs (bleu, rouge, jaune)
palette_3 <- c("#0000FF", "#FF0000", "#FFFF00")  # Bleu, rouge, jaune
# Associer les couleurs aux catégories
color_mapping_3 <- setNames(palette_3, P2_categories_to_include3)

# Graphique COUNT
ggplot(P2_data_filtered3, aes(x = factor(sample_id), y = count)) +
  geom_col(aes(fill = category)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  theme_minimal() +
  labs(title = "P2 : Détritus d'origine vivante par échantillon\n Filet: Mystinet 50 µm ",
       x = "Type de collecteur",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))   
# Sauvegarder le graphique
ggsave("P2_Détritus_d'origine_vivante_par_échantillon_count.jpg", width = 25, height = 20, units = "cm")

# Graphique COUNT avec les barres à la même hauteur pour facilier la lecture 
ggplot(P2_data_filtered3, aes(x = factor(sample_id), y = count)) +
  geom_col(aes(fill = category),  col="black",position="fill") +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  theme_bw() +
  labs(title = "P2 : Détritus d'origine vivante par échantillon, barres de même hauteur\n Filet: Mystinet 50 µm ",
       x = "Type de collecteur",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))   
# Sauvegarder le graphique
ggsave("P2_Détritus_d'origine_vivante_par_échantillon_barres_de_même_hauteur_COUNT.jpg", width = 25, height = 20, units = "cm")

# Graphique biovolume
ggplot(P2_data_filtered3, aes(x = factor(sample_id), y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  theme_minimal() +
  labs(title = "P2 : Détritus d'origine vivante par échantillon\n Filet: Mystinet 50 µm ",
       x = "Type de collecteur",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))   
# Sauvegarder le graphique
ggsave("P2_Détritus_d'origine_vivante_par_échantillon_biovolume.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P2_data_filtered3, aes(x = factor(sample_id), y = biovolume)) +
  geom_col(aes(fill = category),  col="black",position="fill") +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  theme_bw() +
  labs(title = "P2 : Détritus d'origine vivante par échantillon, barres de même hauteur\n Filet: Mystinet 50 µm ",
       x = "Type de collecteur",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))   
# Sauvegarder le graphique
ggsave("P2_Détritus_d'origine_vivante_par_échantillon_barres_de_même_hauteur_biovolume.jpg", width = 25, height = 20, units = "cm")


#_____________________________

###### 4. Graphique mettant en evidence la composition taxonomique par échantillon sans prendre en compte les NOT LIVING


# Filtrer les données pour ne conserver que "living_categories_to_include" et "sample_ids_to_include_P2"
P2_data_filtered4 <- data_transformed_dilution_count %>%
  filter(category %in% living_categories_to_include & sample_id %in% sample_ids_to_include_P2)

# Créer le graphique COUNT avec les données filtrées
ggplot(P2_data_filtered4, aes(x = factor(sample_id), y = count)) +
  geom_col(aes(fill = category)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping) +  
  theme_minimal() +
  labs(title = "P2 : Composition taxonomique du vivant par échantillon\n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  guides(fill = guide_legend(ncol = 2))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_du_vivant_par_échantillon_count.jpg", width = 25, height = 20, units = "cm")

# Graphique COUNT avec les barres à la même hauteur pour facilier la lecture de la diversité taxo 
ggplot(P2_data_filtered4, aes(x = factor(sample_id), y = count)) +
  geom_col(aes(fill = category),  col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_bw() +
  labs(title = "P2 : Composition taxonomique du vivant par échantillon, barres de même hauteur\n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  guides(fill = guide_legend(ncol = 1))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_du_vivant_par_échantillon_barres_de_même_hauteur_COUNT.jpg", width = 25, height = 20, units = "cm")


# Créer le graphique BIOVOLUME avec les données filtrées
ggplot(P2_data_filtered4, aes(x = factor(sample_id), y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  scale_fill_manual(values = color_mapping) +  
  theme_minimal() +
  labs(title = "P2 : Composition taxonomique du vivant par échantillon\n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  guides(fill = guide_legend(ncol = 2))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_du_vivant_par_échantillon_biovolume.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture de la diversité taxo 
ggplot(P2_data_filtered4, aes(x = factor(sample_id), y = biovolume)) +
  geom_col(aes(fill = category),  col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  
  scale_x_discrete(labels = new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_bw() +
  labs(title = "P2 : Composition taxonomique du vivant par échantillon, barres de même hauteur\n Filet: Mystinet 50 µm",
       x = "Type de collecteur",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  guides(fill = guide_legend(ncol = 2))  # Afficher les catégories en 2 colonnes dans la légende
# Sauvegarder le graphique
ggsave("P2_Composition_taxonomique_du_vivant_par_échantillon_barres_de_même_hauteur_biovolume.jpg", width = 25, height = 20, units = "cm")



#__________________________________________________________________________________________________________________________
# PROTOCOLE 4  ------------------------------------------------------------------------------------------------------------
sample_ids_to_include_P4 <- c("P4_HSN_Mystinet_60_150_20_petit_6kts_rep2_large Roscoff",
                              "P4_HSN_Mystinet_60_150_20_petit_6kts_rep3_large Roscoff",
                              "P4_HSN_Mystinet_60_150_50_petit_6kts_rep1_large Roscoff",
                              "P4_HSN_Mystinet_60_150_50_petit_6kts_rep2_large Roscoff",
                              "P4_HSN_Mystinet_60_150_50_petit_6kts_rep3_large Roscoff",
                              "P4_HSN_Mystinet_60_150_20_petit_6kts_rep1_large Roscoff")
# Nouvelles étiquettes pour l'axe des abscisses
P4_new_labels <- c("Réplicat 1","Réplicat 2","Réplicat 3")

###### 1. Living et not-living

# Filtrer les données pour ne conserver que "living" et "not-living"
P4_data_filtered1 <- data_transformed_dilution_count %>%
  filter(living.not.living %in% c("LIVING", "NOT-LIVING") & sample_id %in% sample_ids_to_include_P4)

# Séparer les données en deux groupes
P4_data_filtered1_20µm <- P4_data_filtered1 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_20", sample_id))

P4_data_filtered1_50µm <- P4_data_filtered1 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_50", sample_id))


#____COUNT 

# Créer COUNT le graphique pour 20µm
P4_plot_20µm_1<- ggplot(P4_data_filtered1_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4: Proportion de LIVING et NOT LIVING par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 1100, by = 50),  # Définir les graduations
    limits = c(0, 1100)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 

  



# Créer le graphique pour 50µm
P4_plot_50µm_1 <- ggplot(P4_data_filtered1_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 1100, by = 50),  # Définir les graduations
    limits = c(0, 1100)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))



# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_1 + theme(legend.position = "bottom"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_1 <- P4_plot_20µm_1 + theme(legend.position = "none")
P4_plot_50µm_1 <- P4_plot_50µm_1 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_1 <- (P4_plot_20µm_1 + P4_plot_50µm_1) / legend + plot_layout(ncol = 1, heights = c(10, 1))

# Afficher le graphique combiné
print(P4_combined_plot_1)

# Sauvegarder le graphique
ggsave("P4_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME 

# Créer le graphique pour 20µm
P4_plot_20µm_1 <- ggplot(P4_data_filtered1_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4: Proportion de LIVING et NOT LIVING par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.9, by = 0.05),  # Définir les graduations
    limits = c(0, 0.9)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 

# Créer le graphique pour 50µm
P4_plot_50µm_1 <- ggplot(P4_data_filtered1_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.9, by = 0.05),  # Définir les graduations
    limits = c(0, 0.9)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_1 + theme(legend.position = "bottom"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_1 <- P4_plot_20µm_1 + theme(legend.position = "none")
P4_plot_50µm_1 <- P4_plot_50µm_1 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_1 <- (P4_plot_20µm_1 + P4_plot_50µm_1) / legend + plot_layout(ncol = 1, heights = c(10, 1))

# Afficher le graphique combiné
print(P4_combined_plot_1)

# Sauvegarder le graphique
ggsave("P4_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_biovolume.jpg", width = 25, height = 20, units = "cm")



#_____________________________

###### 2. Graphique mettant en évidence la composition taxonomique par échantillon en regroupant les NOT LIVING


# Créer une nouvelle colonne category avec les regroupements souhaités
P4_data_transformed2 <- data_transformed_dilution_count %>%
  mutate(category = case_when(
    category %in% c("artefact","badfocus<artefact","bubble","cut<artefact","badfocus<other","broken","cyst<Dinophyceae",
                    "detritus","aggregates","dark<detritus","fecal pellets","transparent",
                    "molt","part<Crustacea","rods","spicule") ~ "NOT-LIVING",TRUE ~ category))
# Créer une palette de 32 couleurs distinctes en combinant différentes palettes
palette_brewer_1 <- brewer.pal(12, "Paired")
palette_brewer_2 <- brewer.pal(8, "Dark2")
palette_brewer_3 <- brewer.pal(8, "Set2")
custom_colors <- c("#D32F2F", "#1976D2", "#388E3C", "#FBC02D")
# Combiner les palettes pour créer une palette de 32 couleurs distinctes
palette_32 <- c(palette_brewer_1, palette_brewer_2, palette_brewer_3, custom_colors)
# Ajouter le noir pour "NOT-LIVING"
palette_33 <- c(palette_32, "#000000")
# Spécifiez les catégories à inclure
P4_categories_to_include2 <- c(living_categories_to_include,"NOT-LIVING") 
# Associer les couleurs aux catégories, avec "NOT-LIVING" en noir
color_mapping <- setNames(palette_33, P4_categories_to_include2)
# Filtrer les données pour ne conserver que "not-living" et "categories_to_include"
P4_data_filtered2 <- P4_data_transformed2 %>%
  filter(category %in% P4_categories_to_include2 & sample_id %in% sample_ids_to_include_P4) %>%
  mutate(category=factor(category, levels=c(living_categories_to_include,"NOT-LIVING")))



# Séparer les données en deux groupes
P4_data_filtered2_20µm <- P4_data_filtered2 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_20", sample_id))

P4_data_filtered2_50µm <- P4_data_filtered2 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_50", sample_id))
#___COUNT 

# Créer le graphique pour 20µm
P4_plot_20µm_2 <- ggplot(P4_data_filtered2_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 1050, by = 50),  # Définir les graduations
    limits = c(0, 1050)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_2 <- ggplot(P4_data_filtered2_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 1050, by = 50),  # Définir les graduations
    limits = c(0, 1050)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_2 <- P4_plot_20µm_2 + theme(legend.position = "none")
P4_plot_50µm_2 <- P4_plot_50µm_2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_2 <- (P4_plot_20µm_2 | P4_plot_50µm_2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_2)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT_COUNT.jpg", width = 25, height = 20, units = "cm")

#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour 20µm
P4_plot_20µm_2.2 <- ggplot(P4_data_filtered2_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique par échantillon à même hauteur en regroupant les NOT-LIVING",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_2.2 <- ggplot(P4_data_filtered2_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_2.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_2.2 <- P4_plot_20µm_2.2 + theme(legend.position = "none")
P4_plot_50µm_2.2 <- P4_plot_50µm_2.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_2.2 <- (P4_plot_20µm_2.2 | P4_plot_50µm_2.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_2.2)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME

# Créer le graphique pour 20µm
P4_plot_20µm_2 <- ggplot(P4_data_filtered2_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.9, by = 0.05),  # Définir les graduations
    limits = c(0, 0.9)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_2 <- ggplot(P4_data_filtered2_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.9, by = 0.05),  # Définir les graduations
    limits = c(0, 0.9)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_2 <- P4_plot_20µm_2 + theme(legend.position = "none")
P4_plot_50µm_2 <- P4_plot_50µm_2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_2 <- (P4_plot_20µm_2 | P4_plot_50µm_2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_2)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour 20µm
P4_plot_20µm_2.2 <- ggplot(P4_data_filtered2_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique par échantillon à même hauteur en regroupant les NOT-LIVING",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_2.2 <- ggplot(P4_data_filtered2_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_2.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_2.2 <- P4_plot_20µm_2.2 + theme(legend.position = "none")
P4_plot_50µm_2.2 <- P4_plot_50µm_2.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_2.2 <- (P4_plot_20µm_2.2 | P4_plot_50µm_2.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_2.2)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")


#______________________________

###### 3. Graphique mettant en évidence les détritus d'origine vivante par échantillon 


# Choix des catégories à mettre dans le graphique  
P4_categories_to_include3 <- c("spicule","molt","part<Crustacea")  # Spécifiez les catégories à inclure
# Filtrer les données pour ne conserver que "categories_to_include3" et "sample_ids_to_include_P2"
P4_data_filtered3 <- data_transformed_dilution_count %>%
  filter(category %in% P4_categories_to_include3 & sample_id %in% sample_ids_to_include_P4)
# Créer une palette de 3 couleurs (bleu, rouge, jaune)
palette_3 <- c("#0000FF", "#FF0000", "#FFFF00")  # Bleu, rouge, jaune
# Associer les couleurs aux catégories
color_mapping_3 <- setNames(palette_3, P4_categories_to_include3)

# Séparer les données en deux groupes
P4_data_filtered3_20µm <- P4_data_filtered3 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_20", sample_id))

P4_data_filtered3_50µm <- P4_data_filtered3 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_50", sample_id))

#___COUNT 


# Créer le graphique pour 20µm
P4_plot_20µm_3 <- ggplot(P4_data_filtered3_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Détritus d'origine vivante par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 115, by = 5),  # Définir les graduations
    limits = c(0, 115)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_3 <- ggplot(P4_data_filtered3_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 115, by = 5),  # Définir les graduations
    limits = c(0, 115)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_3 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_3 <- P4_plot_20µm_3 + theme(legend.position = "none")
P4_plot_50µm_3 <- P4_plot_50µm_3 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_3 <- (P4_plot_20µm_3 | P4_plot_50µm_3 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_3)

# Sauvegarder le graphique
ggsave("P4_Détritus_d'origine_vivante_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")

#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 

# Créer le graphique pour 20µm
P4_plot_20µm_3.2 <- ggplot(P4_data_filtered3_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Détritus d'origine vivante par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_3.2 <- ggplot(P4_data_filtered3_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_3.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_3.2 <- P4_plot_20µm_3.2 + theme(legend.position = "none")
P4_plot_50µm_3.2 <- P4_plot_50µm_3.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_3.2 <- (P4_plot_20µm_3.2 | P4_plot_50µm_3.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_3.2)


# Sauvegarder le graphique
ggsave("P4_Détritus_d'origine_vivante_même_hauteur_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME

# Créer le graphique pour 20µm
P4_plot_20µm_3 <- ggplot(P4_data_filtered3_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Détritus d'origine vivante par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.070, by = 0.005),  # Définir les graduations
    limits = c(0, 0.070)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 

# Créer le graphique pour 50µm
P4_plot_50µm_3 <- ggplot(P4_data_filtered3_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.070, by = 0.005),  # Définir les graduations
    limits = c(0, 0.070)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_3 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_3 <- P4_plot_20µm_3 + theme(legend.position = "none")
P4_plot_50µm_3 <- P4_plot_50µm_3 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_3 <- (P4_plot_20µm_3 | P4_plot_50µm_3 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_3)

# Sauvegarder le graphique
ggsave("P4_Détritus_d'origine_vivante_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 

# Créer le graphique pour 20µm
P4_plot_20µm_3.2 <- ggplot(P4_data_filtered3_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Détritus d'origine vivante par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_3.2 <- ggplot(P4_data_filtered3_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_3.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_3.2 <- P4_plot_20µm_3.2 + theme(legend.position = "none")
P4_plot_50µm_3.2 <- P4_plot_50µm_3.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_3.2 <- (P4_plot_20µm_3.2 | P4_plot_50µm_3.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_3.2)


# Sauvegarder le graphique
ggsave("P4_Détritus_d'origine_vivante_même_hauteur_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

#______________________________

###### 4. Graphique mettant en evidence la composition taxonomique par échantillon sans prendre en compte les NOT LIVING


# Filtrer les données pour ne conserver que "living_categories_to_include" et "sample_ids_to_include_P2"
P4_data_filtered4 <- data_transformed_dilution_count %>%
  filter(category %in% living_categories_to_include & sample_id %in% sample_ids_to_include_P4)

# Séparer les données en deux groupes
P4_data_filtered4_20µm <- P4_data_filtered4 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_20", sample_id))

P4_data_filtered4_50µm <- P4_data_filtered4 %>%
  filter(grepl("P4_HSN_Mystinet_60_150_50", sample_id))

#___COUNT

# Créer le graphique pour 20µm
P4_plot_20µm_4 <- ggplot(P4_data_filtered4_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique du vivant par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 465, by = 20),  # Définir les graduations
    limits = c(0, 465)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 



# Créer le graphique pour 50µm
P4_plot_50µm_4 <- ggplot(P4_data_filtered4_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 465, by = 20),  # Définir les graduations
    limits = c(0, 465)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))


# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_4 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_4 <- P4_plot_20µm_4 + theme(legend.position = "none")
P4_plot_50µm_4 <- P4_plot_50µm_4 + theme(legend.position = "none")


# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_4 <- (P4_plot_20µm_4 | P4_plot_50µm_4 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_4)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_du_vivant_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")

#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 

# Créer le graphique pour 20µm
P4_plot_20µm_4.2 <- ggplot(P4_data_filtered4_20µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique du vivant par échantillon, barres de même hauteur",
       x = "HSN Mystinet 20 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_4.2 <- ggplot(P4_data_filtered4_50µm, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_4.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_4.2 <- P4_plot_20µm_4.2 + theme(legend.position = "none")
P4_plot_50µm_4.2 <- P4_plot_50µm_4.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_4.2 <- (P4_plot_20µm_4.2 | P4_plot_50µm_4.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_4.2)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_du_vivant_par_échantillon_barres_de_même_hauteur_COUNT.jpg", width = 25, height = 20, units = "cm")

#__BIOVOLUME

# Créer le graphique pour 20µm
P4_plot_20µm_4 <- ggplot(P4_data_filtered4_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique du vivant par échantillon",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.06, by = 0.005),  # Définir les graduations
    limits = c(0, 0.06)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_4 <- ggplot(P4_data_filtered4_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 0.06, by = 0.005),  # Définir les graduations
    limits = c(0, 0.06)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_4 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_4 <- P4_plot_20µm_4 + theme(legend.position = "none")
P4_plot_50µm_4 <- P4_plot_50µm_4 + theme(legend.position = "none")


# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_4 <- (P4_plot_20µm_4 | P4_plot_50µm_4 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_4)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_du_vivant_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 

# Créer le graphique pour 20µm
P4_plot_20µm_4.2 <- ggplot(P4_data_filtered4_20µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P4 : Composition taxonomique du vivant par échantillon, barres de même hauteur",
       x = "HSN Mystinet 20 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour 50µm
P4_plot_50µm_4.2 <- ggplot(P4_data_filtered4_50µm, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P4_plot_20µm_4.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P4_plot_20µm_4.2 <- P4_plot_20µm_4.2 + theme(legend.position = "none")
P4_plot_50µm_4.2 <- P4_plot_50µm_4.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P4_combined_plot_4.2 <- (P4_plot_20µm_4.2 | P4_plot_50µm_4.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P4_combined_plot_4.2)

# Sauvegarder le graphique
ggsave("P4_Composition_taxonomique_du_vivant_par_échantillon_barres_de_même_hauteur_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")



#__________________________________________________________________________________________________________________________
# PROTOCOLE 5  ------------------------------------------------------------------------------------------------------------
sample_ids_to_include_P5 <- c("P5_HSN_Coryphaenae_40_100_50_grand_grande_6kts_rep1_large Roscoff",
                              "P5_HSN_Coryphaenae_40_100_50_grand_grande_6kts_rep2_large Roscoff",
                              "P5_HSN_Coryphaenae_40_100_50_grand_grande_6kts_rep3_large Roscoff",
                              "P5_HSN_Coryphaenae_40_100_50_petit_6kts_rep1_large Roscoff",
                              "P5_HSN_Coryphaenae_40_100_50_petit_6kts_rep2_large Roscoff",
                              "P5_HSN_Coryphaenae_40_100_50_petit_6kts_rep3_large Roscoff",
                              "P5_HSN_Mystinet_60_150_50_grand_grande_6kts_rep1_large Roscoff",
                              "P5_HSN_Mystinet_60_150_50_grand_grande_6kts_rep2_large Roscoff",
                              "P5_HSN_Mystinet_60_150_50_grand_grande_6kts_rep3_large Roscoff",
                              "P5_HSN_Mystinet_60_150_50_petit_6kts_rep1_large Roscoff",
                              "P5_HSN_Mystinet_60_150_50_petit_6kts_rep2_large Roscoff",
                              "P5_HSN_Mystinet_60_150_50_petit_6kts_rep3_large Roscoff")



###### 1. Living et not-living


# Filtrer les données pour ne conserver que "living" et "not-living"
P5_data_filtered1 <- data_transformed_dilution_count %>%
  filter(living.not.living %in% c("LIVING", "NOT-LIVING") & sample_id %in% sample_ids_to_include_P5)

# Ajouter une nouvelle colonne pour grouper les données en fonction des catégories souhaitées
P5_data_filtered1 <- P5_data_filtered1 %>%
  mutate(group = case_when(
    grepl("P5_HSN_Coryphaenae_40_100_50_grand_grande", sample_id) ~ "Coryphaenae\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Coryphaenae_40_100_50_petit", sample_id) ~ "Coryphaenae\nPetit collecteur",
    grepl("P5_HSN_Mystinet_60_150_50_grand_grande", sample_id) ~ "Mystinet\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Mystinet_60_150_50_petit", sample_id) ~ "Mystinet\nPetit collecteur",
    TRUE ~ "Other"
  ))

# Créer le diagramme à barres avec facettes COUNT
ggplot(P5_data_filtered1, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = living.not.living)) +
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5: Proportion de LIVING et NOT LIVING par échantillon",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_facet_COUNT.jpg", width = 25, height = 20, units = "cm")

# Créer le diagramme à barres avec facettes BIOVOLUME
ggplot(P5_data_filtered1, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = living.not.living)) +
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5: Proportion de LIVING et NOT LIVING par échantillon",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))


# Sauvegarder le graphique
ggsave("P5_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_facet.jpg", width = 25, height = 20, units = "cm")



#_____________________________

###### 2. Graphique mettant en évidence la composition taxonomique par échantillon en regroupant les NOT LIVING


# Créer une nouvelle colonne category avec les regroupements souhaités
P5_data_transformed2 <- data_transformed_dilution_count %>%
  mutate(category = case_when(
    category %in% c("artefact","badfocus<artefact","bubble","cut<artefact","badfocus<other","broken","cyst<Dinophyceae",
                    "detritus","aggregates","dark<detritus","fecal pellets","transparent",
                    "molt","part<Crustacea","rods","spicule") ~ "NOT-LIVING",TRUE ~ category))
# Créer une palette de 32 couleurs distinctes en combinant différentes palettes
palette_brewer_1 <- brewer.pal(12, "Paired")
palette_brewer_2 <- brewer.pal(8, "Dark2")
palette_brewer_3 <- brewer.pal(8, "Set2")
custom_colors <- c("#D32F2F", "#1976D2", "#388E3C", "#FBC02D")
# Combiner les palettes pour créer une palette de 32 couleurs distinctes
palette_32 <- c(palette_brewer_1, palette_brewer_2, palette_brewer_3, custom_colors)
# Ajouter le noir pour "NOT-LIVING"
palette_33 <- c(palette_32, "#000000")
# Spécifiez les catégories à inclure
P5_categories_to_include2 <- c(living_categories_to_include,"NOT-LIVING") 
# Associer les couleurs aux catégories, avec "NOT-LIVING" en noir
color_mapping <- setNames(palette_33, P5_categories_to_include2)
# Filtrer les données pour ne conserver que "not-living" et "categories_to_include"
P5_data_filtered2 <- P5_data_transformed2 %>%
  filter(category %in% P5_categories_to_include2 & sample_id %in% sample_ids_to_include_P5) %>%
  mutate(category=factor(category, levels=c(living_categories_to_include,"NOT-LIVING")))


# Ajouter une nouvelle colonne pour grouper les données en fonction des catégories souhaitées
P5_data_filtered2 <- P5_data_filtered2 %>%
  mutate(group = case_when(
    grepl("P5_HSN_Coryphaenae_40_100_50_grand_grande", sample_id) ~ "Coryphaenae\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Coryphaenae_40_100_50_petit", sample_id) ~ "Coryphaenae\nPetit collecteur",
    grepl("P5_HSN_Mystinet_60_150_50_grand_grande", sample_id) ~ "Mystinet\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Mystinet_60_150_50_petit", sample_id) ~ "Mystinet\nPetit collecteur",
    TRUE ~ "Other"
  ))

#___COUNT 

# Créer le diagramme à barres avec facettes
ggplot(P5_data_filtered2, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_par_échantillon_en_regroupant_lesNOT-LIVING_COUNT.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P5_data_filtered2, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME

# Créer le diagramme à barres avec facettes
ggplot(P5_data_filtered2, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_par_échantillon_en_regroupant_lesNOT-LIVING_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P5_data_filtered2, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")





#______________________________

###### 3. Graphique mettant en évidence les détritus d'origine vivante par échantillon 


# Choix des catégories à mettre dans le graphique  
P5_categories_to_include3 <- c("spicule","molt","part<Crustacea")  # Spécifiez les catégories à inclure
# Filtrer les données pour ne conserver que "categories_to_include3" et "sample_ids_to_include_P5"
P5_data_filtered3 <- data_transformed_dilution_count %>%
  filter(category %in% P5_categories_to_include3 & sample_id %in% sample_ids_to_include_P5)
# Créer une palette de 3 couleurs (bleu, rouge, jaune)
palette_3 <- c("#0000FF", "#FF0000", "#FFFF00")  # Bleu, rouge, jaune
# Associer les couleurs aux catégories
color_mapping_3 <- setNames(palette_3, P5_categories_to_include3)

# Ajouter une nouvelle colonne pour grouper les données en fonction des catégories souhaitées
P5_data_filtered3 <- P5_data_filtered3 %>%
  mutate(group = case_when(
    grepl("P5_HSN_Coryphaenae_40_100_50_grand_grande", sample_id) ~ "Coryphaenae\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Coryphaenae_40_100_50_petit", sample_id) ~ "Coryphaenae\nPetit collecteur",
    grepl("P5_HSN_Mystinet_60_150_50_grand_grande", sample_id) ~ "Mystinet\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Mystinet_60_150_50_petit", sample_id) ~ "Mystinet\nPetit collecteur",
    TRUE ~ "Other"
  ))

#___COUNT 

# Créer le diagramme à barres avec facettes
ggplot(P5_data_filtered3, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Détritus d'origine vivante par échantillon",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Détritus_d'origine_vivante_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P5_data_filtered3, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Détritus d'origine vivante par échantillon, barres de même hauteur",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Détritus_d'origine_vivante_par_échantillon_barres_de_même_hauteur_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME 

# Créer le diagramme à barres avec facettes
ggplot(P5_data_filtered3, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Détritus d'origine vivante par échantillon",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Détritus_d'origine_vivante_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P5_data_filtered3, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Détritus d'origine vivante par échantillon, barres de même hauteur",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Détritus_d'origine_vivante_par_échantillon_barres_de_même_hauteur_BIOVVOLUME.jpg", width = 25, height = 20, units = "cm")


#_______________________________

###### 4. Graphique mettant en evidence la composition taxonomique par échantillon sans prendre en compte les NOT LIVING


# Filtrer les données pour ne conserver que "living_categories_to_include" et "sample_ids_to_include_P2"
P5_data_filtered4 <- data_transformed_dilution_count %>%
  filter(category %in% living_categories_to_include & sample_id %in% sample_ids_to_include_P5)

# Ajouter une nouvelle colonne pour grouper les données en fonction des catégories souhaitées
P5_data_filtered4 <- P5_data_filtered4 %>%
  mutate(group = case_when(
    grepl("P5_HSN_Coryphaenae_40_100_50_grand_grande", sample_id) ~ "Coryphaenae\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Coryphaenae_40_100_50_petit", sample_id) ~ "Coryphaenae\nPetit collecteur",
    grepl("P5_HSN_Mystinet_60_150_50_grand_grande", sample_id) ~ "Mystinet\nGrand collecteur grandes fenêtres",
    grepl("P5_HSN_Mystinet_60_150_50_petit", sample_id) ~ "Mystinet\nPetit collecteur",
    TRUE ~ "Other"
  ))

#___COUNT

# Créer le diagramme à barres avec facettes
ggplot(P5_data_filtered4, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique du vivant par échantillon",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_du_vivant_par_échantillon_COUNT.jpg", width = 20, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P5_data_filtered4, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique par échantillon, barres de même hauteur",
       x = "",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_du_vivant_par_échantillon_barres_de_même_hauteur_COUNT.jpg", width = 20, height = 20, units = "cm")

#___BIOVOLUME 

# Créer le diagramme à barres avec facettes
ggplot(P5_data_filtered4, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique du vivant par échantillon",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_du_vivant_par_échantillon_BIOVOLUME.jpg", width = 20, height = 20, units = "cm")

# Graphique avec les barres à la même hauteur pour facilier la lecture 
ggplot(P5_data_filtered4, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie avec "NOT-LIVING" en noir
  facet_wrap(~ group, scales = "free_x") +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P5 : Composition taxonomique par échantillon, barres de même hauteur",
       x = "",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Sauvegarder le graphique
ggsave("P5_Composition_taxonomique_du_vivant_par_échantillon_barres_de_même_hauteur_BIOVOLUME.jpg", width = 20, height = 20, units = "cm")



#__________________________________________________________________________________________________________________________
# PROTOCOLE 7  ------------------------------------------------------------------------------------------------------------
sample_ids_to_include_P7 <- c("P7_HSN_Corypahenae_40_100_50_petit_6kts_rep1_large Roscoff",
                              "P7_HSN_Coryphaenae_40_100_50_petit_6kts_rep2_large Roscoff",
                              "P7_HSN_Coryphaenae_40_100_50_petit_6kts_rep3_large Roscoff",
                              "P7_HSN_Mystinet_60_150_50_petit_6kts_rep1_large Roscoff",
                              "P7_HSN_Mystinet_60_150_50_petit_6kts_rep2_large Roscoff",
                              "P7_HSN_Mystinet_60_150_50_petit_6kts_rep3_large Roscoff")



###### 1. Living et not-living


# Filtrer les données pour ne conserver que "living" et "not-living"
P7_data_filtered1 <- data_transformed_dilution_count %>%
  filter(living.not.living %in% c("LIVING", "NOT-LIVING") & sample_id %in% sample_ids_to_include_P7)


# Séparer les données en deux groupes
P7_data_filtered1_Cory <- P7_data_filtered1 %>%
  filter(grepl("P7_HSN_Cory", sample_id))

P7_data_filtered1_Mysti <- P7_data_filtered1 %>%
  filter(grepl("P7_HSN_Mystinet", sample_id))

#___COUNT 

# Créer le graphique pour Cory
P7_plot_Cory_1 <- ggplot(P7_data_filtered1_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7: Proportion de LIVING et NOT LIVING par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 150, by = 10),  # Définir les graduations
    limits = c(0, 150)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_1 <- ggplot(P7_data_filtered1_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 150, by = 10),  # Définir les graduations
    limits = c(0, 150)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_1 + theme(legend.position = "bottom"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_1 <- P7_plot_Cory_1 + theme(legend.position = "none")
P7_plot_Mysti_1 <- P7_plot_Mysti_1 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_1 <- (P7_plot_Cory_1 + P7_plot_Mysti_1) / legend + plot_layout(ncol = 1, heights = c(10, 1))

# Afficher le graphique combiné
print(P7_combined_plot_1)

# Sauvegarder le graphique
ggsave("P7_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME

# Créer le graphique pour Cory
P7_plot_Cory_1 <- ggplot(P7_data_filtered1_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7: Proportion de LIVING et NOT LIVING par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 2.9, by = 0.1),  # Définir les graduations
    limits = c(0, 2.8)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_1 <- ggplot(P7_data_filtered1_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = living.not.living)) +
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 2.9, by = 0.1),  # Définir les graduations
    limits = c(0, 2.9)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_1 + theme(legend.position = "bottom"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_1 <- P7_plot_Cory_1 + theme(legend.position = "none")
P7_plot_Mysti_1 <- P7_plot_Mysti_1 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_1 <- (P7_plot_Cory_1 + P7_plot_Mysti_1) / legend + plot_layout(ncol = 1, heights = c(10, 1))

# Afficher le graphique combiné
print(P7_combined_plot_1)

# Sauvegarder le graphique
ggsave("P7_Proportion_de_LIVING_et_NOT_LIVING_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")


#______________________________________

###### 2. Graphique mettant en évidence la composition taxonomique par échantillon en regroupant les NOT LIVING


# Créer une nouvelle colonne category avec les regroupements souhaités
P7_data_transformed2 <- data_transformed_dilution_count %>%
  mutate(category = case_when(
    category %in% c("artefact","badfocus<artefact","bubble","cut<artefact","badfocus<other","broken","cyst<Dinophyceae",
                    "detritus","aggregates","dark<detritus","fecal pellets","transparent",
                    "molt","part<Crustacea","rods","spicule") ~ "NOT-LIVING",TRUE ~ category))
# Créer une palette de 32 couleurs distinctes en combinant différentes palettes
palette_brewer_1 <- brewer.pal(12, "Paired")
palette_brewer_2 <- brewer.pal(8, "Dark2")
palette_brewer_3 <- brewer.pal(8, "Set2")
custom_colors <- c("#D32F2F", "#1976D2", "#388E3C", "#FBC02D")
# Combiner les palettes pour créer une palette de 32 couleurs distinctes
palette_32 <- c(palette_brewer_1, palette_brewer_2, palette_brewer_3, custom_colors)
# Ajouter le noir pour "NOT-LIVING"
palette_33 <- c(palette_32, "#000000")
# Spécifiez les catégories à inclure
P7_categories_to_include2 <- c(living_categories_to_include,"NOT-LIVING") 
# Associer les couleurs aux catégories, avec "NOT-LIVING" en noir
color_mapping <- setNames(palette_33, P7_categories_to_include2)
# Filtrer les données pour ne conserver que "not-living" et "categories_to_include"
P7_data_filtered2 <- P7_data_transformed2 %>%
  filter(category %in% P7_categories_to_include2 & sample_id %in% sample_ids_to_include_P7)%>%
  mutate(category=factor(category, levels=c(living_categories_to_include,"NOT-LIVING")))



# Séparer les données en deux groupes
P7_data_filtered2_Cory <- P7_data_filtered2 %>%
  filter(grepl("P7_HSN_Cory", sample_id))

P7_data_filtered2_Mysti <- P7_data_filtered2 %>%
  filter(grepl("P7_HSN_Mystinet", sample_id))

#___COUNT 
# Créer le graphique pour Cory
P7_plot_Cory_2 <- ggplot(P7_data_filtered2_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "HSN Coryphaenae 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 140, by = 5),  # Définir les graduations
    limits = c(0, 140)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_2 <- ggplot(P7_data_filtered2_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 140, by = 5),  # Définir les graduations
    limits = c(0, 140)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_2 <- P7_plot_Cory_2 + theme(legend.position = "none")
P7_plot_Mysti_2 <- P7_plot_Mysti_2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_2 <- (P7_plot_Cory_2 | P7_plot_Mysti_2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_2)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_par_échantillon_en_regroupant_lesNOT-LIVING_COUNT.jpg", width = 25, height = 20, units = "cm")


#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour Cory
P7_plot_Cory_2.2 <- ggplot(P7_data_filtered2_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "HSN Coryphaenae 50 µm",
       y = "COunt",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_2.2 <- ggplot(P7_data_filtered2_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_2.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_2.2 <- P7_plot_Cory_2.2 + theme(legend.position = "none")
P7_plot_Mysti_2.2 <- P7_plot_Mysti_2.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_2.2 <- (P7_plot_Cory_2.2 | P7_plot_Mysti_2.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_2.2)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME

# Créer le graphique pour Cory
P7_plot_Cory_2 <- ggplot(P7_data_filtered2_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 3, by = 0.1),  # Définir les graduations
    limits = c(0, 3)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_2 <- ggplot(P7_data_filtered2_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 3, by = 0.1),  # Définir les graduations
    limits = c(0, 3)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))


# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_2 <- P7_plot_Cory_2 + theme(legend.position = "none")
P7_plot_Mysti_2 <- P7_plot_Mysti_2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_2 <- (P7_plot_Cory_2 | P7_plot_Mysti_2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_2)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_par_échantillon_en_regroupant_lesNOT-LIVING_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")


#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour Cory
P7_plot_Cory_2.2 <- ggplot(P7_data_filtered2_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique par échantillon en regroupant les NOT-LIVING",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_2.2 <- ggplot(P7_data_filtered2_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_2.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_2.2 <- P7_plot_Cory_2.2 + theme(legend.position = "none")
P7_plot_Mysti_2.2 <- P7_plot_Mysti_2.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_2.2 <- (P7_plot_Cory_2.2 | P7_plot_Mysti_2.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_2.2)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_par_échantillon_à_même_hauteur_en_regroupant_les_NOT-LIVING_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")

#______________________________

###### 3. Graphique mettant en évidence les détritus d'origine vivante par échantillon 


# Choix des catégories à mettre dans le graphique  
P7_categories_to_include3 <- c("spicule","molt","part<Crustacea")  # Spécifiez les catégories à inclure
# Filtrer les données pour ne conserver que "categories_to_include3" et "sample_ids_to_include_P5"
P7_data_filtered3 <- data_transformed_dilution_count %>%
  filter(category %in% P7_categories_to_include3 & sample_id %in% sample_ids_to_include_P7)
# Créer une palette de 3 couleurs (bleu, rouge, jaune)
palette_3 <- c("#0000FF", "#FF0000", "#FFFF00")  # Bleu, rouge, jaune
# Associer les couleurs aux catégories
color_mapping_3 <- setNames(palette_3, P7_categories_to_include3)

# Séparer les données en deux groupes
P7_data_filtered3_Cory <- P7_data_filtered3 %>%
  filter(grepl("P7_HSN_Cory", sample_id))

P7_data_filtered3_Mysti <- P7_data_filtered3 %>%
  filter(grepl("P7_HSN_Mystinet", sample_id))

#___COUNT

# Créer le graphique pour Cory
P7_plot_Cory_3 <- ggplot(P7_data_filtered3_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Détritus d'origine vivante par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 38, by = 2),  # Définir les graduations
    limits = c(0, 38)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_3 <- ggplot(P7_data_filtered3_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0, 38, by = 2),  # Définir les graduations
    limits = c(0, 38)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_3 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_3 <- P7_plot_Cory_3 + theme(legend.position = "none")
P7_plot_Mysti_3 <- P7_plot_Mysti_3 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_3 <- (P7_plot_Cory_3 | P7_plot_Mysti_3  | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_3)

# Sauvegarder le graphique
ggsave("P7_Détritus_d'origine_vivante_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")


#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour Cory
P7_plot_Cory_3.2 <- ggplot(P7_data_filtered3_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Détritus d'origine vivante par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_3.2 <- ggplot(P7_data_filtered3_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_3.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_3.2 <- P7_plot_Cory_3.2 + theme(legend.position = "none")
P7_plot_Mysti_3.2 <- P7_plot_Mysti_3.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_3.2 <- (P7_plot_Cory_3.2 | P7_plot_Mysti_3.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_3.2)

# Sauvegarder le graphique
ggsave("P7_Détritus_d'origine_vivante_même hauteur_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME 

# Créer le graphique pour Cory
P7_plot_Cory_3 <- ggplot(P7_data_filtered3_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Détritus d'origine vivante par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolulme",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0,0.09, by = 0.01),  # Définir les graduations
    limits = c(0,0.09)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_3 <- ggplot(P7_data_filtered3_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0,0.09, by = 0.01),  # Définir les graduations
    limits = c(0,0.09)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_3 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_3 <- P7_plot_Cory_3 + theme(legend.position = "none")
P7_plot_Mysti_3 <- P7_plot_Mysti_3 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_3 <- (P7_plot_Cory_3 | P7_plot_Mysti_3  | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_3)

# Sauvegarder le graphique
ggsave("P7_Détritus_d'origine_vivante_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")


#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour Cory
P7_plot_Cory_3.2 <- ggplot(P7_data_filtered3_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Détritus d'origine vivante par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_3.2 <- ggplot(P7_data_filtered3_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping_3) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_3.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_3.2 <- P7_plot_Cory_3.2 + theme(legend.position = "none")
P7_plot_Mysti_3.2 <- P7_plot_Mysti_3.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_3.2 <- (P7_plot_Cory_3.2 | P7_plot_Mysti_3.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_3.2)

# Sauvegarder le graphique
ggsave("P7_Détritus_d'origine_vivante_même_hauteur_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")


#______________________________

###### 4. Graphique mettant en evidence la composition taxonomique par échantillon sans prendre en compte les NOT LIVING


# Filtrer les données pour ne conserver que "living_categories_to_include" et "sample_ids_to_include_P2"
P7_data_filtered4 <- data_transformed_dilution_count %>%
  filter(category %in% living_categories_to_include & sample_id %in% sample_ids_to_include_P7)

# Séparer les données en deux groupes
P7_data_filtered4_Cory <- P7_data_filtered4 %>%
  filter(grepl("P7_HSN_Cory", sample_id))

P7_data_filtered4_Mysti <- P7_data_filtered4 %>%
  filter(grepl("P7_HSN_Mystinet", sample_id))

#__ COUNT

# Créer le graphique pour Cory
P7_plot_Cory_4 <- ggplot(P7_data_filtered4_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique du vivant par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0,42, by = 2),  # Définir les graduations
    limits = c(0,42)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_4 <- ggplot(P7_data_filtered4_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0,42, by = 2),  # Définir les graduations
    limits = c(0,42)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_4 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_4 <- P7_plot_Cory_4 + theme(legend.position = "none")
P7_plot_Mysti_4 <- P7_plot_Mysti_4 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_4 <- (P7_plot_Cory_4 | P7_plot_Mysti_4  | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_4)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_du_vivant_par_échantillon_COUNT.jpg", width = 25, height = 20, units = "cm")


#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour Cory
P7_plot_Cory_4.2 <- ggplot(P7_data_filtered4_Cory, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique du vivant par échantillon, barres de même hauteur",
       x = "HSN Coryphaenae 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_4.2 <- ggplot(P7_data_filtered4_Mysti, aes(x = sample_id, y = count)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Count",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_4.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_4.2 <- P7_plot_Cory_4.2 + theme(legend.position = "none")
P7_plot_Mysti_4.2 <- P7_plot_Mysti_4.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_4.2 <- (P7_plot_Cory_4.2 | P7_plot_Mysti_4.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_4.2)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_par_échantillon_à_même_hauteur_COUNT.jpg", width = 25, height = 20, units = "cm")

#___BIOVOLUME

# Créer le graphique pour Cory
P7_plot_Cory_4 <- ggplot(P7_data_filtered4_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique du vivant par échantillon",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0,0.07, by = 0.01),  # Définir les graduations
    limits = c(0,0.07)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_4 <- ggplot(P7_data_filtered4_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category)) +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  scale_y_continuous(
    breaks = seq(0,0.07, by = 0.01),  # Définir les graduations
    limits = c(0,0.07)             # Définir les limites de l'axe y
  ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_4 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_4 <- P7_plot_Cory_4 + theme(legend.position = "none")
P7_plot_Mysti_4 <- P7_plot_Mysti_4 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_4 <- (P7_plot_Cory_4 | P7_plot_Mysti_4  | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_4)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_du_vivant_par_échantillon_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")


#_____

# Graphique avec les barres à la même hauteur pour facilier la lecture 
# Créer le graphique pour Cory
P7_plot_Cory_4.2 <- ggplot(P7_data_filtered4_Cory, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "P7 : Composition taxonomique du vivant par échantillon, barres de même hauteur",
       x = "HSN Coryphaenae 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text( size = 12, face = "bold")) 


# Créer le graphique pour Mysti
P7_plot_Mysti_4.2 <- ggplot(P7_data_filtered4_Mysti, aes(x = sample_id, y = biovolume)) +
  geom_col(aes(fill = category), col="black",position="fill") +
  scale_fill_manual(values = color_mapping) +  # Utiliser la palette définie
  scale_x_discrete(labels = P4_new_labels) +  # Modifier les étiquettes de l'axe des abscisses
  theme_minimal() +
  labs(title = "",
       x = "HSN Mystinet 50 µm",
       y = "Biovolume",
       fill = "Catégorie") +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

# Extraire la légende d'un des graphiques
legend <- get_legend(P7_plot_Cory_4.2 + theme(legend.position = "right"))

# Supprimer les légendes des graphiques individuels
P7_plot_Cory_4.2 <- P7_plot_Cory_4.2 + theme(legend.position = "none")
P7_plot_Mysti_4.2 <- P7_plot_Mysti_4.2 + theme(legend.position = "none")

# Combiner les graphiques côte à côte et ajouter la légende en bas
P7_combined_plot_4.2 <- (P7_plot_Cory_4.2 | P7_plot_Mysti_4.2 | legend) + 
  plot_layout(ncol = 3, widths = c(10, 10, 7))

# Afficher le graphique combiné
print(P7_combined_plot_4.2)

# Sauvegarder le graphique
ggsave("P7_Composition_taxonomique_par_échantillon_à_même_hauteur_BIOVOLUME.jpg", width = 25, height = 20, units = "cm")
