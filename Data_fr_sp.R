setwd("C:/R/STAGE/")
#Charger les packages
library(dplyr)
library(taxize)
#install.packages("vegan")
library(vegan)
#install.packages("ade4")
library(ade4)
library(ggplot2)
#install.packages("pheatmap")
library(pheatmap)
#install.packages("ggtern")
#library(ggtern)
#install.packages("factoextra")
library(factoextra)
#install.packages("FactoMineR")
library(FactoMineR)
#install.packages("htmltools")
library(htmltools)
#install.packages("rgbif")
library(rgbif)
#install.packages("stringr")
library(stringr)
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("plot3D")
library(plot3D)
#install.packages("rgl")
library(rgl)
#install.packages("ggrepel")
library(ggrepel)
library(sf)
#library(tidyverse)
library(conflicted)

##LECTURE DES DONNEES ET MODIFICATION JEU DE DONNEES
#data_fr_sp <- read.csv("Data_fr_sp_traits.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)
#data_fr_sp <- dplyr::select(data_fr_sp, -c(Heights5, Individu, BMA..g., Leaf..g., BS, SLA, IC, Age, SLA.10))
#data_fr_sp <- data_fr_sp %>%
#  rename(BS = meanBS,
#         BD = BD..cm.)
#GPS <- data.frame(lat = data_fr_sp$Lat, long = data_fr_sp$Long)

#data_fr_sp$Species <- gsub("_", " ", data_fr_sp$Species)
#data_fr_sp <- na.omit(data_fr_sp)
#data_fr_sp=as.data.frame(data_fr_sp)
#head(data_fr_sp)
#Mettre à jour les bons noms d'espèces
#unique_species <- unique(data_fr_sp$Species)
#resolved_species <- gnr_resolve(sci = unique_species)
#filtered_species <- resolved_species[resolved_species$data_source_title == "Arctos", ]
#updated_names <- setNames(filtered_species$matched_name, filtered_species$submitted_name)
#correspondance <- data.frame(
#  species_original = filtered_species$user_supplied_name,
#  species_update = filtered_species$matched_name)
#correspondance <- correspondance[!duplicated(correspondance$species_original), ]
#data_fr_sp <- merge(data_fr_sp, correspondance, by.x = "Species", by.y = "species_original", all.x = TRUE)
#data_fr_sp$Species <- NULL
#data_fr_sp <- data.frame(Species = data_fr_sp$species_update, data_fr_sp[, !names(data_fr_sp) %in% "species_update"])
#write.csv(data_fr_sp, "data_fr_sp.csv", row.names = FALSE)
#Création d'un df en aggrégeant les données par la moyenne
#data_fr_sp_clean <- read.csv("data_fr_sp.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)
#write.csv(data_fr_sp_clean, "data_fr_sp_clean.csv", row.names = FALSE)
data_fr_sp_final <- read.csv("data_fr_sp_clean.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)

mean_data_fr_sp <- as.data.frame(aggregate(data_fr_sp_final,by=list(as.factor(data_fr_sp_final$Species)),mean))
mean_data_fr_sp$Species=NULL
row.names(mean_data_fr_sp)= mean_data_fr_sp$Group.1
mean_data_fr_sp$Group.1=NULL
mean_data_fr_sp$BS = -mean_data_fr_sp$BS

data_agg_mean <- select(mean_data_fr_sp, IC_class, BudP, BudA, BGR, BS, H5class, LCE)


#write.csv(mean_data_fr_sp,NULL#write.csv(mean_data_fr_sp, "mean_data_fr_sp.csv", row.names = FALSE)
#mean_data_fr_sp <- read.csv("mean_data_fr_sp.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)
#Ouverture des fichiers SCORES SA et FR
#Standardiser 
stand_data_fr_sp = decostand(mean_data_fr_sp,"stand",2)
stand_data_fr_sp_sans_score <- stand_data_fr_sp
stand_data_fr_sp_sans_score$score_fire=NULL
stand_data_fr_sp_sans_score$score_herb=NULL
stand_data_fr_sp_sans_score$score_cover=NULL
stand_data_fr_sp_score <- stand_data_fr_sp[, c("score_fire", "score_herb", "score_cover")]
stand_data_fr_sp_score <- stand_data_fr_sp_score %>%
  rename(Feu = score_fire,
         Herbivorie = score_herb,
         Ombre = score_cover)
write.csv(stand_data_fr_sp_sans_score, "stand_data_fr_sp_sans_score.csv")
write.csv(stand_data_fr_sp, "stand_data_fr_sp.csv")

data_mean_stand <- decostand(data_agg_mean, "stand", 2)
#Rang
range_data_fr_sp = decostand(mean_data_fr_sp,"range",2)
range_data_fr_sp_sans_score <- stand_data_fr_sp
range_data_fr_sp_sans_score$score_fire=NULL
range_data_fr_sp_sans_score$score_herb=NULL
range_data_fr_sp_sans_score$score_cover=NULL
range_data_fr_sp_score <- range_data_fr_sp[, c("score_fire", "score_herb", "score_cover")]
range_data_fr_sp_score <- range_data_fr_sp_score %>%
  rename(Feu = score_fire,
         Herbivorie = score_herb,
         Ombre = score_cover)
write.csv(range_data_fr_sp_sans_score, "range_data_fr_sp_sans_score.csv")
write.csv(range_data_fr_sp, "range_data_fr_sp.csv")

setwd("C:/R/STAGE/")
#data_score_fr_sp <- stand_data_fr_sp_sans_score
#data_score_fr_sp <- select(data_score_fr_sp, IC_class, BudP, BudA, BGR, BS, H5class, LCE)

#Calculer les scores
data_score <- read.csv("scoreSA.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE) 
rownames(data_score) <- data_score[,1]
data_score <- data_score[,-1]
#Création de la matrice data_score_matrix pour calculer les scores
data_score_matrix <- matrix(NA,dim(data_mean_stand)[1],3)
rownames(data_score_matrix)=rownames (data_mean_stand)
colnames(data_score_matrix)=c('FEU','HERB','COUV')
for (i in 1:dim(data_mean_stand)[1])  {
  data_score_matrix[i,1]= sum(data_mean_stand[i,]*data_score[1,])
  data_score_matrix[i,2]= sum(data_mean_stand[i,]*data_score[2,])
  data_score_matrix[i,3]= sum(data_mean_stand[i,]*data_score[3,])
}
data_score_df <- as.data.frame(data_score_matrix)
data_score_df_range <- decostand(data_score_df, "range", 2)
write.csv(data_score_df, "data_score_df.csv")


##ANALYSE STATISTIQUE
#ACP + biplot
res.pca <- PCA(stand_data_fr_sp_sans_score, graph = T)
set.seed(123) # Pour la reproductibilité
wss <- numeric(15)
for (k in 2:15) {
  kmeans_result <- kmeans(res.pca$ind$coord[, 1:2], centers = k) # Utiliser seulement les deux premières composantes principales
  wss[k] <- kmeans_result$tot.withinss
}
plot(2:15, wss[2:15], type = "b", xlab = "Nombre de Clusters", ylab = "Somme des carrés intra-clusters", main = "Méthode du Coude")
res.km <- kmeans(res.pca$ind$coord[, 1:2], centers = 3) 

fviz_pca_ind(res.pca,
             geom.ind = "point", # Utiliser des points pour représenter les individus
             col.ind = as.factor(res.km$cluster), # Colorer les points selon l'information de cluster
             palette = c("black", "darkorange", "darkgreen"), # Définir une palette de couleurs
             addEllipses = T, # Ajouter des ellipses autour des clusters si désiré
             label = "row", # Ajouter des étiquettes pour les individus
             title = " ")


# Visualiser les variables
fviz_pca_var(res.pca,
             col.var = "cos2", # Colorer par le cos2 des variables
             gradient.cols = c("darkgreen", "darkorange"), # Utiliser une palette allant du blanc au bleu
             title = " ")


#Corrélation
cor_feu_herbivorie <- cor(data_score_df$FEU, data_score_df$HERB, method = "pearson")
cor_feu_ombre <- cor(data_score_df$FEU, data_score_df$COUV, method = "pearson")
cor_herbivorie_ombre <- cor(data_score_df$HERB, data_score_df$COUV, method = "pearson")

cor_tableau <- data.frame(
  'Correlation Entre' = c('Feu et Herbivorie', 'Feu et Ombre', 'Herbivorie et Ombre'),
  'Coefficient de Correlation' = c(cor_feu_herbivorie, cor_feu_ombre, cor_herbivorie_ombre)
)
print(cor_tableau)
data_score_df$Species=NULL
data_score_df <- data_score_df %>%
  rename(Feu = FEU,
         Ombre = COUV,
         Herbivorie = HERB)
pheatmap(data_score_df,
         color = colorRampPalette(c("darkblue", "white", "darkred"))(100), # Dégradé de couleurs
         scale = "none", # Les données sont déjà standardisées
         clustering_distance_rows = "euclidean", # Distance euclidienne pour le clustering des lignes
         clustering_distance_cols = "euclidean", # Distance euclidienne pour le clustering des colonnes
         clustering_method = "complete", # Méthode de clustering
         show_rownames = TRUE, # Afficher les noms des lignes
         show_colnames = TRUE, # Afficher les noms des colonnes
         fontsize_row = 10, # Taille de police pour les noms des lignes
         fontsize_col = 10, # Taille de police pour les noms des colonnes
         main = "") # Titre de la heatmap

#Création du plot triangle
data_triangle <- data_score_df_range %>%
  rename(Feu = FEU,
         Herbivorie = HERB,
         Ombre = COUV)
set.seed(123) # Pour la reproductibilité
clusters <- kmeans(data_triangle[, c("Feu", "Ombre", "Herbivorie")], centers = 3)
data_triangle$cluster <- clusters$cluster
data_triangle$cluster <- factor(data_triangle$cluster,
                                         levels = c(1, 2, 3),
                                         labels = c("Ombre", "Herbivorie pyrique", "Feu et Herbivorie"))
ggtern(data_triangle, ggtern::aes(x = Feu, y = Ombre, z = Herbivorie, color = cluster)) +
  geom_point() +
  scale_color_manual(name = " ",
  values = c("Feu et Herbivorie" = "darkgreen", "Ombre" = "black", "Herbivorie pyrique" = "darkorange")) +
  theme_arrowsmall() +
  ggtitle(" ")
#Régression linéaire 
#lm_data <- lm(score_fire ~ score_herb + score_cover, data = range_data_fr_sp)
#summary(lm_data)
#lm(formula = score_fire ~ score_herb, data= range_data_fr_sp)


# Score de feu en fonction du score d'herbivorie
data_score_df_range$Species <- rownames(data_score_df_range)
ggplot(data_score_df_range, ggplot2::aes(x = FEU, y = HERB)) +
  geom_point() +
  geom_text_repel(ggplot2::aes(label = Species), size = 3) +  # Utilisation de ggrepel pour éviter les chevauchements
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +  # Ajout de la droite de régression avec intervalle de confiance
  labs(x = "Score de HERB", y = "Score de FEU", title = "Relation entre HERB et FEU") +
  theme_grey()

model_feu_herb <- lm(FEU ~ HERB, data = data_score_df_range)
summary_model_feu_herb <- summary(model_feu_herb)
p_value_feu_herb <- summary_model_feu_herb$coefficients[2, 4]

ggplot(data_score_df_range, ggplot2::aes(x = FEU, y = HERB)) +
  geom_point() +
  geom_text_repel(ggplot2::aes(label = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(x = "Score d'Herbivorie", y = "Score de Feu", title = "Relation entre la tolérance au Feu et la tolérance à l'Herbivorie") +
  theme_grey() +
  ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, label = paste("P-value:", format(p_value_feu_herb, digits = 2)), size = 4)
# Score d'herbivorie en fonction du score de couverture
ggplot(data_score_df_range, ggplot2::aes(x = HERB, y = COUV)) +
  geom_point() +
  geom_text_repel(ggplot2::aes(label = Species), size = 3) +  # Utilisation de ggrepel pour éviter les chevauchements
  geom_smooth(method = "lm", se = TRUE, color = "brown4") +  # Ajout de la droite de régression avec intervalle de confiance
  labs(x = "Score d'Herbivorie", y = "Score d'Ombre", title = "Relation entre la tolérance à l'Ombre et à l'Herbivorie")+
  theme_grey()

model_herb_cover <- lm(HERB ~ COUV, data = data_score_df_range)
summary_model_herb_cover <- summary(model_herb_cover)
p_value_herb_cover <- summary_model_herb_cover$coefficients[2, 4]

ggplot(data_score_df_range, ggplot2::aes(x = HERB, y = COUV)) +
  geom_point() +
  geom_text_repel(ggplot2::aes(label = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(x = "Score d'Herbivorie", y = "Score d'Ombre", title = "Relation entre la tolérance à l'Ombre et à l'Herbivorie") +
  theme_grey() +
  ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, label = paste("P-value:", format(p_value_herb_cover, digits = 2)), size = 4)
# Score de feu en fonction du score de couverture
ggplot(data_score_df_range, ggplot2::aes(x = FEU, y = COUV)) +
  geom_point() +
  geom_text_repel(ggplot2::aes(label = Species), size = 3) +  # Utilisation de ggrepel pour éviter les chevauchements
  geom_smooth(method = "lm", se = TRUE, color = "brown4") +  # Ajout de la droite de régression avec intervalle de confiance
  labs(x = "Score d'Herbivorie", y = "Score d'Ombre", title = "Relation entre la tolérance à l'Ombre et à l'Herbivorie")+
  theme_grey()

model_fire_cover <- lm(FEU ~ COUV, data = data_score_df_range)
summary_model_fire_cover <- summary(model_fire_cover)
p_value_fire_cover <- summary_model_fire_cover$coefficients[2, 4]

ggplot(data_score_df_range, ggplot2::aes(x = FEU, y = COUV)) +
  geom_point() +
  geom_text_repel(ggplot2::aes(label = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "brown4") +
  labs(x = "Score de Feu", y = "Score d'Ombre", title = "Relation entre la tolérance à l'Ombre et la tolérance au Feu") +
  theme_grey() +
  ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, label = paste("P-value:", format(p_value_fire_cover, digits = 2)), size = 4)

#Anova à deux facteurs
AOV_data <-aov(Feu~Herbivorie + Ombre, data = data_score_df)
summary(AOV_data)
AOV_data_feu <-aov(Herbivorie~Feu + Ombre, data = data_score_df)
summary(AOV_data_feu)
AOV_data_herb <-aov(Feu~Ombre, data = data_score_df)


#Télécharger les données d'occurence
spplist = gsub("_"," ", rownames(mean_data_fr_sp))
taxa = spplist
options(timeout= 400000)
#for(j in 1:length(taxa)){
#  my.gbif.data<-occ_search(scientificName = taxa[j], hasCoordinate = T, hasGeospatialIssue = F, limit = 10000, fields = c("name","countryCode","decimalLatitude","decimalLongitude"))
#write.table(my.gbif.data$data, paste(taxa[j],".txt"))
#}

#Crée une liste appelée "tempq" qui contient les noms de fichier de tous les fichiers dans le répertoire de travail actuel ayant l'extension ".txt"
setwd("C:/R/STAGE/Species/")
tempq=list.files(pattern="*.txt") #make sure you don't have other txt file than occ data in the working dir before running this command
primer=as.data.frame(matrix(NA, 1,5)) # primer
colnames(primer)=c("rep","latitude", "longitude","country","sp") 
for(p in 1:length(tempq)){ 
  temp2=read.table(tempq[p], header = TRUE, row.names=NULL)
  temp2$sp= tempq[p]
  if(colnames(temp2)[2]=="x")(matrix(c(0,tempq[p],NA,NA,NA),1,5))else(temp2)
  colnames(temp2)=c("rep","latitude", "longitude","country","sp")
  primer <- rbind(primer, temp2[duplicated(temp2)==FALSE,])
} 

primer=primer[-1,] #retire le primer
primer$sp=str_sub(primer$sp, end=-6) #les 5 derniers caractères de chaque élément de la colonne "sp" sont supprimés
write.csv(primer,'list_species.csv')
setwd("C:/R/STAGE/Species")
list_species <- read.csv('list_species.csv')

#Création d'un fond de carte
worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')

#CARTOGRAPHIE
setwd("C:/R/STAGE/Species")

for (i in 1:length(tempq)) {
  spname = str_sub(tempq[i], end = -6)  
  sp = as.data.frame(read.table(tempq[i]))
  ggplot() +   # Plot the map    
    geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
    coord_sf(xlim = c(-20, 45), ylim = c(30, 55), expand = FALSE)   +
    geom_point(ggplot2::aes(x = sp$decimalLongitude,  # Specify the x axis as longitude
                  y = sp$decimalLatitude),  # Colour the points based on species name
               colour = 'lightgreen',
               shape = 16,
               alpha = 0.1,  # Set point opacity to 40%
               size = 0.5) +  # Set point size to 1
    ggtitle(spname) +      
    ggplot2::theme_classic() +
    theme(panel.background = element_rect(fill = "#2C3E4F", colour = "navyblue"))   # #2C3E4F 
  ggplot2::ggsave(file.path("C:/R/STAGE/Maps", paste0(spname, ".png")),
         width = 40, height = 22, units = "cm")
}

#Création d'un tableau dans lequel on rajoute les latitudes et longitudes pour les espèces ainsi que des colonnes pour les scores de traits
maps <- list_species
maps$rep = NULL
maps$country=NULL
maps$X=NULL
maps$longitude=round(maps$longitude,2)
maps$latitude=round(maps$latitude,2)
maps=unique(maps)
maps$FEU=0
maps$HERB=0
maps$COUV=0
maps$Genus_species=gsub(" ","_", maps$sp)
rownames(data_score_df_range)=levels(as.factor(maps$Genus_species))
#Création d'un fichier maps avec les données de score standardisées
for (i in 1:length(levels(as.factor(maps$Genus_species))))   {
  maps$FEU[maps$Genus_species==levels(as.factor(maps$Genus_species))[i]]=data_score_df_range[levels(as.factor(maps$Genus_species))[i],1]
  maps$HERB[maps$Genus_species==levels(as.factor(maps$Genus_species))[i]]=data_score_df_range[levels(as.factor(maps$Genus_species))[i],2]
  maps$COUV[maps$Genus_species==levels(as.factor(maps$Genus_species))[i]]=data_score_df_range[levels(as.factor(maps$Genus_species))[i],3]
} #utilisation des données standardisées
write.csv(maps, "maps.csv")
setwd("C:/R/STAGE/Species")

maps <- read.csv("maps.csv")
#Création de maps_clean
maps_clean = maps
maps_clean$sp=NULL
maps_clean$Genus_species=NULL
#write.csv(maps_clean, "maps_clean.csv")
#Création d'un tableau avec les scores de feu, herb et couv sous forme 0 ou 1
for (i in 1:dim(maps_clean)[1]){
  maps_clean$feu01[i]= if(maps_clean$FEU[i]==max(maps_clean[i,3:5]))(1)else(0)    
  maps_clean$herb01[i]= if(maps_clean$HERB[i]==max(maps_clean[i,3:5]))(1)else(0)  
  maps_clean$couv01[i]= if(maps_clean$COUV[i]==max(maps_clean[i,3:5]))(1)else(0)  
}
#write.csv(maps_clean, "maps_clean.csv")
maps_aggregate = aggregate(maps_clean, list(maps_clean$latitude,maps_clean$longitude), mean)
#write.csv(maps_aggregate, "maps_aggregate.csv")
setwd('C:/R/STAGE/Maps/')
maps_aggregate <- read.csv('maps_aggregate.csv')
maps_med <- maps_aggregate %>%
  dplyr::filter(latitude >= 30, latitude <= 50) %>%
  dplyr::filter(longitude >= -20, longitude <= 40)
#write.csv(maps_med, "maps_med.csv")
setwd("C:/R/STAGE/Maps/")
#Création de cartes pour QGIS
maps_med <- maps_med %>%
  select(-Group.1, -Group.2)
#write.csv(maps_med, "maps_med.csv")
maps_med_feu <- maps_med %>%
  dplyr::filter(feu01 == 1)
#write.csv(maps_med_feu, "maps_med_feu.csv")
maps_med_couv <- maps_med %>%
  dplyr::filter(couv01 == 1)
#write.csv(maps_med_couv, "maps_med_couv.csv")
maps_med_herb <- maps_med %>%
  dplyr::filter(herb01 == 1)
#write.csv(maps_med_herb, "maps_med_herb.csv")

maps_med_round <- maps_med
maps_med_round$latitude = round(maps_med$latitude,1)
maps_med_round$longitude = round(maps_med$longitude,1)
maps_med_round_agg <- aggregate(maps_med_round, list(maps_med_round$latitude,maps_med_round$longitude), sum)
maps_med_round_agg <- maps_med_round_agg %>%
  rename(lat = Group.1,
         long = Group.2) %>%
  select(-latitude, -longitude)

#write.csv(maps_med_round_agg, "maps_med_aggregate_by_sum.csv")
setwd("C:/Qgis/")
maps_med_round_agg_feu <- maps_med_round_agg %>%
  dplyr::filter(feu01 > herb01 & feu01 > couv01) %>%
  select(-FEU, -HERB, -COUV, -herb01, -couv01)
write.csv(maps_med_round_agg_feu,"maps_aggregate_feu.csv")
maps_med_round_agg_herb <- maps_med_round_agg %>%
  dplyr::filter(herb01 > feu01 & herb01 > couv01) %>%
  select(-FEU, -HERB, -COUV, -feu01, -couv01)
write.csv(maps_med_round_agg_herb,"maps_aggregate_herb.csv")
maps_med_round_agg_couv <- maps_med_round_agg %>%
  dplyr::filter(couv01 > feu01 & couv01 > herb01) %>%
  select(-FEU, -HERB, -COUV, -feu01, -herb01)
write.csv(maps_med_round_agg_couv,"maps_aggregate_couv.csv")


#maps_med <- maps_med[, c("longitude", "latitude", "FEU", "HERB", "COUV", "feu01", "herb01", "couv01")]
#transformation de maps_med en données sf
maps_med_sf <- st_as_sf(maps_med, coords = c("longitude", "latitude"), crs = 4326)
maps_clean_aggregate_sf <- st_as_sf(maps_clean_aggregate, coords = c("longitude", "latitude"), crs = 4326)
st_write(maps_med_sf, "maps_med_sf.shp", append=T)
st_write(maps_med_sf, "maps_med_sf_json.geojson", append=T)
st_write(maps_clean_aggregate_sf, "maps_clean_aggregate_sf.geojson")

#Essayer dans QGIS, mais problème avec les coordonnées

nombre_feu01 <- sum(maps_med$feu01 == 1, na.rm = TRUE)
nombre_herb01 <- sum(maps_med$herb01 == 1, na.rm = TRUE)
nombre_couv01 <- sum(maps_med$couv01 == 1, na.rm = TRUE)

##CREATION DES CARTES DES SCORES DE TRAITS + CARTE DE LA DIVERSITE + CARTES CLIMATS
#Création de la carte pour les scores de feu à l'échelle du Bassin méd
ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(ggplot2::aes(x = maps_aggregate$longitude,  # Specify the x axis as longitude
                 y = maps_aggregate$latitude),  # Colour the points based on species name
            colour=c('black','orange')[maps_aggregate$feu01+1] ,
             shape=15,
             alpha = 0.4,  # 0.2 --> bien
             size = 0.01) +  # 0.01 --> bien
  ggtitle("All spp") +      
  ggplot2::theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  
ggplot2::ggsave("maps_feu_med.png",width = 40, height = 22, units = "cm")


#Création de la carte pour les scores d'herbivorie à l'échelle du Bassin méditerranéen
ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(ggplot2::aes(x = maps_aggregate$longitude,  # Specify the x axis as longitude
                          y = maps_aggregate$latitude),  # Colour the points based on species name
             colour=c('black','chartreuse')[maps_aggregate$herb01+1],
             shape=15,
             alpha = 0.4,  # Set point opacity to 40%
             size = 0.01) +  # Set point size to 1
  ggtitle("All spp") +      
  ggplot2::theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  
ggplot2::ggsave("carte_herb_med.png",width = 40, height = 22, units = "cm")

#Création d'un plot pour la carte des scores de couverture à l'échelle du Bassin méd
ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(ggplot2::aes(x = maps_aggregate$longitude,  # Specify the x axis as longitude
                 y = maps_aggregate$latitude),  # Colour the points based on species name
             colour=c('black','brown')[maps_aggregate$couv01+1],
             shape=15,
             alpha = 0.4,  # Set point opacity to 40%
             size = 0.01) +  # Set point size to 1
  ggtitle("All spp") +      
  ggplot2::theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  
ggplot2::ggsave("carte_couv_med.png",width = 40, height = 22, units = "cm")

#Création d'une carte de diversité
install.packages("viridis")
library(viridis)
color.gradient <- function(x, colors=c("navyblue","cyan","lightblue","lightyellow","lightgreen", "chartreuse","green"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

div=maps
div$longitude=round(div$longitude,2)
div$latitude=round(div$latitude,2)
div=unique(maps)
div$un=1
Div_agg = aggregate(div$un, list(div$longitude,div$latitude), sum)
setwd("C:/Qgis")
write.csv(Div_agg, "diversite.csv")

color.gradient <- function(x, colors=c("navyblue","cyan","lightblue","lightyellow","lightgreen", "chartreuse","green"), colsteps=100) {
  # Créer une palette de couleurs
  palette <- colorRampPalette(colors)(colsteps)
  # Assigner une couleur à chaque valeur de x basée sur son intervalle
  return(palette[findInterval(x, seq(min(x), max(x), length.out=colsteps))])
}
Div_agg$colors <- color.gradient(Div_agg$x)
ggplot() +
  geom_sf(data = worldmap, colour = "blue", fill = "black") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = Div_agg, aes(x = Group.1, y = Group.2, colour = colors), shape = 15, alpha = 0.4, size = 0.01) +
  scale_colour_identity() + # Pour utiliser les couleurs directement telles quelles dans 'colour'
  ggtitle("") +      
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#2C3E4F", colour = "black"))
ggplot2::ggsave("carte_div.png",width = 40, height = 22, units = "cm", dpi=600)

ggplot() +
  geom_sf(data = worldmap, colour = "blue", fill = "black") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = Div_agg, ggplot2::aes(x = Group.1, y = Group.2, colour = color.gradient(Div_agg$x)), shape = 15, alpha = 0.4, size = 0.01) +
  ggtitle("") +      
  labs(x = NULL, y = NULL) +
  ggplot2::theme_classic() +
  theme(panel.background = element_rect(fill = "#2C3E4F", colour = "black"))  
ggplot2::ggsave("carte_div.png",width = 40, height = 22, units = "cm", dpi=600)

##TEST PAR PERMUTATION
#données à charger avant d'effectuer le code 
setwd("C:/R/STAGE/Species")
traits_stand <- data_mean_stand
#x=read.csv('x.csv')
#FullTraitsFrStand=read.csv('FULLTRAITSFRSTAND.csv',dec=".",header=TRUE,row.names=1)
#SASCORES=read.csv("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre/fullscoreSA.csv",dec=".",header=TRUE,row.names=1) 

#maps <- list_species
#maps$rep = NULL
#maps$country=NULL
#maps$X=NULL
#maps$longitude=round(maps$longitude,2)
#maps$latitude=round(maps$latitude,2)
#maps=unique(maps)
#maps$FEU=0
#maps$HERB=0
#maps$COUV=0
#maps$Genus_species=gsub(" ","_", maps$sp)
#rownames(data_score_df_range)=levels(as.factor(maps$Genus_species))

#w=x
#w$rep=NULL
#w$country=NULL
#w$X=NULL
#w$longitude=round(w$longitude,2)
#w$latitude=round(w$latitude,2)
#w=unique(w)
#w$feu=0
#w$herb=0
#w$couv=0
#w$Genus_species=gsub(" ","_", w$sp)
#rownames(AFF_FR)=levels(as.factor(w$Genus_species))
#for (i in 1:length(levels(as.factor(w$Genus_species))))   {
#  w$feu[w$Genus_species==levels(as.factor(w$Genus_species))[i]]=AFF_FR[levels(as.factor(w$Genus_species))[i],1]
#  w$herb[w$Genus_species==levels(as.factor(w$Genus_species))[i]]=AFF_FR[levels(as.factor(w$Genus_species))[i],2]
#  w$couv[w$Genus_species==levels(as.factor(w$Genus_species))[i]]=AFF_FR[levels(as.factor(w$Genus_species))[i],3]
#}
#W=w
#W$sp=NULL
#W$Genus_species=NULL 
#Wagg= aggregate(W, list(W$longitude,W$latitude), mean)

#Jeu de données simulés
nperm=10 #nombre de permutation
matrice_feu <-matrix(NA, nrow=113529, ncol=nperm)#création des matrices
matrice_herb <- matrix(NA, nrow=113529, ncol=nperm)
matrice_couv <- matrix(NA, nrow=113529, ncol=nperm)
maps_aggregate$X=NULL
maps_aggregate <- maps_aggregate %>%
  rename (feu = FEU,
          herb = HERB,
          couv = COUV)


for (j in 1:nperm){ #lancement de la boucle
  sim=traits_stand
  sim=apply(traits_stand, 2, FUN=sample)#sample des colonnes (2) de FullTraitsFrStand
  a=Sys.time()
  for (l in 1:dim(sim)[1])  {
    data_score_matrix[l,1]= sum(sim[l,]*data_score[1,])
    data_score_matrix[l,2]= sum(sim[l,]*data_score[2,])
    data_score_matrix[l,3]= sum(sim[l,]*data_score[3,])
  }
  
  w_sim=list_species #calcul de w_sim 
  w_sim$rep=NULL
  w_sim$country=NULL
  w_sim$X=NULL
  w_sim$longitude=round(w_sim$longitude,2)
  w_sim$latitude=round(w_sim$latitude,2)
  w_sim=unique(w_sim)
  w_sim$feu=0
  w_sim$herb=0
  w_sim$couv=0
  w_sim$Genus_species=gsub(" ","_", w_sim$sp)
  rownames(data_score_matrix)=levels(as.factor(w_sim$Genus_species))
  
  for (i in 1:length(levels(as.factor(w_sim$Genus_species))))   {
    w_sim$feu[w_sim$Genus_species==levels(as.factor(w_sim$Genus_species))[i]]=data_score_matrix[levels(as.factor(w_sim$Genus_species))[i],1]
    w_sim$herb[w_sim$Genus_species==levels(as.factor(w_sim$Genus_species))[i]]=data_score_matrix[levels(as.factor(w_sim$Genus_species))[i],2]
    w_sim$couv[w_sim$Genus_species==levels(as.factor(w_sim$Genus_species))[i]]=data_score_matrix[levels(as.factor(w_sim$Genus_species))[i],3]
  }
  W_sim=w_sim #calcul de W_sim_agg (aggregate)
  W_sim$sp=NULL
  W_sim$Genus_species=NULL 
  W_sim_agg= aggregate(W_sim, list(W_sim$longitude,W_sim$latitude), mean)
  b=Sys.time()
  b-a
  matrice_feu[,j]=(W_sim_agg$feu)<(maps_aggregate$feu)
  matrice_herb[,j]=(W_sim_agg$herb)<(maps_aggregate$herb)
  matrice_couv[,j]=(W_sim_agg$couv)<(maps_aggregate$couv)
  
}

#calul des p-value
1-(apply(matrice_feu*1,1,sum)/nperm) #calcul des p_value
apply(matrice_feu*1,1,sum)/nperm
counts_feu <- apply(matrice_feu*1, 1, sum)
counts_herb <- apply(matrice_herb*1, 1, sum)
counts_couv <- apply(matrice_couv*1, 1, sum)
scorepix_feu <- counts_feu / max(counts_feu, na.rm = TRUE)
scorepix_herb <-counts_herb / max(counts_herb, na.rm = TRUE)
scorepix_couv <- counts_couv / max(counts_couv, na.rm = TRUE)

# Exemple pour calculer les p-valeurs pour feu
p_values_feu <- apply(matrice_feu*1, 1, sum) / nperm

# Conversion en valeurs logiques pour tester la significativité à p < 0.05
significant_feu <- p_values_feu < 0.05


#Création de color.gradient
color.gradient <- function(x, colors=c("navyblue","cyan","lightblue","lightyellow","lightgreen", "chartreuse","green"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-20, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(aes(x =  W_sim_agg$longitude,  # Specify the x axis as longitude
                 y = W_sim_agg$latitude),  # Colour the points based on species name
             colour=color.gradient(counts_feu) ,
             shape=15,
             alpha = 0.2,  # 0.2 --> bien
             size = 0.01) +  # 0.01 --> bien
  ggtitle("All spp") +      
  theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  

(counts_feu==nperm)*1

ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-20, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(aes(x = W_sim_agg$longitude,  # Specify the x axis as longitude
                 y = W_sim_agg$latitude),  # Colour the points based on species name
             colour=color.gradient((counts_feu==10)*1) ,
             shape=15,
             alpha = 0.2,  # 0.2 --> bien
             size = 0.01) +  # 0.01 --> bien
  ggtitle("All spp") +      
  theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  
clim=as.data.frame(clim)
plot(scorepix~clim$bio14,cex=0.1,pch=16)
classrain=round(clim$bio14/10)*10
boxplot(W_sim_agg$feu~classrain)
tableau
W_sim_agg

#Carte de synthèse

synthese_feu <- as.data.frame(W_sim_agg$feu * scorepix_feu)
synthese_herb <- as.data.frame(W_sim_agg$herb * scorepix_herb)
synthese_couv <- as.data.frame(W_sim_agg$couv * scorepix_couv)

filtered_values <- subset(synthese_feu, synthese_feu > 0.034)
filtered_values <- subset(synthese_feu, synthese_feu > 0.034)

filtered_indices <- which(synthese_feu > 0.034)
filtered_values <- synthese_feu[filtered_indices]
length(filtered_values)

#CARTE FEU DIV TEST
ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "black") +
  coord_sf(xlim = c(-10, 20), ylim = c(35, 50), expand = FALSE)   +
  geom_point(aes(x = Wagg$longitude,  # Specify the x axis as longitude
                 y = Wagg$latitude),  # Colour the points based on species name
             colour="orange" ,
             shape=15,
             alpha = (synthese_feu>0.034)*(synthese_herb>0.01)*(synthese_couv>0.04)*0.02,  # 0.2 --> bien
             size = 0.15) +  # 0.01 --> bien
  ggtitle("All spp") +      
  theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "black"))
ggsave("carte_feu_div_test.png",width = 40, height = 22, units = "cm", dpi=600)





#HISTOGRAMME SEUIL BMA
hist(mean_data_fr_sp$BMA, 20, xlim=c(0,0.5))
abline(v=0.1)
hist(mean_data_fr_sp$H5class, 20, xlim=c(0,4))


#CLIMATE ANALYSIS
climate <- getData("worldclim",var="bio",res=2.5)    #Bio 1 and Bio12 are mean anual temperature and anual precipitation;  other resolution res=0.5 or 2.5 or 5 (minutes of a degree)   see for full http://www.worldclim.org/bioclim   
install.packages("geodata")
library(geodata)
plot(climate[[18]]) # representer carte mondiale precipitation warmest quarter
clim=extract(climate,Wagg[,3:4]) #Wagg 3-4 = lat et long
clim_Yagg = extract(climate,Yagg[,1:2])
Bio<-c("bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
descriptions <- c("Annual mean temperature", "Mean diurnal range", "Isothermality", "Temperature seasonality", "Max temperature of warmest month", "Min temperature of coldest month", "Temperature annual range (bio05-bio06)", "Mean temperature of wettest quarter", "Mean temperature of driest quarter", "Mean temperature of warmest quarter", "Mean temperature of coldest quarter", "Annual precipitation", "Precipitation of wettest month", "Precipitation of driest month", "Precipitation seasonality", "Precipitation of wettest quarter", "Precipitation of driest quarter", "Precipitation of warmest quarter", "Precipitation of coldest quarter")
tableau <- data.frame(Variables = Bio, Description = descriptions)

# carte du monde avec les endroits où les précipitations durant les mois les plus secs sont inférieures à 40mm et où les précipitations durant le mois le plus sec en été sont inférieures au tiers du mois hivernal le plus arrosé en vert.
#carte du monde des régions méditérranéennes selon Koppen
#Définition de la zone méditerranéenne en fonction des variables climatiques de Koppen
#HOT DRY SUMMER AND WET COLD WINTER
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_5.pdf")
climate_plot_5<-plot(climate[[10]]==climate[[9]]&climate[[11]]==climate[[08]], main="Zones ou les mois les plus chauds sont les plus secs et où les mois les plus froids sont les plus humides")
#SUMMER > 22°C AND WINTER <-3 AND >18°C --> KOPPEN 
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_6.pdf")
climate_plot_6<-plot(climate[[9]]>=220&climate[[14]]<40&climate[[11]]<180&climate[[11]]>(-3), main="Zones ou le climat est méditerranéen selon la classification de Koppen")
#PRECIPITATION DU MOIS LE PLUS SEC < 1/3 PRECIPITATION DU MOIS LE PLUS ARROSE
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_7.pdf")
climate_plot_7<-plot(climate[[14]]<climate[[13]]/3, main="mois plus sec < 1/3 mois plus arrosé")
#FUSIOOOOOOON
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_8.pdf")
climate_plot_8<-plot(climate[[10]]==climate[[9]]&climate[[11]]==climate[[08]]|climate[[9]]>=220&climate[[14]]<40&climate[[11]]<180&climate[[11]]>(-3)&climate[[14]]<climate[[13]]/3, main="Zones géographiques ou le climat est méditerranéen selon caractéristiques définies pas Koppen")

#Histogramme de la distribution des plantes méd en fonction de variables climatiques
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_1.pdf")
hist(WaggNoNaClim$bio10/10,50, main="La fréquence d'espèces méditerranéennes en fonction de la température moyenne du trimestre le plus chaud", xlab="Température moyenne du trimestre le plus chaud", ylab="nombre d'individu")
dev.off()
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_2.pdf")
climate_plot_2<-hist(WaggNoNaClim$bio11/10,50, xlab="Température moyenne du trimestre le plus froid", ylab="nombre d'individu")
dev.off()
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_3.pdf")
climate_plot_3<-hist(WaggNoNaClim$bio12/12,50, xlab="Précipitation mensuelle moyenne (mm)", ylab="nombre d'individu")
dev.off()
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre//Climate/climate_plot_4.pdf")
climate_plot_4<-hist(WaggNoNaClim$bio18/3,50, xlab="Précipitation mensuelle moyenne du trimestre le plus chaud (mm)", ylab="nombre d'individu")
dev.off()


#NOUVELLE CARTE AVEC SP POUR CLIMAT_MED
clim_Yagg=as.data.frame(clim_Yagg)
climat_med=Yagg[clim_Yagg$bio6>(-3)&clim_Yagg$bio14<40&clim_Yagg$bio10==clim_Yagg$bio9&clim_Yagg$bio14<clim_Yagg$bio13/3,]
w$un=1
climat_med$un=1
attrib_climat=as.data.frame(cbind(levels(as.factor(climat_med$sp)),as.data.frame(aggregate(climat_med$un, list(climat_med$sp), sum))$x/as.data.frame(aggregate(w$un, list(w$sp), sum))$x))

attrib_climat[,2]=as.numeric(attrib_climat[,2])
par(mar=c(15,5,1,1))
barplot((attrib_climat[,2]~attrib_climat[,1]), ylim=c(0,1), las=2, xlab="", ylab="% d'occurence dans le climat méd", col=colors)
abline(v = mean(attrib_climat[,2]))
ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-20, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(aes(x = climat_med$longitude,  # Specify the x axis as longitude
                 y = climat_med$latitude),  # Colour the points based on species name
             colour="yellow" ,
             shape=15,
             alpha = 0.8,  # Set point opacity to 40%
             size = 0.4) +  # Set point size to 1
  ggtitle("climat_med") +      
  theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  
ggsave("clim_med.pdf",width = 40, height = 22, units = "cm")



#AVEC NOS DONNEES MTN CA SUFFIT DE JOUER AVEC LES CARTES OUESH
#Wagg= aggregate(W, list(W$longitude,W$latitude), mean)
Wagg_sum= aggregate(W, list(W$longitude,W$latitude), sum)
Wagg_sum$driver=NA
Wagg_sum$driver[Wagg$feu01==1]='feu'
Wagg_sum$driver[Wagg$herb01==1]='herb'
Wagg_sum$driver[Wagg$couv01==1]='couv'
Wagg$driver=NA
Wagg$driver[Wagg$feu01==1]='feu'
Wagg$driver[Wagg$herb01==1]='herb'
Wagg$driver[Wagg$couv01==1]='couv'
WaggNoNaClim=na.omit(cbind(Wagg,clim))
Wagg_sumNoNaClim=na.omit(cbind(Wagg_sum,clim))

plot(WaggNoNaClim[,2]~WaggNoNaClim[,1],xlim=c(-20,40),ylim=c(30,50),pch=10)
plot(Wagg_sumNoNaClim[,2]~WaggNoNaClim[,1],xlim=c(-20,40),ylim=c(30,50),pch=10)

# définition des couleurs en fonction des variables climatiques
colormap <- colorRampPalette(c("lightblue", "lightyellow",'lightsalmon'))
colors <- colormap(3)[cut(Wagg_sumNoNaClim$bio10, breaks = 3)]

# définition des couleurs en fonction de la température (bio10)
colormap_2 <- colorRampPalette(c("lightyellow", "lightblue", "blue",'navyblue'))

# représentation graphique
plot(WaggNoNaClim[,2]~WaggNoNaClim[,1], xlim=c(-20,40), ylim=c(30,50), pch=20, col=colors, cex=sizes, xlab="longitude", ylab="latitude")

install.packages("maps")
library(maps)

carte_sp_temp <- plot(Wagg_sumNoNaClim[,2]~Wagg_sumNoNaClim[,1],pch=1, cex=0.1, alpha=0.5, col=as.factor(ClimKopKop), xlim=c(-20,40), ylim=c(30,50), xlab="longitude", ylab="latitude")
map(add = TRUE)
library(ggplot2)

ggplot(Wagg_sumNoNaClim, aes(x = longitude, y = latitude, color = colors, alpha = 0.5)) +
  geom_point(size = 1, shape = 1) +
  xlim(-20, 40) + ylim(30, 50) +
  labs(x = "longitude", y = "latitude")

#Extract the climatic zone from Koeppen-Geiger-ASCII
Koppen_climate=read.table("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre/Kopkop/Koeppen-Geiger-ASCII.txt",header=T) # Choose the txt file   Koeppen-Geiger-ASCII.txt
pts = Koppen_climate
coordinates(pts)=~Lon+Lat
gridded(pts) = TRUE
ra = raster(pts)
plot(ra)
ClimKopKop=levels(as.factor(Koppen_climate[,3]))[extract(ra,WaggNoNaClim[,1:2])]

plot(WaggNoNaClim[,2]~WaggNoNaClim[,1], col=as.factor(ClimKopKop),xlim=c(-20,40),ylim=c(30,50),pch=10)
plot(Wagg_sumNoNaClim[,2]~Wagg_sumNoNaClim[,1], col=as.factor(ClimKopKop),xlim=c(-20,40),ylim=c(30,50),pch=10)
library(ggplot2)
ggplot(WaggNoNaClim, aes(x = WaggNoNaClim[,1], y = WaggNoNaClim[,2], color = as.factor(ClimKopKop))) + 
  geom_point(size = 3) +
  xlim(-20, 40) +
  ylim(30, 50) +
  labs(x = "longitude", y = "latitude") +
  scale_color_discrete(name = "Classification de Koppen")
map(add = TRUE)

#TENTATIVE DE CREATION DE CARTE DES DRIVERS
# définir les couleurs pour chaque option de "driver"
col_feu <- "orange"
col_herb <- "lightpink"  # marron clair en code hexadécimal
col_couvert <- "darkgreen"

# créer la palette de couleurs
colormap <- colorRampPalette(c(col_feu, col_herb, col_couvert))

# assigner la couleur appropriée à chaque option de "driver"
colors <- ifelse(Wagg_sumNoNaClim$driver == "feu", col_feu,
                 ifelse(Wagg_sumNoNaClim$driver == "herb", col_herb, col_couvert))

ggplot() +   # Plot the map    
  geom_sf(data = worldmap, colour = "blue", fill = "navyblue") +
  coord_sf(xlim = c(-20, 40), ylim = c(30, 50), expand = FALSE)   +
  geom_point(aes(x = WaggNoNaClim$longitude,  # Specify the x axis as longitude
                 y = WaggNoNaClim$latitude),  # Colour the points based on species name
             colour=colors,
             shape=15,
             alpha = 0.06,  # Set point opacity to 40%
             size = 0.01) +  # Set point size to 1
  ggtitle("All driver") +      
  theme_classic()  +
  theme(panel.background=element_rect(fill = "#2C3E4F", colour = "navyblue"))  
ggsave("all_drivers.pdf",width = 40, height = 22, units = "cm")

#CARTE CLASSIFICATION DE KOPPEN / DISTRIBUTION
plot(WaggNoNaClim[,2]~WaggNoNaClim[,1],col=as.factor(ClimKopKop),xlim=c(-10,20),ylim=c(35,50),pch=15)

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey90") +
  geom_path(data = world_map, aes(x = long, y = lat, group = group), color = "black", alpha=1) +
  geom_point(data = WaggNoNaClim, aes(x = WaggNoNaClim[,1], y = WaggNoNaClim[,2]), colour = colors, size = 0.005, alpha=0.05, show.legend = TRUE) +
  xlim(-20, 40) +
  ylim(30, 50) +
  labs(x = "longitude", y = "latitude") +
  scale_color_discrete(name = "Classification de Koppen") +
  theme_bw() +
  coord_fixed(1.3) +
  guides(color = guide_legend(override.aes = list(size=5, alpha=1)))
ggsave("Koppen_distrib_5.pdf",width = 40, height = 22, units = "cm", dpi=500)

#UTILISATION DE LENGHT
lenght_sec <-length(WaggNoNaClim$bio10[WaggNoNaClim$bio10==WaggNoNaClim$bio9])/length(WaggNoNaClim$bio10)# 31% en dehors de prédiction sur mois dont la saison sèche est la saison chaude 
length(WaggNoNaClim$bio9[WaggNoNaClim$bio9>=200])/length(WaggNoNaClim$bio9)#40% en dehhors des prédictions sur mois dont la moyenne des temp des mois les plus  secs > ou = à la moyenne des températures des mois les plus chauds
length(WaggNoNaClim$bio9[WaggNoNaClim$bio9==WaggNoNaClim$bio10])/length(WaggNoNaClim$bio9)
lenght_precipitation <- length(WaggNoNaClim$bio14[WaggNoNaClim$bio14<40])/length(WaggNoNaClim$bio14)#56% en dehors des prédictions lorsque les précipitations sont inférieures à 40 mm quand les mois sont les plus secs
boxplot(lenght_sec, lenght_precipitation)
hist(WaggNoNaClim$bio10)
abline(v=200)
hist(WaggNoNaClim$bio6/10)
abline(v=-3)
WaggNoNaClim

Ki2 <- table(as.data.frame(cbind(ClimKopKop,WaggNoNaClim$driver)))
chisq.test(Ki2)
Ki2_result <- chisq.test(Ki2)
print(Ki2_result)
summary(Ki2_result)
df_Ki2<-data.frame(Ki2)

mosaicplot(Ki2, col=1:nrow(Ki2), main="Mise en évidence des relations entre climat et perturbations")

# Charger le package graphics
library(graphics)
# Créer le plot
pdf("C:/Users/pierr/OneDrive/Bureau/Stage_R/Analyses Pierre/Score_plot/mosaic_plot.pdf")
mosaic_plot<-mosaicplot(Ki2, color = c("chartreuse", "orange", "pink"), main = "Tableau de contingence", legend = c("feu", "herb", "couv"), cex = 0.5)
dev.off()
# Ajouter une légende
legend("right", legend = c("Couverture", "Feu", "Herbivorie"), col = c('chartreuse', 'orange', 'pink'), pch = 15)

#JE VEUX FAIRE UN BOXPLOT AVEC EN ABSCISSE LES CLIMAT KOPPEN ET EN Y LES SCORES DE FEU ASSOCIES
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(ggpubr)
library(stats)
install.packages("wesanderson")
library(wesanderson)

length(ClimKopKop)
ClimKopKop_noNA <- na.omit(ClimKopKop)
feu_noNA <- WaggNoNaClim_noNA$feu
herb_noNA <- WaggNoNaClim_noNA$herb
couv_noNA <- WaggNoNaClim_noNA$couv
dim(WaggNoNaClim)
class(ClimKopKop)
class(WaggNoNaClim)
feu_noNA <- na.omit(feu_noNA)
herb_noNA <-na.omit(herb_noNA)
couv_noNA <- na.omit(couv_noNA)
ClimKopKop_noNA <- na.omit(ClimKopKop_noNA)
length(ClimKopKop_noNA)
WaggNoNaClim_noNA <- WaggNoNaClim[!is.na(ClimKopKop), ]

length(feu_noNA)
length(herb_noNA)
length(couv_noNA)
boxplot(feu_noNA ~ ClimKopKop_noNA, main = "Boxplot des scores de feu par climat Koppen",
        xlab = "Climat Koppen", ylab = "Scores de feu")

# Création du data frame pour le boxplot
df <- data.frame(feu_noNA = feu_noNA, ClimKopKop_noNA = ClimKopKop_noNA)
dh <- data.frame(herb_noNA = herb_noNA, ClimKopKop_noNA = ClimKopKop_noNA)
dc <- data.frame(couv_noNA = couv_noNA, ClimKopKop_noNA = ClimKopKop_noNA)
df[df$ClimKopKop_noNA==c("Csb", "Csa"),]
#test stat
kruskal_test_feu <- kruskal.test(feu_noNA ~ ClimKopKop_noNA, data = df)
kruskal_test_herb <- kruskal.test(herb_noNA ~ ClimKopKop_noNA, data = dh)
kruskal_test_couv <- kruskal.test(couv_noNA ~ ClimKopKop_noNA, data = dc)

posthoc_test_feu <- pairwise.wilcox.test(df$feu_noNA, df$ClimKopKop_noNA, p.adjust.method = "bonferroni")
posthoc_test_herb <- pairwise.wilcox.test(dh$herb_noNA, dh$ClimKopKop_noNA, p.adjust.method = "bonferroni")
posthoc_test_couv <- pairwise.wilcox.test(dc$couv_noNA, dc$ClimKopKop_noNA, p.adjust.method = "bonferroni")
print(kruskal_test_feu)
print(kruskal_test_herb)
print(kruskal_test_couv)
print(posthoc_test_feu)
print(posthoc_test_herb)
print(posthoc_test_couv)

df_posthoc_test_feu <- data.frame(posthoc_test_feu)


# Définir une palette de couleurs pour chaque plot
colors_anderson <- wes_palette(n = 20, name = "GrandBudapest1")
colors_anderson <- wes_palette(name=c("FantasticFox1", "GrandBudapest1", "Moonrise1", "IsleofDogs1"), 20, type = c("discrete", "continuous"))
palette_ff <- wes_palette(n = 4, name = "FantasticFox1", type = "discrete")
palette_gb <- wes_palette(n = 4, name = "GrandBudapest1", type = "discrete")
palette_mr <- wes_palette(n = 4, name = "Moonrise1", type = "discrete")
palette_id <- wes_palette(n = 4, name = "IsleofDogs1", type = "discrete")
palette_br <- wes_palette(n = 4, name = "BottleRocket1", type = "discrete")

colors <- c(palette_ff, palette_gb, palette_mr, palette_id, palette_br)
colors <- c("#0000FF", "#4B0082", "#FFA500", "#FFFF99", "#FF0000", "#FFC0CB", "#B7CE9C", "#7FFF00", "#006400", "#FFFF00", "#FFA500", "#436EEE", "#808000", "#00FFFF", "#ADD8E6", "#000080", "#8A2BE2", "#9370DB", "#00CED1", "#2F4F4F")

print(colors)

#FEU_KOPPEN
# Tracer le graphique avec les couleurs personnalisées et la police Times New Roman
ClimKopKop_noNA

ggplot(df, aes(x = ClimKopKop_noNA, y = feu_noNA, fill = ClimKopKop_noNA)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  labs(title = "", x = "Classification de Koppen", y = "Score des traits liés au feu")
# Exporter le graphique en haute résolution pour un article scientifique
ggsave("graphique_feu_koppen.pdf", width = 8, height = 6, dpi = 400)

#HERB_KOPPEN
ggplot(dh, aes(x = ClimKopKop_noNA, y = herb_noNA, fill = ClimKopKop_noNA)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  labs(title = "", x = "Classification de Koppen", y = "Score des traits liés à l'herbivorie")
# Exporter le graphique en haute résolution pour un article scientifique
ggsave("graphique_herb_koppen.png", width = 8, height = 6, dpi = 400)

#COUV_KOPPEN
ggplot(dc, aes(x = ClimKopKop_noNA, y = couv_noNA, fill = ClimKopKop_noNA)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  labs(title = "", x = "Classification de Koppen", y = "Score des traits liés au couvert végétal")
# Exporter le graphique en haute résolution pour un article scientifique
ggsave("graphique_couv_koppen.png", width = 8, height = 6, dpi = 400)



