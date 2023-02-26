install.packages(c("FactoMineR", "factoextra"))
install.packages('corrplot')
library(FactoMineR)
library(factoextra)
library(corrplot) 
library('prettyR')
require(MASS) 
library("caret")
library("psych")
library(ggplot2)
library(plotly)
library(scales)
library(tidyverse)
library(dplyr)
library(plyr)
library(kernlab)
library(caret)
library(cowplot)
library(gridExtra)

# récupération du dataset
bankData = read.csv2('../../lab/origin_values/billets.csv', sep=',', dec = '.')


#renommer  les valeurs booléenes  is_genuine  (vrai / faux)
bankData$is_genuine = factor(bankData$is_genuine, levels = c('True', 'False'), labels = c('vraiBillet', 'fauxBillet'))

## get only numeric values
bankDataNumeric = c('length', 'height_left', 'height_right', 'margin_low', 'margin_up', 'diagonal')


summarise(bankData[, bankDataNumeric], num.desc = c('mean', 'sd'))


#### Analyse univarié 

## nombre de faux billet 
nbVraiBillet = sum(bankData$is_genuine == 'vraiBillet')
nbFauxBillet = sum(bankData$is_genuine == 'fauxBillet')

## extraire les datas pour traiter les boléens 
pieGender <- data.frame(
  label =  c("vraieBillet", "fauxBillet"),
  value =  c(nbVraiBillet, nbFauxBillet)
)

## generation du plot 
ggplot2::ggplot(pieGender, aes(x = "", y = value, fill = label)) +
  geom_col() +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")


## on constate qu'il y 100 vrais billet 70 faux billet 


### Analyse bi varié 

## nous prenons l'ensemble du dataset qui ont des valeurs numérique et nous regarder si elle sont corrélé 
arrayCorrelation <- corrplot(cor(bankData[, bankDataNumeric]), type="upper", tl.col="black")
arrayCorrelation

## plus les couleurs s'approche de bleu foncé qui sont les variable de base plus elle sont corréler plus elle sont dans le rouge moins elle le sont

## digrame de moustache pour connaitre la correlation des variable selon si c'est un vraie ou faux billet

#taille de  la frame
par(mfrow=c(2,3))
# on parcours toutes les colonnes du dataset et on compare leur valeurs selon les differentes variable qu'on corrèle avec vrais ou faux billet
# génération d'un diagrame de moustache
for (var in bankDataNumeric) {
  boxplot(
    bankData[bankData$is_genuine == 'vraiBillet', var],
    bankData[bankData$is_genuine == 'fauxBillet', var],
    col = c('#00B233', '#B20000'),
    horizontal = TRUE
  )
  title(main=var)
}
# on distingue que les varialbe les moins corréler en lien avec le booléen ce sont les margin et la diagonal


## Nous devons d'abord définir la notion d'apprentissage non-supervisé 

# Apprentissage NON-SUPERVISE

#C'est une méthode d'apprentissahe dans laquelle au lieu de montrer à la machine des exemples x/y de ce qu'elle doit 
#apprendre on lui fournit uniquement des données x et on lui demande d'analyser uniquement la structure de ces données.

#On peux décrire l'étude non-supervisé en 3 grand point

#1 - cluster (k means)
#Par exemple la machine peux apprendre elle même à classer des data (cluster) en exemple nous pouvons classifiers des billets
#des tweets des photos selon le caractèristique. En étude non-supervisé on utilise le K-mean clustering

#2 - detection anomaly
#Une autre tâche de que les études non-supervisé nous apportes c'est la detection d'anomalie.
#La machine analyse la structure de nos data parvient à trouvé les échantillions dont les caractéristiques sont très éloignées
#de celles des autres échantillions cela isole donc un échantillion on peux se baser pour la fraude bancaire ou bien les usines 
#qui  fabriques des pièces au milimetres près

#3 - dimension reduction (PCA)
#En analysant la structure de nos données la machine est capable d'apprendre comment simplifier cette structure tout en
#conservant les principales informations comme example imaginer la description d'un chien en dessin la machine va garder les 
#caracteristique de base de ce chien et pourras déduire que d'autres données est équivos à un chien


## Vous réaliserez une analyse en composantes principales de l'échantillon

# 1 - ACP

## on charge les données pour utiliser la pca 
acp = PCA(X=bankData, quali.sup = 1, graph = FALSE)

PCA(X=bankData, quali.sup = 1, graph = TRUE)

## faut que  je decrive plus 


## Eboulis des valeurs propres
eigen_data <- eigen(cov(acp))

fviz_eig(acp, addlabels = TRUE)




## Correlation via ACP 
corrplot(get_pca_var(acp)$cos2, is.corr=FALSE)
## dans la première dimmension il y a une majorité de dimmension corrélé

#https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html
## Cercle de corrélation

fviz_pca_var(acp,
             axes=c(1,2),
             col.var = "cos2",
             gradient.cols =  c("blue", "yellow", "red"),
             geom=c('arrow', 'text'),
             labelsize = 4,
             repel = TRUE
)

# les varaibles se rejoigne positivement pour la hauteur 
# cependant nous avons deux extrême la diagonal et réellement opposé à la margin



## Nuage d'individus 

fviz_pca_ind(acp,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = bankData$is_genuine, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

# on le fait sur  2 plan 
fviz_pca_ind(acp, 
             axes = c(1,2),
             geom=c('point'),
             habillage = 1,
             palette = c('#46B12E', '#FF3A3A'),
             alpha.ind="cos2",
             select.ind = list(cos2 = 0.3),
             mean.point = FALSE,
             addEllipses = TRUE,
             pointshape=19)

# on distinct bien les deux vrais des faux billet il n'y presque pas de corrélation dans les données 

# sur 3 plans

fviz_pca_ind(acp, 
             axes = c(1,3),
             geom=c('point'),
             habillage = 1,
             palette = c('#46B12E', '#FF3A3A'),
             alpha.ind="cos2",
             select.ind = list(cos2 = 0.3),
             mean.point = FALSE,
             addEllipses = TRUE,
             pointshape=19)

             
# les billet ce confonde beaucoup plus 



fviz_pca_ind(acp, 
             axes = c(2,3),
             geom=c('point'),
             habillage = 1,
             palette = c('#46B12E', '#FF3A3A'),
             alpha.ind="cos2",
             select.ind = list(cos2 = 0.3),
             mean.point = FALSE,
             addEllipses = TRUE,
             pointshape=19)

#Contient (seulement) 36% de l'information.
# elle sont corrélé 

# nous prenons le premier plan factoriel car il represente le mieux les vrais des faux billets

## on genere les clusters 
#premiere etape on doit normaliser les data quantitatives  
normalizeBillet = scale(bankData[, bankDataNumeric])
## silouhaite

fviz_nbclust(normalizeBillet, kmeans, method = "wss") 

## nb cluster
fviz_nbclust(normalizeBillet, kmeans, method = "silhouette")

fviz_nbclust(normalizeBillet, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(normalizeBillet, kmeans, nstart = 1,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


k2 <- kmeans(normalizeBillet, centers = 2, nstart = 25)



# cluster 

kmeansGraph <- fviz_cluster(k2, data = normalizeBillet)
kmeansGraph + scale_color_discrete(labels = c("fauxBillet", "vraiBillet"))




# Calcul de la distance euclidienne
distance <- dist(normalizeBillet, method = "euclidean")

# Calcul des liens hiérarchiques avec l'algorithme Ward
hclust_obj <- hclust(distance, method = "ward.D2")

clusters <- cutree(hclust_obj, k = 2)

# Coloration des branches correspondantes en rouge
plot(hclust_obj, main = "Dendrogramme avec 2 clusters")
rect.hclust(hclust_obj, k = 2, border = "red")




library(gridExtra)
library(cowplot)

# Calcul de la distance euclidienne
distance <- dist(normalizeBillet, method = "euclidean")

# Calcul des liens hiérarchiques avec l'algorithme Ward
hclust_obj <- hclust(distance, method = "ward.D2")

# Calcul des affectations de cluster
clusters <- cutree(hclust_obj, k = 2)

# Comptage du nombre de lignes dans chaque cluster
counts <- table(clusters)

# Création du dendrogramme coloré par cluster
dend <- fviz_dend(hclust_obj, k = 2, rect = TRUE, rect_fill = TRUE, 
                  rect_border = "grey", rect_colors = c("simpsons", "rickandmorty")[clusters],
                  main = "Dendrogramme avec 2 clusters")

print(dend)

# Ajout de la légende avec les nombres de lignes dans chaque cluster
legend <- cowplot::get_legend(ggplot2::ggplot(data.frame(x = 1, y = 1), 
                                              ggplot2::aes(x = x, y = y, fill = as.factor(clusters))) +
                                ggplot2::geom_tile(show.legend = TRUE) +
                                ggplot2::theme_void() +
                                ggplot2::theme(legend.position = "right", legend.title = ggplot2::element_blank()))

legend <- legend + ggplot2::labs(fill = "Cluster") + 
  cowplot::theme(legend.text = ggplot2::element_text(size = 12),
                 legend.key.size = unit(0.45, "cm"))

# Ajout de la légende au dendrogramme avec `grid.arrange()`
dend_with_legend <- gridExtra::grid.arrange(dend, legend, ncol = 2, widths = c(4, 1))

# Affichage du dendrogramme avec la légende
print(dend_with_legend)


table(clusters)

# Affichage du dendrogramme
plot(hclust_obj)


# on génére des clusters 
clusterUnsupervised = hclust(dist(normalizeBillet))
# on découpe en 2
clusters  = cutree(clusterUnsupervised, 2)





#matrice de conf
# cela retourne un tableau de 2 cluster 
cahConfusion = table(clusters, bankData$is_genuine)
cahConfusion


percentClassificationCah = (cahConfusion[2,1] + cahConfusion[1,2]) / sum(cahConfusion) * 100
percentClassificationCah

resultConfusion = 100 - percentClassificationCah
resultConfusion

#cluster 1 contient:
#    69 faux billets et 24 vrai billet donc vraiBillet < fauxBillet
#cluster 2 contient:
#    76 vrais billets 1 faux billet donc vraiBillet > fauxBillet

# nous avons 85.2 % de billet bien classé selon la matrice de confusion 


## on génére l'habillage via le clusters
acpLabel = factor(paste(clusters , bankData$is_genuine, sep = ' - '))
# acp en points 
fviz_pca_ind(acp, 
             geom=c('point'),
             pointshape = 19,
             habillage = acpLabel,
             addEllipses=FALSE,
             alpha.ind="cos2",
             mean.point = FALSE,
             legend.title = "Clusters"
)


## representation 


# KMEANS

fviz_nbclust(normalizeBillet, kmeans, method = "wss")

generateKmeans = kmeans(x = normalizeBillet,2)
kmeansCluster = generateKmeans$cluster

## kmeans - matrice de confusion

kmeansMatrice = table(kmeansCluster, bankData$is_genuine)

# cluster majoritaire
firstCluster  = ifelse(kmeansMatrice[1,2] > kmeansMatrice[2,2], 1, 2)
secondCluster = ifelse(kmeansMatrice[1,1] > kmeansMatrice[2,1], 1, 2)

kmeansMatrice

# cluster 1 majorité de faux billet
# cluster 2 majorité de vrai billet

rankingBanknoteKmeans = 100 - (kmeansMatrice[firstCluster, 1] + kmeansMatrice[secondCluster, 2] / nrow(bankData) * 100)
rankingBanknoteKmeans

##  91 % des billets sont bien classés

kmeansLabel = factor(paste(kmeansCluster, bankData$is_genuine, sep = ' - '))


fviz_pca_ind(acp, 
             geom=c('point'),
             pointshape = 19,
             habillage = kmeansLabel,
             addEllipses=FALSE,
             alpha.ind="cos2",
             mean.point = FALSE,
             legend.title = "Clusters"
)

# On constate que le nombre de faux négatif a baissé à 8 (il y a moins de points oranges).




# HCPC ( hierarchical clustering ) = acp + cah + kmeans

## calculate 
calculateHcpc = HCPC(acp, nb.clust = 2, graph = FALSE)
clusterHcpc = calculateHcpc$data.clust$clust

## dendograme

fviz_dend(calculateHcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
)
 
## matrice de confusion

hcpcMatrice = table(clusterHcpc, bankData$is_genuine)
hcpcMatrice

## même résultat que le Kmeans

fviz_dend(calculateHcpc, k = 2, show_labels = FALSE, rect = TRUE)
