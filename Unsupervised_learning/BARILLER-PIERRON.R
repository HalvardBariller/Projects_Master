### WORKING DIRECTORY ###

# setwd("/Users/halvardbariller/Desktop/M1 MATH&IA/S1/Methodes IA/Unsupervised Learning/Projet")

### Librairies
detach("package:reshape", unload = TRUE)
library(corrplot)
library(car)
library(GGally)
library(FactoMineR)
library(factoextra)
library(mclust)
library(ClustOfVar)
library(DT)
library(dplyr)

### Initialisation de l'environnement
rm(list = objects())
graphics.off
dev.off()
set.seed(2023)



#####################################################
########           PARTIE I : PCA          ##########
#####################################################



leaf = read.csv(file = "Unsupervised_Learning/data/leaf.csv",header=F)

summary(leaf)
str(leaf)

leaf = leaf%>%rename("Class" = "V1",
                      "Specimen" = "V2",
                      "Eccentricity" = "V3",
                      "Aspect_ratio" = "V4",
                      "Elongation"="V5",
                      "Solidity"="V6",
                      "Stochastic_convexity"="V7",
                      "Isoperimetric_factor"="V8",
                      "MID"="V9",
                      "Lobedness"="V10")

### Acquisition des données de l'article
leaf = leaf[,-11:-16]
leaf = leaf[leaf$Class<16,]
### Mise en facteur des paramètres Classe et Spécimen
leaf$Class = factor(leaf$Class)
leaf$Specimen = factor(leaf$Specimen)
str(leaf)
### Vérification de la représentation des classes
table(leaf$Class)

###### ANALYSE UNI ET BI VARIEE ######


p = ncol(leaf)
boxplot(leaf[,3:p]) 
pairs(leaf[,3:p])
ggpairs(leaf[,3:p])
round(cor(leaf[,3:p]),2)

corrplot(cor(leaf[,3:p]), method="circle")
scatterplotMatrix(as.matrix(leaf[,3:p]))

round(apply(leaf[,3:p], 2, var), 2)


### Centrage et normalisation des variables actives
n = nrow(leaf)
leaf[,-1:-2] = scale(leaf[,-1:-2],center=TRUE,scale=TRUE)/sqrt((n-1)/n)
# Vérification de la normalisation
apply(leaf[,-1:-2]^2,2,mean)

######    PCA   ######
res = PCA(leaf,quali.sup = 1:2,scale.unit = F,ncp=7)
plt1 = plot(res,axes = c(1,2), choix = "ind",label="none",habillage=1)
plt2 = plot(res,axes = c(1,2), choix = "var")
cowplot::plot_grid(plt1, plt2, ncol = 2, nrow = 1)


### Représentation des individus
library(reshape2)
# Préparation à la représentation des individus sur les 2 CP retenues
leaf$pc1 <- res$ind$coord[, 1] 
leaf$pc2 <- res$ind$coord[, 2]
pca.vars <- res$var$coord %>% data.frame
pca.vars$vars <- rownames(pca.vars)
pca.vars.m <- melt(pca.vars, id.vars = "vars")
p <- ggplot(data = leaf, aes(x = pc1, y = pc2, color = Class)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(alpha = 0.8)
p
### Représentation des individus avec ellipses de confiance autour des centres de gravité
### de chaque classe
p <- p + stat_ellipse(geom="polygon", aes(fill = Class), 
                      alpha = 0.2, 
                      show.legend = FALSE, 
                      level = 0.95) +
  xlab("PC 1 (61.89%)") + 
  ylab("PC 2 (29.29%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"))
p

# Graphique des valeurs propres
fviz_screeplot(res,addlabels = TRUE,ylim = c(0, 65), ncp=8)

#Graphique variance cumulée
dev.off()
dev.new(width=20, height=10, unit="in")
plot(as.numeric(res$eig[,3]),type='o',col='blue',lty = 3,
     xlab = "n-ième valeur propre", 
     ylab = "Variance expliquée cumulée (en %)", 
     ylim = c(0,102),
     main = "Inertie (en %)")
grid()
abline(h=7*100/8,col='red',lty=2)
text(as.numeric(res$eig[,3]),
     pos=1, offset = 0.2,
     labels=as.integer(as.numeric(res$eig[,3])),
     col='blue',cex=0.65)
text(1.25, 87, "87.5",col='red',cex=0.75)

#Graphique des observations sur la dimension 1 et 2
fviz_pca_ind(res,axes=c(1,2),geom="point")

#Qualité de représentation des variables
variable = res$var
var_coord = variable$coord[,1:2] # coordonnées des composantes principales
var_qualite = variable$cos2[,1:2] # tableau de la qualité de répresentation
fviz_cos2(res, choice = "var", axes = 1:2)
corrplot(variable$cos2[,1:2], is.corr=FALSE,method="circle",
         cl.pos='r',
         cl.ratio=0.8,
         cl.align.text = 'l')

contrib =variable$contrib[,1:2] #contribution des variables aux composantes principales



#####################################################
########      PARTIE II : CLUSTERING.      ##########
#####################################################


### Nettoyage et réinitialisation de l'environnement
rm(list = objects())
dev.off()
df = read.csv(file = "leaf.csv",header=F)
test = df[,1]
test = test[test<16]
categorie = df[1]
categorie = categorie[categorie$V1 < 16,]
categorie = factor(categorie)
df2 = df[c(1:10)]
colnames(df2) = c("class",
                  "Specimen Number",
                  "Eccentricity",
                  "Aspect Ratio",
                  "Elongation",
                  "Solidity",
                  "Stochastic Convex",
                  "Isoperimetric Factor",
                  "Max Indent Depth",
                  "Lobedness")
df2 = df2[df2$class < 16,]
df2$class = factor(df2$class)
df2$`Specimen Number`=factor(df2$`Specimen Number`)
str(df2)
n = nrow(df2)
p = ncol(df2)
df_normal = scale(df2[,3:p],center=TRUE,scale=TRUE)/sqrt((n-1)/n)
df_normal = as.data.frame(df_normal)
###

res.pca2 = PCA(df_normal,scale.unit = F,graph=FALSE,ncp = 7)
df3 = as.data.frame(res.pca2$ind$coord[,1:2])

#K-means pour différentes valeurs de K
K = seq(3,21,3)
k_moyenne = list()
par (mfrow=c(2,3), mai=c(0.6,0.6,0.3,0.1))
for (k in K){
  set.seed(2023) #Pour que les centres des kmeans soient les memes
  res.kmeans = kmeans (df_normal,k)
  if (k %in% seq(9,18,3)){
    show(fviz_cluster(res.kmeans,stand=F,
                 data=df3,geom='point',
                 main= paste0("Cluster plot for K =",k)))
  }
  new_list = list( res.kmeans)
  k_moyenne = c(k_moyenne,new_list)
}

k_mean_precision = data.frame()
for (i in 1:7){
  precision = adjustedRandIndex(k_moyenne[[i]]$cluster, categorie)
  k_mean_precision = rbind(k_mean_precision,precision)
  print(paste0("K = ", 3*i," ,précision =" ,precision))
}
k_mean_precision = cbind(3*seq(1:7),k_mean_precision)
colnames(k_mean_precision) = c("Nombre de cluster","precision")


#Utilisation de Méthode de Classification Ascendante

res1=hclust(dist(df_normal)) #methode = complete
par(mfrow=c(1,2))
plot(res1,cex=0.5) # dendogramme
abline(h=rev(res1$height)[12],col="red") #coupe l'arbre pour avoir 12 classes
text(120, 2.3, "k=12",col='red',cex=1)
barplot(rev(res1$height)[1:15],xlab = "nombre de cluster",ylab = "hauteur",
        main="diagramme des hauteurs")
abline(h=rev(res1$height)[15])       

rep = cutree(res1,k=15) # répartition de l'arbre en 15 classes

# visualisation des classifications de 6 a 15 groupes
# on sauvegarde l'environnement avant le changement de marge
old.par = par(no.readonly = TRUE)
# pour profiter au maximum de l'espace disponible
par (mfrow=c(2,2), mai=c(0.7,0.7,0.4,0.2))
#représentation graphique pour plusieurs nombre de groupe
for (i in seq(9,18,3)){
  grp=cutree(res1,h=rev(res1$height)[i])
  plot(df3[,1],df3[,2],
       col=grp, pch=grp,
       main=paste("nombre de groupes=",i),
       ylim=c(-4,6),cex=0.5,xlab="Dim 1",ylab="Dim 2")
}


# On utilise hclust pour le clustering
#On Utilise plusieurs methodes differentes pour hclust
#On rappelle que res1 utilise la methode complete
compare_method = function(df){
  res.pca = PCA(df,scale.unit=F,graph=FALSE,ncp=7)
  #df = res.pca$ind$coord[,1:2]
  methode1 = data.frame(Precision=double())
  methode2 = data.frame(Precision=double())
  methode3 = data.frame(Precision=double())
  methode4 = data.frame(Precision=double())
  methode5 = data.frame(Precision=double())
  res1 = hclust(dist(df))
  res2 = hclust(dist(df),method="ward.D")
  res3 = hclust(dist(df),method="average")
  res4 = hclust(dist(df),method="ward.D2")
  res5 = hclust(dist(df),method="centroid")
  h = seq(3,16,1)
  for (i in h) {
    precision = c(adjustedRandIndex(cutree(res1,k=i), 
                                    categorie),
                  adjustedRandIndex(cutree(res2,k=i), 
                                    test),
                  adjustedRandIndex(cutree(res3,k=i), 
                                    categorie),
                  adjustedRandIndex(cutree(res4,k=i), 
                                    categorie),
                  adjustedRandIndex(cutree(res5,k=i), 
                                    categorie))
    methode1 = rbind(methode1,precision[1])
    methode2 = rbind(methode2,precision[2])
    methode3 = rbind(methode3,precision[3])
    methode4 = rbind(methode4,precision[4])
    methode5 = rbind(methode5,precision[5])
  }
  comparatif = cbind(h,methode1,
                     methode2,methode3,methode4,methode5)
  colnames(comparatif) = c("Nombre de Cluster","complete","ward.D",
                           "average","ward.D2","centroid")
  return(comparatif)
}


#On compare 5 méthodes différentes avec les données normalisées
compare_normalized = compare_method(df_normal)
max(compare_normalized[,2:ncol(compare_normalized)]) 
# 0.5844607 bien meilleur resultat qu'avec K-means

# On teste maintenant avec les données non normalisées
compare_unnormalized = compare_method(df2[,3:p])
max(compare_unnormalized[,2:ncol(compare_unnormalized)]) 
# 0.4857174 moins bon résultats qu'avec les données normalisées 
# et inférieurs a la méthode k-mean



#On utilise a present les données normalisees uniquement
#On teste a present l'utilisation d'une méthode mixte
set.seed(2023) #Nécessaire pour la reproductibilité des centres de kmeans

res.pca = PCA(df_normal,scale.unit=F, graph=F)
df3 = as.data.frame(res.pca$ind$coord[,1:2])
res.mixte = kmeans(df_normal,140)
names(res.mixte)
centre = res.mixte$centers
clusters = cutree(hclust(dist(centre),method="ward.D2"),k=15)
cluster_center = aggregate(centre,list(cluster=clusters),mean)
res.mixte = kmeans(df_normal,centers =cluster_center[2:ncol(cluster_center)])
adjustedRandIndex(res.mixte$cluster, categorie)
#0.5269685
#précision inférieures a hclust et similaire a k-means

#Clustering pour k = {1,2,3,4,5,6,7} premieres composantes principales 
#avec methode mixte
#Pour k = 1, une autre méthode a été retenue 
#car la méthode mixte renvoie une erreur pour k = 1
clust_prcp = function(df){
  set.seed(2024) #Necessaire a la reproductibilite pour les centres de kmean
  res.pca = PCA(df,scale.unit = F,graph = F,ncp=7)
  results =data.frame()
  #Pour k = 1, on utilise juste hclust car la méthode mixte présente un probleme
  results= rbind(results,
                 adjustedRandIndex(cutree(hclust(dist(res.pca$ind$coord[,1:1]),
                                                 method = "ward.D"),
                                          k=15), 
                                   categorie))
  for (i in seq(2,7)){
    nb_col = i+1
    df3 = res.pca$ind$coord[,1:i]
    res.mixte = kmeans(df3,100,iter.max = 25)
    centre = res.mixte$centers
    clusters = cutree(hclust(dist(centre),method="ward.D2"),k=15)
    cluster_center = aggregate(centre,list(cluster=clusters),mean)
    res.mixte = kmeans(df3,cluster_center[2:nb_col])
    
    results= rbind(results,
                   adjustedRandIndex(res.mixte$cluster, categorie))
  }
  colnames(results) = c("précision méthode mixte (sauf k=1)")
  return (results)
}

result_k_clustering1 =clust_prcp(df_normal)
which.max(result_k_clustering1[,1])
max_cluster =max(result_k_clustering1$`précision méthode mixte (sauf k=1)`) 
#0.5508558

#Clustering pour k = {1,2,3,4,5,6,7} premieres composantes principales 
#uniquement en utilisant la CAH
clust_prcp2 = function(df){
  set.seed(2023) #Necessaire a la reproductibilite pour les centres de kmean
  res.pca = PCA(df,scale.unit = F,graph = F,ncp=7)
  results =data.frame()
  #Pour k = 1, on utilise juste hclust car la méthode mixte présente un probleme
  results= rbind(results,
                 adjustedRandIndex(cutree(hclust(dist(res.pca$ind$coord[,1:1]),
                                                 method = "ward.D2"),
                                          k=15), 
                                   categorie))
  for (i in seq(2,7)){
    nb_col = i+1
    df3 = res.pca$ind$coord[,1:i]
    res_int= hclust(dist(df3),method="ward.D2")
    results= rbind(results,
                   adjustedRandIndex(cutree(res_int,k=15), categorie))
  }
  colnames(results) = c("précision par CAH")
  return (results)
}
result_k_clustering2 =clust_prcp2(df_normal)
which.max(result_k_clustering2[,1]) #7
max_cluster2 =max(result_k_clustering2$`précision par CAH`) 
#0.5921704

#Utilisation de HCPC avec le data_set normalisée en entier
set.seed(2023)
res_hcpc = HCPC(df_normal,nb.clust=15)
result_hcpc = adjustedRandIndex(res_hcpc$data.clust$clust,categorie)
#0.0.5228286

#Utilisation de HCPC en utilisant la résolution du PCA pour df_normal
res_hcpc2 = HCPC(res.pca,nb.clust=15)
result_hcpc2 = adjustedRandIndex(res_hcpc2$data.clust$clust,categorie)
#0.5318487

#Clustering sur les variables

par(mfrow=c(1,1))
set.seed(2023)
res_var = hclustvar(df_normal)
plot(res_var)
stab = stability(res_var, B=100,graph=T)
res_k_mean_var =  kmeansvar(df_normal, init=5)
res_k_mean_var$cluster
res_k_mean_var$E
coef_clust_var = res_k_mean_var$coef
