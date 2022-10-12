############################## scale ######################################
cotton
scd <- scale(cotton[,-1:-2], center = T)
scd
############################# options #####################################
options(max.print = 100000)
options(scipen = 100)  # set wd
############################ visualization package #########################
install.packages("factoextra")
library("factoextra")
install.packages("pca3d")
library(pca3d)
############################ built in ####################################
# spectral decomposition approach
pca1 <- princomp(scd)
pca1
1.7505264^2
biplot(pca1)
# singular value decomposition
pca2 <- prcomp(scd)
pca2
pca2$x
summary(pca2)
biplot(pca2)
pca1$scores == pca2$x # only use one function
fviz_pca(pca2)
fviz_pca_var(pca2)
fviz_pca_var(pca2, repel = T)
fviz_pca_ind(pca2)
fviz_screeplot(pca2)
############################# 3d visulisation ############################
pca3d(pca2)
?pca3d
pca3d(pca2, show.labels = T)
pca3d(pca2, show.labels = T, fancy = T)
########################### FactoMineR  ####################################
install.packages("FactoMineR")
library(FactoMineR)
############################## pca svd######################################
fpca <- PCA(scd, ncp=18)
fpca
fpca$eig
fpca$ind$coord #  aka scores
fpca$ind$cos2 #  quality of representation It shows the importance of a principal component for a given observation
fpca$var$contrib # contributions 
# biplot
fviz_pca(fpca) 
fviz_pca_biplot(fpca, repel = T) # repulsion avoid overlap
fviz_pca_biplot(fpca, repel = T, col.ind = "cos2") # quality of representation
fviz_pca_biplot(fpca, repel = T, col.ind = "cos2", col.var = "red") # above line
?fviz_pca_biplot
# scatter plot
fviz_pca_ind(fpca) 
fviz_pca_ind(fpca, repel = T)
fviz_pca_ind(fpca, repel = T, col.ind = "cos2")  # quality of representation
# variables
fviz_pca_var(fpca)
fviz_pca_var(fpca, repel = T) # repel labels 
fviz_pca_var(fpca, repel = T, col.var = "contrib") # colour of variables
# scree plot
fviz_screeplot(fpca)
fviz_screeplot(fpca, ncp=18)
fviz_screeplot(fpca, ncp=18, geom="line")
fviz_screeplot(fpca, ncp=18, geom="bar")
fviz_screeplot(fpca, ncp=18, geom="bar", barfill="red") # above line only
fviz_screeplot(fpca, choice="eigenvalue")
fviz_screeplot(fpca, choice="eigenvalue", ncp=18)
fpca$eig
table1<- fpca$eig
class(table1)
table1 <- as.data.frame(table1)
class(table1)
#install.packages(writexl)
library(writexl)
write_xlsx(table1, "table.xlsx")
############################ elbow method ####################################
plot(table1$`cumulative percentage of variance`)
######################## rotated component ####################################
install.packages("psych")
library("psych")
rpca <- principal(scd, nfactors = 7, rotate = "varimax", scores = T)
rpca
rpca$loadings
print(rpca$loadings, digits = 3, cutoff = 0)
rpca$communality
barplot(rpca$loadings)
barplot(rpca$loadings, beside = T)
barplot(rpca$loadings, beside = T, col = "blue", main = "Rotated component matrix")
# R colour palette
install.packages("pals")
library(pals)
barplot(rpca$loadings, beside = T, col = brewer.accent(18), main = "Rotated component matrix")
barplot(rpca$loadings, beside = T, col = brewer.greens(18), main = "Rotated component matrix")
barplot(rpca$loadings, beside = T, col = brewer.spectral(18), main = "Rotated component matrix")
barplot(rpca$loadings, beside = T, col = alphabet(n=18), main = "Rotated component matrix")
?pal.bands
# import scores
rpca$scores
scores <- rpca$scores
scores <- as.data.frame(scores)
class(scores)
write_xlsx(scores, "scores.xlsx")
