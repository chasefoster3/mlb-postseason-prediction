########## Chase Foster - Honors Thesis ##########


########## Team Analysis ##########

install.packages("xlsx")
install.packages("dplyr")
library(xlsx)
library(dplyr)

teamdata <- xlsx::read.xlsx("C:\\Users\\chase\\OneDrive\\Desktop\\Honors Thesis\\New Team Data\\Team_H&PData_1998-2024.xlsx", sheetIndex = 1)
head(teamdata)

# Remove Unnecessary Variables
teamdata <- teamdata[, -c(1)]
head(teamdata)

dim(teamdata)

# Optional Remove Covid Season Variables
cleaned_data <- teamdata %>%
  filter(Season != 2020)
cleaned_data$Season

head(cleaned_data)


# Still Many Variables Remaining So Perform Principle Component Analysis (PCA)

########## Principal Component Analysis (PCA) ##########

#scale the data- using prcomp() function.
# Treat Cat. Vars. as Factors
pcadat <- data.frame(lapply(teamdata, function(x) {
  if (is.factor(x) || is.character(x)) as.numeric(as.integer(x)) else x
}))

# Remove all columns that have variance=0 (All same values o.w. errors in scaling)
pcadat <- pcadat[, apply(pcadat, 2, var) > 0]

apply(pcadat, 2, var)

# Check structure of numeric data -> team and league produce NAs so remove them
str(pcadat)
pcadat <- pcadat[, -c(2,3)]


teamdata_pca = prcomp(pcadat, scale = TRUE)
teamdata_pca


# Without Covid Season
pcadat2 <- data.frame(lapply(cleaned_data, function(x) {
  if (is.factor(x) || is.character(x)) as.numeric(as.integer(x)) else x
}))

# Remove all columns that have variance=0 (All same values o.w. errors in scaling)
pcadat2 <- pcadat2[, apply(pcadat2, 2, var) > 0]

apply(pcadat2, 2, var)

# Check structure of numeric data -> team and league produce NAs so remove them
str(pcadat2)
pcadat2 <- pcadat2[, -c(2,3)]


cleandata_pca = prcomp(pcadat2, scale = TRUE)
cleandata_pca


#Some of the output of prcomp function is displayed neatly using summary()

summary(teamdata_pca)
names(teamdata_pca)

summary(cleandata_pca)
names(cleandata_pca)


# $center and $scale give the mean and standard deviations for the original 
# variables. 

teamdata_pca$center
teamdata_pca$scale

cleandata_pca$center
cleandata_pca$scale

# $rotation gives the loading vectors that are used to rotate the original 
# data to obtain the principal components.

teamdata_pca$rotation

cleandata_pca$rotation


# The principal components are stored in $x.

head(teamdata_pca$x)

head(cleandata_pca$x)

# To see how the loadings obtain the principal components from the original 
# data. 

scale(as.matrix(pcadat))[1, ] %*% teamdata_pca$rotation[, 1]

scale(as.matrix(pcadat))[1, ] %*% teamdata_pca$rotation[, 2]

scale(as.matrix(pcadat))[1, ] %*% teamdata_pca$rotation[, 3]

scale(as.matrix(pcadat))[1, ] %*% teamdata_pca$rotation[, 4]

head(scale(as.matrix(pcadat)) %*% teamdata_pca$rotation[,1])

head(teamdata_pca$x[, 1])


scale(as.matrix(pcadat2))[1, ] %*% cleandata_pca$rotation[, 1]

scale(as.matrix(pcadat2))[1, ] %*% cleandata_pca$rotation[, 2]

scale(as.matrix(pcadat2))[1, ] %*% cleandata_pca$rotation[, 3]

scale(as.matrix(pcadat2))[1, ] %*% cleandata_pca$rotation[, 4]

head(scale(as.matrix(pcadat2)) %*% cleandata_pca$rotation[,1])

head(cleandata_pca$x[, 1])


# To check that the loading vectors are normalized. (Should sum to 1)

sum(teamdata_pca$rotation[, 1] ^ 2)

sum(cleandata_pca$rotation[, 1] ^ 2)


# Check for orthogonality of both the loading vectors and the principal 
# components. (Note the inner products arent exactly 0, but that is 
# simply a numerical issue.)

teamdata_pca$rotation[, 1] %*% teamdata_pca$rotation[, 2]

teamdata_pca$rotation[, 1] %*% teamdata_pca$rotation[, 3]

teamdata_pca$x[, 1] %*% teamdata_pca$x[, 2]

teamdata_pca$x[, 1] %*% teamdata_pca$x[, 3]


cleandata_pca$rotation[, 1] %*% cleandata_pca$rotation[, 2]

cleandata_pca$rotation[, 1] %*% cleandata_pca$rotation[, 3]

cleandata_pca$x[, 1] %*% cleandata_pca$x[, 2]

cleandata_pca$x[, 1] %*% cleandata_pca$x[, 3]



# A biplot can be used to visualize both the principal component scores 
# and the principal component loadings.

biplot(teamdata_pca, scale = 0, cex = 0.5)

biplot(cleandata_pca, scale = 0, cex = 0.5)


# Frequently we will be interested in the proportion of variance 
# explained by a principal component.

teamdata_pca$sdev

teamdata_pca$sdev ^ 2 / sum(teamdata_pca$sdev ^ 2)

get_PVE = function(pca_out) {
  pca_out$sdev ^ 2 / sum(pca_out$sdev ^ 2)
}

pve = get_PVE(teamdata_pca)
pve


cleandata_pca$sdev

cleandata_pca$sdev ^ 2 / sum(cleandata_pca$sdev ^ 2)

get_PVE = function(pca_out) {
  pca_out$sdev ^ 2 / sum(pca_out$sdev ^ 2)
}

pve2 = get_PVE(cleandata_pca)
pve2

# We can then plot the proportion of variance explained for each PC.
plot(
  pve,
  xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  ylim = c(0, 1),
  type = 'b'
)

plot(
  pve2,
  xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  ylim = c(0, 1),
  type = 'b'
)


# Often we are interested in the cumulative proportion. 
# A common use of PCA outside of visualization is
# dimension reduction for modeling. 
# If p is large, PCA is performed, and the principal components that
# account for a large proportion of variation, say 95%, are used for 
# further analysis. 

cumsum(pve)
cumsum(pve2)

plot(
  cumsum(pve),
  xlab = "Principal Component",
  ylab = "Cumulative Proportion of Variance Explained",
  ylim = c(0, 1),
  type = 'b'
)

plot(
  cumsum(pve2),
  xlab = "Principal Component",
  ylab = "Cumulative Proportion of Variance Explained",
  ylim = c(0, 1),
  type = 'b'
)

# In certain situations that can reduce the dimensionality of data 
# significantly. 

# Choose best # of principal components that correspond to the variance plots
loadings <- teamdata_pca$rotation
print(loadings)

loadings2 <- cleandata_pca$rotation
print(loadings2)

cumsum(pve)    #Approximately 95% of variance explained at PC 18

# Identify the best variables for up to pc 18
loadings <- teamdata_pca$rotation
print(loadings)

top_vars_pc1 <- sort(abs(loadings[, 1]), decreasing = TRUE)
print(top_vars_pc1)

top_vars_pc2 <- sort(abs(loadings[, 2]), decreasing = TRUE)
print(top_vars_pc2)

top_vars_pc3 <- sort(abs(loadings[, 3]), decreasing = TRUE)
print(top_vars_pc3)

top_vars_pc4 <- sort(abs(loadings[, 4]), decreasing = TRUE)
print(top_vars_pc4)

top_vars_pc5 <- sort(abs(loadings[, 5]), decreasing = TRUE)
print(top_vars_pc5)

top_vars_pc6 <- sort(abs(loadings[, 6]), decreasing = TRUE)
print(top_vars_pc6)

top_vars_pc7 <- sort(abs(loadings[, 7]), decreasing = TRUE)
print(top_vars_pc7)

top_vars_pc8 <- sort(abs(loadings[, 8]), decreasing = TRUE)
print(top_vars_pc8)

top_vars_pc9 <- sort(abs(loadings[, 9]), decreasing = TRUE)
print(top_vars_pc9)

top_vars_pc10 <- sort(abs(loadings[, 10]), decreasing = TRUE)
print(top_vars_pc10)

top_vars_pc11 <- sort(abs(loadings[, 11]), decreasing = TRUE)
print(top_vars_pc11)

top_vars_pc12 <- sort(abs(loadings[, 12]), decreasing = TRUE)
print(top_vars_pc12)

top_vars_pc13 <- sort(abs(loadings[, 13]), decreasing = TRUE)
print(top_vars_pc13)

top_vars_pc14 <- sort(abs(loadings[, 14]), decreasing = TRUE)
print(top_vars_pc14)

top_vars_pc15 <- sort(abs(loadings[, 15]), decreasing = TRUE)
print(top_vars_pc15)

top_vars_pc16 <- sort(abs(loadings[, 16]), decreasing = TRUE)
print(top_vars_pc16)

top_vars_pc17 <- sort(abs(loadings[, 17]), decreasing = TRUE)
print(top_vars_pc17)

top_vars_pc18 <- sort(abs(loadings[, 18]), decreasing = TRUE)
print(top_vars_pc18)

# Without Covid Season
top_vars_pc1_clean <- sort(abs(loadings2[, 1]), decreasing = TRUE)
print(top_vars_pc1_clean)

top_vars_pc2_clean <- sort(abs(loadings2[, 2]), decreasing = TRUE)
print(top_vars_pc2_clean)

top_vars_pc3_clean <- sort(abs(loadings2[, 3]), decreasing = TRUE)
print(top_vars_pc3_clean)

top_vars_pc4_clean <- sort(abs(loadings2[, 4]), decreasing = TRUE)
print(top_vars_pc4_clean)

top_vars_pc5_clean <- sort(abs(loadings2[, 5]), decreasing = TRUE)
print(top_vars_pc5_clean)

top_vars_pc6_clean <- sort(abs(loadings2[, 6]), decreasing = TRUE)
print(top_vars_pc6_clean)

top_vars_pc7_clean <- sort(abs(loadings2[, 7]), decreasing = TRUE)
print(top_vars_pc7_clean)

top_vars_pc8_clean <- sort(abs(loadings2[, 8]), decreasing = TRUE)
print(top_vars_pc8_clean)

top_vars_pc9_clean <- sort(abs(loadings2[, 9]), decreasing = TRUE)
print(top_vars_pc9_clean)

top_vars_pc10_clean <- sort(abs(loadings2[, 10]), decreasing = TRUE)
print(top_vars_pc10_clean)

top_vars_pc11_clean <- sort(abs(loadings2[, 11]), decreasing = TRUE)
print(top_vars_pc11_clean)

top_vars_pc12_clean <- sort(abs(loadings2[, 12]), decreasing = TRUE)
print(top_vars_pc12_clean)


# Project Original Scaled Data Onto New Principal Components Scores
pca_scores <- teamdata_pca$x[, 1:18]
pca_scores

pca_scores2 <- cleandata_pca$x[, 1:4]
pca_scores2

#Continue with Clustering Using PCs



##### Hierarchical Clustering #####
hc.complete=hclust(dist(teamdata),method="complete")

plot(hc.complete,main="Complete Linkage", xlab="",sub="",cex=.9)

cutree(hc.complete,2)

par(mfrow=c(1,2))
plot(hclust(dist(teamdata),method="complete"),main="Hierarchical Clustering (Data not Scaled)")
rect.hclust(hclust(dist(teamdata),method="complete"), k = 2, border = "red")


hc.complete2=hclust(dist(cleaned_data),method="complete")

plot(hc.complete2,main="Complete Linkage", xlab="",sub="",cex=.9)

cutree(hc.complete2,2)

par(mfrow=c(1,2))
plot(hclust(dist(cleaned_data),method="complete"),main="Hierarchical Clustering (Data not Scaled)")
rect.hclust(hclust(dist(cleaned_data),method="complete"), k = 2, border = "red")

# Treat Cat. Vars. as Factors
factdat <- data.frame(lapply(teamdata, function(x) {
  if (is.factor(x) || is.character(x)) as.numeric(as.integer(x)) else x
}))

factdat2 <- data.frame(lapply(cleaned_data, function(x) {
  if (is.factor(x) || is.character(x)) as.numeric(as.integer(x)) else x
}))

# Scale Data
xsc=scale(factdat)
hc.scale=hclust(dist(xsc),method="complete")

plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled Features")
rect.hclust(hclust(dist(xsc),method="complete"), k = 2, border = "red")


xsc2=scale(factdat2)
hc.scale2=hclust(dist(xsc2),method="complete")

plot(hclust(dist(xsc2),method="complete"),main="Original Data")
rect.hclust(hclust(dist(xsc2),method="complete"), k = 2, border = "red")

# Principal Component Clustering
hc.pc=hclust(dist(pca_scores),method="complete")

plot(hc.pc,main="Complete Linkage", xlab="",sub="",cex=.9)

hc.pc_clusters<-cutree(hc.pc,3)
hc.pc_clusters

par(mfrow=c(1,2))
plot(hclust(dist(pca_scores),method="complete"),main="Hierarchical Clustering (PCs)")
rect.hclust(hclust(dist(pca_scores),method="complete"), k = 3, border = "blue")
rect.hclust(hclust(dist(pca_scores),method="complete"),k=2, border="red")


hc.pc2=hclust(dist(pca_scores2),method="complete")

plot(hc.pc2,main="Complete Linkage", xlab="",sub="",cex=.9)

hc.pc_clusters2<-cutree(hc.pc2,3)
hc.pc_clusters2

par(mfrow=c(1,2))
plot(hclust(dist(pca_scores2),method="complete"),main="Hierarchical Clustering (PCs)")
rect.hclust(hclust(dist(pca_scores2),method="complete"), k = 3, border = "blue")
rect.hclust(hclust(dist(pca_scores2),method="complete"),k=2, border="red")


######### Altering Dendrograms ################
hcd=as.dendrogram(hc.complete)
plot(hcd)
plot(hcd,type="triangle")

scd=as.dendrogram(hc.scale)
plot(scd)
plot(scd, type="triangle")

pcd=as.dendrogram(hc.pc)
plot(pcd)
plot(pcd,type="triangle")

# Cleaned data
hcd2=as.dendrogram(hc.complete2)
plot(hcd2)
plot(hcd2,type="triangle")

scd2=as.dendrogram(hc.scale2)
plot(scd2)
plot(scd2, type="triangle")

pcd2=as.dendrogram(hc.pc2)
plot(pcd2)
plot(pcd2,type="triangle")


par(mfrow = c(2, 1))
plot(cut(hcd, h = 600)$upper, main = "Upper tree of cut at h=4")
plot(cut(hcd, h = 600)$lower[[2]], main = "Second branch of lower tree with cut at h=4")

par(mfrow = c(2, 1))
plot(cut(scd, h = 600)$upper, main = "Upper tree of cut at h=4")
plot(cut(scd, h = 600)$lower[[2]], main = "Second branch of lower tree with cut at h=4")


par(mfrow = c(2, 1))
plot(cut(pcd, h = 600)$upper, main = "Upper tree of cut at h=4")
plot(cut(pcd, h = 600)$lower[[2]], main = "Second branch of lower tree with cut at h=4")

# Cleaned Data
par(mfrow = c(2, 1))
plot(cut(hcd2, h = 600)$upper, main = "Upper tree of cut at h=4")
plot(cut(hcd2, h = 600)$lower[[2]], main = "Second branch of lower tree with cut at h=4")

par(mfrow = c(2, 1))
plot(cut(scd2, h = 600)$upper, main = "Upper tree of cut at h=4")
plot(cut(scd2, h = 600)$lower[[2]], main = "Second branch of lower tree with cut at h=4")

par(mfrow = c(2, 1))
plot(cut(pcd2, h = 600)$upper, main = "Upper tree of cut at h=4")
plot(cut(pcd2, h = 600)$lower[[2]], main = "Second branch of lower tree with cut at h=4")


install.packages("dendextend")
library(dendextend)

dend1 <- color_branches(hcd2, k = 2)
dend2 <- color_labels(hcd2, k = 2)
dend3 <- hcd2 %>% color_branches(k=2) %>% color_labels(k=2)

par(mfrow = c(1,3))
plot(dend1, main = "Colored branches")
plot(dend2, main = "Colored labels")
plot(dend3 ,main="Colored branches and labels")

#Cleaned Data
dend12 <- color_branches(scd2, k = 2)
dend22 <- color_labels(scd2, k = 2)
dend32 <- scd2 %>% color_branches(k=2) %>% color_labels(k=2)
dend33 <- pcd2 %>% color_branches(k=2) %>% color_labels(k=2)

par(mfrow = c(1,3))
plot(dend12, main = "Colored branches")
plot(dend22, main = "Colored labels")
plot(dend32 ,main="Original Data")
plot(dend33, main="Principal Components")


dend1sc <- color_branches(scd, k = 2)
dend2sc <- color_labels(scd, k = 2)
dend3sc <- scd %>% color_branches(k=2) %>% color_labels(k=2)

par(mfrow = c(1,3))
plot(dend1sc, main = "Colored branches")
plot(dend2sc, main = "Colored labels")
plot(dend3sc ,main="Colored branches and labels")

#Cleaned Data
dend1sc2 <- color_branches(scd2, k = 2)
dend2sc2 <- color_labels(scd2, k = 2)
dend3sc2 <- scd2 %>% color_branches(k=2) %>% color_labels(k=2)

par(mfrow = c(1,3))
plot(dend1sc2, main = "Colored branches")
plot(dend2sc2, main = "Colored labels")
plot(dend3sc2 ,main="Colored branches and labels")


dend1pc <- color_branches(pcd, k = 3)
dend2pc <- color_labels(pcd, k = 3)
dend3pc <- pcd %>% color_branches(k=3) %>% color_labels(k=3)

par(mfrow = c(1,3))
plot(dend1pc, main = "Colored branches")
plot(dend2pc, main = "Colored labels")
plot(dend3pc ,main="Colored branches and labels")

#Cleaned Data
dend1pc2 <- color_branches(pcd2, k = 3)
dend2pc2 <- color_labels(pcd2, k = 3)
dend3pc2 <- pcd2 %>% color_branches(k=3) %>% color_labels(k=3)

par(mfrow = c(1,3))
plot(dend1pc2, main = "Colored branches")
plot(dend2pc2, main = "Colored labels")
plot(dend3pc2 ,main="Colored branches and labels")


############ Complete Hierarchical Clustering Analysis Using the PCs 1:18 ############

# Calculate centroids for each cluster (on principal components) (Shows distance and which are closer or farther apart)
cluster_cut <- cutree(pcd2, k = 2)
aggregate(teamdata_pca$x[, 1:4], by = list(cluster_cut), FUN = mean)

cluster_cut2 <- cutree(pcd2, k = 2)
aggregate(cleandata_pca$x[, 1:4], by = list(cluster_cut2), FUN = mean)

# Summary of original variables by cluster
clustdat <- cbind(pcadat, cluster_cut)
aggregate(clustdat[, -ncol(clustdat)], by = list(cluster_cut), FUN = summary)

clustdat2 <- cbind(pcadat2, cluster_cut2)
aggregate(clustdat2[, -ncol(clustdat2)], by = list(cluster_cut2), FUN = summary)


# Silhouette Analysis
library(cluster)
sil <- silhouette(cluster_cut, dist(teamdata_pca$x[, 1:18]))
plot(sil, main = "Silhouette Plot")

sil2 <- silhouette(cluster_cut2, dist(cleandata_pca$x[, 1:4]))
plot(sil2, main = "Silhouette Plot")


# Cluster Means Analysis to identify how the mean of the variables differ between each cluster
clustdat <- cbind(pcadat, cluster = cluster_cut)

clustdat2 <- cbind(pcadat2, cluster = cluster_cut2)

# Compute the mean of each variable for each cluster
cluster_means <- aggregate(. ~ cluster, data = clustdat, FUN = mean)
print(cluster_means)

cluster_means2 <- aggregate(. ~ cluster, data = clustdat2, FUN = mean)
print(cluster_means2)

############ Box Plot of Variables by Cluster
library(ggplot2)

ggplot(clustdat, aes(x = as.factor(cluster), y = WL., fill = as.factor(cluster))) +
  geom_boxplot() +
  ggtitle("Distribution of WL% Across Clusters") +
  xlab("Cluster") + ylab("WL%") +
  theme_minimal()

ggplot(clustdat2, aes(x = as.factor(cluster), y = WL., fill = as.factor(cluster))) +
  geom_boxplot() +
  ggtitle("Distribution of WL% Across Clusters") +
  xlab("Cluster") + ylab("WL%") +
  theme_minimal()

dim(clustdat)

dim(clustdat2)

summary(clustdat2)


# Compute means of principal component scores per cluster
cluster_pc_means <- aggregate(teamdata_pca$x[, 1:18], by = list(cluster_cut), FUN = mean)
print(cluster_pc_means)

cluster_pc_means2 <- aggregate(cleandata_pca$x[, 1:4], by = list(cluster_cut2), FUN = mean)
print(cluster_pc_means2)


############# Radar Plot
install.packages("fmsb")  
library(fmsb) 

# Normalize data
normalized_data <- as.data.frame(scale(cluster_means[,-1]))

normalized_data2 <- as.data.frame(scale(cluster_means2[,-1]))

# Add min-max rows for visualization purposes
min_vals <- apply(normalized_data, 2, min)
max_vals <- apply(normalized_data, 2, max)
normalized_data <- rbind(max_vals, min_vals, normalized_data)

min_vals2 <- apply(normalized_data2, 2, min)
max_vals2 <- apply(normalized_data2, 2, max)
normalized_data2 <- rbind(max_vals2, min_vals2, normalized_data2)

# Radar plot for the first cluster
radarchart(normalized_data[1:3,], title = paste("Cluster", 1, "Profile"))
radarchart(normalized_data[c(1,2,4),], title = paste("Cluster", 2, "Profile"))
radarchart(normalized_data[c(1,2,5),], title = paste("Cluster", 3, "Profile"))

par(mfrow(2,1))
radarchart(normalized_data2[1:3,], title = paste("Cluster", 1, "Profile"))
radarchart(normalized_data2[c(1,2,4),], title = paste("Cluster", 2, "Profile"))



#### Correlation-based distance ##################

dd=as.dist(1- cor(t(pcadat)))

plot(hclust(dd, method ="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub ="")


dd2=as.dist(1- cor(t(pcadat2)))

plot(hclust(dd2, method ="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub ="")


#################### K-means Clustering ###################

####Using Principal Component Analysis
wss <- sapply(1:10, function(k) {
  kmeans(pca_scores, centers = k, nstart = 20)$tot.withinss
})

wss2 <- sapply(1:10, function(k) {
  kmeans(pca_scores2, centers = k, nstart = 20)$tot.withinss
})

# Plot WSS to find the "elbow"
png("KMeans_PCA_Plot.png", width = 1200, height = 800)
plot(1:10, wss, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")
dev.off()

png("KMeans_PCA_Plot_WO2020.png", width = 1200, height = 800)
plot(1:10, wss2, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")
dev.off()

# k=3 clusters
km.out=kmeans(pca_scores,3,nstart=20)
km.out$cluster

km.out2=kmeans(pca_scores2,4,nstart=20)
km.out4$cluster


plot(pca_scores, col = (km.out$cluster + 1), main = "Clustering Results with K=2", pch = 20, cex = 2)

plot(pca_scores2, col = (km.out2$cluster + 1), main = "Clustering Results with K=2", pch = 20, cex = 2)

png("clustering_plot.png", width = 1200, height = 800)
plot(pca_scores, col = (km.out$cluster + 1), main = "Clustering Results with K=3", pch = 20, cex = 2)
dev.off()
getwd()

png("clustering_plot2.png", width = 1200, height = 800)
plot(pca_scores2, col = (km.out2$cluster + 1), main = "Clustering Results with K=2", pch = 20, cex = 2)
dev.off()
getwd()

# k=3 clusters, 1 random start
km.out1=kmeans(pca_scores,3,nstart=1)
#Total within cluster sum of squares
km.out1$tot.withinss

# k=3 clusters, 1 random start
km.out12=kmeans(pca_scores2,2,nstart=1)
#Total within cluster sum of squares
km.out12$tot.withinss

# k=3, 20 random starts
km.out20=kmeans(pca_scores,3,nstart=20)
#Total within cluster sum of squares
km.out20$tot.withinss

# k=2, 20 random starts
km.out202=kmeans(pca_scores2,2,nstart=20)
#Total within cluster sum of squares
km.out202$tot.withinss


######### RAND INDEX??? #############
install.packages("cluster")   
install.packages("mclust") 
install.packages("fossil")
library(cluster)
library(mclust)
library(fossil)

hc.pc_clusters
km.out$cluster

hc.pc_clusters2
km.out2$cluster

# Rand Index = 0.5437
ri_score <- rand.index(hc.pc_clusters, km.out$cluster)
print(ri_score)

ri_score2 <- rand.index(hc.pc_clusters2, km.out2$cluster)
print(ri_score2)

# Adjusted Rand Index = 0.0838 meaning the two clustering methods hardly agree
ari_score <- adjustedRandIndex(hc.pc_clusters, km.out$cluster)
print(paste("Adjusted Rand Index:", ari_score))

ari_score2 <- adjustedRandIndex(hc.pc_clusters2, km.out2$cluster)
print(paste("Adjusted Rand Index:", ari_score2))


######### Cross Validation ###############

#Cluster Stability Test With BOOTSTRAPPING
install.packages("fpc")
library(fpc)

# Perform k-means clustering with resampling-based stability
kmeans_stability <- clusterboot(pcadat, clustermethod = kmeansCBI, k = 3, B = 100)
print(kmeans_stability$bootmean) 
print(kmeans_stability$bootbrd)   # How many times each cluster was recovered

kmeans_stability2 <- clusterboot(pcadat2, clustermethod = kmeansCBI, k = 4, B = 100)
print(kmeans_stability2$bootmean) 
print(kmeans_stability2$bootbrd)   # How many times each cluster was recovered


# All values for the clusters are extremely close to 1, so kmeans produces highly stable clusters

# Perform complete hierarchical clustering with resampling-based stability
dist_matrix <- dist(pcadat)
dist_matrix2 <- dist(pcadat2)

hc_stability <- clusterboot(dist_matrix, 
                            clustermethod = hclustCBI, 
                            method = "complete", 
                            k = 3,  # Number of clusters
                            B = 100)  # Number of bootstrap iterations

print(hc_stability$bootmean)  # Average stability per cluster
print(hc_stability$bootbrd)   # How many times each cluster was recovered

hc_stability2 <- clusterboot(dist_matrix2, 
                            clustermethod = hclustCBI, 
                            method = "complete", 
                            k = 2,  # Number of clusters
                            B = 100)  # Number of bootstrap iterations

print(hc_stability2$bootmean)  # Average stability per cluster
print(hc_stability2$bootbrd)   # How many times each cluster was recovered


#Only cluster 1 has a value close to 1, so it is stable but the others have values close to 0.5 meaning less stability


### Compare the Clustering Methods Using Silhouette Means
sil_kmeans <- mean(silhouette(km.out$cluster, dist(pcadat))[, 3])
sil_hc <- mean(silhouette(hc.pc_clusters, dist(pcadat))[, 3])

cat("Silhouette Score - K-means:", sil_kmeans, "\n")
cat("Silhouette Score - Hierarchical:", sil_hc, "\n")

sil_kmeans2 <- mean(silhouette(km.out2$cluster, dist(pcadat2))[, 3])
sil_hc2 <- mean(silhouette(hc.pc_clusters2, dist(pcadat2))[, 3])

cat("Silhouette Score - K-means:", sil_kmeans2, "\n")
cat("Silhouette Score - Hierarchical:", sil_hc2, "\n")

# Hierarchical has a higher silhouette score so we would choose hierarchical



######### Identify most important clustering variables #############

#With COVID SEASON - H:OBP,OPS,BA,SO(H),R/Gm,SLG | P:SO(P),SO/9,SO/BB,ERA+,H/9,BB/9,HR/9,Season

#W/O COVID SEASON - H:SO,AB,OBP,H,BA,OPS,R/Gm,LOB,SLG | P:So/9,ERA+,ERA,WHIP,BB/9,H/9,SO/BB,HR/9,R(P)



#######################################################################
#######################################################################
###################### Supervised Learning ############################
#######################################################################
#######################################################################

##### Project Code (FIT TO NEW DATA!!!) #####
#############################################
head(teamdata)
dim(teamdata)

postdata_full <- xlsx::read.xlsx("C:\\Users\\chase\\OneDrive\\Desktop\\Honors Thesis\\New Team Data\\PlayoffData.xlsx", sheetIndex = 1)
head(postdata_full)
dim(postdata_full)

postdata <- postdata_full[, -c(1, 2, 3)] #Remove Rank, Team, League
head(postdata)

postdata$Lg <- as.factor(postdata$Lg)
postdata$Playoffs <- as.factor(postdata$Playoffs)

str(postdata)
dim(postdata)

postdata <- postdata[, !names(postdata) %in% "NA."]
names(postdata)

# Scale Data
numeric_vars <- sapply(postdata, is.numeric)
scaledata <- postdata
scaledata[, numeric_vars] <- scale(postdata[, numeric_vars])

head(scaledata)

# Validation Set Approach
set.seed(123)

library(FNN)

dim(scaledata)

index.mlb<-sample(1:810,0.8*810)
trainmlb<-scaledata[index.mlb,]
testmlb<-scaledata[-index.mlb,]

dim(trainmlb)
dim(testmlb)

head(trainmlb)
head(testmlb)

# Full Model
lin.mod.mlb <- glm(Playoffs ~ ., data = trainmlb, family = binomial)
summary(lin.mod.mlb)

# Predict on validation set
pred_probs <- predict(lin.mod.mlb, newdata = testmlb, type = "response")
pred_class <- ifelse(pred_probs > 0.5, "Yes", "No")
pred_class <- factor(pred_class, levels = levels(testmlb$Playoffs))

# Evaluate
table(Predicted = pred_class, Actual = testmlb$Playoffs)
mean(pred_class == testmlb$Playoffs)  # Accuracy


# Reduced Model Using Classification Columns
red.mod.mlb <- glm(Playoffs ~ SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1, data = trainmlb, family = binomial)
summary(red.mod.mlb)

# Predict on validation set
pred_probs1 <- predict(red.mod.mlb, newdata = testmlb, type = "response")
pred_class1 <- ifelse(pred_probs1 > 0.5, "Yes", "No")
pred_class1 <- factor(pred_class1, levels = levels(testmlb$Playoffs))

# Evaluate
table(Predicted = pred_class1, Actual = testmlb$Playoffs)
mean(pred_class1 == testmlb$Playoffs)  # Accuracy


# Further Reduced Model Using Significant Variables
red.mod.mlb2 <- glm(Playoffs ~ OBP+H+BA+OPS+R.Gm+SLG+ERA+SO.BB+R.1, data = trainmlb, family = binomial)
summary(red.mod.mlb2)

# Predict on validation set
pred_probs2 <- predict(red.mod.mlb2, newdata = testmlb, type = "response")
pred_class2 <- ifelse(pred_probs2 > 0.5, "Yes", "No")
pred_class2 <- factor(pred_class2, levels = levels(testmlb$Playoffs))

# Evaluate
table(Predicted = pred_class2, Actual = testmlb$Playoffs)
mean(pred_class2 == testmlb$Playoffs)  # Accuracy



# KNN Classification
head(trainmlb)
train_labels <- as.factor(trainmlb[, 1])
test_labels <- as.factor(testmlb[, 1])


knn.1 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 1)
mean(knn.1 == test_labels)
#Test Errpr=0.8209877 (Good)

table(Predicted = knn.1, Actual = test_labels)


knn.2 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 2)
mean(knn.2 == test_labels)
#Test Error=0.8209877 (Good)

table(Predicted = knn.2, Actual = test_labels)


knn.3 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 3)
mean(knn.3 == test_labels)
#Test Error=0.845679 (Better)

table(Predicted = knn.3, Actual = test_labels)


knn.4 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 4)
mean(knn.4 == test_labels)
#Test Error=0.8641975 (Very Good)

table(Predicted = knn.4, Actual = test_labels)


knn.5 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 5)
mean(knn.5 == test_labels)
#Test Error=0.8641975 (Very Good)

table(Predicted = knn.5, Actual = test_labels)


knn.7 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 7)
mean(knn.7 == test_labels)
#Test Error=0.8641975 (Very Good)

table(Predicted = knn.7, Actual = test_labels)


knn.10 <- knn(train = trainmlb[,-c(1,2)],  
             test = testmlb[,-c(1,2)],    
             cl = train_labels,
             k = 10)
mean(knn.10 == test_labels)
#Test Error=0.8703704 (Best)

table(Predicted = knn.10, Actual = test_labels)



########## Cross Validation ##########
library(boot)
library(caret)


# Full Model
head(lin.mod.mlb)

# Reduced Model
head(red.mod.mlb)

# New Model
# Reduced Model
head(red.mod.mlb2)


# 5-fold Cross Validation for Multiple Regression
library(caret)

# Set up 5-fold cross-validation
ctrl5 <- trainControl(method = "cv", number = 5)


# Fit logistic regression with CV for FULL
cv_full <- train(Playoffs ~ ., 
                  data = scaledata, 
                  method = "glm",
                  family = binomial,
                  trControl = ctrl5)
# Accuracy Full = 0.9086328
cv_full$results$Accuracy


# Fit logistic regression with CV for RED1
cv_red1 <- train(Playoffs ~ SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1, 
                 data = scaledata, 
                 method = "glm",
                 family = binomial,
                 trControl = ctrl5)
# Accuracy Full = 0.8741106
cv_red1$results$Accuracy


# Fit logistic regression with CV for RED2
cv_red2 <- train(Playoffs ~ OBP+H+BA+OPS+R.Gm+SLG+ERA+SO.BB+R.1, 
                 data = scaledata, 
                 method = "glm",
                 family = binomial,
                 trControl = ctrl5)
# Accuracy Full = 0.8827447
cv_red2$results$Accuracy


# Set up 10-fold cross-validation
ctrl10 <- trainControl(method = "cv", number = 10)

# Fit logistic regression with CV for FULL
cv_full2 <- train(Playoffs ~ ., 
                 data = scaledata, 
                 method = "glm",
                 family = binomial,
                 trControl = ctrl10)
# Accuracy Full = 0.902369
cv_full2$results$Accuracy


# Fit logistic regression with CV for RED1
cv_red12 <- train(Playoffs ~ SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1, 
                 data = scaledata, 
                 method = "glm",
                 family = binomial,
                 trControl = ctrl10)
# Accuracy Full = 0.8825523
cv_red12$results$Accuracy


# Fit logistic regression with CV for RED2
cv_red22 <- train(Playoffs ~ OBP+H+BA+OPS+R.Gm+SLG+ERA+SO.BB+R.1, 
                 data = scaledata, 
                 method = "glm",
                 family = binomial,
                 trControl = ctrl10)
# Accuracy Full = 0.8778647
cv_red22$results$Accuracy


### LDA - (Internally standardizes so no need to scale data ###
library(MASS)
library(caret)

head(scaledata)

# Full LDA
lda_full <- lda(Playoffs ~ ., data = trainmlb)
print(lda_full)

lda_full_test <- lda(Playoffs ~., data=testmlb)

# Predict on the same or test data
lda_pred <- predict(lda_full_test)

# Confusion matrix
table(Predicted = lda_pred$class, Actual = testmlb$Playoffs)

# Accuracy = 0.9382716
mean(lda_pred$class == testmlb$Playoffs)


# Cross Validation to check
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Fit the LDA model using caret
# Replace 'target' with your actual target variable column name, and adjust predictors accordingly
lda_model_cv <- train(Playoffs ~ ., data = trainmlb, method = "lda", trControl = train_control)

# Print the cross-validation results
print(lda_model_cv)

# You can also access the results for more detailed evaluation
# Accuracy and other metrics
lda_model_cv$results


# Reduced LDA
lda_red <- lda(Playoffs ~ SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1, data = trainmlb)
print(lda_red)

lda_red_test <- lda(Playoffs ~SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1, data=testmlb)

# Predict on the same or test data
lda_pred1 <- predict(lda_red_test)

# Confusion matrix
table(Predicted = lda_pred1$class, Actual = testmlb$Playoffs)

# Accuracy = 0.8864198
mean(lda_pred1$class == testmlb$Playoffs)


# Further Reduced LDA
lda_red2 <- lda(Playoffs ~ OBP+H+BA+OPS+R.Gm+SLG+ERA+SO.BB+R.1, data = trainmlb)
print(lda_red2)

lda_red2_test <- lda(Playoffs ~OBP+H+BA+OPS+R.Gm+SLG+ERA+SO.BB+R.1, data=testmlb)

# Predict on the same or test data
lda_pred2 <- predict(lda_red2_test)

# Confusion matrix
table(Predicted = lda_pred2$class, Actual = testmlb$Playoffs)

# Accuracy = 0.8777778
mean(lda_pred2$class == testmlb$Playoffs)


# Set up 10-fold CV for LDA
ldactrl <- trainControl(method = "cv", number = 10)

# Fit FULL LDA with CV
lda_cvf <- train(Playoffs ~ ., 
                data = postdata, 
                method = "lda", 
                trControl = ldactrl)

# Accuracy = 0.9073957
lda_cvf$results$Accuracy


# Fit RED LDA with CV
lda_cvr <- train(Playoffs ~ SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1, 
                 data = postdata, 
                 method = "lda", 
                 trControl = ldactrl)

# Accuracy = 0.8728053
lda_cvr$results$Accuracy


# Fit RED2 LDA with CV
lda_cvr2 <- train(Playoffs ~ OBP+H+BA+OPS+R.Gm+SLG+ERA+SO.BB+R.1, 
                 data = postdata, 
                 method = "lda", 
                 trControl = ldactrl)

# Accuracy = 0.8739103
lda_cvr2$results$Accuracy



# Non-Linear Relationships between Playoffs (most significant predictor)?

#################
#Fit some non-linear models to the Default data with default as the response 
#variable. Is there evidence for non-linear relationships in this data set? 
head(trainmlb)
summary(red.mod.mlb2)

##### Polynomials #####
# OPS for Hitting
fit.1h<- glm(Playoffs~OPS ,data=trainmlb, family = binomial)
fit.2h<- glm(Playoffs~poly(OPS,2) ,data=trainmlb, family = binomial)
fit.3h<- glm(Playoffs~poly(OPS,3) ,data=trainmlb, family = binomial)
fit.4h<- glm(Playoffs~poly(OPS,4) ,data=trainmlb, family = binomial)
fit.5h<- glm(Playoffs~poly(OPS,5) ,data=trainmlb, family = binomial)

summary(fit.1h)

anova(fit.1h, fit.2h, fit.3h, fit.4h, fit.5h)

# SO/BB for Pitching
fit.1p<- glm(Playoffs~SO.BB ,data=trainmlb, family = binomial)
fit.2p<- glm(Playoffs~poly(SO.BB,2) ,data=trainmlb, family = binomial)
fit.3p<- glm(Playoffs~poly(SO.BB,3) ,data=trainmlb, family = binomial)
fit.4p<- glm(Playoffs~poly(SO.BB,4) ,data=trainmlb, family = binomial)
fit.5p<- glm(Playoffs~poly(SO.BB,5) ,data=trainmlb, family = binomial)

summary(fit.1p)

anova(fit.1p, fit.2p, fit.3p, fit.4p, fit.5p)


#CV for Polynomials
#Hitting
kcv.5poly1<-cv.glm(trainmlb,fit.1h,K=10)
kcv.5poly1$delta[1]

kcv.5poly2<-cv.glm(trainmlb,fit.2h,K=10)
kcv.5poly2$delta[1]

kcv.5poly3<-cv.glm(trainmlb,fit.3h,K=10)
kcv.5poly3$delta[1]

kcv.5poly4<-cv.glm(trainmlb,fit.4h,K=10)
kcv.5poly4$delta[1]

kcv.5poly5<-cv.glm(trainmlb,fit.5h,K=10)
kcv.5poly5$delta[1]

#Pitching
kcv.5poly1p<-cv.glm(trainmlb,fit.1p,K=10)
kcv.5poly1p$delta[1]

kcv.5poly2p<-cv.glm(trainmlb,fit.2p,K=10)
kcv.5poly2p$delta[1]

kcv.5poly3p<-cv.glm(trainmlb,fit.3p,K=10)
kcv.5poly3p$delta[1]

kcv.5poly4p<-cv.glm(trainmlb,fit.4p,K=10)
kcv.5poly4p$delta[1]

kcv.5poly5p<-cv.glm(trainmlb,fit.5p,K=10)
kcv.5poly5p$delta[1]


#There is evidence of a non-linear relationship between Playoffs and team OPS (3)


##### Splines #####
library(splines)
library(boot)

spl1<-glm(Playoffs~bs(SO.BB ,df=6 ),data=trainmlb, family=binomial)
summary(spl1)

predsspl<-predict(spl1,newdata=list(SO.BB=P.grid),se=TRUE,type="response")
se.bandsspl<-cbind(predsspl$fit+2*predsspl$se.fit,predsspl$fit-2*predsspl$se.fit)


plot(scaledata$SO.BB,scaledata$Playoffs,xlim=Plims ,cex =.5, col =" black ", xlab="SO.BB", ylab="Playoffs")
title("Spline",outer =F)
lines(P.grid ,predsspl$fit ,lwd =2, col =" blue ")
matlines(P.grid,se.bandsspl ,lwd =1, col =" black",lty =3)

kcv.10spl<-cv.glm(trainmlb,spl1,K=10)
kcv.10spl$delta[1]

# Natural Spline
spln<-glm(Playoffs~ns(SO.BB,df =4) ,data=trainmlb, family=binomial)
summary(spln)

predsspln<-predict(spln,newdata=list(SO.BB=P.grid),se=TRUE,type="response")
se.bandsspln<-cbind(predsspln$fit+2*predsspln$se.fit,predsspln$fit-2*predsspln$se.fit)


plot(scaledata$SO.BB,scaledata$Playoffs,xlim=Plims ,cex =.5, col =" black ", xlab="OPS", ylab="Playoffs")
title("Natural Spline",outer =F)
lines(P.grid ,predsspln$fit ,lwd =2, col =" red ")
matlines(P.grid,se.bandsspln ,lwd =1, col =" black",lty =3)

kcv.10spln<-cv.glm(trainmlb,spln,K=10)
kcv.10spln$delta[1]


##### Generalized Additive Functions #####
gam.fit<-glm(Playoffs~ns(SO.BB,3),data=trainmlb, family=binomial)
summary(gam.fit)

predsgam<-predict(gam.fit,newdata=list(SO.BB=P.grid),se=TRUE,type="response")
se.bandsgam<-cbind(predsgam$fit+2*predsgam$se.fit,predsgam$fit-2*predsgam$se.fit)

plot(scaledata$SO.BB,scaledata$Playoffs,xlim=Plims ,cex =.5, col =" black ", xlab="OPS", ylab="Playoffs")
title("Generalized Addititive Model",outer =F)
lines(P.grid ,predsgam$fit ,lwd =2, col =" darkgreen ")
matlines(P.grid,se.bandsgam ,lwd =1, col =" black",lty =3)

kcv.10gam<-cv.glm(trainmlb,gam.fit,K=10)
kcv.10gam$delta[1]



### Decision Trees
head(scaledata)

#(b) Fit a regression tree to the training set. Plot the tree, and interpret the #results. What test MSE do you obtain?
library(rpart)
library(rpart.plot)

#SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1

##Classification Tree
tree.fitfull <- rpart(Playoffs~.,  
                  data = trainmlb, 
                  method = "class")
rpart.plot(tree.fitfull)

# Predict class labels
tree.pred <- predict(tree.fitfull, newdata = testmlb, type = "class")

# Predict class probabilities
tree.prob <- predict(tree.fitfull, newdata = testmlb, type = "prob")

printcp(tree.fitfull)  # Check cross-validation error
bestcp <- tree.fitfull$cptable[which.min(tree.fitfull$cptable[,"xerror"]),"CP"]

# Prune the tree
pruned.treefull <- prune(tree.fitfull, cp = bestcp)
rpart.plot(pruned.treefull)

##Classification Tree Reduced
tree.fitred <- rpart(Playoffs~SO+AB+OBP+H+BA+OPS+R.Gm+LOB+SLG+SO9+ERA.+ERA+WHIP+BB9+H9+SO.BB+HR9+R.1,  
                      data = trainmlb, 
                      method = "class")
rpart.plot(tree.fitred)

# Predict class labels
tree.pred3 <- predict(tree.fitred, newdata = testmlb, type = "class")

# Predict class probabilities
tree.prob3 <- predict(tree.fitred, newdata = testmlb, type = "prob")

printcp(tree.fitred)  # Check cross-validation error
bestcp3 <- tree.fitred$cptable[which.min(tree.fitred$cptable[,"xerror"]),"CP"]

# Prune the tree
pruned.treefull <- prune(tree.fitfull, cp = bestcp)
rpart.plot(pruned.treefull)


#Random Forest & Bagging
install.packages("randomForest")
library(randomForest)

head(trainmlb)

trainbag <- trainmlb[,-c(3,4,5,6)]  #Remove Win Loss and WL% Variables
head(trainbag)

testbag <- testmlb[,-c(3,4,5,6)]
head(testbag)
dim(testbag)

#ERA+, SV, OPS+, RA MOST IMPORTANT VARIABLES
Bagging<-randomForest(Playoffs~., data=trainbag, mtry=53, ntree=500, importance=T)
importance(Bagging)
varImpPlot(Bagging)

preds.bagfull <- predict(Bagging, newdata = testbag, type = "class")

# Classification error (proportion of incorrect predictions)  Test Error = 0.154321
bagfull.err <- mean(preds.bagfull != testbag$Playoffs)
bagfull.err


# ERA+, RA, OPS+, SV
RandomForest<-randomForest(Playoffs~., data=trainbag, mtry=18, ntree=500, importance=T)
importance(RandomForest)
varImpPlot(RandomForest)


preds.rffull <- predict(RandomForest, newdata = testbag, type = "class")

# Classification error (proportion of incorrect predictions)  Test Error = 0.117284
rffull.err <- mean(preds.rffull != testbag$Playoffs)
rffull.err



### Decision Trees w/o W,L,WL%
class.tree<-rpart(Playoffs~., data=trainbag)
rpart.plot(class.tree)

# Predict class probabilities
classtree.prob <- predict(class.tree, newdata = testbag, type = "prob")

printcp(class.tree)  # Check cross-validation error
bestcp2 <- class.tree$cptable[which.min(class.tree$cptable[,"xerror"]),"CP"]

# Prune the tree
pruned.tree <- prune(class.tree, cp = bestcp2)
rpart.plot(pruned.tree)




##########################################################################
##########################################################################
##################### Model Selection and Prediction #####################
##########################################################################
##########################################################################
preddata <- xlsx::read.xlsx("C:\\Users\\chase\\OneDrive\\Desktop\\Honors Thesis\\New Team Data\\April12-2025MLB-Data.xlsx", sheetIndex = 1)
head(preddata)
dim(preddata)

# Remove columns not included in the training data
head(trainbag)
predmlb <- preddata[, -c(1,2,3,5,8,9)]
# ERA+, RA, OPS+, SV
predmlb$Playoffs <- NA  # Add the placeholder response variable


# Move Playoffs column from last to first
predmlb <- predmlb[, c("Playoffs", setdiff(names(predmlb), "Playoffs"))]
head(predmlb)

head(trainbag)
head(predmlb)

dim(trainbag)
dim(predmlb)

predmlb <- predmlb[, -c(3,4)]

dim(predmlb)
head(predmlb)
head(trainbag)

# Check Column Names
colnames(trainbag[, -1])  # Assuming 'Playoffs' is in the first column

# Check column names of prediction data (excluding 'Playoffs')
colnames(predmlb[, -1])

predmlb$Lg <- as.factor(predmlb$Lg)

train_scaled <- scale(trainbag[, -c(1, which(colnames(trainbag) == "Lg"))])
#You can retrieve the scaling parameters from the training data using the attr() function to get the mean and standard deviation values used for scaling.

#Apply the Same Scaling to the Prediction Data:
  
#You need to apply the same mean and standard deviation from the training set to the predmlb data.

# Get the mean and standard deviation used for scaling the training data
means <- attr(train_scaled, "scaled:center")
sds <- attr(train_scaled, "scaled:scale")

# Now, scale the prediction data (predmlb) using these parameters
pred_scaled <- scale(predmlb[, -c(1, which(colnames(predmlb) == "Lg"))], center = means, scale = sds)

head(trainbag)
head(pred_scaled)

# Add variables back
Playoffs <- rep(NA, nrow(pred_scaled))

# Step 2: Get the Lg column from the original predmlb data
Lg <- predmlb$Lg  # Make sure this is already a factor, or convert it

# Step 3: Combine Playoffs, Lg, and pred_scaled into a new data frame
pred_final <- data.frame(Playoffs, Lg, pred_scaled)

# Predict using the trained model
predicted_classes <- predict(RandomForest, newdata = pred_final)
predicted_classes

#### IT WOULD BE INTERESTING TO LOOK AT RATIO BETWEEN RUNS SCORED AND RUNS ALLOWED AND
#### DETERMINE IF THERE IS A MAGIC NUMBER/PROPORTION THAT A TEAM NEEDS TO MAINTAIN TO MAKE POSTSEASON

#### FOR COUNT VARIABLES, CALCULATE EXPECTED END OF SEASON VALUE BY Stat/games played * 162

### INCLUDE R DOCUMENTATION WEBSITE AND MLB POSTSEASON WEBSITE IN REFERENCES


# Predict probabilities for "Yes" class
prob_yes <- predict(RandomForest, newdata = pred_final, type = "prob")[, "Yes"]

# Get indices of top 10 probabilities
top10_indices <- order(prob_yes, decreasing = TRUE)[1:10]

top10_indices

preddata$Team
# Predicted Playoff Teams

#ATH, ATL, BOS, DET, HOU, LAD, NYM, SDP, NYY, CHC

# Rewritten: AL: ATH, HOU, DET, BOS, NYY and NL: LAD, SDP, CHC, ATL, NYM

?randomForest()
