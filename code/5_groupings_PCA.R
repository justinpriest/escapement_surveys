# Load ketchikan_escapement_2021.R but only run the first part and make ktn_imp4



library(vegan)
library(tidymodels)



#### CREATE DATA ####
ktn_imp4 
# But we need values that are imputed and contain no NAs
ktn_mtrx_imp <- ktn_imp4 %>% dplyr::select(year, stream_name, total_count) %>% 
  pivot_wider(year, names_from = stream_name, values_from = total_count)
ktn_mtrx_imp



#### PCA ####
ktnpca <- princomp(ktn_mtrx_imp[,-1]) # by year
biplot(ktnpca)


pairs(ktn_mtrx_imp[,c(2, 3, 11)])
ktnpca <- princomp(ktn_mtrx_imp[,c(2,3,11)]) # by year
biplot(ktnpca)


ktnpca <- princomp(t(ktn_mtrx_imp)[-1,1:14])
biplot(ktnpca)

ktnpca <- princomp(t(scale(ktn_mtrx_imp))[-1,22:35])
biplot(ktnpca)




#############################
#### CLUSTER AND K-MEANS ####


ktn_mtrx_imp
clustdat <- ktn_mtrx_imp %>% select(-year) 
clustdatscale <- clustdat %>% data.frame() %>% t() %>% scale()


kmeans(clustdat %>% t(), centers = 2)
kmeans(clustdatscale, centers = 2)


ktn_mtrx_imp %>% select(-year) %>% data.frame() %>% t()


hc <- hclust(dist(as.matrix(clustdatscale)))   # apply hierarchical clustering 
plot(hc, xlab = "Cluster groupings", main = "Ketchikan Cluster Dendrogram - Scaled Escapements")   


d <- dist(as.matrix(clustdat %>% t()))   # find distance matrix 
hc <- hclust(d)                # apply hierarchical clustering 
plot(hc)   


# Testing cluster and k-means with PCA
plot(hclust(dist(ktnpca$scores)))
kmeans(ktnpca$scores, centers = 2)
# doesn't result in clear plots


###################
##### SUMMARY #####
# There are two ideal groupings and scaling the data is better
# Scaling takes care of issue of rivers having orders of mag larger escapements
# The two groupings are close to the North end, South end
# Blossom, Marten, Keta, and Tombstone separate out vs all others
# Notably, this doesn't include Humpback, and possibly Carroll






