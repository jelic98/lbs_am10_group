calories <- read_csv('data/fastfood_calories.csv') %>%
  clean_names() %>%
  rename(total_prot=protein) %>%
  mutate(restaurant=str_replace_all(tolower(restaurant), '[^\\w]', '')) %>%
  filter(
    !is.na(fiber),
    !is.na(total_prot),
    restaurant %in% c('mcdonalds', 'burgerking', 'tacobell', 'subway')) %>%
  select(calories, cal_fat, total_fat, sat_fat, trans_fat, cholesterol, sodium, total_carb, fiber, sugar, total_prot)

skimr::skim(calories)

model_kmeans_2clusters<-eclust(calories, "kmeans", k = 2,nstart = 50, graph = FALSE)

model_withClusters <- calories %>%
  mutate(cluster = as.factor(model_kmeans_2clusters$cluster))


#Visualisations  
library(ggpubr)
library(ggplot2)
a<-ggplot(model_withClusters, aes(x = calories, y = total_carb, color =  as.factor(cluster))) +
  geom_jitter()+labs(color = "Cluster")
# Note that geom_jitter adds a small noise to each observation so that we can see overlapping points
a
b<-ggplot(model_withClusters, aes(x = calories, y = cholesterol, color = as.factor(cluster),size=calories)) +
  geom_jitter()+labs(color = "Cluster")
b
#Let's arrange these visualizations so that they fit in the html file nicely
library(gridExtra)
grid.arrange(a, b, nrow = 2)
#Plot centers for k=2
#First generate a new data frame with cluster centers and cluster numbers
cluster_centers<-data.frame(cluster=as.factor(c(1:2)),model_kmeans_2clusters$centers)
#transpose this data frame
cluster_centers_t<-cluster_centers %>% gather(variable,value,-cluster,factor_key = TRUE)
#plot the centers
graphkmeans_2clusters<-ggplot(cluster_centers_t, aes(x = variable, y = value))+  geom_line(aes(color =cluster,group = cluster), linetype = "dashed",size=1)+ geom_point(size=1,shape=4)+geom_hline(yintercept=0)+theme(text = element_text(size=10),
                                                                                                                                                                                                                       axis.text.x = element_text(angle=45, hjust=1),)+ggtitle("K-means Centers k=2")
graphkmeans_2clusters

#Checking if 2 is the optimal cluster 
fviz_nbclust(calories, kmeans, method = "silhouette",k.max = 15)+labs(subtitle = "Silhouette method")

#Making a plot of Clusters 
fviz_cluster(model_kmeans_2clusters, calories, palette = "Set2", ggtheme = theme_minimal())

#DONE!
