#================================
library(dplyr)
library(skimr)
qp.raw<-read.csv2("./qp.csv", sep=",",dec=".") %>% 
  select(-tx_et_eprec,-part_chomprinc,-tx_tot_et,-tx_f_et,-pmimp,-tx_f_eprec,-tx_et_empl) %>%
  mutate(percou=percou/pop_mun,
         a=a/pop_mun,
         abcde=abcde/pop_mun,
         abcde_f=abcde_f/pop_mun,
         abcde_h=abcde_h/pop_mun,
         abc_capbep=abc_capbep/pop_mun,
         abc_bac=abc_bac/pop_mun,
         abc_supbac=abc_supbac/pop_mun,
         ec_mat=ec_mat/pop_mun,
         ec_elem=ec_elem/pop_mun,
         ec_elem_pri=ec_elem_pri/pop_mun,
         coll=coll/pop_mun,
         coll_pri=coll_pri/pop_mun,
         nbetab=nbetab/pop_mun,
         ens_sante=ens_sante/pop_mun,
         serv_par=serv_par/pop_mun,
         serv_tot=serv_tot/pop_mun,
         auto_ent=auto_ent/pop_mun
  ) %>% 
  select(-starts_with("pop"))
qp.raw %>% skim()
write.csv2(qp.raw,"./QP_cleaned.csv")

qp.numeric<-qp.raw %>% select_if(is.numeric)
#median impute ----
for(i in 1:ncol(qp.numeric)){
  qp.numeric[is.na(qp.numeric[,i]), i] <- median(qp.numeric[,i], na.rm = TRUE)
}

#scaling----
qp.scaled<-qp.numeric %>% scale() %>% as.data.frame()
library(FactoMineR)
library(factoextra)
res.pca <- FactoMineR::PCA(qp.scaled,  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

#visualisation des axes 1 et 2
#visualisation des axes 1 et 2 en limitant aux 10 variables qui contribuent le plus
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             select.var=list(contrib=20)
)

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()

#kmeans---
df.kmeans<-as.data.frame(res.pca$ind$coord)
library(cluster)
avg_sil <- function(k) {
  print(paste0("silhouette k:",k))
  km.res <- kmeans(df.kmeans, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df.kmeans))
  return(mean(ss[, 3]))
}
# Set maximum cluster 
max_k <-15
# Run algorithm over a range of k 
avg.silhouette <- sapply(2:max_k, avg_sil)
# Create a data frame to plot the graph
silhouette <-data.frame(2:max_k, avg.silhouette)
# Plot the graph
ggplot(silhouette, aes(x = X2.max_k, y = avg.silhouette)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))

cluster.2 <- kmeans(df.kmeans, 4, nstart = 200)
cluster.2$size
# graph des individus, en fonction des dimensions 1 et 2 de l'ACP
fviz_cluster(cluster.2, data = qp.numeric,geom="point")
cluster.2

qp.clustered <-qp.raw %>%
  mutate(cluster = cluster.2$cluster)
write.csv2(qp.clustered,"./QP_clustered.csv")

qp.numeric.clust<-qp.numeric %>%
  mutate(cluster = cluster.2$cluster)

qp.scaled.clust <-qp.scaled %>%
  mutate(cluster = cluster.2$cluster)

stats<-qp.numeric.clust %>% 
  group_by(cluster) %>%
  summarise_all("mean")

#affichage sous forme de heatmap
library(RColorBrewer)
# create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
# create the heatmap
stats.reshaped<-reshape2::melt(stats, id.vars=c("cluster"))
ggplot(data = stats.reshaped, aes(x = cluster, y =variable, fill = value)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()


"""
cluster 1 : bcp équipements / tx emploi le + élevé / tx chomage élevé également 
            / population + vieille (indice jeunesse le plus bas)=>bcp retraités
            / revenus 'élevés'
cluster 2 : moins d'équipements que le 1 / population un peu plus jeune, un peu moins 'riche'
cluster 3 : bas revenus / taux pauvreté élevé / emplois précaires
cluster 4 : chomage bas / precarite basse / pop assez jeune


