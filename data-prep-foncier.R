foncier.raw<-read.csv2("./foncier.csv", sep=",",dec=".")

library(skimr)
library(dplyr)
foncier.clean<-foncier.raw %>%
  filter(nature_mutation=="Vente") %>% 
  filter(valeur_fonciere>5000) %>% 
  filter(type_local=="Appartement" | type_local=="Maison") %>% 
  select(code_departement,code_commune,code_qp,nom_qp,nombre_lot,
         id_mutation,annee_mutation,nature_mutation,type_local,
         longitude,latitude,valeur_fonciere, surface_reelle_bati,surface_terrain,
         nombre_pieces_principales)

foncier.clean.agg<-foncier.clean %>% 
  group_by(code_departement,code_commune,code_qp,nom_qp,
           id_mutation,annee_mutation) %>% 
  summarise(
    type_local=min(type_local),
    nb_biens=n(),
    valeur_fonciere=max(valeur_fonciere),
    surface_reelle_bati=sum(surface_reelle_bati),
    surface_terrain=max(surface_terrain),
    nombre_pieces_principales=sum(nombre_pieces_principales),
    longitude=max(longitude),
    latitude=max(latitude)
  ) %>% 
  mutate(prix_m2=valeur_fonciere/surface_reelle_bati) %>% 
  filter(prix_m2>100) %>% 
  filter(prix_m2<10000) %>% 
  ungroup

write.csv2(foncier.clean.agg,"./valeurs_foncieres_cleaned.csv")

foncier.clean.agg2<-foncier.clean.agg %>%  
  group_by(code_qp) %>% 
  summarize(
    nb_biens_tot=sum(nb_biens),
    prix_m2_tot=mean(prix_m2),
    nb_biens_2016=sum(ifelse(annee_mutation=="2016",nb_biens,0)),
    nb_biens_2017=sum(ifelse(annee_mutation=="2017",nb_biens,0)),
    nb_biens_2018=sum(ifelse(annee_mutation=="2018",nb_biens,0)),
    nb_biens_2019=sum(ifelse(annee_mutation=="2019",nb_biens,0)),
    nb_biens_2020=sum(ifelse(annee_mutation=="2020",nb_biens,0)),
    prix_m2_2016=mean(ifelse(annee_mutation=="2016",prix_m2,NA),na.rm=TRUE),
    prix_m2_2017=mean(ifelse(annee_mutation=="2017",prix_m2,NA),na.rm=TRUE),
    prix_m2_2018=mean(ifelse(annee_mutation=="2018",prix_m2,NA),na.rm=TRUE),
    prix_m2_2019=mean(ifelse(annee_mutation=="2019",prix_m2,NA),na.rm=TRUE),
    prix_m2_2020=mean(ifelse(annee_mutation=="2020",prix_m2,NA),na.rm=TRUE)
  ) %>% 
  mutate(
    prix_m2_2020=ifelse(is.na(prix_m2_2020),prix_m2_2019,prix_m2_2020),
    prix_m2_2018=ifelse(is.na(prix_m2_2018),prix_m2_2017,prix_m2_2018)
  )

write.csv2(foncier.clean.agg2,"./valeurs_foncieres_aggregate_QP.csv")


QP<-read.csv2("./QP_clustered.csv")

QP.VF<-QP %>% inner_join(foncier.clean.agg2,by="code_qp") %>% 
  mutate(dynamique_prix=prix_m2_2020/prix_m2_2018-1,
         dynamique_vente=nb_biens_2020/nb_biens_2018-1
  ) %>% 
  select(code_qp,cluster,dynamique_prix,dynamique_vente,nb_biens_tot) %>% 
  filter(nb_biens_tot>20)

write.csv2(QP.VF,"./dataforboxplot.csv")


library(ggplot2)
ggplot(QP.VF, aes(x=as.factor(cluster), y=dynamique_prix)) + 
  geom_boxplot()

ggplot(QP.VF, aes(x=as.factor(cluster), y=dynamique_vente)) + 
  geom_boxplot()
