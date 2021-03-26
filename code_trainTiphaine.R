library(lubridate)
library(stringr)
library(dplyr)
library(highcharter)
library(tidyverse)
library(xts)

setwd("C:/Users/tipha/OneDrive/Documents/Cours/Marathon du web")

# Chargement des données

dataCA = read.csv(file = "MRTHN_CA_20210317.csv",header = TRUE,sep = ';')
dataEtab = read.csv(file = "MRTHN_ETABLISSEMENTS_20210317.csv",header = TRUE,sep = ';')
dataFlx = read.csv(file = "MRTHN_FLX_SLD_DAV_20210317.csv",header = TRUE,sep = ';')


# Modification de dates pour les mettre au format date

date <- as.character(dataFlx$MOIS)
years <- substr(date,1,4) 
month <- substr(date,5,6) 
date <- paste0(years, "-", month, "-01")
dataFlx <- dataFlx %>% mutate(Date = date)

date <- as.character(dataCA$YRMM_ARRT_CMPTBL)
years <- substr(date,1,4) 
month <- substr(date,5,6) 
date <- paste0(years, "-", month, "-01")
dataCA <- dataCA %>% mutate(Date = date)

# Enlever les valeurs abérrante dans les flux

data_NA <- dataFlx %>% filter(MT_FLUX_CRED_CONF<100)
dataFlx <- dataFlx[!paste(dataFlx$IDENTR,
                          dataFlx$MT_FLUX_CRED_CONF, 
                          dataFlx$Date)%in%paste(
                            data_NA$IDENTR, data_NA$MT_FLUX_CRED_CONF,
                            data_NA$Date),]

# Filtrer par secteur

CA_secteur <- left_join(dataCA, dataEtab, by="IDENTR") %>%
  dplyr::select(IDENTR, MT_CA, Date, SECTEUR_ACTIVITE, NB_J_DUR_EXRCC)

flux_secteur <- left_join(dataFlx, dataEtab, by="IDENTR") %>%
  dplyr::select(IDENTR, Date, SECTEUR_ACTIVITE, MT_FLUX_CRED, 
                MT_FLUX_CRED_CONF, MT_FLUX_DEB, MT_SLD_DAV, CD_EFFECTIF,
                CD_ANC_CREA_ENTR, TYPE_ACTIVITE )


loisir <- flux_secteur %>% filter(SECTEUR_ACTIVITE == "Loisirs")

# Définir flux avant et après covid

CA_vente <- CA_secteur %>% filter(SECTEUR_ACTIVITE=="Vente Par Correspondance")

type_act_covid <- NULL

for(i in unique(flux_secteur$TYPE_ACTIVITE)){
   print(i)

  flux_vente <- flux_secteur %>% filter(TYPE_ACTIVITE == i)

  vente_avant_covid <- flux_vente %>% filter(Date <= "2020-02-29") 
  vente_apres_covid <- flux_vente %>% filter(Date > "2020-02-29")
  
  data_vente <- data.frame(activite = i,
                           Avant_covid = mean(vente_avant_covid$MT_FLUX_CRED_CONF),
                           Apres_covid = mean(vente_apres_covid$MT_FLUX_CRED_CONF))
  type_act_covid <- rbind(type_act_covid, data_vente)
}

type_act_covid <- left_join(type_act_covid, nb_client_act, by="activite")

type_act_covid <- type_act_covid %>% mutate(variation = (Apres_covid-Avant_covid)/Avant_covid*100)
  data_vente <- data_vente %>% mutate(Variation = (Apres_covid-Avant_covid)/Avant_covid*100)



flx_loisir_moy <- NULL
for(j in unique(month(loisir$Date))){
  print(j)
    flux_month <- loisir %>% filter(month(Date) == j)
    df_inter <- NULL
    for(k in unique(year(flux_month$Date))){
      flux_year <- flux_month %>% filter(year(Date) == k)
      flx <- data.frame(secteur = "loisir",
                        Date = unique(flux_year$Date),
                        moy = mean(flux_year$MT_FLUX_CRED))
      df_inter <- rbind(df_inter, flx)
      
    }
    flx_loisir_moy <- rbind(flx_loisir_moy, df_inter)
}

id_loisir <- loisir %>% filter(IDENTR == 3592206)

table_loisir <- left_join(id_loisir, flx_loisir_moy, by="Date") %>%
  dplyr::select(c(IDENTR, Date, MT_FLUX_CRED_CONF, moy))

table_loisir <- table_loisir %>% mutate(ecart = (MT_FLUX_CRED_CONF-moy)/moy*100)

# Définir le nombre de client par secteur

nb_client_act <- NULL
for(i in unique(flux_secteur$TYPE_ACTIVITE)){
  sect_1 <- flux_secteur %>% filter(TYPE_ACTIVITE == i)
  print(i)
  nb_client <- data.frame(activite = i, 
                         nb_clients = length(unique(sect_1$IDENTR)))

  nb_client_act <- rbind(nb_client_act, nb_client)

}

data_secteur <- left_join(data_secteur, nb_client_secteur, by="Secteur")

# Calculer la somme des flux des 12 mois précédents

df_sante <- NULL
i=1
while(i<=nrow(flux_sante)){
  print(i)
  id <- flux_sante$IDENTR[i]
  data_ca <- CA_sante %>% filter(IDENTR == id)
  dataexempl <- flux_sante %>% filter(IDENTR == id)
  for(j in 1:nrow(data_ca)){
    data_ca2 <- data_ca[j,]
    
    dataexempl2 <- dataexempl %>% filter(Date >  ymd(data_ca2$Date) %m-% months(12) ,
                                         Date <= data_ca2$Date) 
    df <- data.frame(IDENTR = id,
                     Secteur = data_ca2$SECTEUR_ACTIVITE,
                     Date = data_ca2$Date,
                     CA_reel = data_ca2$MT_CA,
                     sum_flux_cred_conf = sum(dataexempl2$MT_FLUX_CRED_CONF),
                     sum_flux_cred = sum(dataexempl2$MT_FLUX_CRED),
                     sum_flux_deb = sum(dataexempl2$MT_FLUX_DEB),
                     sum_dav = sum(dataexempl2$MT_SLD_DAV),
                     moy_dav = mean(dataexempl2$MT_SLD_DAV))
    
    df_sante <- rbind(df_sante, df)
    
  }
  i = i+nrow(data_ca)
}

# Estimer la santé de l'entreprise 

flx_sect_id2 <- flx_sect_id2 %>% mutate(ecart = (MT_FLUX_CRED_CONF-moy)/moy*100)

data_clust <- flux_secteur %>% dplyr::select(MT_FLUX_CRED_CONF, CD_EFFECTIF) 

data_cluster <- left_join(dataEtab,df_flux, by=c("IDENTR","TYPE_CC","MARCHE_CC",
                                                 "TYPE_ACTIVITE", "SECTEUR_ACTIVITE")) %>%
  dplyr::select(c(CD_EFFECTIF, DEPT, NOTE_BALE2,
                  TYPE_ACTIVITE, SECTEUR_ACTIVITE, IDENTR, CD_ANC_CREA_ENTR, NOTE_BALE2,MT_CA, MT_FLX_CRED_CONF_AVG ))


data_kmean <- data_cluster %>% mutate(TYPE_ACTIVITE=as.numeric(as.factor(TYPE_ACTIVITE)),
                                      SECTEUR_ACTIVITE=as.numeric(as.factor(SECTEUR_ACTIVITE)),
                                      NOTE_BALE2=as.numeric(as.factor(NOTE_BALE2))
                                      ) %>%
  drop_na()

knn <- kmeans(data_kmean, 5, nstart = 25)
knn <- cbind(data_kmean, Cluster = knn$cluster)

# En analysant les données dans chacun des groupe, nous leurs attribuons un label
clust1 <- knn %>% filter(knn$Cluster==1) %>% mutate(Sante = "BONNE SANTE" )
clust2 <- knn %>% filter(knn$Cluster==2) %>% mutate(Sante = "GRAND DANGER" )
clust3 <- knn %>% filter(knn$Cluster==3) %>% mutate(Sante = "EQUILIBRE" )
clust4 <- knn %>% filter(knn$Cluster==4) %>% mutate(Sante = "TRES BONNE SANTE" )
clust5 <- knn %>% filter(knn$Cluster==5) %>% mutate(Sante = "EN DANGER" )

data_clust_finale <- rbind(clust1,clust2,clust3,clust4,clust5)

# Pour écire en json

data_sect <- data_secteur %>% mutate(Avant_la_covid =round(Avant_covid,0), Apres_la_covid = round(Apres_covid, 0), Variation = round(Variation, 2))

library(jsonlite)
df <- aggregate(secteur ~ Date + moy, flx_sect_moy, list)
flx_moy_json <- jsonlite::toJSON(df, pretty = TRUE)

write(flx_moy_json, "flx_moy_json.json")

# Essai visu

test_xts <- xts(exemple[-1], order.by = exemple$Date) 

highchart(type = "stock") %>%
  hc_add_series(test_xts, type = "line") %>%
  hc_xAxis(labels = list(format = '{value:%b %d}'))%>%
  hc_chart(
    events = list(
      load = JS("function(){
            var chart = this;

            chart.renderer.button('Période de pandémie',1, 50)
                .attr({
                    zIndex: 2
                })
                .on('click', function () {
                    chart.xAxis[0].setExtremes(Date.UTC(2020, 2, 1), Date.UTC(2020, 12, 1));
                })
                .add();
        }")
    ))
  #hc_rangeSelector(buttons=list(list(type='month', text='Covid', count=2)))

