
getwd()
#setwd("/Users/sina/Downloads/Marathon")

###Chargement des données
dataCA = read.csv(file = "MRTHN_CA_20210317.csv",header = TRUE,sep = ';')
dataEtab = read.csv(file = "MRTHN_ETABLISSEMENTS_20210317.csv",header = TRUE,sep = ';')
dataFlx = read.csv(file = "MRTHN_FLX_SLD_DAV_20210317.csv",header = TRUE,sep = ';')

###

dataCA[1678,]
table(is.na(dataCA$YRMM_ARRT_CMPTBL))

table(is.na(dataCA$MT_CA))

table(is.na(dataCA$NB_J_DUR_EXRCC))

table(is.na(dataCA$SOURCE))

table(is.na(dataCA$IDENTR))

summary(dataCA)

typeof(dataCA$YRMM_ARRT_CMPTBL)

library(sqldf)

dataCaEtab <- merge(dataCA, dataEtab, by = "IDENTR")
dataCA

dataCA2020 = sqldf("SELECT * FROM dataCA WHERE CAST(YRMM_ARRT_CMPTBL AS TEXT) LIKE '2020%'")
dataCA2019 = sqldf("SELECT * FROM dataCA WHERE CAST(YRMM_ARRT_CMPTBL AS TEXT) LIKE '2019%'")
dataCA2018 = sqldf("SELECT * FROM dataCA WHERE CAST(YRMM_ARRT_CMPTBL AS TEXT) LIKE '2018%'")


sqldf("SELECT * from dataCA WHERE IDENTR = 3506400")

sqldf("SELECT IDENTR,COUNT(*) as somme from dataCA GROUP BY IDENTR")
