
getwd()
setwd("/Users/sina/Downloads/Marathon")
###Chargement des données
dataCA = read.csv(file = "MRTHN_CA_20210317.csv",header = TRUE,sep = ';')
dataEtab = read.csv(file = "MRTHN_ETABLISSEMENTS_20210317.csv",header = TRUE,sep = ';')
dataFlx = read.csv(file = "MRTHN_FLX_SLD_DAV_20210317.csv",header = TRUE,sep = ';')
dataFlx2 = read.csv(file = "FLUX_CRED_DEP2015_20210317.csv",header = TRUE,sep = ';')
###

dataEtabdataCA[1678,]
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


dataCA2020$IDENTR

dataFlx2020 = sqldf("SELECT * FROM dataFlx WHERE CAST(MOIS AS TEXT) LIKE '2020%'")
dataFlx2019 = sqldf("SELECT * FROM dataFlx WHERE CAST(MOIS AS TEXT) LIKE '2019%'")
dataFlx2018 = sqldf("SELECT * FROM dataFlx WHERE CAST(MOIS AS TEXT) LIKE '2018%'")
dataFlx2017 = sqldf("SELECT * FROM dataFlx WHERE CAST(MOIS AS TEXT) LIKE '2017%'")
dataFlx2015 = sqldf("SELECT * FROM dataFlx2 WHERE CAST(MOIS AS TEXT) LIKE '2015%'")
dataFlx2016 = sqldf("SELECT * FROM dataFlx2 WHERE CAST(MOIS AS TEXT) LIKE '2016%'")


dataexempl = sqldf("SELECT * from dataFlx2019 WHERE IDENTR = 3512730")
sqldf("SELECT * from dataFlx2018 WHERE IDENTR = 3698206")
sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2015 WHERE IDPART = 9296268")

som = dataexempl2$MT_FLUX_CRED_CONF[10:12]
part1 = sum(som)
part2 = sum(dataexempl$MT_FLUX_CRED_CONF[1:9])

part1 + part2



sqldf("SELECT * from dataCA2020 WHERE IDENTR = 3501154")

sqldf("SELECT IDENTR,COUNT(*) as somme from dataCA GROUP BY IDENTR")

sqldf("SELECT * from dataCA LEFT JOIN dataFlx ON dataCA.IDENTR = dataFlx.IDENTR where dataFlx.IDENTR is NULL")

dataCA2020$IDENTR
table(dataCA2019$IDENTR %in% dataCA2020$IDENTR)

listIndice = match(dataCA2020$IDENTR,dataCA2019$IDENTR)

listindice = which(dataCA2019$IDENTR %in% dataCA2020$IDENTR)

which(dataCA_01_05$IDENTR %in% dataFlx2018$IDENTR)
which(dataCA_01_05$IDENTR %in% dataFlx2017$IDENTR)
which(dataCA_01_05$IDENTR %in% dataFlx2019$IDENTR)

#Liste des ids qui sont en 2019 dans le ca mais pas en 2020 qu'il faudra prédire pour 2020 : 10549
new_data = dataCA2019[-listindice,]

table(duplicated(new_data$IDENTR))
#Parmis les ids de 2020, lesquelles ont un contrat le 12/2020 et donc on a déja le chiffre d'affaire de 232 entreprises
new_data_202012 = sqldf("SELECT * from dataCA2020 WHERE CAST(YRMM_ARRT_CMPTBL AS TEXT) LIKE '202012%'")




nrow(sqldf("SELECT * from dataCA2018 WHERE CAST(YRMM_ARRT_CMPTBL AS TEXT) NOT LIKE '%12%'"))

#2092 entreprises qui ont un ca que l'on va devoir calculer sur 2021
new_data_2020 = sqldf("SELECT * from dataCA2020 WHERE CAST(YRMM_ARRT_CMPTBL AS TEXT) NOT LIKE '202012%'")

#Entreprises qui ont un ca 2020 quil faudra calculer jusquen 2021
dataCA_01_05 = new_data_2020[(new_data_2020$Date>= "2020-01-01" & new_data_2020$Date < "2020-06-01"),]
#Entreprises dont le ca 2020 doit être calcule par rapport à l'année d'avant
dataCA_06_11 = new_data_2020[(new_data_2020$Date>= "2020-06-01" & new_data_2020$Date < "2020-12-01"),]

dataCA_01_05$IDENTR


dataFlx = readRDS("dataFlx.RDS")
data2 = readRDS("flx_sect_moy.RDS")

resto = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Restaurants%'")
biens = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Biens%'")
alim = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Alimentaires%'")
ind = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Indetermin%'")
loisir = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Loisirs%'")
sante = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Sant%'")
service = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Services%'")
transport = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Transports%'")
corresp = sqldf("SELECT * FROM data2 WHERE CAST(secteur AS TEXT) LIKE '%Correspondance%'")


loisir = loisir[order(loisir$Date),]
loisir
ts_cor = ts(loisir$moy, start=c(2017, 1), end=c(2020, 12), frequency=12)
hw_model <- HoltWinters(ts_cor)
time_serie <- forecast(hw_model, h=12)
plot(time_serie)


data2020Corresp
data20202021Corresp = as.data.frame(time_serie$mean)
plot(time_serie$mean)
data20202021Corresp$Date = df[37:60,1]
data20202021Corresp
data2021Corresp = as.data.frame(time_serie$mean)


data2021Corresp$Date = as.Date(c("2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01",
                            "2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01"))
corresp$secteur = NULL

data2021Corresp = data2021Corresp %>% 
  rename(
    "moy" = x
  )

dfSansCovid = rbind(corresp,data2021Corresp)
corresp$Date[corresp$Date > "2020-01-01",]
corresp = corresp[-c(37:60),] 
dfCovid = rbind(corresp,data20202021Corresp)
dfSecteurCovid = dfCovid
ts_cor = ts(dfSecteurCovid$moy, start=c(2017, 1), end=c(2021, 12), frequency=12)
plot(ts_cor)
dfSecteurSansCovid = dfSansCovid

myvector5 = sqldf("SELECT MT_FLUX_CRED_CONF AS moy,Date from dataFlx2019 where IDENTR = 3709623")
myvector4 = sqldf("SELECT MT_FLUX_CRED_CONF AS moy,Date from dataFlx2018 where IDENTR = 3709623")
myvector3 = sqldf("SELECT MT_FLUX_CRED_CONF AS moy,Date from dataFlx2017 where IDENTR = 3709623")
myvector1 =  sqldf("SELECT MT_FLUX_CRED_CONF AS moy,MOIS AS Date from dataFlx2015 WHERE IDPART = 9431311")
myvector2 =  sqldf("SELECT MT_FLUX_CRED_CONF AS moy,MOIS AS Date from dataFlx2016 WHERE IDPART = 9431311")
myv =  sqldf("SELECT MT_FLUX_CRED_CONF AS moy,Date from dataFlx2020 WHERE IDENTR = 3698206")
sqldf("SELECT * from dataFlx2020 WHERE IDENTR = 3592206")


vector5 = sqldf("SELECT MT_FLUX_CRED_CONF AS moy from dataFlx2019 where IDENTR = 3592206")
vector4 = sqldf("SELECT MT_FLUX_CRED_CONF AS moy from dataFlx2018 where IDENTR = 3592206")
vector3 = sqldf("SELECT MT_FLUX_CRED_CONF AS moy from dataFlx2017 where IDENTR = 3592206")
vector1 =  sqldf("SELECT MT_FLUX_CRED_CONF AS moy from dataFlx2015 WHERE IDPART = 1377434")
vector2 =  sqldf("SELECT MT_FLUX_CRED_CONF AS moy from dataFlx2016 WHERE IDPART = 1377434")
myv =  sqldf("SELECT MT_FLUX_CRED_CONF AS moy from dataFlx2020 WHERE IDENTR = 3592206")

list = as.data.frame(Map(c,vector2,vector3,vector4,vector5))
myts <- ts(list, start=c(2016, 1), end=c(2019, 12), frequency=12) 
#ts2020 = ts(myvector5, start=c(2020, 1), end=c(2020, 12), frequency=12) 
hw_model <- HoltWinters(myts)
time_serie <- forecast(hw_model, h=24)
plot(myvector2)
plot(ts2019)
MAPE(time_serie$mean, ts2019) * 100
plot(time_serie)


dfIdLoisir2021=as.data.frame(time_serie$mean)
dfIdLoisir20202021 = as.data.frame(time_serie$mean)
## 
##

dfSecteurLoisir2021=as.data.frame(time_serie$mean)
dfSecteurLoisir20202021 = as.data.frame(time_serie$mean)


write.csv(dfIdLoisir2021,"dfIdLoisir2021.csv")
write.csv(dfIdLoisir20202021,"dfIdLoisir20202021.csv")


write.csv(dfSecteurLoisir2021,"dfSecteurLoisir2021.csv")
write.csv(dfSecteurLoisir20202021,"dfSecteurLoisir20202021.csv")

DF_pred <- data.frame(Date =c("2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01",
                             "2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01",
                             "2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01"),moy = as.numeric(unlist(time_serie[4])))

DF_pred2 = data.frame(Date =c("2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01",
                              "2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01"),moy = as.numeric(unlist(time_serie[4])))

dfIdAvecCovid = rbind(myvector3,myvector4,myvector5,DF_pred)
dfIdSansCovid = rbind(myvector3,myvector4,myvector5,myv,DF_pred2)


write.csv(dfIdAvecCovid,"dfIdAvecCovid.csv")
write.csv2(dfIdSansCovid,"dfIdSansCovid.csv")
write.csv2(dfSecteurCovid,"dfSecteurCovid.csv")
write.csv2(dfSecteurSansCovid,"dfSecteurSansCovid.csv")

dfSecteurCovid
myts <- ts(dfSecteur20202021$moy, start=c(2017, 1), end=c(2021, 12), frequency=12) 
myts2 <- ts(dfSecteur2021$moy, start=c(2017, 1), end=c(2021, 12), frequency=12) 
plot(myts)
par(new=TRUE)
plot(myts2,col="red")

getwd()
myv = data2021Corresp %>% 
  rename(
    "moy" = x
  )


data2021Corresp
plot(ts_cor)
par(new=TRUE)
plot(time_serie)
datacor = as.data.frame(corresp$moy,corresp$Date)



data
data2$Date = as.Date(data2$Date)

data2$Date = 
glimpse(data2)
library(dplyr)

str(data2)
plot()
date
cpt=0

dataCA_01_05$IDENTR[0:100]
DF_pred_2021 <- NULL
DF_pred_2020 <- NULL
prediction <- function() {
  cpt=0
  #val1 = 0
  #val2 = 0
  #val3 = 0
  #val4 = 0
 # val5 = 0
  #val6 = 0
  #val7 = 0
  for (id2 in dataCA_01_05$IDENTR) {
    
    
    tryCatch({
      vector1 = as.data.frame(subset(dataFlx2017, IDENTR == id2)$MT_FLUX_CRED_CONF)
      vector2 = as.data.frame(subset(dataFlx2018, IDENTR == id2)$MT_FLUX_CRED_CONF)
      vector3 = as.data.frame(subset(dataFlx2019, IDENTR == id2)$MT_FLUX_CRED_CONF)
      list = as.data.frame(Map(c, vector1, vector2,vector3))
      myts <- ts(list, start=c(2017, 1), end=c(2019, 12), frequency=12) 
      #ts2019 = ts(myvector3, start=c(2019, 1), end=c(2019, 12), frequency=12) 
      hw_model <- HoltWinters(myts)
      time_serie <- forecast(hw_model, h=12)
      
      DF_pred <- data.frame(id =id2,value = as.numeric(unlist(time_serie[4])))
      DF_pred_2020 <- rbind(DF_pred_2020, DF_pred)
      
      #NAIVE METHOD 
      
     # naive_mod <- snaive(myts, h = 12)
      
      #ETS
     # ets_model = ets(myts, allow.multiplicative.trend = TRUE)
     # ets_forecast = forecast(ets_model, h=12)
      #TBATS
      
      #SE
    #  se_model <- ses(myts, h = 12)
      #ARIMA
    #  mymodel <- Arima(myts,order=c(8,2,0),method = "ML")
      #mymodel = auto.arima(myts)
    #  myforecast <- forecast(mymodel, level=c(95), h=12)
      #print(cpt)
      #HW
      
    #  model <- hw(myts, initial = 'optimal', h=(12))
      #dshw_model = dshw(myts, period1=2, period2 = 12, h=12)
      #TBATS
    #  tbats_model = tbats(myts)
    #  tbats_forecast = forecast(tbats_model, h=12)
      

      
    #  val1 = val1 + MAPE(naive_mod$mean, ts2019) * 100
    #  val2 = val2 + MAPE(tbats_forecast$mean, ts2019) * 100
     # val3 = val3 + MAPE(ets_forecast$mean, ts2019) *100
      #val4 = val4 + MAPE(dshw_model$mean, ts2019)*100
     # val5 = val5 + MAPE(time_serie$mean, ts2019) * 100
     # val6 = val6 + MAPE(model$mean, ts2019) * 100
     # val7 = val7 + MAPE(myforecast$mean, ts2019) * 100
     # valarima = valarima + sum(as.data.frame(ts2019) / as.data.frame(myforecast$mean)) / 12
     # valse = valse + sum(as.data.frame(ts2019) / as.data.frame(se_model$mean)) / 12
     # valhw = valhw + sum(as.data.frame(ts2019) / as.data.frame(model$mean)) / 12
     # valhw2 = valhw2 + sum(as.data.frame(ts2019) / as.data.frame(time_serie[4])) / 12
     # valnaive = valnaive + sum(as.data.frame(ts2019) / as.data.frame(naive_mod$mean)) / 12
      cpt= cpt+1
      print(cpt)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
   
  }
 # print(val1)
 # print(val2)
 # print(val3)
 # print(val4)
 # print(val5)
 # print(val6)
 # print(val7)
  return(DF_pred_2020)
}

prediction()
DF_pred_2021 = prediction()
DF_pred_2020 = prediction()
DF_pred_2020







dataCA_01_05$IDENTR[73]

as.data.frame(subset(dataFlx2019, IDENTR == 3541508)$MT_FLUX_CRED_CONF)

subset(dataFlx2019, IDENTR == 4940705)$MT_FLUX_CRED_CONF

str(new_data_2020)



3501347

myvector3 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2019 where IDENTR = 4940705")
myvector2 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2018 where IDENTR = 4940705")
myvector = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2017 where IDENTR = 4940705")
sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2020 where IDENTR = 3513223")

ts2019 = ts(myvector3, start=c(2019, 1), end=c(2019, 12), frequency=12) 
plot(ts2019)
myvector


list = as.data.frame(Map(c, myvector, myvector2))
myts <- ts(list, start=c(2017, 1), end=c(2018, 12), frequency=12) 
plot(myts)
myts
myvector
install.packages("forecast")
library(forecast)
ddata <- decompose(myts, "multiplicative")
plot(ddata)
mymodel <- Arima(myts,order=c(9,1,0))
mymodel
plot.ts(mymodel$residuals)
myforecast <- forecast(mymodel, level=c(95), h=12)
plot(myforecast)


myvector3 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2019 where IDENTR = 3535619")
myvector2 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2018 where IDENTR = 3535619")
myvector = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2017 where IDENTR = 3535619")

ts2019 = ts(myvector3, start=c(2019, 1), end=c(2019, 12), frequency=12) 
list = as.data.frame(Map(c, myvector, myvector2,myvector3))
myts <- ts(list, start=c(2017, 1), end=c(2019, 12), frequency=12) 
hw_model <- HoltWinters(myts)
time_serie <- forecast(hw_model, h=12)
par(mfrow=c(2,1))
plot(time_serie)
plot(ts2019)
as.data.frame("ddd",time_serie[4])
myts


str(DF_pred)
str(DF_pred_2021)

time_serie[4]


#NAIVE METHOD 
install.packages("MLmetrics")
library(MLmetrics)
naive_mod <- snaive(myts, h = 12)
summary(naive_mod)
plot(naive_mod)
MAPE(naive_mod$mean, ts2019) * 100


ets_model = ets(myts,damped = NULL, allow.multiplicative.trend = TRUE)
ets_forecast = forecast(ets_model, h=12)
MAPE(ets_forecast$mean, ts2019) *100

#SE

se_model <- ses(myts, h = 12)
MAPE(se_model$mean, ts2019) *100
plot(se_model)

#ARIMA

mymodel <- auto.arima(myts)
myforecast <- forecast(mymodel, level=c(95), h=12)
myforecast$mean
plot(myforecast)

dshw_model = dshw(myts, period1=4, period2 = 12, h=12)
MAPE(dshw_model$mean, ts2019)*100


tbats_model = tbats(myts)
tbats_forecast = forecast(tbats_model, h=12)
MAPE(tbats_forecast$mean, ts2019) * 100
plot(tbats_forecast$mean)




#HW

model <- hw(myts, initial = 'optimal', h=(12))
plot(ts2020)
plot(model$mean)
MAPE(model$mean, ts2020) * 100

plot(model)
accuracy(model)
plot(time_serie$mean)

sum(as.data.frame(ts2019) / as.data.frame(myforecast$mean)) / 12
sum(as.data.frame(ts2019) / as.data.frame(se_model$mean)) / 12
sum(as.data.frame(ts2019) / as.data.frame(model$mean)) / 12
sum(as.data.frame(ts2019) / as.data.frame(time_serie[4])) / 12
sum(as.data.frame(ts2019) / as.data.frame(naive_mod$mean)) / 12

plot(ts2019,type = 'b') 
par(new=TRUE)
plot(model$mean, col="red")
plot(time_serie$mean, col="red")
plot(myforecast)
MAPE(myforecast$mean, ts2019) * 100
plot(myforecast$mean)
myforecast$mean
ts2019

mymodel <- Arima(myts,order=c(3,1,3))
mymodel <- auto.arima(myts,seasonal = FALSE)
myforecast <- forecast(mymodel, level=c(95), h=12)
mymodel
Acf(myts)
pacf(myts)
MAPE(myforecast$mean, ts2019) * 100


install.packages("smooth")
install.packages("Mcomp")

require(smooth)
require(Mcomp)

modelma = sma(myts, h=12, silent=FALSE)
modelma = sma(myts, h=12, interval=TRUE,silent=FALSE)
modelma$forecast
modelma$
MAPE(modelma$forecast, ts2019) * 100
cmodelma
plot(modelma)

moving_average = forecast(ma(myts, order=2), h=12)
plot(moving_average)


mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

library(randomForest)
myvector2 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2018 where IDENTR = 4940705")
myvector = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2017 where IDENTR = 4940705")
myvector3 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2019 where IDENTR = 4940705")
myvector4 = sqldf("SELECT MT_FLUX_CRED_CONF from dataFlx2020 where IDENTR = 4940705")
data_CA_join = sqldf("SELECT MOIS,MT_FLUX_CRED_CONF,dataEtab.IDENTR,SECTEUR_ACTIVITE from dataFlx inner join dataEtab on dataEtab.IDENTR = dataFlx.IDENTR")
data_CA_join_2017 = sqldf("SELECT * FROM data_CA_join WHERE CAST(MOIS AS TEXT) LIKE '2017%'")
data_CA_join_2018 = sqldf("SELECT * FROM data_CA_join WHERE CAST(MOIS AS TEXT) LIKE '2018%'")
data_CA_join_2019 = sqldf("SELECT * FROM data_CA_join WHERE CAST(MOIS AS TEXT) LIKE '2019%'")

resto = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Restaurants%'")
biens = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Biens%'")
alim = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Alimentaires%'")
ind = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Indetermin%'")
loisir = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Loisirs%'")
sante = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Sant%'")
service = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Services%'")
transport = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Transports%'")
corresp = sqldf("SELECT * FROM data_CA_join_2018 WHERE CAST(SECTEUR_ACTIVITE AS TEXT) LIKE '%Correspondance%'")




str(data_CA_join_2018)


ts2020 = ts(myvector4, start=c(2020, 1), end=c(2020, 12), frequency=12) 
rf = randomForest(as.data.frame(ts2020)$MT_FLUX_CRED_CONF ~ myvector$MT_FLUX_CRED_CONF + myvector2$MT_FLUX_CRED_CONF + myvector3$MT_FLUX_CRED_CONF)
str(myvector)
str(as.data.frame(ts2019)$MT_FLUX_CRED_CONF)
print(rf)
predictions = predict(rf)

plot(predictions,type = 'b',col="green")
par(new=TRUE)
plot(ts2020,type = 'b',col="red")

mape(ts2020, predictions)


clust <- kmeans(data, centers = 5)
dataCAhrc = sqldf("Select AVG(MT_CA),DEPT from dataCA inner join dataEtab on dataEtab.IDENTR = dataCA.IDENTR where DEPT IN ('34', '11', '30','48','66') AND SECTEUR_ACTIVITE = 'Caf<e9>s, H<f4>tels, Restaurants' group by DEPT ")
dataCAvpc= sqldf("Select AVG(MT_CA),DEPT from dataCA inner join dataEtab on dataEtab.IDENTR = dataCA.IDENTR where DEPT IN ('34', '11', '30','48','66') AND SECTEUR_ACTIVITE = 'Vente Par Correspondance' group by DEPT ")

write.csv(dataCAhrc,"dataCAhrc.csv")
write.csv(dataCAvpc,"dataCAvpc.csv")

data =sqldf("SELECT MOIS,MT_FLUX_CRED_CONF,dataEtab.IDENTR,SECTEUR_ACTIVITE,NOTE_BALE2,CD_ANC_EER,CD_EFFECTIF,DEPT from dataFlx inner join dataEtab on dataEtab.IDENTR = dataFlx.IDENTR")
sqldf("SELECT count(*),NOTE_BALE2 from data group by NOTE_BALE2")





#Tiphaine Part

library(lubridate)
library(stringr)
library(dplyr)
library(highcharter)
library(tidyverse)
library(xts)

setwd("C:/Users/tipha/OneDrive/Documents/Cours/Marathon du web")

# Chargement des donn?es

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

# Enlever les valeurs ab?rrante dans les flux

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

# D?finir flux avant et apr?s covid

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

# D?finir le nombre de client par secteur

nb_client_act <- NULL
for(i in unique(flux_secteur$TYPE_ACTIVITE)){
  sect_1 <- flux_secteur %>% filter(TYPE_ACTIVITE == i)
  print(i)
  nb_client <- data.frame(activite = i, 
                          nb_clients = length(unique(sect_1$IDENTR)))
  
  nb_client_act <- rbind(nb_client_act, nb_client)
  
}

data_secteur <- left_join(data_secteur, nb_client_secteur, by="Secteur")

# Calculer la somme des flux des 12 mois pr?c?dents

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

# Estimer la sant? de l'entreprise 

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

# En analysant les donn?es dans chacun des groupe, nous leurs attribuons un label
clust1 <- knn %>% filter(knn$Cluster==1) %>% mutate(Sante = "BONNE SANTE" )
clust2 <- knn %>% filter(knn$Cluster==2) %>% mutate(Sante = "GRAND DANGER" )
clust3 <- knn %>% filter(knn$Cluster==3) %>% mutate(Sante = "EQUILIBRE" )
clust4 <- knn %>% filter(knn$Cluster==4) %>% mutate(Sante = "TRES BONNE SANTE" )
clust5 <- knn %>% filter(knn$Cluster==5) %>% mutate(Sante = "EN DANGER" )

data_clust_finale <- rbind(clust1,clust2,clust3,clust4,clust5)

# Pour ?cire en json

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

            chart.renderer.button('P?riode de pand?mie',1, 50)
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




