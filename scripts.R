
# INSTALAZION LIBRERIAS
library(dplyr)
library(tidyr)
library(ggplot2)
library(smooth)
library(forecast)
library(prophet)
library(zoo)
library(xts)
library(dygraphs)
library(data.table)
library(tseries)
library(fpp2)
library(hts)

# carga datos 
rm(list=ls())
mes_actual <- as.yearmon("2020-03")
setwd("/home/lucas/Documents/03.Pateo/_TFM/Pateo_entrada/")
articulos <- read.csv(file = "articulos.csv",header = TRUE, sep = ";",stringsAsFactors = FALSE )
colnames(articulos)[1] <- c("sku")
articulos$pvp <- gsub(pattern = ",",replacement = ".",x = articulos$pvp)
articulos$pvp <- as.numeric(articulos$pvp)
articulos$PC_medio <- gsub(pattern = ",",replacement = ".",x = articulos$PC_medio)
articulos$PC_medio <- as.numeric(articulos$PC_medio)

stock_actual <- read.csv(file = "stock_29_02_2020.csv",header = TRUE, sep = ";",stringsAsFactors = FALSE)
colnames(stock_actual) <- c("sku","stock_actual")
stock_actual$actual <- gsub(",",".",stock_actual$stock_actual)
stock_actual$actual <- as.numeric(stock_actual$stock_actual)

ventas <- read.csv(file = "ventas_2014_2020.csv",header = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(ventas)[1:2] <- c("sku","year")
colnames(ventas)[3:14] <- c("01","02","03","04","05","06","07","08","09","10","11","12")
ventas <- ventas %>%  gather(key = "month", value = "ventas", 3:14)
ventas$date <- as.yearmon(paste(ventas$year,ventas$month,sep="-"))
ventas <- ventas %>% arrange(sku,date)
ventas <- ventas[,c(1,4,5)] 
ventas$ventas <- gsub(",",".",ventas$ventas)
ventas$ventas <- as.numeric(ventas$ventas)
ventas[is.na(ventas)] <- 0
ventas$ventas[ventas$ventas < 0] <- 0


stock <- read.csv("inventario_almacenes_historico.csv", sep = ";", header=T, stringsAsFactors = FALSE)
colnames(stock)[1] <- "sku"
stock$date <- as.Date(stock$date,"%d/%m/%Y")
stock$date <- as.yearmon(stock$date)
stock$stock <- gsub(",",".",stock$stock)
stock$stock <- as.numeric(stock$stock)
stock$stock[is.na(stock$stock)] <- 0
#hay ciertos sku con registros de stock en lineas diferentes para mismo mes
stock <- stock %>% group_by(sku,date) %>% summarise(stock = sum(stock))


familias <- read.csv(file = "familias.csv",header = TRUE, sep = ";")
colnames(familias)[1] <- "sku"
ventas <- left_join(ventas, stock, by=c("sku","date"))
ventas <- left_join (ventas, familias, by = 'sku')
ventas <- left_join (ventas, articulos[c(1,5,6,9)], by = 'sku')
ventas <- ventas %>% mutate(ventas_usd = ventas * pvp)
ventas[is.na(ventas)] <- 0
ventas <- ventas %>% filter(date < mes_actual)
ventas <- ventas %>% filter(I=="ALIMENTAR")
ventas[5] <- NULL
colnames(ventas[5:8]) <- c("I","II","III","V")

################## codificacion de categorias para hts ###################3


 colnames(ventas[5:8]) <- 
colnames(ventas[5:8]) <- c("I","II","III","V")
matriz_categorias <- unique(ventas[,c(6:9)]) 
colnames(matriz_categorias) <- c("I","II","III","V")

cat_I <- data.frame(I = unique(matriz_categorias[1]), code_I = LETTERS[1:nrow(unique(matriz_categorias[1]))])
cat_II <- data.frame(II = unique(matriz_categorias[2]), code_II = letters[1:nrow(unique(matriz_categorias[2]))])
cat_III <- data.frame(III = unique(matriz_categorias[3]), code_III = sprintf("%03d",1:nrow(unique(matriz_categorias[3]))))
cat_V <- data.frame(V = unique(matriz_categorias[4]), code_V = sprintf("%04d",1:nrow(unique(matriz_categorias[4]))))

ventas_cat <- left_join(ventas, cat_I, by = 'I')



############# Analisis exploratorio de datos ###################
ventas <- na.omit(ventas)
ventas_I <- ventas %>% group_by(date) %>% summarise(ventas_usd = sum(ventas_usd))
ventas_I_xts <- as.xts(ts(ventas_I$ventas_usd, start = ventas_I$date[1], frequency = 12))
ventas_II <- ventas %>% group_by(date,II) %>% summarise(ventas_usd = as.integer(sum(ventas_usd)))
ventas_II <- as.data.frame(na.omit(ventas_II))
ventas_II <- ventas_II %>% filter(II != 'nuevos')


ventas_III <- ventas %>% group_by(date,III) %>% summarise(ventas_usd = as.integer(sum(ventas_usd)))

ts_ventas_I <- as.xts(ts(ventas_I$ventas_usd, start = c(2014,01),frequency = 12))

ts_ventas_II <- list()
for(s in unique(ventas_II$II)){
  ts_ventas_II[[s]] <- ts(ventas_II$ventas_usd[ventas_II$II ==s], start = min(min(ventas_II$date[ventas_II$II ==s])), frequency = 12)
}
a <- do.call(cbind,ts_ventas_II)
dygraph(a, main = "sales by cat_II") %>% dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2"))

ts_ventas_III <- list()
for(s in unique(ventas_III$III)){
  aux <- ventas_III
  ts_ventas_III[[s]] <- ts(aux$ventas_usd[aux$III == s], start = min(aux$date[aux$III == s]), frequency = 12)
}

II_III <- with(unique(familias[,c(3,4)]),split(as.character(III), II))    
ts_ventas_II_III <-  lapply(II_III, function(x) ts_ventas_III[x])

a <- ts_ventas_II_III$MERCEARIA
a <- do.call(cbind, a)


        

################ evolucion del numero de referencias vendidas ##############
sku_mes <- ventas %>% group_by(date) %>% summarise(n_sku = n())
sku_mes_xts <- as.xts(ts(sku_mes$n_sku, start=sku_mes$date[1], frequency = 12))

############## an?lisis de sku para predici?n #################
#queremos generar predicci?n para aquellas referencias que sigan teniendo stock en el almac?n o tengan ventas en el ?ltimo mes
sku_total <- unique(ventas$sku)
sku_actual <- unique(ventas$sku[ventas$date == (mes_actual-1/12)])#sku que tienen ventas en el mes en curso
sku_dates <- ventas %>% group_by(sku) %>% summarise(min_date = min(date), max_date =    max(date), n_date = n(), sales_period = as.numeric(max_date-min_date)*12+1, dif_mes = as.integer(sales_period - n_date))#df con info de periodos de venta por sku '8900'
sku_dates_actual <- sku_dates %>% filter(sku %in% sku_actual)#df sku_dates pero solo con los sku actuales '3612'
sku_dates_actual_congruentes <- sku_dates_actual %>% filter(dif_mes == 0, sales_period > 12)#df_sku_dates_actual con fid_mes = 0 '3231'
sku_prev <- unique(sku_dates_actual_congruentes$sku)#todos los sku que vamos a analizar
sku_con_stock <- unique(stock_actual$sku[stock_actual$stock_actual>0])#sku con stock '4413'
sku_con_stock_no_prev <- sku_con_stock[!sku_con_stock %in% sku_prev] #`1504' sku con stock que no tienen datos en el ultimo mes por tanto no vamos a aplicar los algoritmos o deberiamos incluir datos con valor 0
#filtro la hoja de familias cuyos sku est?n dentro de sku_prev
familias_prev <- familias %>% filter(sku %in% sku_prev)
#cojo solo los sku que esten dentro de sku_prev y dentro de la matriz de familias
sku_prev <- unique(familias_prev$sku)

setwd("C:/Users/Lucas/Documents/03.Pateo/_TFM/Entrada_modelo")
save(sku_prev,file = 'sku_prev.Rdata')

####################### regresor OFS ##################

ventas_list <- list()
ventas_red <- ventas[c(1,2,3,4)]
for (s in sku_total){
  ventas_list[[s]] <- ventas_red %>% filter(sku == s)
}

#tenemos valores NA en el stock, probablemente por fala de dato, los sustituimos por el valor anterior 
for(s in sku_total){
  a <- which(is.na(ventas_list[[s]]$stock))
  if (!is_empty(a)){
    for(p in a){
      if (p == 1) {
        ventas_list[[s]]$stock[p] <- 0
      }else{
        ventas_list[[s]]$stock[p] <- ventas_list[[s]]$stock[p-1]
      }
    } 
  }
}


rle_len <- list()
rle_val <- list()
ceros <- list()
st <- list()
detalles_ceros <- list()
detalles_sku <- list()

# genreamos lista 'st' con los valores de stock cuando las ventas son '0' para cada periodo con ventas '0'
for (i in sku_total) {
  rle_len[[i]] <- rle(ventas_list[[i]]$ventas)$length # rle de la lista de ventas
  rle_val[[i]] <- rle(ventas_list[[i]]$ventas)$value
  
  if(!is_empty(which(rle_val[[i]]==0))){
    ceros[[i]] <- which(rle_val[[i]]==0) # ceros es una lista con las posiciones de los valores '0' en ventas
    st[[i]] <- list()
    detalles_ceros[[i]] <- list()
    
    for (j in 1:length(ceros[[i]])){
      detalles_ceros[[i]][[j]] <- list() 
      if (ceros[[i]][[1]] == 1) {
        st[[i]][[1]] <- ventas_list[[i]]$stock[1:rle_len[[i]][[1]]]
        st[[i]][[j]] <- ventas_list[[i]]$stock[(sum(rle_len[[i]][1:(ceros[[i]][[j]]-1)])):(sum(rle_len[[i]][1:ceros[[i]][[j]]]))] 
      }else{
        st[[i]][[j]] <- ventas_list[[i]]$stock[(sum(rle_len[[i]][1:(ceros[[i]][[j]]-1)])):(sum(rle_len[[i]][1:ceros[[i]][[j]]]))] 
      }
    }
  }
}

# detales_ceros es una lista con la info de la lista st 
for (i in sku_total){
  detalles_ceros[[i]] <- list()
  if (!is_empty(which(rle_val[[i]]==0))) {
    for (j in 1:length(ceros[[i]])){
      detalles_ceros[[i]][[j]] <- list()
      detalles_ceros[[i]][[j]]$ceros <- sum(st[[i]][[j]]==0)
      detalles_ceros[[i]][[j]]$long <- length(st[[i]][[j]])
      detalles_ceros[[i]][[j]]$no_ceros_distintos <- length(unique(st[[i]][[j]][st[[i]][[j]]!=0]))
      detalles_ceros[[i]][[j]]$pos <- j
      detalles_ceros[[i]][[j]]$cero_inicial <- case_when(st[[i]][[j]][1] == 0~1, st[[i]][[j]][1] != 0~0)
      detalles_ceros[[i]][[j]]$cero_final <- case_when(st[[i]][[j]][length(st[[i]][[j]])] == 0~1, st[[i]][[j]][length(st[[i]][[j]])] != 0~0)
    }
  }
}

# como hay valores de ventas_list_ceros que est?n vac?os porque no han pasado por el bucle anterior porque no tienen ningun cero
# genero una lista solo con los que si est?n rellenos

ventas_list_ceros <- lapply(detalles_ceros, function(x) do.call(rbind, lapply(x, as.data.frame)))
ventas_list_ceros <- ventas_list_ceros[names(ceros)]
ventas_list_ceros <- do.call(rbind,Map(cbind, sku = names(ventas_list_ceros),ventas_list_ceros))


ventas_list_ceros <- ventas_list_ceros %>% filter(sku %in% sku_prev)

a <- ventas_list_ceros 
a$sku <- as.numeric(as.character(a$sku))
nrow(a[a$ceros>0 & a$cero_inicial==0   ,])

aux <- list()
reg_ofs <- list()
#generacion de reg_ofs
for (i in sku_prev){
  aux[[i]] <- rep(1,length(rle_len[[i]])) 
  if (!is_empty(which(rle_val[[i]]==0))) {
    for (j in 1:length(ceros[[i]])){
      #consideramos rotura cuando hay al menos un cero la secuencia de valores de stock con ventas 0
      if(detalles_ceros[[i]][[j]]$ceros > 0){
        aux[[i]][ceros[[i]][j]] = 0
      }  
    }
    reg_ofs[[i]] <- rep(aux[[i]],rle_len[[i]]) 
  } else{reg_ofs[[i]] <- rep(aux[[i]],rle_len[[i]]) }
}
#comprobamos que se ha generado bien la lista reg_ofs
comprueba_reg_ofs <- list()
for(i in sku_total){
  comprueba_reg_ofs[[i]] <- length(reg_ofs[[i]])-length(ventas_list[[i]]$ventas)
}

#serie temporal con el regresor

save(reg_ofs,file='reg_ofs.Rdata')



################### correlaciones entre series ##############
meses_test <- 3  
fec_ini <- list()
fec_fin <- list()
fec_fin_train <- list()
fec_ini_test <- list()
ventas_list_train <- list()
ventas_list_test <- list()
ts_sales <- list()
ts_train <- list()
ts_test<- list()

for (s in sku_total){
  ventas_list[[s]]$reg <- reg_ofs[[s]]
  fec_ini[[s]] <- min(ventas_list[[s]]$date)
  fec_fin[[s]] <- max(ventas_list[[s]]$date)
}

sku_category <- with(familias_prev,split(as.character(sku), V))    
sales_fam_sku <-  lapply(sku_category, function(x) ts_sales[x])
regresor_fam_sku <- lapply(sku_category, function(x) ts_reg_ofs[x])
sales_reg_mtrx <- lapply(Map(c, sales_fam_sku, regresor_fam_sku), function(x) do.call(ts.intersect, x))

category <- (unique(as.list(as.character(familias_prev$V))))
sku_correlation <- list()
for (s in category){
  for(p in sku_category[[s]])
    sku_correlation[[p]] <- sales_reg_mtrx[[s]][,c(which(p == sku_category[[s]]),
                                                   seq(length(sku_category[[s]])+1,length(sku_category[[s]])*2))]
  
}

list_correlations <- list()
for (s in sku_prev){
  list_correlations[[s]] <- cor(sku_correlation[[s]])[1,]
}
list_correlations <- lapply(list_correlations, function(x) ifelse(is.na(x),0,x))

#quiero iterar sobre el proceso de generacion de sku_correlation 
#para reajustar el ts.intersect solo con los sku que tiene correlacion
#para no perder tanto periodo de ventas al hacer el intersect
#lo hacemos con la matriz crlt
#hacemos una matriz de relaciones de sku con los que tengan ua correlacion
#mayor que el umbral
sku_category_2 <- with(crlt,split(as.character(sku_correlate), sku))    
sales_fam_sku_2 <-  lapply(sku_category_2, function(x) ts_sales[x])
regresor_fam_sku_2 <- lapply(sku_category_2, function(x) ts_reg_ofs[x])
sales_reg_mtrx_2 <- lapply(Map(c, sales_fam_sku_2, regresor_fam_sku_2), function(x) do.call(ts.intersect, x))
#meter graficacomparando cuanto hemos ampliado el window de ts
#puede cambiar un poco la correlacion, por eso rehacemos el proceso
category_2 <- (unique(as.list(as.character(crlt$sku))))
sku_correlation_2 <- list()
for (s in category_2){
  for(p in sku_category_2[[s]])
    sku_correlation_2[[p]] <- sales_reg_mtrx_2[[s]][,c(which(p == sku_category_2[[s]]),
                                                   seq(length(sku_category_2[[s]])+1,length(sku_category_2[[s]])*2))]
  
}

list_correlations_2 <- list()
for (s in names((sku_correlation_2))){
  list_correlations_2[[s]] <- cor(sku_correlation_2[[s]])[1,]
}
# quitar columnas duplicadas de regresores
#ahora tenemos que ajustar las series temporales de los regresores para 
# anexar los regresores al data frame
# bucle para generacion de data frame para prophet
ventas_list_crlt <- list()
n_regresores <- list()
for(s in names(sku_correlation_2)){
  ventas_list_crlt[[s]] <- as.data.frame(sku_correlation_2[[s]])
  ventas_list_crlt[[s]] <- unique.matrix(ventas_list_crlt[[s]], MARGIN=2) 
  ventas_list_crlt[[s]]$date <- as.yearmon(time(sku_correlation_2[[s]]))
  n_regresores[[s]] <- ncol(ventas_list_crlt[[s]])-2
  if(ncol(ventas_list_crlt[[s]]) == 3){
    colnames(ventas_list_crlt[[s]]) <- c("y","reg1","ds")
  } else if(ncol(ventas_list_crlt[[s]]) == 4){
    colnames(ventas_list_crlt[[s]]) <- c("y","reg1","reg2","ds")
  } else if(ncol(ventas_list_crlt[[s]]) >= 5){
    ventas_list_crlt[[s]] <- ventas_list_crlt[[s]][c(1,2,3,4,ncol(ventas_list_crlt[[s]]))]
    colnames(ventas_list_crlt[[s]]) <- c("y","reg1","reg2","reg3","ds")
    n_regresores[[s]] <- 3
    #faltaria ordenar las columnas por orden de correlacion en funcion del sku
  }
}


df_correlations = lapply(list_correlations, function(x) data.frame(sku_correlate = names(x), Val = x, stringsAsFactors = FALSE))
df_correlations <- dplyr::bind_rows(df_correlations, .id = "sku_orignal")
#df_correlations <- do.call(rbind, Map(cbind, lapply(list_correlations,stack), P = names(list_correlations)))

View(df_correlations)

df_correlations[is.na(df_correlations)]<- 0
df_correlations <- df_correlations %>% filter(Val != 1)

save(df_correlations,'df_correlations.Rdata')


###############  train test split ####################
for (s in sku_prev){
  fec_fin_train[[s]] <- fec_ini[[s]] + length(ventas_list[[s]]$date)/12 - ((meses_test+1)/12)
  fec_ini_test[[s]] <- fec_ini[[s]] + length(ventas_list[[s]]$date)/12 - ((meses_test)/12)
  ##train test split ARIMA
  ts_sales[[s]] <- ts(ventas_list[[s]]$ventas,start =     c(as.numeric(format(fec_ini[[s]],"%Y")),as.numeric(format(fec_ini[[s]],"%m"))), frequency = 12)
  ts_train[[s]] <- window(ts_sales[[s]], end=c(as.numeric(format(fec_fin_train[[s]],"%Y")),as.numeric(format(fec_fin_train[[s]],"%m"))))
  ts_test[[s]] <- window(ts_sales[[s]], start=c(as.numeric(format(fec_ini_test[[s]],"%Y")),as.numeric(format(fec_ini_test[[s]],"%m"))))
  ##train test split PROPHET
  ventas_list_train[[s]] <- ventas_list[[s]] %>% filter(date <= fec_fin_train[[s]])
  ventas_list_test[[s]] <- ventas_list[[s]] %>% filter(date >= fec_ini_test[[s]])
}

ts_reg_ofs <- list()
ts_reg_ofs_train <- list()
ts_reg_ofs_test <- list()
for(s in sku_prev){
  ts_reg_ofs[[s]] <- ts(reg_ofs[[s]],start = c(as.numeric(format(fec_ini[[s]],"%Y")),as.numeric(format(fec_ini[[s]],"%m"))), frequency = 12)
  ts_reg_ofs_train[[s]] <- window(ts_reg_ofs[[s]], end=c(as.numeric(format(fec_fin_train[[s]],"%Y")),as.numeric(format(fec_fin_train[[s]],"%m"))))
  ts_reg_ofs_test[[s]] <- window(ts_reg_ofs[[s]], start=c(as.numeric(format(fec_ini_test[[s]],"%Y")),as.numeric(format(fec_ini_test[[s]],"%m"))))
}

ventas_list_crlt_train <- list()
ventas_list_crlt_test <- list()
for(s in names(sku_correlation_2)){
  ventas_list_crlt_train[[s]] <- ventas_list_crlt[[s]] %>%  filter(ds < mes_actual-(3/12))
  ventas_list_crlt_test[[s]] <- ventas_list_crlt[[s]] %>%  filter(ds >= mes_actual-(3/12))
}



save(ts_reg_ofs, file='ts_reg_ofs.Rdata')
save(ts_reg_ofs_train, file='ts_reg_ofs_train.Rdata')
save(ts_reg_ofs_test, file='ts_reg_ofs_test.Rdata')
save(ts_train, file='ts_train.Rdata')
save(ts_test, file='ts_test.Rdata')
save(ventas_list, file='ventas_list_train.Rdata')
save(ventas_list_test, file='ventas_list_test.Rdata')
save(ventas_list_train, file='ventas_list_train.Rdata')

################### carga modelos ##################
setwd("C:/Users/Lucas/Documents/03.Pateo/_TFM")
load('pr_reg_fcst.Rdata')

################3### funcion accuracy #################

eps <- 1e-11
e <- 2.713
accuracy_lucas <- function(forecast, test, venta_media){
  accuracy_lucas <- abs(forecast - test+eps)/venta_media
}

setwd("C:/Users/Lucas/Documents/03.Pateo/_TFM/Salida_modelo")
load('ar_fcst.Rdata')
load('ar_log_fcst.Rdata')
load('ar_log_reg_fcst.Rdata')
load('pr_fcst.Rdata')
load('pr_reg_fcst.Rdata')
load('pr_reg_log_fcst.Rdata')

acc <- list()
for(s in sku_prev){
  acc[[s]]$ar <- accuracy_lucas(sum(ar_fcst[[s]]),sum(ventas_list_test[[s]]$ventas),metricas$venta_media_sin_ofs[metricas$sku==s])
  acc[[s]]$ar_log <- accuracy_lucas(sum(unlist(ar_log_fcst[[s]])),sum(ventas_list_test[[s]]$ventas),metricas$venta_media_sin_ofs[metricas$sku==s])
  acc[[s]]$ar_log_reg <- accuracy_lucas(sum(ar_log_reg_fcst[[s]]),sum(ventas_list_test[[s]]$ventas),metricas$venta_media_sin_ofs[metricas$sku==s])
  acc[[s]]$pr <- accuracy_lucas(sum(pr_fcst[[s]]),sum(ventas_list_test[[s]]$ventas),metricas$venta_media_sin_ofs[metricas$sku==s])
  acc[[s]]$pr_reg <- accuracy_lucas(sum(pr_reg_fcst[[s]]),sum(ventas_list_test[[s]]$ventas),metricas$venta_media_sin_ofs[metricas$sku==s])
  acc[[s]]$pr_reg_log <- accuracy_lucas(sum(unlist(pr_reg_log_fcst[[s]])),sum(ventas_list_test[[s]]$ventas),metricas$venta_media_sin_ofs[metricas$sku==s])
}

acc_df  <- bind_rows(acc, .id = 'sku')
acc_df_gather <- acc_df %>% gather(key=modelo, value=accuracy, 2:7)
#ggplot(acc_df_gather, aes(x=modelo, y= accuracy)) + geom_boxplot()
View(acc_df)

###################### ESTADISTICAS ########################

df_ventas <- do.call(rbind,ventas_list)
df_ventas_0 <- df_ventas %>% filter(ventas == 0) %>%   group_by(sku) %>% summarise(ventas_0 = n())
metricas <- df_ventas %>%  group_by(sku) %>% summarise(meses_ventas = n(), meses_con_stock = sum(reg))
metricas <- metricas %>% mutate(meses_sin_stock = meses_ventas - meses_con_stock)
#ventas_0 son los meses que hay con ventas 0 y meses_sin stock la suma de reg_ofs
metricas <- left_join(metricas, df_ventas_0, by = 'sku')
metricas[is.na(metricas)] <- 0
metricas <- metricas %>%  mutate(prc_0_ofs = 1-(ventas_0 - meses_sin_stock)/ventas_0)
ventas_sku <- ventas %>% group_by(sku) %>% summarise(ventas_sku = sum(ventas))
metricas <- left_join(metricas, ventas_sku, by = 'sku')
metricas <- metricas %>%  mutate(venta_media_sin_ofs = ventas_sku/meses_con_stock,venta_media_basica = ventas_sku/meses_ventas,porc_ofs = meses_sin_stock/meses_ventas)


ofs_test <- list()
for(s in sku_total){
  ofs_test[[s]] <- sum(ts_reg_ofs_test[[s]])
}
df_ofs_test <- do.call(rbind, Map(cbind, sku = names(ofs_test), ofs_test))
df_ofs_test <- as.data.frame(df_ofs_test)
colnames(df_ofs_test)[2] <- "ofs_test"
metricas <- left_join(metricas, df_ofs_test, by = 'sku')
metricas <- metricas %>% mutate(units_lost_ofs = meses_sin_stock*venta_media_sin_ofs)
metricas <- left_join(metricas,articulos[c(1,5,6)], by = 'sku')
metricas <- metricas %>% mutate(venta_media_mensual_usd = venta_media_sin_ofs*pvp, usd_lost_ofs = pvp*units_lost_ofs)
metricas[is.na(metricas)] <- 0




###############3### ploteado variables ###################

names(metricas)
metricas_prev <- metricas %>% filter(sku %in% sku_prev)
metricas_prev$ofs_test <- as.numeric(as.character(metricas_prev$ofs_test))
metricas_prev <- metricas_prev %>% mutate(ofs_test_bin = case_when(ofs_test > 0 ~ 1,
                                                                   ofs_test == 0 ~ 0))

str(metricas_prev)
metricas_prev$meses_ventas <- as.numeric(metrica_prev$meses_ventas)
var_plot <- c("meses_ventas", "venta_media_sin_ofs")
var_plot
par(mfrow=c(3,2), mar = c(2,0,2,2))

for (var in var_plot){
  boxplot(metricas_prev[,var], horizontal=TRUE,  outline=TRUE,col = "green1",main = var)
  hist(metricas_prev[,var], main = var,col = "pink")  
}

###################### K-means #########################
normalizar <- function(c){(c-min(c))/(max(c)-min(c))}

metricas_kmeans <- metricas_prev %>% select(meses_ventas, meses_sin_stock)

metricas_kmeans  <- as.data.frame(lapply(metricas_kmeans, normalizar))
km <- kmeans(metricas_kmeans,2)
metricas_prev$cluster <- as.factor(km$cluster)

#Es necesario crear una funcion para asignar siempre los cluster en orden creciente en funcion de la variable "n_ceros" independientemente del numero de cluster que haya salido al correr el algoritmo"


median<-list()
for (i in  1:3) {
  median[i] <- median(df_x$n_ceros[df_x$cluster==i])  
}
a <-order(unlist(median))
b <-data.frame(a,1:3)
names(b)<-c("cluster","cluster.ordered")
b$cluster<-as.factor(b$cluster)
df_x <- left_join(df_x,b, by = 'cluster')
df_x$cluster.ordered <- as.factor(df_x$cluster.ordered)





######################### pesos ventas sku #########################
ventas_prev <- ventas %>% filter(sku %in% sku_prev)
pesos_sku <- ventas_prev %>% group_by(sku) %>% summarise(ventas_usd_sku = sum(ventas_usd),V=unique(V))
ventas_V <- ventas_prev %>% group_by(V) %>% summarise(ventas_usd_V = sum(ventas_usd))
pesos_sku <- left_join(pesos_sku,ventas_V,by='V') %>% mutate(peso =ventas_usd_sku/ventas_usd_V)


# vamos a genrar una matriz con los indices de correlacion de cada sku
#para elo creamos una lista con los valores de correlacion y sku


