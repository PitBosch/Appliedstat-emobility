## Restart & Cleaning the workspace 
rm(list=ls())
gc()
cat("\014")

#######  LIBRERIE  #########
library(flipTime)      # per usare funzioni tipo AsDateTime
library(lubridate)     # sempre per gestire tempo/date (non sono sicuro ma potrebbe essere inutile per questo script)
############################

# 1 CARICARE IL DATASET

## caricare dataset: scegliere tra JPL.txt e caltech.txt

acn <- read.table( 'caltech.txt', header = TRUE)

## selezionare solo righe in cui ci sono i dati degli utenti (CLAIMED)

acn_users <- acn[ -which(is.na(acn$userID)) ,]

## eliminare colonne con dati degli utenti

acn <- acn[ , -(13:20)]

# 2 ORARI : conversione numerica + calcolo differenza di tempo tra disconessione/carica completata e connessione

## inserire in data il dataset su cui si vuole lavorare ##
data <- acn

data$conn_time <- AsDateTime(data$connectionTime) 
disc_time <- AsDateTime(data$disconnectTime)
charg_time <- AsDateTime(data$doneChargingTime)

data$disc_difference <- as.numeric(difftime(disc_time, data$conn_time), units = 'mins')
data$charg_difference <- as.numeric(difftime(charg_time,data$conn_time), units = 'mins')

data$disc_difference_hour <- data$disc_difference/60
data$charg_difference_hour <- data$charg_difference/60

ore <- as.numeric(substr(data$connection_hour,1,2))
minuti <- as.numeric(substr(data$connection_hour,4,5))
data$connection_hour <- ore + minuti/60

## esempio di possibile trasformazione di connection_hour in factor ##

ind1=which(data$connection_hour >= 0 & data$connection_hour <= 6)
ind2=which(data$connection_hour > 6 & data$connection_hour <= 12) 
ind3=which(data$connection_hour > 12 & data$connection_hour <= 18)
ind4=which(data$connection_hour > 18 & data$connection_hour <= 24) 

data$fasce <- as.character(data$id)
data$fasce[ind1] <- '0-6'
data$fasce[ind2] <- '6-12'
data$fasce[ind3] <- '12-18'
data$fasce[ind4] <- '18-24'
data$fasce <- factor(data$fasce, levels=c('0-6', '6-12', '12-18', '18-24'), ordered =T)
