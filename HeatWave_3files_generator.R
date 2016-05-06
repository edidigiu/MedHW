##############################################################
#    ANALISI DELLE ONDATE DI CALORE                          #
##############################################################
#  1)  LEGGO "SUB_tn90_filled_binary.nc" sequenza 0-1 con superamento soglia 90°percentile   
#  
#  2) ONDATE DI CALORE:
#     2a) PERIODO RISCHIO MAY-SEPT
#     2b) THRESHOLD1=90 PERC PER OGNI GIORNO CALCOLATO SUL PERIODO 1971-2000
#     2c) THRESHOLD2=6  I GIORNI MINIMI PER DEFINIRE UN'ONDATA (SOGLIA IMPOSTABILE NELLA FUNZIONE)
#
#  3) USCITA:
#     - File .nc serie completa con 0-1 dove 1 indica l'inizio della spell (la data è attributo)
#     - File .nc serie completa con 0-1 dove 1 è attribuito al 1° di quei mesi che hanno una spell
#             iniziata nel mese precedente (spells che passano da un mese all'altro)
#     - File .nc serie completa con 0-$Durata_spell indicata all'inizio della spell (la data è attributo)
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(raster)
library(ncdf4)
##library(climdex.pcic)   #per il calcolo dei 27 indici

NomeDataset<-"ECAD"

################ SPAZIO FUNZIONI  ################################
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#

####  FUNZIONE SPELL SU UNA SERIE STORICA--OUT:0/1 1=data inizio spell######
spell1<-function(x,thres_days){  
  TimeSeries<-x
  ### SPELLS COMPUTATION #####
  #---------------------------------------------------------------
  a<-which(TimeSeries>0)            #NUMERI DI RIGA DEI SUPERAMENTI
  hot<-TimeSeries[a]
  e<-numeric(length(hot))    #CONTENITORE PER LE SEQUENZE (+ DI X GIORNI )
  
  index<-2
  for (oreste in 2:(length(hot))) {
    if (a[oreste]-1==a[oreste-1]){
      e[index]<-1
    }
    index<-index+1
  }
  
  d1<-data.frame(a,e)
  
  inizio.serie<-numeric(length(hot))  	#CONTENITORE PER LE SEQUENZE
  fine.serie<-numeric(length(hot))
  
  index2<-1
  for (mimma in 1:(length(hot)-1)) {
    if ((e[mimma]!=e[mimma+1])& (e[mimma]==0)){
      inizio.serie[index2]<-as.numeric(a[mimma])
    }
    if ((e[mimma]!=e[mimma+1])& (e[mimma]==1)){
      fine.serie[index2]<-as.numeric(a[mimma])
    }
    index2<-index2+1
  }
  #SI FISSA ULTIMA RIGA COME CHIUSURA DI UNA COOL/HEAT
  if (a[length(a)]==(a[length(a)-1]+1)){fine.serie[length(fine.serie)]<-a[length(a)]}
  
  d<-data.frame(a,e,inizio.serie,fine.serie)   
  d2<-d[d$inizio.serie>0,]
  anno.inizio<-as.numeric(substr(rownames(d2),2,5))
  mese.inizio<-as.numeric(substr(rownames(d2),7,8))
  giorno.inizio<-as.numeric(substr(rownames(d2),10,11))  
  
  d3<-d[d$fine.serie>0,]
  
  durata<-d3$fine.serie-d2$inizio.serie+1
  report<-data.frame(d2,anno.inizio,mese.inizio,giorno.inizio,durata)
  report2<-report[report$durata>=thres_days,]
  date_formatted<-as.Date(strptime(substr(rownames(report2),2,11),format="%Y.%m.%d"))
  report3<-data.frame(date_formatted,rep(1,length(date_formatted)),report2$durata)
  names(report3)<-c("day","spell","duration")
  #Ricostruisco la serie completa mettendo tutti i giorni e 1 alle date di inizio di una spell>6 giorni:
  SerieInizioSpell<-merge(DaysSeq,report3,by="day",all=T)
  SerieInizioSpell[is.na(SerieInizioSpell)]<-0
  rownames(SerieInizioSpell)<-SerieInizioSpell$day
  
  return(SerieInizioSpell[,"spell"])
}

###########   FINE FUNZIONE SPELL ##############
#-----------------------------------------------
#### Funzione ONDATE DI CALORE-OUT:file 0/1 1:data inizio ondata   ##############
#----------------------------------------------
spells<-function(x,thres_days,filename){      #"x" is raster;
  # "thres_days" is minimum days for determining spells
  # "filename" isname and path of file to be created
  out<-brick(x)   #contenitore rasterBrick vuoto con le stesse caratteristiche di x
  out@z<-x@z      #setto il time che si era perso
  out <- writeStart(out, filename, format="CDF",varname=paste("t",substr(x@title,1,3),sep=""),overwrite=TRUE)
  for (i in 1:nrow(out)) {
    v <- getValues(x, i)
    v[]<-t(apply(v,1,FUN=spell1,thres_days=thres_days))
    
    out <- writeValues(out, v, i)
  }  #chiudo ciclo "i" sulle rows del raster x
  
  names(out)<-names(x)
  
  out <- writeStop(out)
  
  return(out)
  
}
#------------ FINE FUNZIONE ------------------------------------------------


####  FUNZIONE SPELL2 SU UNA SERIE STORICA--OUT:0/duration######
spell2<-function(x,thres_days){  
  TimeSeries<-x
  ### SPELLS COMPUTATION #####
  #---------------------------------------------------------------
  a<-which(TimeSeries>0)            #NUMERI DI RIGA DEI SUPERAMENTI
  hot<-TimeSeries[a]
  e<-numeric(length(hot))    #CONTENITORE PER LE SEQUENZE (+ DI X GIORNI )
  
  index<-2
  for (oreste in 2:(length(hot))) {
    if (a[oreste]-1==a[oreste-1]){
      e[index]<-1
    }
    index<-index+1
  }
  
  d1<-data.frame(a,e)
  
  inizio.serie<-numeric(length(hot))  	#CONTENITORE PER LE SEQUENZE
  fine.serie<-numeric(length(hot))
  
  index2<-1
  for (mimma in 1:(length(hot)-1)) {
    if ((e[mimma]!=e[mimma+1])& (e[mimma]==0)){
      inizio.serie[index2]<-as.numeric(a[mimma])
    }
    if ((e[mimma]!=e[mimma+1])& (e[mimma]==1)){
      fine.serie[index2]<-as.numeric(a[mimma])
    }
    index2<-index2+1
  }
  #SI FISSA ULTIMA RIGA COME CHIUSURA DI UNA COOL/HEAT
  if (a[length(a)]==(a[length(a)-1]+1)){fine.serie[length(fine.serie)]<-a[length(a)]}
  
  d<-data.frame(a,e,inizio.serie,fine.serie)   
  d2<-d[d$inizio.serie>0,]
  anno.inizio<-as.numeric(substr(rownames(d2),2,5))
  mese.inizio<-as.numeric(substr(rownames(d2),7,8))
  giorno.inizio<-as.numeric(substr(rownames(d2),10,11))  
  
  d3<-d[d$fine.serie>0,]
  
  durata<-d3$fine.serie-d2$inizio.serie+1
  report<-data.frame(d2,anno.inizio,mese.inizio,giorno.inizio,durata)
  report2<-report[report$durata>=thres_days,]
  date_formatted<-as.Date(strptime(substr(rownames(report2),2,11),format="%Y.%m.%d"))
  report3<-data.frame(date_formatted,rep(1,length(date_formatted)),report2$durata)
  names(report3)<-c("day","spell","duration")
  #Ricostruisco la serie completa mettendo tutti i giorni e 1 alle date di inizio di una spell>6 giorni:
  SerieInizioSpell<-merge(DaysSeq,report3,by="day",all=T)
  SerieInizioSpell[is.na(SerieInizioSpell)]<-0
  rownames(SerieInizioSpell)<-SerieInizioSpell$day
  
  return(SerieInizioSpell[,"duration"])
}
###########   FINE FUNZIONE SPELL2 ##############
#-----------------------------------------------
#### Funzione ONDATE DI CALORE-OUT:file 0/duration ondata   ##############
#----------------------------------------------
spells2<-function(x,thres_days,filename){      #"x" is raster;
  # "thres_days" is minimum days for determining spells
  # "filename" isname and path of file to be created
  out<-brick(x)   #contenitore rasterBrick vuoto con le stesse caratteristiche di x
  out@z<-x@z      #setto il time che si era perso
  out <- writeStart(out, filename, format="CDF",varname=paste("t",substr(x@title,1,3),sep=""),overwrite=TRUE)
  for (i in 1:nrow(out)) {
    v <- getValues(x, i)
    v[]<-t(apply(v,1,FUN=spell2,thres_days=thres_days))
    
    out <- writeValues(out, v, i)
  }  #chiudo ciclo "i" sulle rows del raster x
  
  names(out)<-names(x)
  
  out <- writeStop(out)
  
  return(out)
  
}
#------------ FINE FUNZIONE -----------------------------------------------



####  FUNZIONE SPELL SU UNA SERIE STORICA--OUT:0/1 1=data inizio spell che finisce in un altro mese######
spell3<-function(x,thres_days){  
  TimeSeries<-x
  ### SPELLS COMPUTATION #####
  #---------------------------------------------------------------
  a<-which(TimeSeries>0)            #NUMERI DI RIGA DEI SUPERAMENTI
  hot<-TimeSeries[a]
  e<-numeric(length(hot))    #CONTENITORE PER LE SEQUENZE (+ DI X GIORNI )
  
  index<-2
  for (oreste in 2:(length(hot))) {
    if (a[oreste]-1==a[oreste-1]){
      e[index]<-1
    }
    index<-index+1
  }
  
  d1<-data.frame(a,e)
  
  inizio.serie<-numeric(length(hot))    #CONTENITORE PER LE SEQUENZE
  fine.serie<-numeric(length(hot))
  
  index2<-1
  for (mimma in 1:(length(hot)-1)) {
    if ((e[mimma]!=e[mimma+1])& (e[mimma]==0)){
      inizio.serie[index2]<-as.numeric(a[mimma])
    }
    if ((e[mimma]!=e[mimma+1])& (e[mimma]==1)){
      fine.serie[index2]<-as.numeric(a[mimma])
    }
    index2<-index2+1
  }
  #SI FISSA ULTIMA RIGA COME CHIUSURA DI UNA COOL/HEAT
  if (a[length(a)]==(a[length(a)-1]+1)){fine.serie[length(fine.serie)]<-a[length(a)]}
  
  d<-data.frame(a,e,inizio.serie,fine.serie)   
  d2<-d[d$inizio.serie>0,]
  anno.inizio<-as.numeric(substr(rownames(d2),2,5))
  mese.inizio<-as.numeric(substr(rownames(d2),7,8))
  giorno.inizio<-as.numeric(substr(rownames(d2),10,11))  
  
  d3<-d[d$fine.serie>0,]
  anno.fine<-as.numeric(substr(rownames(d3),2,5))
  mese.fine<-as.numeric(substr(rownames(d3),7,8))
  giorno.fine<-as.numeric(substr(rownames(d3),10,11))  
  
  
  durata<-d3$fine.serie-d2$inizio.serie+1
  report<-data.frame(d2,anno.inizio,mese.inizio,giorno.inizio,anno.fine,mese.fine,giorno.fine,durata)
  report2<-report[report$durata>=thres_days,]
  date_formatted<-as.Date(strptime(substr(rownames(report2),2,11),format="%Y.%m.%d"))
  report3<-data.frame(date_formatted,numeric(length(date_formatted)))
  names(report3)<-c("day","SpellCrossing")
  report3[which(report2$mese.inizio != report2$mese.fine),"SpellCrossing"]<-1
  #Ricostruisco la serie completa mettendo tutti i giorni e 1 alle date di inizio di una spell che pasa nel giorno successivo:
  SerieInizioSpell<-merge(DaysSeq,report3,by="day",all=T)
  SerieInizioSpell[is.na(SerieInizioSpell)]<-0
  rownames(SerieInizioSpell)<-SerieInizioSpell$day
  
  return(SerieInizioSpell[,"SpellCrossing"])
}
#-----------------------------------------------
#### Funzione ONDATE DI CALORE-OUT:file 0/duration ondata   ##############
#----------------------------------------------
spells3<-function(x,thres_days,filename){      #"x" is raster;
  # "thres_days" is minimum days for determining spells
  # "filename" isname and path of file to be created
  out<-brick(x)   #contenitore rasterBrick vuoto con le stesse caratteristiche di x
  out@z<-x@z      #setto il time che si era perso
  out <- writeStart(out, filename, format="CDF",varname=paste("t",substr(x@title,1,3),sep=""),overwrite=TRUE)
  for (i in 1:nrow(out)) {
    v <- getValues(x, i)
    v[]<-t(apply(v,1,FUN=spell3,thres_days=thres_days))
    
    out <- writeValues(out, v, i)
  }  #chiudo ciclo "i" sulle rows del raster x
  
  names(out)<-names(x)
  
  out <- writeStop(out)
  
  return(out)
  
}
#------------ FINE FUNZIONE -----------------------------------------------

##################################################################################

##################   OUTPUT  ####################################

### TMAX e TMIN###
#Importo il file con la serie dei giorni che superano lo storico del 90 percentile calcolato su 1971-2000 (fatto con cdo)
for (var in c("tx","tn")){
  
  datax<-brick(paste("Data/",NomeDataset,"-SUB_",var,"90_filled_binary.nc",sep=""))  

  DaysSeq<-data.frame(datax@z$Date)   ##sequence of all days in the time series
  names(DaysSeq)<-c("day")

  s<-spells(datax,6,paste("Data/",var,"_",NomeDataset,"_Begin_HeatWave.nc",sep=""))
  l<-spells2(datax,6,paste("Data/",var,"_",NomeDataset,"_Duration_HeatWave.nc",sep=""))
  #m<-spells3(datax,6,paste("Data/",var,"_",NomeDataset,"_MonthCrossing_HeatWave.nc",sep=""))
} 











