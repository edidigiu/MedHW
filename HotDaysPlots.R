########################################################
#  ANALISI DELLE HEAT WAVES               ##############
#
#  1) Duration, Occurrencies, WSDI
#  2) Sub-periods e Sub-areas
### IMPORTA FILE 0/1 DI HEATWAVE DAYS  #################
########################################################
library(raster)
library(rts)

library(maps)
library(maptools)
library(rgeos)
library(rgdal)

# librerie per lo sviluppo grafico
library(ggplot2)     # per i grafici avanzati/mappe
library(latticeExtra) # visualizzazione di mappe spaziali di dati raster
library(rasterVis) # trasforma i Raster Object in formato leggile per ggplot2
library(reshape2)

#libreria per le date
library(lubridate) 
library(gdata)

### PREPARATION  ##########################################
#########################################################
#creo cartella per salvataggio plots:
dir.create(path=paste(getwd(),"/Plots/",Sys.Date(),sep=""))

### INFO BASE  ##########################################
#########################################################

#Per aggiungere la mappa ITALIA sui grafici:
italy.map <- maps::map("italy")
italy.map.shp <-maptools::map2SpatialLines(italy.map)#, proj4string=projection(precsat.raster))
italy<-maps::map("italy",fill=TRUE,col="transparent", plot=F)
p4s<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#SLitaly<-map2SpatialLines(italy,proj4string=p4s)     # se voglio LINEE con il dettaglio delle PROVINCE
province<-italy$names
SPitaly<-map2SpatialPolygons(italy,IDs= province,proj4string=p4s)
IT<-gUnionCascaded(SPitaly, id = NULL)   #or aggregate(SPitaly)
IT_utm<-spTransform(IT,CRS("+proj=utm"))
IT_buff_utm<-gBuffer(IT_utm,width=20000)
IT_buff<-spTransform(IT_buff_utm,p4s)

#Per aggiungere la mappa WORLD sui grafici:
world.map <- maps::map("world")
world.map.shp <-maptools::map2SpatialLines(world.map)#, proj4string=projection(precsat.raster))
world<-maps::map("world",fill=TRUE,col="transparent", plot=F)
p4s<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#SLworld<-map2SpatialLines(world,proj4string=p4s)     # se voglio LINEE con il dettaglio delle PROVINCE
countries<-world$names
SPworld<-map2SpatialPolygons(world,IDs=countries,proj4string=p4s)
#WORLD<-gUnaryUnion(SPworld, id = NULL)   #or aggregate(SPworld)
# WORLD_utm<-spTransform(WORLD,CRS("+proj=utm"))
# WORLD_buff_utm<-gBuffer(WORLD_utm,width=20000)
# WORLD_buff<-spTransform(WORLD_buff_utm,p4s)



for (variabile in c("tn","tx")){
   
  #Leggo i files nella dir dati e ciclo su quelli già tagliati:
  files.dati<-list.files(path=paste(getwd(),"/Data",sep=""),pattern="Cut",full.names=T)
  files.nobuoni<-files.dati[grep("M",files.dati)]
  
  #numero di aree:
  n.areas<-(length(files.dati)-length(files.nobuoni))/4    #sia Duration che Begin, sia tx che tn
   
  
  for (i in 1:n.areas){    #ciclo sui files tagliati per aree e periodo
  
    datax2<-brick((paste("Data/",variabile,"_ECAD_Begin_HeatWave_Cut",i,".nc",sep="")))
    datax3<-brick((paste("Data/",variabile,"_ECAD_Duration_HeatWave_Cut",i,".nc",sep="")))
    ##mesi.lab<-month.abb[as.numeric(levels(as.factor(getMonth(getZ(datax2)))))]
    
    #Extent dell'area studio:
    #-----------------------------------------------------------
    contenitore<-raster(datax2) #SPworld
    #MASCHERA WORLD::::::
    #contenitore_sp<-rasterToPolygons(contenitore,na.rm=F,dissolve=T)
    #mask_negativo<-gDifference(contenitore_sp,IT)#,drop_not_poly=T)
    

    #########################################
    #######  ANALISI HEAT WAVE NUMBER  ######
    #########################################
    #numero<-sum(datax2)
    #freq(datax2)
    ztime<-datax2@z$Date
    Ybegin<-year(ztime)[1]
    Yend<-year(ztime)[length(ztime)]
    
    ####  GRAFICI PER TS  ####
    datax2bis=datax2+0     #artificio per passare in memory e creare oggetto rts
    datax2TS0<-rts(datax2bis,time=datax2@z$Date)
    #Selezione May-Sept:
    layersMaySept<-which(as.numeric(getMonth(index(datax2TS0)))>4 & as.numeric(getMonth(index(datax2TS0)))<10 )
    datax2TS<-subset(datax2TS0,subset=layersMaySept)
    mesi.lab<-month.abb[as.numeric(levels(as.factor(getMonth(index(datax2TS@time)))))]
    
    ##Grafico serie storica annuale numero di ondate (area Italia):
    ############################
    #NUmero ondate cumulate in ogni anno:
    YearSum<-period.apply(datax2TS, INDEX=endpoints(datax2TS,"years"), FUN=sum, na.rm=T)
    ##YearSumTable<-table(freq(YearSum@raster))
    idx_year<-year(YearSum@time)
    names(YearSum@raster)<-idx_year
   
    trellis.device(png,file=paste("Plots/",Sys.Date(),"/",variabile,"_YearSum_Distribution_OndateCaldo_Cut",i,".png",sep=""),
                   width=15, height=20,res=300, units="cm")
    
    histogram(YearSum@raster)#,main="Distribuzione di frequenza del Numero di Ondate di Caldo\n per tutti gli anni in Italia",
              #sub="Temperatura minima giornaliera")
    dev.off()
    #########################
    
    ##Grafico MAx numero di ondate (area Italia):
    ############################
    YearSumMaxArea<-cellStats(YearSum@raster,"max")
    
    trellis.device(png,file=paste("Plots/",Sys.Date(),"/",variabile,"_YearSumMaxArea_TimeSeries_OndateCaldo_Cut",i,".png",sep=""),
                   width=15, height=12,res=300, units="cm")
    
    plot(YearSumMaxArea ~ idx_year,type="l", 
         # main="Massimo Numero di Ondate di Caldo\n in un anno in Italia",
         sub=paste("Maximum value registered in",ncell(datax2),"cells from",mesi.lab[1],"to",mesi.lab[length(mesi.lab)]),
         xlab="",ylab=paste("Yearly",variabile,"Heat Waves occurences"),cex.sub=0.8,cex.lab=0.8
    )
    dev.off()
    
  
  
  #############################
  
  ##Grafico Differenze trentennali:
  #################################################################
  NumSpells_51.80<-period.apply(subset(datax2TS,"1951-01-01/1980-12-31"), INDEX=1, FUN=sum, na.rm=T)
  NumSpells_61.90<-period.apply(subset(datax2TS,"1961-01-01/1990-12-31"), INDEX=1, FUN=sum, na.rm=T)
  NumSpells_71.00<-period.apply(subset(datax2TS,"1971-01-01/2000-12-31"), INDEX=1, FUN=sum, na.rm=T)
  NumSpells_81.10<-period.apply(subset(datax2TS,"1981-01-01/2010-12-31"), INDEX=1, FUN=sum, na.rm=T)
  
  
  #Compongo un solo RasterBrick object:
  diff<-brick(NumSpells_51.80@raster$layer,NumSpells_61.90@raster$layer,
              NumSpells_71.00@raster$layer,NumSpells_81.10@raster$layer)
  names(diff)<-c("1951-1980","1961-1990","1971-2000","1981-2010")
  
  #imposta palette:
  colori<-rev(heat.colors(14))
  myTheme<-rasterTheme(region=colori) # definisco la mia palette di colori  i.e. brewer.pal("BrBG",n=9)
  
  myplot<-levelplot(diff,
            margin=F,
            par.setting=myTheme,           
            #at=my.at,
            xlab="Longitude", ylab="Latitude",
            #main="Numero Cumulato delle Ondate di Caldo \n nel periodo Maggio-Settembre",cex.main=0.4,
            #sub=paste("Temperatura minima giornaliera"),
            layout=c(2,2), # panel orizzontale, c(1,3) per un panel verticale
            names.attr=sub(pattern="X",x=names(diff),replacement=" ")
  ) #+ layer(sp.polygons(mask_negativo,col="white",fill="white")) #layer(sp.lines(italy.map.shp, lwd=0.6, col='white')) +
    
  trellis.device(png,file=paste("Plots/",Sys.Date(),"/",variabile,"_Diff_trentenni_NumOndateCaldo_Cut",i,".png",sep=""),
                 width=15, height=12,res=300, units="cm")
  print(myplot)               
  dev.off()
  ########################

  
  #########################################
  #######  ANALISI HEAT WAVE DURATION  ####
  #########################################
  
  ####  GRAFICI PER TS  ####
  datax3bis=datax3+0     #artificio per passare in memory e creare ogetto rts
  datax3bis@data@values[datax3bis@data@values==0]<-NA    ### ESCLUDO GLI ZERI
  
  datax3TS0<-rts(datax3bis,time=datax3@z$Date)
  #tolgo il 1950 e taglio i mesi:
  layersMaySept<-which(as.numeric(getMonth(index(datax3TS0)))>4 & as.numeric(getMonth(index(datax3TS0)))<10 )
  datax3TS<-subset(datax3TS0,subset=layersMaySept)
  mesi.lab<-month.abb[as.numeric(levels(as.factor(getMonth(index(datax3TS@time)))))]
  
  
  ##Grafico serie storica annuale della DURATA di ondate (area Italia):
  ############################
  
  #Durata media ondate cumulate in ogni anno:
  YearMeanDuration<-period.apply(datax3TS, INDEX=endpoints(datax3TS,"years"), FUN=mean, na.rm=T)
  ##YearSumTable<-table(freq(YearSum@raster))
  idx_year<-year(YearMeanDuration@time)
  names(YearMeanDuration@raster)<-idx_year
  
  ## Grafico distribuzione::
  trellis.device(png,file=paste("Plots/",Sys.Date(),"/",variabile,"_YearMeanDuration_Distribution_OndateCaldo_Cut",i,".png",sep=""),
                 width=15, height=20,res=300, units="cm")
  
  histogram(YearMeanDuration@raster)
#             ,main="Distribuzione di frequenza del Numero di Ondate di Caldo\n per tutti gli anni in Italia",
#             sub="Temperatura minima giornaliera")
  dev.off()
  #########################
  
  ##Grafico Durata MEdia di ondate:
  ############################
  YearMeanDurationArea<-cellStats(YearMeanDuration@raster,"mean",na.rm=T)
  #YearMeanDurationArea[is.nan(YearMeanDurationArea)]<-0  

  trellis.device(png,file=paste("Plots/",Sys.Date(),"/",variabile,"_YearMeanDurationArea_TimeSeries_OndateCaldo_Cut",i,".png",sep=""),
                 width=15, height=12,res=300, units="cm")
  
  plot(YearMeanDurationArea ~ idx_year,type="l", 
       main=paste("Heat Waves Mean Duration\n  from",mesi.lab[1],"to",mesi.lab[length(mesi.lab)]),
       #sub="Temperatura minima giornaliera",xlab="Anni",ylab="Durata Ondate"
       xlab="Years",ylab="Duration (days)"
  )
  dev.off()
  #############################
  
  #Durata massima ondate cumulate in ogni anno:
#   YearMaxDuration<-period.apply(datax3TS, INDEX=endpoints(datax3TS,"years"), FUN=max, na.rm=T)
#   ##YearSumTable<-table(freq(YearSum@raster))
#   idx_year<-year(YearMaxDuration@time)
#   names(YearMaxDuration@raster)<-idx_year
#   
#   YearMaxDurationArea<-cellStats(YearMaxDuration@raster,"max",na.rm=T)
#   #YearMeanDurationArea[is.nan(YearMeanDurationArea)]<-0  
#   
#   trellis.device(png,file=paste("Plots/",variabile,"_YearMeanDurationArea_TimeSeries_OndateCaldo.png",sep=""),
#                  width=15, height=12,res=300, units="cm")
#   
#   plot(YearMaxDurationArea ~ idx_year,type="l", 
#        #main="Massima Durata di Ondate di Caldo\n in un anno in Italia",
#        #sub="Temperatura minima giornaliera",xlab="Anni",ylab="Durata Ondate"
#        xlab="Anni",ylab="Durata Massima delle Ondate (gg)"
#   )
#   dev.off()
# #########################

  #######   ANALISI Annuale HOT DAYS IN ONDA   #####################
  YearHotDays<-period.apply(datax3TS, INDEX=endpoints(datax3TS,"years"), FUN=sum, na.rm=T)
  idx_year<-year(datax3TS@time)
  #per grafico con raster:::
#   YearHotDays_r<-YearHotDays@raster
#   names(YearHotDays_r)<-levels(as.factor(idx_year))
#   bwtheme  <- canonical.theme(color = FALSE)  
#   bwplot( YearHotDays_r ,xlab="Totale giorni caldi nel periodo Maggio-Settembre",ylab="",
#           violin=F, par.settings=bwtheme
#   )

  YearHotDays_m<-getValues(YearHotDays@raster)  #YearHotDays@raster@data@values
  colnames(YearHotDays_m)<-levels(as.factor(idx_year))
   
  YearHotDays_l<-melt(YearHotDays_m,varnames="idx_year")
  names(YearHotDays_l)[2:3]<-c("Years","HotDays")  
  
  #lues(YearHotDays@raster)rHotDays Boxplot dei valori di Hot days in onda per tutte le celle dell'area (totale da Maggio a Settembre)
  png(filename=paste("Plots/",Sys.Date(),"/",variabile,"_HotDaysWithin_Years_BoxPlot_Cut",i,".png",sep=""),width=17, height=12,res=300, units="cm")
  boxplot(YearHotDays_l$HotDays ~ YearHotDays_l$Years,notch=F,col="blue",varwidth=T,outcex=0.65,
          ylab=paste("WSDI from",mesi.lab[1],"to",mesi.lab[length(mesi.lab)],"(days)"), cex.lab=0.7,
              xlab="Years",cex.axis=0.5,las=2)
  dev.off()

  

  #######   ANALISI MENSILE HOT DAYS IN ONDA   #####################
  #!!! Trovare gli endpoints a mano perchè monthly necessita di tutta la serie(questa è gia tagliata su may-sept)
#   endp<-endpoints(datax3TS, on="months", k=1)
#   prova<-apply.monthly(datax3TS,FUN=sum)
#   MonthlyHotDays<-period.apply(datax3TS, INDEX=endpoints(datax3TS,"months"), FUN=sum, na.rm=T)
#   ##YearSumTable<-table(freq(YearSum@raster))
#   idx_year<-year(YearMeanDuration@time)
#   names(YearMeanDuration@raster)<-idx_year

  }  # Chiusura n.areas

}  ## Chiusura ciclo sulle variabili





# 
# #######   ANALISI DURATION MENSILE  #####################
# 
# idx_year<-year(datax@z$Date)
# idx_month<-month(datax@z$Date)
# # idx_month_abb<-month.abb[month(datax@z$Date)]
# # names(datax)<-idx_month_abb
# # # 
# # xyplot(Jan.1 ~ Jul.1,data=datax)
# 
# 
# MonthlyValues<-data.frame(idx_year,idx_month,cellStats(datax,mean,na.rm=T))
# names(MonthlyValues)[3]<-"HeatDuration"
# MonthlyValuesMaySept<-subset(MonthlyValues,MonthlyValues$idx_month>=5 & MonthlyValues$idx_month<=9)
# #CDO monmean salta gli NA e restituisce sempre un valore, quindi gli Nan presenti sono dovuti all'assenza di ONdate::
# MonthlyValuesMaySept[is.nan(MonthlyValuesMaySept$HeatDuration),"HeatDuration"]<-0
# 
# HeatDuration1<-MonthlyValuesMaySept$HeatDuration
# idx_year1<-as.factor(MonthlyValuesMaySept$idx_year)
# idx_month1<-as.factor(month.abb[MonthlyValuesMaySept$idx_month])
# classi<-cut(HeatDuration1,breaks=c(0,1,seq(5,max(HeatDuration1),by=5)))
# 
# #Labels per gli anni:
# prettyC <- pretty(levels(idx_year1),10)
# labC <- prettyC
# axis.CF<-panel.axis(side = "bottom", outside = TRUE, at = prettyC, labels = labC)
# 
# 
# # ##Grafico Number of Heat Wave Years/Months:
# # trellis.device(pdf,file="Plots/HeatDuration_Years_vs_Months.pdf",
# #                width=8, height=8)
# # xyplot( HeatDuration1  ~ idx_year1| idx_month1, 
# #         scales=list(cex=.6, col="red",
# #                     at = seq(min(idx_year),max(idx_year),5),
# #                     labels = seq(min(idx_year),max(idx_year),5)),   #at=seq(min(idx_year),max(idx_year),5),
# #         ylab="Hot Days Number",xlab="Years",aspect="fill",
# #         groups=classi,auto.key = list(columns = nlevels(classi)),
# #         type = c("p","p","p","p"), pch=c(16,17,18,19),col=c("grey10","grey40","grey80","grey90")        
# #       )
# # 
# # dev.off()
# 
# 
# #Grafico B/N per volume Georgofili:
# ##Grafico Number of Heat Wave Years/Months:
# trellis.device(pdf,file="Plots/TX_HeatDuration_Years_vs_Months_BN.pdf",
#                width=8, height=8)
# xyplot( HeatDuration1  ~ idx_year1| idx_month1, 
#         scales=list(cex=.9, draw=T, at = seq(min(idx_year),max(idx_year),5),
#                     labels = seq(min(idx_year),max(idx_year),5)),   #at=seq(min(idx_year),max(idx_year),5),
#         ylab="Numero giorni con TX > TX90",xlab="Anni",aspect="fill",
#         groups=classi,auto.key = list(columns = nlevels(classi)),
#         type = c("p","p","p","p"), pch=c(8,15,16,17),col=rev(c("grey10","grey20","grey60","grey80"))        
# )
# 
# dev.off()
# 
# 
# # #Tolgo Settembre::
# # MonthlyValuesMayAug<-subset(MonthlyValues,MonthlyValues$idx_month>=5 & MonthlyValues$idx_month<=8)
# # HeatWaveNumber2<-MonthlyValuesMayAug$HeatWaveNumber
# # idx_year2<-as.factor(MonthlyValuesMayAug$idx_year)
# # idx_month2<-as.factor(month.abb[MonthlyValuesMayAug$idx_month])
# 
# 
# 
# ##Grafico Number of Heat Wave Years Box-Plot con i mesi:
# trellis.device(pdf,file="Plots/HeatDuration_Years_BoxPlot.pdf",
#                width=8, height=8)
# bwplot( HeatDuration1 ~ idx_year1 ,ylab="Hot Days Number",main="Monthly mean number for May-September period",
# #         prepanel=function(x,y) {
# #                     xx <- x[,drop=TRUE]
# #                     list(xlim=levels(xx),xat=sort(unique(as.numeric(xx))))
# #                     }
# #                     )
#         scales=list(cex=.6, xaxs="r", col="black",at=prettyC,labels=labC)
# #         xscale.components=xscale.components.default(prettyC,
# #                                   packet.number = 0,
# #                                   packet.list = NULL,
# #                                   top = TRUE))
# #                                   #labels=labC))
#         #axis = panel.axis(side = "bottom", outside = TRUE, at = prettyC, labels = labC))
# )
# dev.off()
# 
# 
# ##Grafico Number of Heat Wave Years Box-Plot con i mesi:
# trellis.device(pdf,file="Plots/TX_HeatDuration_Years_BoxPlot_BN.pdf",
#                width=8, height=8)
# bwplot( HeatDuration1 ~ idx_year1 ,ylab="Aggregato mensile del Numero giorni con TX > TX90",
#         main=" Distribuzione annuale dei giorni caldi aggregati\n mensilmente",
#         sub="Maggio-Settembre"
#         #scales=list(cex=.6, xaxs="r", col="black",at=prettyC,labels=labC)
# )
# 
# dev.off()
# 
# 
# 
# barchart( idx_year ~ HeatDuration1)
# 
# 
# # ## HovMoeller diagram::
# # hovmoller(datax,
# #           at = seq(0, 31, 2),
# #           #panel = panel.levelplot.raster,
# #           interpolate = FALSE,
# #           yscale.components = yscale.raster.subticks,
# #           par.settings = BuRdTheme)
# # 
# # ## Horizon Plot
# horizonplot(datax2,
#             origin=10,
#             scale=list('free'),
#             horizonscale=5,
#             #scale = seq(0, 31, 2),
#             col.regions = rev(brewer.pal(n = 10, 'RdBu')))
# 
