##########  Extending 1s to every days of an heat wave  #########
##                            
##  Edmondo Di Giuseppe    May 9, 2016 
#################################################################
# 1) From "Data/"$var"_ECAD_"$i"_HeatWave_Cut"$index".nc", where the ts is composed of zeros except
#     for the intial day of heat waves, takes the non-zero values and extends 1 to every days of 
#     the heat wave;
# 2) Produce a new .nc file
#################################################################

library(raster)

### FUNCTIONs HUB         ###
#############################

### Function for single cell:: 
# x<-getValues(data.cropped)[5,]
# x00<-x
Put1_SingleCell<-function(x){
  x0<-x
  x<-na.omit(x)
  if(any(x > 1)){
    inizio<-which(x>1)
    duration<-x[inizio]
    for(i in 1:length(inizio)){x[inizio[i]:(inizio[i]+duration[i]-1)]<-1}
  }else{
    x<-x0
  }
  return(x)
} 
# prova<-Put1_SingleCell(x)
# summary(prova)
#--------------------------------

####   FUNCTION FOR RASTER   #####
# x is a RasterStack or RasterBrick
##x<-crop(datax,extent(c(12.3,13.3,43.5,44.5)))
Put1_Raster<-function(x,filename){  
  out<-brick(x,values=F) #contenitore rasterBrick vuoto con le stesse caratteristiche di x
  
  out <- writeStart(out, filename, format="CDF",overwrite=TRUE)#,zname="time",zunit="days")    
  for (i in 1:nrow(out)) {   
    v <- getValues(x, i)
    v <- t(apply(v,MARGIN=1,FUN=Put1_SingleCell))  
    out <- writeValues(out,v , i) 
  }  #chiudo ciclo "i" sulle rows del raster x
  
  out <- writeStop(out)
  return(out)  
}

#tutti1<-Put1_Raster(x,filename="prova.nc")
############################################


######   APPLYING FUNCTIONS ON HEAT WAVES FILES   ###############

for (var in c("tn","tx")){
  
  #Leggo i files nella dir dati e ciclo su quelli già tagliati:
  files.dati<-list.files(path=paste(getwd(),"/Data",sep=""),pattern="Cut",full.names=T)
  files.nobuoni<-files.dati[grep("M",files.dati)]
  
  #numero di aree:
  n.areas<-1         #ceiling((length(files.dati)-length(files.nobuoni))/4)    #sia Duration che Begin, sia tx che tn
  
  
  for (i in 1:n.areas){    #ciclo sui files tagliati per aree e periodo
    
    datax<-brick((paste("Data/",var,"_ECAD_Begin_HeatWave_Cut",i,".nc",sep="")))
    timex<-getZ(datax)
    tutti1<-Put1_Raster(x=datax,filename=paste(getwd(),"/Data/",var,"_ECAD_Duration_HeatWave_Cut",i,"_Extended0.nc",sep=""))
    tutti1<-setZ(tutti1,timex)
  }
}



