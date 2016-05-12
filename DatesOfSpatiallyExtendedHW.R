######    IDENTIFICATION OF HEAT WAVES DATES AT LARGE SPATIAL DOMAIN      ######
#                                                                             ##
#     Edmondo Di Giuseppe      12 May 2016   ###################################
################################################################################
#
# 1) starts from t?_ECAD_Duration_HeatWave_Cut1_Extended_Marginal.nc files;
# 2) moving averages a 10 termini;
# 3) identifies the whole relative maximum and their dates---new file for Marco
#------------------------------------------------------------------------------

library(ncdf4)

### FUNCTIONs HUB         ###
#############################
# Function for relative maximum::



#------------------------------
#numero di aree:
n.areas<-1         

for (var in c("tn","tx")){
  
  for (i in 1:n.areas){    #ciclo sui files tagliati per aree e periodo
    
    #Extracting data from NetCDF file:
    NC<-nc_open(paste("Data/",var,"_ECAD_Duration_HeatWave_Cut",i,"_Extended_Marginal.nc",sep=""))
    print(paste("The file has",NC$nvars,"variables"))
    v1 <- NC$var[[1]]
    (varsize <- v1$varsize)
    (ndims   <- v1$ndims)
    (nt <- varsize[ndims]) # Remember timelike dim is always the LAST dimension!
    timeval <- ncvar_get( NC, v1$dim[[ndims]]$name)#, start=1, count=1 )
    timeunit <- v1$dim[[ndims]]$units
    timedate <- as.Date(substr(timeunit,12,nchar(timeunit)))
    
    ## Lettura della variabile v1:
    Marginal <- ncvar_get(NC,v1)
    nc_close(NC)
    
    summary(Marginal)
    
    Marginal.ts<-ts(Marginal,start=c(1951,1),frequency=365)  #seq(timedate,by="day",length.out=nt))
    plot(Marginal.ts,col="red")
    require(raster)
    appo<-movingFun(Marginal.ts,n=10,fun=mean)
    require(zoo)
    x<-zoo(Marginal,seq(timedate,by="day",length.out=nt))
    x.ma<-rollmean(x,10)
    plot(x.ma) 
   }
}
