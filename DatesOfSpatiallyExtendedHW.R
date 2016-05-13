######    IDENTIFICATION OF HEAT WAVES DATES AT LARGE SPATIAL DOMAIN      ######
#                                                                             ##
#     Edmondo Di Giuseppe      12 May 2016   ###################################
################################################################################
#
# 1) starts from t?_ECAD_Duration_HeatWave_Cut1_Extended_Marginal.nc files;
# 2) moving averages a 10 termini;
# 3) from daily to hourly time step using linear regression
# 4) identifies the whole relative maximum and their dates---new file for Marco
#------------------------------------------------------------------------------

library(ncdf4)

### FUNCTIONs HUB         ###
#############################
# Function for relative maximum::
derivative <- function(f, x, ..., order = 1, delta = 0.1, sig = 6) {
  # Numerically computes the specified order derivative of f at x
  vals <- matrix(NA, nrow = order + 1, ncol = order + 1)
  grid <- seq(x - delta/2, x + delta/2, length.out = order + 1)
  vals[1, ] <- sapply(grid, f, ...) - f(x, ...)
  for (i in 2:(order + 1)) {
    for (j in 1:(order - i + 2)) {
      stepsize <- grid[i + j - 1] - grid[i + j - 2]
      vals[i, j] <- (vals[i - 1, j + 1] - vals[i - 1, j])/stepsize
    }
  }
  return(signif(vals[order + 1, 1], sig))
}


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
    
    # Marginal.ts<-ts(Marginal,start=c(1951,1),frequency=365)  #seq(timedate,by="day",length.out=nt))
    # plot(Marginal.ts,col="red")
    # require(raster)
    # appo<-movingFun(Marginal.ts,n=10,fun=mean)
    require(zoo)
    MargZoo<-zoo(Marginal,seq(timedate,by="day",length.out=nt))
    x<-rollmean(MargZoo,10)
    plot(x) 
    
    table(cut(x, quantile(x)))
    
    RelMax<-function(x)
    {
      # x2<-rep(0,length(x))
      # for(i in 1:length(x))
      # {
      #   x2[which.max(x)]<-1  
      #   x[which.max(x)] <-0
      #   
      # }
      xlag1<-diff(x,1)
      tutti<-cbind(x,xlag1,xlag1/24)
      daily<-expand.grid(x=x,y=rep(xlag1/24,each=24))
      library(pspline)
      time=time(x)
      d1<-predict(sm.spline(time, x), time, 1)
      d2<-predict(sm.spline(time, x), time, 2)
      appo<-x[d1>-0.5 & d1<0.5 & d2<(-0.5)]
      plot(appo)
      abline(v=as.vector(time(appo)),col="lightgrey")
       
      
      dx<-D(x~time,"x")
      d1<-eval(dx)
    }
    
   }
}
