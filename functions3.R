### Working Functions
# Required Packages:

#Importing libraries
library(ggplot2);library(dplyr);library(zoo); 
#ibrary(xts); 
library(tseries); library(lunar)
library(grid); library(gridExtra);
#library(splines); 
library(fractal); library(changepoint)
#library(TideHarmonics)

#####
station_map <- function(lat,lon,name,save = NULL) {
  # Lat is the latitude of the station (as value from 0 to +-90)
  # Lon is the longitude of the station (as value 0 to 360, or 0 to +-180)
  # Name is the name of the station (as character)
  # Save is the pathway to save the map (optional)
  # Creates a world map and plot the station on top of it
  # Requirement: ggplot2
  # Created by Carolina Camargo - 13/03/2018
  
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() +   mapWorld
  #Now Layer the station
  mp <- mp+ geom_point(aes(x=lon, y=lat) ,color="red", size=1.5) +
    ggtitle(paste("Working Station",name)) + 
    labs(y="Latitude(°)", x = "Longitude(°)")
  if (is.null(save)){
  }else ggsave(sprintf("%s_map.png",name), plot = mp, device = "png", path = save)
  return(mp)
}
#####
lunarmonth <- function(time, level = NULL) {
  # Fuction that separates a date vector in the lunar months presents in that vector
  # Returns a vector with the 1:n, where 1 is the first lunar month in the vector, and n is the last  
  # Returns a vector with the numbers(indicies) of the start of the lunar month in the time vector
  # optional input: Level vector
  # Package Requirements: dplyr, lunar
  if (is.null(level)){ 
    p <- data.frame(time = time)
  }else{ p <- data.frame(time = time, level = level)}
  
  p <- mutate(p,date = as.yearmon(time,"%b %y"))
  p$moon <- lunar.phase(p$time)
  
  d <- diff(p$moon)
  d[length(p$moon)]=d[length(d)]
  p <- mutate(p, dif = d)
  
  td <- as.numeric(diff(p$time)) #time difference in minutes
  td[length(p$moon)]=td[length(td)]
  p <- mutate(p, timedif = td)
  
  p <- mutate (p, div = ifelse(abs(dif)>pi,1,0))
  ind <- which(p$div==1 | p$timedif>43200)
  # ind <- ind+1
  m <- ifelse(p$div==1 | p$timedif>43200 ,1,0)
  for (i in 1:length(ind)){
    if (i==1){ 
      m[1:ind[i]] <-i
    }else {m[(ind[i-1]+1):ind[i]] <-i}
    if (i==length(ind)){
      if (ind[i] != length(m)){
        m[(ind[i]+1):length(m)] <- i+1
      }
    }          
  }
  return(list(lunarmonth = m, indices = ind ))
}
#####
lunarapply <- function(time, level, phases = F, func, perc = NULL ){
  # Function that applys a required function f(x), according to the lunar months
  # Inputs: time vector, level vector, and the function wished to apply to each lunar month.
  # Default is to apply for each lunar month. If you want to apply to each moon phase in each lunar month,
  # then phases = T (default: phases = F)
  # Possible functions: mean, sd, median, quantile (than also the percentage needs to be specified)
  # Outputs: the result of the function applied for each lunar month
  # Example: You can ask for the mean level at the lunar month, or the mean level at each phase of the lunar month
  # Package Requirements: dplyr, lunar
  
  g <- data.frame(time, level)
  lm <- lunarmonth(time,level)
  lunarmonth<- lm$lunarmonth
  ind = max(lunarmonth, na.rm = T)
  v <- NULL
  t <- NULL
  for(i in 1:ind){
    dat <- filter(g,lunarmonth==i)
    if (length(dat$level) ==0){
    }else{
      if (phases == T){
        vp <- NULL
        tp <- NULL
        l=0
        dat$phase <- lunar.phase(dat$time)
        kmax <- max (dat$phase, na.rm = T)
        
        if (kmax >= 0 & kmax< pi/2){cy = 1} # we have 1 moon phase ( our time vector ends in the new moon)
        if (kmax >= pi/2 & kmax< pi){cy = 2} # we have 2 moon phases (our time vector ends in the first quarter)
        if (kmax >= pi & kmax< (3*pi)/2){cy = 3} # we have 3 moon phases (our time vector ends in the full moon)
        if (kmax >= (3*pi)/2 & kmax< 2*pi){cy = 4} # we have 4 moon phases (our time vector ends in the third quarter) -> complete cycle
        
        kmin <- min (dat$phase, na.rm = T)
        
        if (kmin >= 0 & kmin< pi/2){st = 1} # we have 1 moon phase ( our time vector ends in the new moon)
        if (kmin >= pi/2 & kmin< pi){st = 2} # we have 2 moon phases (our time vector ends in the first quarter)
        if (kmin >= pi & kmin< (3*pi)/2){st = 3} # we have 3 moon phases (our time vector ends in the full moon)
        if (kmin >= (3*pi)/2 & kmin< 2*pi){st = 4} # we have 4 moon phases (our time vector ends in the third quarter) -> complete cycle
        
        for (j in st:cy){
          if (j ==1){dat2<-filter(dat,phase >= 0 & phase<pi/2)}
          if (j ==2){dat2<-filter(dat,phase >= pi/2 & phase< pi)}
          if (j ==3){dat2<-filter(dat,phase >= pi & phase< (3*pi)/2)}
          if (j ==4){dat2<-filter(dat,phase >= (3*pi)/2 & phase< 2*pi)}
          
          if (length(dat2$level)==0){
          }else{ l =l+1
          if (is.null(perc)){vp[l]<- func(dat2$level, na.rm = T)
          }else { vp[l]  <- func(dat2$level, perc, na.rm=TRUE)} 
          tp[l]<-as.character(dat2$time[length(dat2$time)])    
          } #closes else of if length(data2$level) ==0  
        } #closes for 1:cy
        v <-c(v,vp)
        t <-c(t,tp)
      }else { 
        if (is.null(perc)){v[i]<- func(dat$level, na.rm = T)
        }else { v[i]  <- func(dat$level, perc, na.rm=TRUE)}
        
        t[i]<-as.character(dat$time[length(dat$time)])
      } #closes else of if phases ==T
    }  #close else of if dat$level == 0
  }#close for (i in 1:ind)
  z <- zoo(v,t)
  g2 <- zoo(level,as.character(time))
  gz <- merge(g2,z)
  gz <- transform(gz, z = na.locf(z,fromLast = T))
  out <- gz$z
  OUT  <- fortify.zoo(out)
  
  #  return(list(value= out))
  return(value=OUT[,2])#,time=OUT[,1])
}

#####
global_outlier <- function(level, time) {
  # Level and time must have the same dimensions
  # Function aplies a gross filter to identify outliers according:
  # outliers is a (value-mean > 3*sd) or (value-mean < -3*sd)
  # It used the total mean and standard deviation for the period 
  
  # Creates a flag vector, with 1 to values that are kept and 
  # 0 to values that are removed 
  #
  # Requirements: dplyr
  # 
  # Created by Carolina Camargo - 13/03/2018

  #  a <- data.frame(time = y$time,level = y$level)
  
  a <- data.frame(time,level)
  
  msl <- mean(a$level,na.rm=T) #total mean
  sdsl<- sd(a$level,na.rm=T)  #total standard deviation
  a <- mutate(a,clean=ifelse(abs(level-msl)>4*sdsl,NA,level))
  a <- mutate(a,flag=ifelse(abs(level-msl)>4*sdsl,0,1)) #values that have been excluded
  
  return(list(clean = a$clean,flag = a$flag, time = a$time))
}

outlier_gloss <- function(level,time, lunarmonth = NULL) {
  # Function aplies a filter to identify outliers according:
  # Outlier is a (value + median > 3*(percentile90+median)) or (value - median < 3*(percentile90-median))
  # According to Gloss treatment (http://www.ioc-sealevelmonitoring.org/service.php)
  # outlier is a value that X - median exceeds the tolerance value
  # the tolarance value is calculated by: 3*abs(Percentile90 - median)
  # 
  # Creates a flag vector, with 1 to values that are kept and 
  # 0 to values that are removed 
  #
  # Inputs: level vector, time vector (both with same lengths), 
  #         lunarmonth = vector with lunarmonth (default = NULL, if lunarmonth = T,it will calculate the median and percentile90 in the lunarmonth, instead of in the calendar month)
  #
  # Outputs: clean =  level after the outlier detection. Suspected values have been replaced with NA
  #          flag  =  vector with 0 for values removed and 1 for values kept
  #
  # Requirements: dplyr, zoo
  # Created by Carolina Camargo - 13/03/2018
  # Modified to replace zoo to dplyr - 17/04/2018  
  
  if (any(level==-999,na.rm = T)){  #If missing values in the level vector havent yet been replaced by NA, do it now:
    level = ifelse(level ==-999, NA, level)
  }
  
  
  if (is.null(lunarmonth) ==F){ #using lunar month
    g<-data.frame(Time=time, Level = level,lunarmonth=lunarmonth)
    
    g<-group_by(g,lunarmonth)
    slm<-summarise(g,medsl=median(Level,na.rm=T),stdv=sd(Level,na.rm = T) ,p90=quantile(Level,0.9,na.rm=T),p10=quantile(Level,0.1,na.rm=T))
    g<-inner_join(g,slm, by="lunarmonth")
    g<-ungroup(g)
    g<- mutate(g,clean =ifelse( abs(Level-medsl) > 3*abs(p90-medsl), NA, Level))
    g<-mutate(g, flag = ifelse( abs(Level-medsl) > 3*abs(p90-medsl), 0, 1))
    
  }else{ # using calendar month
    g<-data.frame(Time=time, Level = level)
    g<-mutate(g, month=format(x$Time, "%Y%m"))
    g<-group_by(g,month)
    slm<-summarise(g,medsl=median(Level,na.rm = T),stdv=sd(Level,na.rm = T) ,p90=quantile(Level,0.9,na.rm = T),p10=quantile(Level,0.1,na.rm = T))
    g<-inner_join(g,slm, by="month")
    g<-ungroup(g)
    g<- mutate(g,clean =ifelse( abs(Level-medsl) > 3*abs(p90-medsl), NA, Level))
    g<-mutate(g, flag = ifelse( abs(Level-medsl) > 3*abs(p90-medsl), 0, 1))
    
    }

  ## Old Version (using zoo)  
  # if (lunarmonth == T){  #Using Lunar Month
  #   
  #   medsl <- lunarapply(time,level,func = median)
  #   p90 <- lunarapply(time,level,func = quantile,perc=.90)
  #   clean <- ifelse( abs(level-medsl) > 3*abs(p90-medsl), NA, level)
  #   flag <- ifelse( abs(level-medsl) > 3*abs(p90-medsl), 0, 1)
  #   
  # }else { # Using calendar month
  #   
  #   z <- zoo(level, time)
  #   medsl <- apply.monthly(z,median, na.rm = T)
  #   p90   <- apply.monthly(z,FUN=function(x) quantile(x, .90, na.rm=TRUE))
  #   df    <- merge(z,medsl,p90)
  #   df    <- transform(df, medsl = na.locf(medsl,fromLast = T))
  #   df    <- transform(df, p90 = na.locf(p90,fromLast = T))
  #   df    <- transform(df, QC = ifelse(abs(z-medsl) > (3*abs(p90-medsl)) , NA, z))
  #   clean  <- fortify.zoo(df$QC)
  #   clean  <- clean[,2]
  #   df   <- transform(df, flag = ifelse(abs(z-medsl) > (3*abs(p90-medsl)) , 0, 1))
  #   flag  <- fortify.zoo(df$flag)
  #   flag  <- flag[,2]
  #   
  # }
  
  return(list(clean=g$clean, flag=g$flag))
}

out_of_range <- function(level, time, lunarmonth = NULL,lunarphase = NULL) {
  # Function aplies a filter for out-of-range modules according to OPPE manual
  # Out-of-range value is a (value > monthly.msl+(3*monthly.sd)) or (value < monthly.msl-(3*monthly.sd))  
  # According to OPPE in Gloss QC Manual
  # Out-of-range values are those beyond a seasonal limit
  # For a given area and month, the seasonal limit is:
  # season_limit = 2 * stdv +- mean
  # Out-of-range values are flagged and not considered in the subsequent checks
  # BUT because using 2*stdv was cutting REAL data, we changed it for 3.
  # 
  # Creates a flag vector, with 1 to values that are kept and 0 to values that are removed 
  #
  # Inputs: level vector, time vector (both with same lengths), 
  #         lunarmonts = T/F (default = F, if lunarmonth = T,it will calculate the median and percentile90 in the lunarmonth, instead of in the calendar month)
  #         lunarphase = T/F (default = F, if lunarphase = T,it will calculate the median and percentile90 for each lunar phase (so for each tidal cycle, e.g., spring and neap) in the lunarmonth, instead of in the calendar month)

  # Outputs: clean =  level after the outlier detection. Suspected values have been replaced with NA
  #          flag  =  vector with 0 for values removed and 1 for values kept
  # Requirements: dplyr, zoo, xts
  # Created by Carolina Camargo - 13/03/2018
  # Modified 17/04/2018
  
  
  if (any(level==-999,na.rm = T)){  #If missing values in the level vector havent yet been replaced by NA, do it now:
    level = ifelse(level ==-999, NA, level)
  }
  if (is.null(lunarmonth) == F){  # Using Lunar Month
    if (is.null(lunarphase) == F){
      # msl   <- lunarapply(time,level,func = mean,phases = T)
      # sd    <- lunarapply(time,level,func = sd,phases = T)
      # clean <- ifelse( abs(level-msl) > 3*sd, NA, level)
      # flag <- ifelse( abs(level-msl) > 3*sd, 0, 1)
      g<- data.frame(Time=time,Level=level,lunarphase=lunarphase)
      g<-group_by(g,lunarphase)
      slm<-summarise(g,msl=mean(Level,na.rm=T),stdv=sd(Level,na.rm=T))
      g<-inner_join(g,slm, by="lunarphase")
      g<-ungroup(g)
      g<- mutate(g,clean =ifelse( abs(level-msl) > 3*stdv, NA, level))
      g<-mutate(g, flag = ifelse( abs(level-msl) > 3*stdv, 0, 1))
      
    }else{
      g<- data.frame(Time=time,Level=level,lunarmonth=lunarmonth)
      g<-group_by(g,lunarmonth)
      slm<-summarise(g,msl=mean(Level,na.rm=T),stdv=sd(Level,na.rm=T))
      g<-inner_join(g,slm, by="lunarmonth")
      g<-ungroup(g)
      g<-arrange(g,by=Time)
      g<- mutate(g,clean =ifelse( abs(Level-msl) > 3*stdv, NA, Level))
      g<-mutate(g, flag = ifelse( abs(Level-msl) > 3*stdv, 0, 1))
      # msl   <- lunarapply(time,level,func = mean)
      # sd    <- lunarapply(time,level,func = sd)
      # clean <- ifelse( abs(level-msl) > 3*sd, NA, level)
      # flag <- ifelse( abs(level-msl) > 3*sd, 0, 1)
    } #closing of if lunarphase = T
    
  }else{ # Using calendar months
    
    g<- data.frame(Time=time,Level=level)
    g<-mutate(g, calmonth=format(g$Time, "%m"))
    g<-group_by(g,calmonth)
    slm<-summarise(g,msl=mean(Level,na.rm=T),stdv=sd(Level,na.rm=T))
    g<-inner_join(g,slm, by="calmonth")
    g<- mutate(g,clean =ifelse( abs(Level-msl) > 3*stdv, NA, Level))
    g<-mutate(g, flag = ifelse( abs(Level-msl) > 3*stdv, 0, 1))
    g<-ungroup(g)
    g<-arrange(g,by=Time)
    
    # z <- zoo(level, time)
    # if (is.null(clim)==F){  # Using given climatological mean and stdv
    #   msl <- clim[,1]
    #   sd  <- clim[,2]
    # }else{
    #   msl   <- apply.monthly(z,mean, na.rm = T)
    #   sd    <- apply.monthly(z,FUN=function(x) sd(x, na.rm=TRUE))
    # }
    # df   <- merge(z,msl,sd)
    # df   <- transform(df, msl = na.locf(msl,fromLast = T))
    # df   <- transform(df, sd = na.locf(sd,fromLast = T))
    # df   <- transform(df, clean = ifelse(abs(z-msl) > (3*sd) , NA, z))
    # df   <- transform(df, flag = ifelse(abs(z-msl) > (3*sd) , 0, 1))
    # clean  <- fortify.zoo(df$clean)
    # clean  <- clean[,2]
    # flag  <- fortify.zoo(df$flag)
    # flag  <- flag[,2]
  }
  
  return(list(clean = g$clean, flag = g$flag))
}

outlier_mod <- function(level, time, GO = T, OG = T, OR = T, lunarmonth = F, lunarphase = F, clim = NULL) {
  
  # Function aplies a filter QC Outlier Module
  # Uses 3 subfunctions: outlier_gross, outlier_gloss, out_of_range
  # see functions descriptions for more details
  # 
  # Creates a flag vector, with 1 to values that are kept and 0 to values that are removed 
  # 
  # Inputs: level vector, time vector (both with same lengths), 
  #         GO = T/F (default = T, applies the gross_outlier step. If =F, then it skips it)
  #         OG = T/F (default = T, applies the outlier_gloss step. If =F, then it skips it)  
  #         OR = T/F (default = T, applies the out_of_range step. If =F, then it skips it)
  #         lunarmonth = T/F (default = F, if lunarmonth = T, outlier_gloss and out_of_range will be applied for the lunarmonth. See functions for more details.)
  #         lunarphase = T/F (default = F, if lunarphase = T,it will be applied for out_of_range. See functions for more details).
  #         clim = NULL ('clim' is a vector[12x2] with the climatological means and standard deviation calculated for a station,
  #                             this is an optional input. (If clim is given, it will be applied for out_of_range. See functions for more details).
  #
  # Outputs: Time = time vector correspondend to the level vector
  #         Level_orig = level as the raw level, BUT with -999 values replace for NA
  #         Level_clean = level after the outlier detection. Suspected values have been replaced with NA
  #         Flag  =  vector with 0 for values removed and 1 for values kept, combining the different steps
  #         df_flag = data frame with the flags for each subfunction applied
  #
  # Requirements: dplyr, zoo, xts, lunar
  # Created by Carolina Camargo - 13/03/2018
  # Modified - 17/08/2018
  
  # level <- x$Level
  # time <- x$Time
  
  #Testing if station was online for the given period
  if ((all(is.na(level)) | (all(level == 0, na.rm = T)))==T){
    message("Offline station")
    return(list(Time = y$time, Level_orig = level)) }
  if ((all(is.na(level)) & (all(level == 0, na.rm = T)))==T){
    message("Offline station")
    return(list(Time = y$time, Level_orig = level)) }
  
  y <- data.frame(time,level)
  # Replacing values with -999 with NA
  y <- mutate(y,levelna = ifelse(level ==-999, NA, level))
  y <- mutate(y,flagna = ifelse(level ==-999, 0, 1))
  
  #Testing if station was online for the given period
  if ((all(is.na(y$levelna)) | (all(y$levelna == 0, na.rm = T)))==T){
    message("Offline station")
    return(list(Time = y$time, Level_orig = y$levelna))}
  
  if ((all(is.na(y$levelna)) & (all(y$levelna == 0, na.rm = T)))==T){
    message("Offline station")
    return(list(Time = y$time, Level_orig = y$levelna))}
  
  if(lunarmonth==T){
    lt <- lunartable()
    y<-mutate(y, daytime=format(y$time, "%Y-%m-%d"))
    sdate <- y$daytime[1]
    fdate<- y$daytime[length(y$daytime)]
    dm2<-subset(lt,lt$time>=sdate & lt$time<=fdate)
    
#   y<-group_by(y,daytime)
    y<-mutate(y,lunarmonth=ifelse(daytime==dm2$time,dm2$lunarmonth,NA))
#   y<-ungroup(y)
    y<-mutate(y,lunarmonth=na.locf(lunarmonth))
    if(lunarphase==T){
#     y<-group_by(y,daytime)
      y<-mutate(y,lunarphase=ifelse(daytime==dm2$time,dm2$phase,NA))
#     y<-ungroup(y)
      y<-mutate(y,lunarphase=na.locf(lunarphase))
    }
  }
  
  # Step 1: global_outlier filter
  # Filter based on total mean and standard deviation.
  # See outlier function for more details
  if (GO == T){
    y1 <- global_outlier(y$levelna,y$time)
    
  }else { y1 <- data.frame(clean =y$levelna) }
  
  # Step 2
  # Filter based on the Medial and 90 Percentile - GLOSS Manual
  if (OG == T){
    if (lunarmonth == T){
      y2 <- outlier_gloss(y1$clean, y$time, lunarmonth = y$lunarmonth)
    }else { y2 <- outlier_gloss(y1$clean, y$time)}
    
  }else{ y2<- data.frame(clean = y1$clean)}
  
  # Step 3
  # Filter of out-of-range values
  if (OR == T){
    if (lunarmonth == T & lunarphase == T){
      y3 <- out_of_range(y2$clean, y$time,lunarmonth = y$lunarmonth,lunarphase = y$lunarphase)
    }else{ 
      if (lunarmonth == T){ y3 <- out_of_range(y2$clean, y$time,lunarmonth = y$lunarmonth)  
      }else{      y3 <- out_of_range(y2$clean, y$time) 
      } }
    
  }else{y3 <- data.frame(clean=y2$clean)}
  
  # Creating a flag data frame:
  if (GO == T & OG == T & OR == T){
    Df_flag <- data.frame(y$flagna,y1$flag,y2$flag,y3$flag)
  }
  if (OG == T & OR == T){
    Df_flag <- data.frame(y$flagna,y2$flag,y3$flag)
  }
  if (GO == T & OR == T){
    Df_flag <- data.frame(y$flagna,y1$flag,y3$flag)
  }
  if (GO == T & OG == T ){
    Df_flag <- data.frame(y$flagna,y1$flag,y2$flag)
  }
  # Creating a unique vector that combines all the flags from each step
  flag = apply(Df_flag, 1, prod, na.rm=T)
  
  # Getting the output level clean  
  clean <- y3$clean
  return(list(Time = y$time, Level_orig = y$levelna, Level_clean = clean, Flag = flag, Df_flag=Df_flag))
} 
#####
speed_change <- function(level, time, lunarmonth = F) {
  # Function aplies a filter to check the speed of change
  # 
  # Creates a flag vector, with 1 to values that are kept and 4 to values that are removed 
  # Returns list with: clean level, flag
  #
  # Inputs: level vector, time vector (both with same lengths), 
  #         lunarmonts = T/F (default = F, if lunarmonth = T,it will calculate the amplitude for each lunarmonth, instead of  calendar month)
  # Outputs: Level_clean = vector with the level after the speed of change filter
  #          Flag = vector with 0 for values that were removed and 1 values kept
  #
  # Requirements: dplyr, zoo, xts, lunar
  # Created by Carolina Camargo - 13/03/2018
  
  # level <- x$Levelna
  # time <- x$Time
  
  # Testing if station was online for the given period
  if ( (all(is.na(level)) | (all(level == 0, na.rm = T)))==T){
    message ("Offline station")
    flag<-level
    flag[]<-0
    return( list(Level_clean = level, Flag=flag))}  
  if ( (all(is.na(level)) & (all(level == 0, na.rm = T)))==T){
    message ("Offline station")
    flag<-level 
    flag[]<-0
    return(list(Level_clean = level, Flag=flag))}  
  
  if(lunarmonth==T){
    y<-data.frame(Time=time,Level=level)
    lt <- lunartable()
    y<-mutate(y, daytime=format(y$Time, "%Y-%m-%d"))
    sdate <- y$daytime[1]
    fdate<- y$daytime[length(y$daytime)]
    dm2<-subset(lt,lt$time>=sdate & lt$time<=fdate)
    
#    y<-group_by(y,daytime)
    y<-mutate(y,lunarmonth=ifelse(daytime==dm2$time,dm2$lunarmonth,NA))
#    y<-ungroup(y)
    y<-mutate(y,lunarmonth=na.locf(lunarmonth))
    
    y<-group_by(y,lunarmonth)
    slm<-summarise(y,Hwl=max(Level,na.rm=T),Lwl=min(Level,na.rm=T))
    slm<-mutate(slm,Amp=abs(Hwl-Lwl))
    y<-inner_join(y,slm, by="lunarmonth")
    y<-ungroup(y)
    y<-arrange(y,Time)
    
  }else{
    y<- data.frame(Time=time,Level=level)
    y<-mutate(y, calmonth=format(y$Time, "%m"))
    
    y<-group_by(y,calmonth)
    slm<-summarise(y,Hwl=max(Level,na.rm=T),Lwl=min(Level,na.rm=T))
    slm<-mutate(slm,Amp=abs(Hwl-Lwl))
    y<-inner_join(y,slm, by="calmonth")
    y<-ungroup(y)
    y<-arrange(y,Time)
  }  

  y <- mutate(y, prev = lag(Level, order_by = y$Time))
  
  # we cant compare a value at time t with a value at time t-1, if that value is NA
  # So:
  if (is.na(y$prev[1])){ # Checking if the first value is NA
    e<- which(is.na(y$prev))
    if (length(e)==1){
      k <- e[1]+1
    }else{
      ee <- diff(e)
      a <- which(ee > 1)
      k <- e[a[1]] +1 #position of the fist non NA
      rm(ee);rm(a)
    }
    y$prev[1] <- y$prev[k] # Replacing the first value by the non-na, in case it is na
    rm(e);rm(k)}
  
  if (is.na(y$prev[length(y$prev)])){ #Checking if the last value is NA
    e<- which(is.na(y$prev))
    if (length(e)==1){
      k <- e[1]+1
    }else{
      ee <- diff(e)
      a <- which(ee > 1)
      k <- e[a[length(a)]]+1 #position of the last non NA
    }
    y$prev[length(y$prev)] <- y$prev[k]} # Replacing the last by non-NA
  
  # Doing a NA Interpolation, where the na values are replaced by an interpolated one.
  y <- mutate(y, prev2 = na.approx(prev))
  
  # Calculating the Frequency: 
  freq <-frequency(time)
  if (freq <= 5 ){ tol = 0.05}
  if (freq > 5 & freq <= 15){ tol = 0.08}
  if (freq > 15 & freq <= 20){ tol = 0.1}
  if (freq > 20 & freq <= 30){ tol = 0.15}
  if (freq > 30 & freq <= 60){ tol = 0.3}
  # Tolerance values based on SIMORC QC Manual v3
  
  y <- mutate(y, clean = ifelse(abs(Level - prev2) > abs(tol*Amp),NA, Level )) #According SIMORC Manual  
  y <- mutate(y, flag = ifelse(abs(Level - prev2) > abs(tol*Amp),0, 1 )) 
  
  return(list(Level_clean= y$clean, Flag = y$flag))
}
#####
stability_check <- function(v,t,lag) {
  # Function that checks for mal-functioning of the sensor
  # It flags and removes values that are kept for more than 2 hours
  # Creates a flag vector with 1 to values kept and 0 to values removed
  #
  # Inputs: v is a value of a time series
  #         t is the time of the time series
  #         lag is the time interval/range we want to check our stability
  # Outputs:  Level_clean: level after the suspected values have been removed
  #           Flag: flag vector with 1 for the values that were kept and 0 for the ones removed
  #
  # Requirements: zoo, dplyr, tseries
  # Created by Carolina Camargo - 13/03/2018
  
  # v<-x$levelna
  # t<-x$Time
  # lag=120
  
  # Testing if the station was offline
  if ( ( all(is.na(v)) | (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  
  if ( ( all(is.na(v)) & (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  
  rounds <- round(length(t)/lag)
  tmp <- data.frame(Time = t, Level = v)
  out<- NULL
  flag <- NULL
  if (any(!is.na(v))){ 
    # We have values that are not NA
    if (any(v != 0, na.rm = T)){
      #We have values that are not NA and not ). So we check for stability
      for (j in 1:(rounds)){
        i <- lag*j
        
        if( j==1){
          data <- slice(tmp,1:lag)
        }else { 
          if (j == rounds) { data <- slice(tmp, (lag*j):length(t)); i <- length(t)}
          data <- slice(tmp, (lag*(j-1)):(lag*j) )}
        
        if (all(is.na(data$Level) == T)){out[i]<- "stable"; flag[i] <- 0
        } else{ if (any(data$Level != 0, na.rm = T)){ 
          datana = na.remove(data$Level)
          if (length(datana)==1){ 
            out[i]<- "stable"
            flag [i] <- 0
          }else {
            susp <- datana[1]/datana[length(datana)]
            if (is.na(susp)){ out[i]<- "stable"; flag[i] <- 0
            }else {
              susp2 <- datana[2]/datana[length(datana)-1]
              if (susp == 1 | susp2==1){ 
                if (length(datana)>2){
                  #susp3 <- datana[3]/datana[length(datana)-2]
                  all(datana==datana[1])
                  if (all(datana==datana[1])){ out[i]<- "stable"
                  #if (susp3 == 1){ out[i]<- "stable"
                  flag [i] <- 0
                  } #close susp3==1
                } #closes length(datana)>2
              }else { out[i]<- "ok";flag[i] <- 1} #close susp==1 |susp2==1
            } #close else if is.na(susp)                        
          }#close else of if length(datana)==1
          
        }else { out[i]<- "stable"; flag[i] <- 0} #close if any(data$level) !=0
        } #close else of if all(is.na(data$level))
      }#close for loop 1:rounds
      
    }else{ message("Station Offline  - All values are 0 ") #closes if any(v) !=0
      out[length(t)]<- "stable"
      flag [length(t)] <- 0
    }
    
  }else{ message("Station Offline  - All values are NA")
    out[length(t)]<- "stable"
    flag [length(t)] <- 0
  } #closes else of if any(!is.na(v))
  
  out =  na.locf(out, fromLast = T)
  flag = na.locf(flag,fromLast = T)
  tmp<- mutate(tmp, clean = ifelse(flag==0,NA,Level))
  # data<- mutate(data, clean = ifelse(flag==0,NA,Level))
  tmp$flag <- flag
  tmp<- mutate(tmp, flag = ifelse(is.na(Level) & is.na(clean),0,flag))
  
  
  return(list(Level_clean = tmp$clean,Flag = tmp$flag))
}
#####
medfilt <- function(v,t,n=3,method1="runmed"){
  # Function that aplies a median filter to smooth over spikes.
  # Inputs: v is a vector with the variable to be smoothed over (e.g. sea level)
  #         t is a time vector correspondend to the observations in v
  #         n is the size of the window to apply the median filter (default n=3)
  # Outputs:
  # Requirement: zoo (rollapply)
  #
  # Created by Carolina Camargo 06/04/2018
  # f = slice(x,43200:86400)
  # f = slice(x,86400:129600)
  # f = slice(x,129600:172800)
  # v <- x$Levelna
  # t <- x$Time
  # n <- 3
  
  if ( ( all(is.na(v)) | (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  if ( ( all(is.na(v)) & (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  
  vt <- data.frame(v,t)
  
  if (method1 =="runmed"){
    med <- runmed(na.locf(v),n)
  }
  if (method1 =="rollapply"){
    med=rollapply(v, width=n, FUN=function(x) median(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
  }
  if (method1 =="fractal"){
    med<- medianFilter(v,order = n)
  }
  
  stdv= zoo::rollapply(v, width=n, FUN=function(x) sd(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left")
  vt <- mutate(vt, clean = ifelse(abs(v) > abs(med + stdv),NA, v )) 
  vt <- mutate(vt, flag  = ifelse(abs(v) > abs(med + stdv),0, 1 )) 
  
  return (list (Level_clean = vt$clean, Flag = vt$flag, Med = med))} 
#####
splinefilt <- function(v,t,wd=NULL){
  #
  #Function that applies a spline to remove spikes.
  # Inputs: v is a vector with the variable to be smoothed over (e.g. sea level) 
  #         t is a time vector correspondend to the observations in v
  #         wd is the size of the window (in hours) to apply the spline filter 
  # Outputs:
  # Requirement: Splines
  # Created by Carolina Camargo 06/04/2018  
  
  # v<- w$Level_clean
  # t<- y$Time
  
  if ( ( all(is.na(v)) | (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  if ( ( all(is.na(v)) & (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  
  if(is.na(v[1]) & is.na(v[length(v)])==T){ 
    # vsp <- na.spline(v)
    # v[1]<-vsp[1]
    # v[length(v)]<-vsp[length(vsp)]}
    
    dt<- data.frame(time=t,level=v)
    ind<-which(is.na(v)==F) 
    ind<-ind[1]
    # Make a new df without the first NA's
    dt2<-dt[ind:nrow(dt),]
    dt2$level=na.locf(dt2$level) # remove the NA's 
    # Make the smooth spline
    if(is.null(wd)==F){
      freq <-(60/frequency(t))
      lag = freq*wd
      sp <- smooth.spline(dt2$time,dt2$level, df=lag)
    }else {  sp <- smooth.spline(dt2$time,dt2$level)}
    
    if(length(sp$y) != length(dt2$time)){
      sp<-data.frame(timenum=sp$x,spline=sp$y)
      dt2$timenum<-as.numeric(dt2$time)
      dtf<- full_join(dt2,sp,by="timenum")
      dtf<-arrange(dtf,time)
      dtf$level2<-ifelse(is.na(dtf$spline),NA,dtf$spline)
      sp<-data.frame(y=dtf$level2,x=dtf$timenum)
      
    }else{   
      dt3<-data.frame(time=dt2$time,spline=sp$y,timenum=sp$x)
    if(nrow(dt)!=nrow(dt2)){
      dtf<-full_join(dt3,dt,by="time")
      dtf<-arrange(dtf,time)
      dtf$level2<-ifelse(is.na(dtf$spline),NA,dtf$spline)
      sp<-data.frame(y=dtf$level2,x=dtf$timenum)
    } }

  }else{
    
    if(is.na(v[1])==T){v<-na.locf(v,fromLast = T)}
    if(is.na(v[length(v)])==T){v<-na.locf(v)}
    
    if(is.null(wd)==F){
      freq <-(60/frequency(t))
      lag = freq*wd
      sp <- smooth.spline(t, na.locf(v), df=lag)
    }else {  sp <- smooth.spline(t, na.locf(v))}
    
  }
  #Last test to make sure it will come out with the same length
  if(length(sp$y) != length(t)){
    
    sp<-data.frame(timenum=sp$x,spline=sp$y)
    dt2<-data.frame(time=t,level=v)
    dt2$timenum<-as.numeric(dt2$time)
    dtf<- full_join(dt2,sp,by="timenum")
    dtf<-arrange(dtf,time)
    dtf$level2<-ifelse(is.na(dtf$spline),NA,dtf$spline)
    sp<-data.frame(y=dtf$level2)
  }
  
  return(sp=sp) 
}
#####
spike <- function(v,t,wd=12,n=3,k=3,method1="runmed",filt=1){
  # Function to detect and remove spikes (smaller spikes that pass by the outlier module) from the signal
  # Uses two filters: Median Filter and a Spline Filter
  #
  # Inputs: v:
  #         t:
  #         wd: window, in hours, to apply the spline. Default = 12
  #         k: degree of the standard deviation used to dected the spikes.Default = 2 
  #           (e.g. k= 2, a value that differs from the median by 2*sdtv is removed)
  #         n: window size for the Median filter. Default = 3. must be an odd number!
  #         method: methods for the Median Filter. Default = "runmed"
  #         filt: Option to choose if you want both Median and Spline filter (filt=1),
  #               Only the median filter (filt=2), or only the spline filter (filt=3). Default = 1
  #
  # Outputs:
  # Requirements: Zoo, Splines, Fractal
  
  # Testing if there is data
  if ( ( all(is.na(v)) | (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  if ( ( all(is.na(v)) & (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  
  if(filt==1){
    m <- medfilt(v,t,n,method1 = method1)
    vm<- m$Level_clean
    sp <- splinefilt(vm,t,wd)
    
    stdv <- sd(v,na.rm = T)

    vclean <- ifelse(abs(v-sp$y)>abs(k*stdv) ,NA,v)
    flag <-  ifelse(abs(v-sp$y)>abs(k*stdv) ,0,1)
  }
  if (filt ==2){
    m <- medfilt(v,t,n,method1 = method1)
    vm<- m$Level_clean

    stdv <- sd(v,na.rm = T)

    vclean <- ifelse(abs(v-vm)>abs(k*stdv) ,NA,v)
    flag <-  ifelse(abs(v-vm)>abs(k*stdv) ,0,1)
  }
  if (filt==3){
    
  sp <- splinefilt(v,t,wd)
  
  stdv <- sd(v,na.rm = T)
  
  vclean <- ifelse(abs(v-sp$y)>abs(k*stdv) ,NA,v)
  flag <-  ifelse(abs(v-sp$y)>abs(k*stdv) ,0,1)
  
  
  }
  
  return (list (Level_clean = vclean, Flag = flag)) 
}  
#####
breakpoint <- function(v,t,method2 ="BinSeg",type="mean"){
  # Function to find changepoints or breakpoints in the time series.
  # Uses the package changepoint from R. For more information, refer to the changepoint.pdf 
  # https://cran.r-project.org/web/packages/changepoint/index.html
  #
  # After identifying the changepoints, the time series is sliced in sets according to the changepoints.
  # Then the mean is calculated for each set, and removed from the data.
  # The result is a time series "alligned". This helps us removed the change in the signal due to communcation failure during data transmission,
  # or even datum changes or changes in the reference point. Because we are working with RELATIVE sea level, it is ok to do this correction.
  #
  # Inputs: v,t are a value and time vector of a time series
  #         method: method to identifiy changepoints. default is "BinSeg". 
  #                 choise between: "PELT", "AMOC", "SegNeigh"or "BinSeg"
  #                 given the size of the time series, BinSeg is recommendable, unless in very fast systems.
  #         type: choice between mean, var or meanvar. You can dected change based on the mean, variance, or in both.
  #
  # Outputs: chpt: location of the changepoints
  #          flag: vector same size of v, with 1 for the position where change happens, and 0 for the rest
  #          changelevel: vector same size of v, with only values where change happened (rest NA) (useful for visualization)
  #          changetime: vector with the time of the changes (same length as chpt)
  #          level,time: vectors after the mean correction
  
  # v <- x$Levelna
  # t <- x$Time
  # method2 ="BinSeg"
  # type="mean"
  
  if(any(v==-999,na.rm = T)){
    v = ifelse (v==-999, NA, v)}
  
  if ( ( all(is.na(v)) | (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  if ( ( all(is.na(v)) & (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(Time = time, Level_clean = v))} 
  
  vt <- data.frame(t,v)
  vt <- mutate(vt,year = format(t,"%Y"))

  year2<-levels(factor(vt$year))
  
  chptf = NULL; newlevel = NULL; changetime=NULL; flag =NULL;levelf=NULL;timef=NULL
    #  output <- NULL
    for (j in 1:length(year2)){
      v2 <- filter(vt,vt$year==year2[j])
     # nam <- paste("year", j, sep = "")
      #assign(nam, v2)
      if (type=="mean"){
        x.bs <-cpt.mean(na.locf(v2$v),method = method2)
      }
      if (type=="var"){
        x.bs <-cpt.var(na.locf(v2$v),method = method2)
      }
      if (type=="meanvar"){
        x.bs <-cpt.meanvar(na.locf(v2$v),method = method2)
      }
      chpt <- cpts(x.bs)
      rm(x.bs)
      if(length(chpt)==0){
        if (type=="mean"){ #we used first only mean, and didnt find anything. So we change the method
          x.bs <-cpt.meanvar(na.locf(v2$v),method = method2)
          chptmeanvar <- cpts(x.bs)
          rm(x.bs)
          if (length(chptmeanvar)==0){ #If still there wasnt any breakpoint, we change again the method
            x.bs <-cpt.var(na.locf(v2$v),method = method2)
            chptvar <- cpts(x.bs)
            rm(x.bs)
            if (length(chptvar)==0){ #test the last method
              #If the thrid trial is still empty, return the function
              message("No Breakpoint identified")
              return(list(level=v,time=t))
            }else{   message ("Warning: Requested Type might been changed, because the type 'mean' was not finding a breakpoint.
               Type used was 'var' ")
              chpt = chptvar}
          }else{   message ("Warning: Requested Type might been changed, because the type 'mean' was not finding a breakpoint.
               Type used was 'meanvar' ")
            chpt = chptmeanvar}
        }
        if (type=="var"){ #we used first only mean, and didnt find anything. So we change the method
          x.bs <-cpt.meanvar(na.locf(v2$v),method = method2)
          chptmeanvar <- cpts(x.bs)
          rm(x.bs)
          if (length(chptmeanvar)==0){ #If still there wasnt any breakpoint, we change again the method
            x.bs <-cpt.mean(na.locf(v2$v),method = method2)
            chptmean <- cpts(x.bs)
            rm(x.bs)
            if (length(chptmean)==0){ #test the last method
              #If the thrid trial is still empty, return the function
              message("No Breakpoint identified")
              return(list(level=v,time=t))
            }else{   message ("Warning: Requested Type might been changed, because the type 'Var' was not finding a breakpoint.
               Type used was 'Mean' ")
              chpt = chptmean}
          }else{   message ("Warning: Requested Type might been changed, because the type 'Var' was not finding a breakpoint.
               Type used was 'meanvar' ")
            chpt = chptmeanvar}
        }
        if (type=="meanvar"){ #we used first only mean, and didnt find anything. So we change the method
          x.bs <-cpt.mean(na.locf(v2$v),method = method2)
          chptmean <- cpts(x.bs)
          rm(x.bs)
          if (length(chptmean)==0){ #If still there wasnt any breakpoint, we change again the method
            x.bs <-cpt.var(na.locf(v2$v),method = method2)
            chptvar <- cpts(x.bs)
            rm(x.bs)
            if (length(chptvar)==0){ #test the last method
              #If the thrid trial is still empty, return the function
              message("No Breakpoint identified")
              return(list(level=v,time=t))
            }else{   message ("Warning: Requested Type might been changed, because the type 'meanvar' was not finding a breakpoint.
               Type used was 'var' ")
              chpt = chptvar}
          }else{   message ("Warning: Requested Type might been changed, because the type 'meanvar' was not finding a breakpoint.
               Type used was 'mean' ")
            chpt = chptmean}
        }
        
        
      } #closes if length(chpt)==0
      # Dealing with gaps
      empt <- v2$v
      empt[]<- NA
      v3 <- na.locf(v2$v)
      empt[chpt] <- v3[chpt]
      newlevel<-c(newlevel,empt)
      timetpm <-v2$t[chpt]
      changetime<-c(changetime,timetpm)
      # plot(t,v,col="grey")
      # points(t,newlevel,col="red")
      
      flagtmp <- v2$v
      flagtmp[]<-0
      flagtmp[chpt]<-1
      flag<-c(flag,flagtmp)
      #Creating empty vectors for time and level
      level<-v2$v;level[]<-NA
      time<-v2$t;time[]<-NA
      # Creating data frame
      vt2<- data.frame(v2$t,v2$v)
      
      #We centralize the data
      for(i in 1:(length(chpt)+1)){
        if (i==1){
          data<- slice(vt2,1:chpt[i])
        }else {
          j <- chpt[i-1]+1
          if (i == length(chpt)+1){
            data <- slice(vt2,j:length(v2$v))
          }else {
            data <-slice(vt2,j:chpt[i])
          }# close else of i==length(chpt)
        } #close else of if i==1
        
        mu <- mean(data$v2.v, na.rm=T)
        if (i==1){
          level[1:chpt[i]] <- data$v2.v-mu
          time[1:chpt[i]] <- data$v2.t
        }else {
          j<- chpt[i-1]+1
          if (i == length(chpt)+1){
            level[j:length(v2$v)] <- data$v2.v-mu
            time[j:length(v2$v)] <- data$v2.t
          }else {
            level[j:chpt[i]] <- data$v2.v-mu
            time[j:chpt[i]] <- data$v2.t
          }# close else of i==length(chpt)
        } #close else of if i==1
        
      } #close for
      levelf<-c(levelf,level)
      timef<-c(timef,time)
      chptf<-c(chptf,chpt)
      rm(v2);#rm(nam)
      
    } #for j in year
  
   return(list(chpt = chpt, changelevel = newlevel, changetime=changetime, flag =flag,level=levelf,time=timef))
}
#####
QCmodule <- function(v,t, ALL = T, BP= T, ST = T, OUT = T, SC = T, SP =T,
                     go = T, og= T, or = T, lm = F, lp = F,cli=NULL,
                     method1 = "runmed", method2="BinSeg",lagi=120,ni=3,ka=3,wid=2,filtro=1,type="mean"){
  # Function that applied the QC Module
  # BP = Breakpoint Module
  # ST = Stability Module
  # OUT = Outlier Module
  # SC = Speed of Change Module
  # SP = Spike Module
  # You can choose to turn off one module (e.g. BP = F). for that ALL = F. At least one needs to be T.
  # Or you can just say ALL = T, then the 5 modules are applied.
  # The rest of the parameters are related to each of the modules above.
  # method1 refers to the Median Filter method applied in the Spike Module
  # method2 refers to the Changepoint method applied in the Breakpoint Module
  # ka,ni,wid,filtro are parameters of the Spike Module
  # lagi is a parameter of the Stability Check Module
  # lm and lp are the lunarmonth and lunarphase are options of the Outlier Module to apply treatments according to the lunarmonth or lunar phase.
  # cli is a vector with the climatological means and amplitudes, to use as clim in Out_of_Range function of the Outlier Module
  # type is parameter of the Breakpoint Module.
  # go,og,or are parameters of the Outlier Module, to choose which filter to (des)active
  # filtro: Option to choose if you want both Median and Spline filter (filtro=1),
  #       Only the median filter (filtro=2), or only the spline filter (filtro=3). Default = 1
  #
  # For more information, look at each Module/function
  #
  # Output is a clean time series, and a flag vector (0 for removed 1 for kept)
  # a flag data frame, with the flag value for each Module is also given
  # The flag of the breakpoint module is separate, as it indicates only where changes occured
  # Changepoints from breakpoint module are also returned.
  # In case of a failure in the middle of the Modules, a msg is added to the output
  
  # v<-x$Level
  # t<-x$Time
  xQC <- NULL
  xQC$Time<-t

  
  if ( ( all(is.na(v)) | (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(xQC=xQC))} 
  if ( ( all(is.na(v)) & (all(v == 0, na.rm = T)) )==T ){
    message ("Offline Station")
    return(list(xQC=xQC))} 
  if (any(v==-999,na.rm = T)){  #If missing values in the level vector havent yet been replaced by NA, do it now:
    flagna = ifelse(v ==-999, 0, 1)
    v = ifelse(v ==-999, NA, v)

    if (all(is.na(v)==T)){
      message("Offline Station - only levels=-999")
      msg ="Offline station - only levels=-999"
      
      xQC$Level_clean <- v
      xQC$Flag_final <-flagna
      return(list(xQC=xQC,msg=msg)) 
    }else{
      vtest = na.remove(v)
      if (all(vtest==0)){
        message("Offline Station - only levels=-999 & level =0")
        msg ="Offline station - only levels=-999 & level =0"
        xQC <- NULL
        xQC$Level_clean <- v
        xQC$Flag_final <-flagna
        return(list(xQC=xQC,msg=msg))
      }
    }
  }else{ flagna=v;flagna[]<-1}
  
  if (ALL == T ){
    # Time Series Break Point Correction
    bp <- breakpoint(v,t)
    if (all(is.na(bp$level) | (all(bp$level == 0, na.rm = T))==T)){
      message("empty time series after Breakpoint Module")
      msg ="empty time series after Breakpoint Module"

      xQC$Level_clean <- v
      xQC$Flag_final <-flagna
      return(list(xQC=xQC,msg=msg))
    } 
    # Stability Module
    st <- stability_check(bp$level,t,lag=lagi)
    stflag<-st$Flag
    bpflag<-bp$flag;chpt=bp$chpt;rm(bp)
    if (all(is.na(st$Level_clean) | (all(st$Level_clean == 0, na.rm = T))==T)){
      message("empty time series after Stability Module")
      msg ="empty time series after Stability Module"
      xQC$Level_clean <- st$Level_clean
      stflag<-st$Flag
      xQC$Flag_final <-flagna * stflag
      return(list(xQC=xQC,msg=msg))
    }else{ 
      dt <- data.frame(time = t, level = st$Level_clean)
      ind <- which(!is.na(st$Level_clean))
      dt2 <- dt[ind,]
      
      if ( (dt2$time[length(ind)]-dt2$time[1]) <= 24){ #checking if the rest of the data has more than 1 day
        message("time series only had 1 day of data after Stability Module")
        msg ="time series only had 1 day of data after Stability Module"
        xQC$Level_clean <- st$Level_clean
        stflag<-st$Flag
        xQC$Flag_final <-flagna * stflag
        
        return(list(xQC=xQC,msg=msg))
      }
    }
    
    # Outlier Module
    out <- outlier_mod(st$Level_clean,t,lunarmonth=lm,lunarphase = lp, GO = go, OG = og,OR = or, clim=cli )
    rm(st)
    if ((all(is.na(out$Level_orig)) | (all(out$Level_orig == 0, na.rm = T)))==T){
      message("empty time series after Outlier Module")
      msg ="empty time series after Outlier Module"
      xQC$Level_clean <- out$Level_clean
      outflag<-out$Flag
      xQC$Flag_final <-flagna * outflag * stflag
      return(list(xQC=xQC,msg=msg))
    }
    # Speed of change Module
    sc <- speed_change(out$Level_clean,out$Time,lunarmonth = lm) # uses the result of the outlier module as level
    outflag<-out$Flag
    rm(out)
    if (all(is.na(sc$Level_clean) | (all(sc$Level_clean == 0, na.rm = T))==T)){
      message("empty time series after Speed of Change Module")
      msg ="empty time series after Speed of Change Module"
      xQC <- NULL
      xQC$Level_clean <- sc$Level_clean
      outflag<-out$Flag
      scflag<-sc$Flag
      xQC$Flag_final <-flagna * stflag * outflag*scflag
      return(list(xQC=xQC,msg=msg))    
    } 
    # Spike detection Module
    sp <- spike(sc$Level_clean,t,filt = filtro,k = ka,n=ni,wd = wid)
    scflag<-sc$Flag
    rm(sc)
    if (all(is.na(sp$Level_clean) | (all(sp$Level_clean == 0, na.rm = T))==T)){
      message("empty time series after Spike Module")
      msg ="empty time series after Spike Module"
      xQC$Level_clean <- sp$Level_clean
      xQC$Flag_final <-flagna * stflag * outflag*scflag *sp$Flag
      return(list(xQC=xQC,msg=msg))    
    } 
    
  }else{ 
    
    # Breakpoint Module
    if(BP==F){
      bp<- NULL
      bp$level <- v
      bp$Flag <-v
      bp$Flag[]<-NA
      bp$chpt <- 0
    }else{
      bp <- breakpoint(v,t)
      if (all(is.na(bp$level) | (all(bp$level == 0, na.rm = T))==T)){
        message("empty time series after Breakpoint Module")
        msg ="empty time series after Breakpoint Module"
        xQC$Level_clean <- v
        xQC$Flag_final <-flagna
        return(list(xQC=xQC,msg=msg))
      } 
    }
    
    # Stability Module
    if(ST==F){
      st <- NULL
      st$Level_clean <-bp$level
      st$Flag <-v
      st$Flag[]<-1
      bpflag<-bp$flag;chpt=bp$chpt;rm(bp)
      
    }else{
      st <- stability_check(bp$level,t,lag=lagi)
      bpflag<-bp$flag;chpt=bp$chpt;rm(bp)
      if (all(is.na(st$Level_clean) | (all(st$Level_clean == 0, na.rm = T))==T)){
        message("empty time series after Stability Module")
        msg ="empty time series after Stability Module"
        xQC$Level_clean <- st$Level_clean
        stflag<-st$Flag
        xQC$Flag_final <-flagna * stflag
        return(list(xQC=xQC,msg=msg))
      }else{ 
        dt <- data.frame(time = t, level = st$Level_clean)
        ind <- which(!is.na(st$Level_clean))
        dt2 <- dt[ind,]
        
        if ( (dt2$time[length(ind)]-dt2$time[1]) <= 24){ #checking if the rest of the data has more than 1 day
          message("time series only had 1 day of data after Stability Module")
          msg ="time series only had 1 day of data after Stability Module"
          xQC$Level_clean <- st$Level_clean
          stflag<-st$Flag
          xQC$Flag_final <-flagna * stflag
          return(list(xQC=xQC,msg=msg))
        }
      }
    }
    
    # Outlier Module
    if(OUT==F){
      out <- NULL
      out$Level_clean<-st$Level_clean
      out$Level_orig<-v
      out$Flag <-v
      out$Flag[]<-1
      stflag<-st$Flag;rm(st)
    }else{
      out <- outlier_mod(st$Level_clean,t,lunarmonth=lm,lunarphase = lp)
      stflag<-st$Flag;rm(st)
      if ((all(is.na(out$Level_orig)) | (all(out$Level_orig == 0, na.rm = T)))==T){
        message("empty time series after Outlier Module")
        msg ="empty time series after Outlier Module"
        xQC$Level_clean <- out$Level_clean
        outflag<-out$Flag
        xQC$Flag_final <-flagna * outflag * stflag
        return(list(xQC=xQC,msg=msg))
      }
    }
    
    # Speed of change Module
    if(SC==F){
      sc <- NULL
      sc$Level_clean <- out$Level_clean 
      sc$Flag <-v
      sc$Flag[]<-1
      outflag<-out$Flag;rm(out)
      }else{
      sc <- speed_change(out$Level_clean,t,lunarmonth = lm) # uses the result of the outlier module as level
      outflag<-out$Flag;rm(out)
      if (all(is.na(sc$Level_clean) | (all(sc$Level_clean == 0, na.rm = T))==T)){
        message("empty time series after Speed of Change Module")
        msg ="empty time series after Speed of Change Module"
        scflag<-sc$Flag
        xQC$Flag_final <-flagna * stflag * outflag*scflag
        xQC$Level_clean <- sc$Level_clean
        return(list(xQC=xQC,msg=msg))    
      } 
    }
    
    # Spike detection Module
    if(SP==F){
      sp <- NULL
      sp$Level_clean<-sc$Level_clean
      sp$Flag <-v
      sp$Flag[]<-1
      scflag<-sc$Flag;rm(sc)
    }else{
      sp <- spike(sc$Level_clean,t,filt = filtro,k=ka,n=ni,wd=wid)
      scflag<-sc$Flag;rm(sc)
      if (all(is.na(sp$Level_clean) | (all(sp$Level_clean == 0, na.rm = T))==T)){
        message("empty time series after Spike Module")
        msg ="empty time series after Spike Module"
        xQC<- NULL
        xQC$Level_clean <- sp$Level_clean
        xQC$Level_clean <- sp$Level_clean
        xQC$Flag_final <-flagna * stflag * outflag*scflag *sp$Flag
        return(list(xQC=xQC,msg=msg))    
      } 
    }
  }  

  vclean <-sp$Level_clean
  Df_flags <- data.frame(stflag,outflag,scflag,sp$Flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  
  x<-data.frame(Time=t,Level_orig=v,FlagNA=flagna,FlagST=stflag,FlagOUT=outflag,FlagSC=scflag,FlagSP=sp$Flag,Flag_final=flags,Level_clean=sp$Level_clean)

  
  xclean<- dplyr::filter(x,Flag_final==1)
  xclean<-dplyr::select(xclean,-Level_orig,-FlagNA,-FlagST,-FlagOUT,-FlagSC,-FlagSP)
  xclean<-dplyr::rename(xclean,Level=Level_clean)
   # return(list(vclean = vclean,vorig = v ,time=t ,bpflag=bpflag ,chpt=chpt ,flag = flags, Df_flags = Df_flags))
  return(list(xQC=x,xclean=xclean))
  
}

############
# Lunar Table
lunartable <- function(sdate= as.Date("2005-12-27"),fdate=as.Date("2021-01-09")){
   # Function that creates a lunartable, given a stard and final date
   # Output: a table with time, moon phase in radians, moon phase name, 
   #         moon phase number (where 1 = New, 2= First Quarter, 3=Full, 4=Third Quarter)
   # lunarmonth: counting from the sdate to the fdate, a lunar month is defined as passing by the 4 lunar phases.
   # if:  sdate <- as.Date("2005-12-27")
   #      fdate <- as.Date("2021-01-09")
   # Then the number of lunarmonth is 186.
   # Requirements: Lunar
  
time<- as.Date(seq.Date(from = sdate,to=fdate, by='day'),'%d %b %y')
moon <- lunar.phase(time) #Lunar phase in Radians 
mooname <- lunar.phase(time,name=T) #Name of the lunar phases

dm <- data.frame(time,moon,mooname)

#Create a column for a number instead of the phase name 
dm<- mutate (dm, phase=ifelse(mooname=="New",1,NA))
dm<- mutate (dm, phase=ifelse(mooname=="Waxing",2,phase))
dm<- mutate (dm, phase=ifelse(mooname=="Full",3,phase))
dm<- mutate (dm, phase=ifelse(mooname=="Waning",4,phase))

d <- diff(dm$phase)
d[length(dm$phase)]=d[length(d)]
dm<-mutate(dm,Lag=lag(phase))
dm$Lag[1]=4
dm <- mutate(dm, dif = Lag-phase)
dm <- mutate(dm, div = ifelse(dif==3,1,NA))

ind <- which(dm$div==1)
m<-NULL
for (i in 1:length(ind)){
  if (i==1){ 
    m[1:(ind[i+1]-1)] <-i
  }else{  if (i==length(ind)){
    #  if (ind[i] != length(dm$phase)){
    m[(ind[i]):length(dm$phase)] <- i
    #}
  }else{
    m[(ind[i]):(ind[i+1]-1)] <-i
  }          
  }}

dm<-mutate(dm,lunarmonth=m)
dm<-select(dm,-Lag,-dif,-div)
lunartable <- dm

#save(lunartable,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/lunartable.Rda')
return(lt=dm)
}
# print(Sys.time() - start_time)
############


