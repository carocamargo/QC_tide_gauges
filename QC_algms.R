# rm(list=ls()) 
# source('/Claudio/Documents/Oceans&Lakes/Thesis/Data/1year/x/functions3.R')
# # Creating Tables to compare different Algorithms
# 
# setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/1year/') 
# filenames <- list.files(".", pattern="*.Rda", full.names=TRUE)
# # Creating Tables to compare different Algorithms
# #####
# fname<- NULL
# for (i in 1:length(filenames)){
#   #  i=1
#   load(filenames[i]) 
#   station <- as.character(x$Station[1]) 
#   sensor <- x$Sensor[1]
#   fname[i]<-paste(station,sensor,sep="_")
# }
# rm(x)
# NRows=length(filenames)
# 
# algm = paste ("A", 0:34, sep="")  #like this there will be no space
# algm[1]<- c("Raw")
# algm[2:35] = paste (rep(LETTERS[1:2],each =17), 1:17, sep="")
# NCols=length(algm) 
# 
# Tab1<-matrix(NA, NRows, NCols)
# 
# colnames(Tab1)<-algm
# rownames(Tab1)<-fname
# 
# Tab2<- Tab1
# Tab3<- Tab2
# 
# # WITH PLOTS
# #####
# 
# setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/1year/') 
# stime <- Sys.time()
# for(i in 13:length(filenames)){
#   load(filenames[i]) 
#   station <- as.character(x$Station[1]) 
#   sensor <- x$Sensor[1]
#   x <- mutate(x,Levelna = ifelse(Level ==-999, NA, Level))
#   # Raw
#   #####
#   k=0
#   na <- which(is.na(x$Levelna) == T) #Number of missing values in Raw dataset
#   na <- length(na)
#   nt <- length(x$Level) # total number of observations
#   n <- nt - na # Real number of observations in the Raw Dataset
#   Tab1[i,k+1]<- n
#   
#   msl <- mean(x$Levelna,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwl <- max(x$Levelna, na.rm = T)
#   lwl <- min(x$Levelna, na.rm = T)
#   amp <- abs(hwl - lwl)
#   Tab3[i,k+1] <- amp
#   #####
#   
#   # A1 - All modules and filters, calendar months
#   #####
#   k=1
#   A <- QCmodule(x$Level,x$Time, ALL = T) # All modules and filters, calendar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   
#   
#   #####
#   
#   # B1 - All modules and filters, lunar months
#   #####
#   k=18
#   A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T) # All modules and filters, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A2 - All modules and filters (GO is off), calendar months
#   #####
#   k=2
#   A <- QCmodule(x$Level,x$Time,ALL = T, go= F) # All modules and filters (but the GO in Outlier), calendar months
#   
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- mslA
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   #B2 - All modules and filters (GO is off), lunar months
#   #####
#   k=19
#   A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- mslA
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A3 - Without BreakPoint Module, calendar months
#   #####
#   k=3
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F, go= F) # Without BreakPoint Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA  
#   bp_level <- A$XQC$Level_clean
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B3 - Without BreakPoint Module, lunar months
#   #####
#   k=20
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,lm = T,lp = T, go= F) # Without BreakPoint Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA  
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A4 - Without Stability Module, calendar months
#   #####
#   k=4
#   A <- QCmodule(x$Level,x$Time,ALL = F,ST = F, go= F) # Without Stability Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B4 - Without Stability Module, lunar months
#   #####
#   k=21
#   A <- QCmodule(x$Level,x$Time,ALL = F,ST = F,lm = T,lp = T, go= F) # Without Stability Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A5 - # Without OUT Module, calendar months
#   #####
#   k=5
#   A  <- QCmodule(x$Level,x$Time,ALL = F,OUT = F,lm = T,lp = T, go= F) # Without OUT Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B5 - # Without OUT Module, lunar months
#   #####
#   k=22
#   A  <- QCmodule(x$Level,x$Time,ALL = F,OUT = F,lm = T,lp = T, go= F) # Without OUT Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A6 - Without SC Module, calendar months
#   #####
#   k=6
#   A  <- QCmodule(x$Level,x$Time,ALL = F,SC = F, go= F) # Without SC Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B6 - Without SC Module, lunar months
#   #####
#   k=23
#   A  <- QCmodule(x$Level,x$Time,ALL = F,SC = F,lm = T,lp = T, go= F) # Without SC Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   #A7 - Without SP Module. calendar months
#   #####
#   k=7
#   A <- QCmodule(x$Level,x$Time,ALL = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- mslA
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   #B7 - Without SP Module. Lunarmonths
#   #####
#   k=24
#   A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- mslA
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A8 - Only Stability Module, calendar months
#   #####
#   k=8
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = T,SC = F,OUT = F,SP = F, go= F) # Only Stability Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B8 - Only Stability Module, lunar months
#   #####
#   k=25
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = T,SC = F,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Stability Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   st_level<-A$xQC$Level_clean
#   st_flag<-A$xQC$Flag_final
#   
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A9 - # Only Outlier Module (all filters), calendar months
#   #####
#   k=9
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   out_cal_level<-A$xQC$Level_clean
#   out_cal_flag<-A$xQC$Flag_final
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B9 - # Only Outlier Module (all filters), lunar months
#   #####
#   k=26
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   out_lun_level<-A$xQC$Level_clean
#   out_lun_flag<-A$xQC$Flag_final
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A10 - Only Speed of Change Module, calendar months
#   #####
#   k=10
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = T,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Speed of Change Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   sc_cal_level<-A$xQC$Level_clean
#   sc_cal_flag<-A$xQC$Flag_final
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B10 - Only Speed of Change Module, lunar months
#   #####
#   k=27
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = T,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Speed of Change Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   
#   sc_lun_level<-A$xQC$Level_clean
#   sc_lun_flag<-A$xQC$Flag_final
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A11 - # Only Spike Module, calendar months
#   #####
#   k=11
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F) # Only Spike Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B11 - # Only Spike Module, lunar months
#   #####
#   k=28
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F) # Only Spike Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   sp_level<-A$xQC$Level_clean
#   sp_flag<-A$xQC$Flag_final
#   
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A12 - # Only Outlier Module with OR and OG, calendar months
#   #####
#   k=12
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   
#   out_2cal_level<-A$xQC$Level_clean
#   out_2cal_flag<-A$xQC$Flag_final
#   
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B12 - # Only Outlier Module with OR and OG, lunar months
#   #####
#   k=29
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   out_2lun_level<-A$xQC$Level_clean
#   out_2lun_flag<-A$xQC$Flag_final
#   
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A13 - Only Outlier Module with OG filter, calendar months
#   #####
#   k=13
#   A<- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F, go= F, or= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B13 - Only Outlier Module with OG filter, lunar months
#   #####
#   k=30
#   A<- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F, or= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A14 - Only Outlier Module with OR filter, calendar months
#   #####
#   k=14
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,go= F, og= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B14 - Only Outlier Module with OR filter, lunar months
#   #####
#   k=31
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F, og= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A15 - Only Outlier Module with GO filter, calendar months
#   #####
#   k=15
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,or= F, og= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # B15 - Only Outlier Module with OR filter, lunar months
#   #####
#   k=32
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, or= F, og= F) # Only Outlier Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A16 - # Only Spike Module with Med Filter, lunar months
#   #####
#   k=16
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F,filtro=2) # Only Spike Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   
#   #####
#   
#   # B16 - # Only Spike Module with Med Filter, lunar months
#   #####
#   k=33
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F,filtro=2) # Only Spike Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   # A17 - # Only Spike Module with Spline Filter, lunar months
#   #####
#   k=17
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F,filtro=3) # Only Spike Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   # PLOTS
#   if (any(A$xQC$Level_clean != 0, na.rm = T)){
#     
#     ylow <- min(lwl,lwlA)
#     yup <-max(hwl,hwlA)
#     d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
#     d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
#     d$Col <- factor(d$Col, levels = c("kept", "out"))
#     
#     
#     # Year Plot (Grey & Green)
#     p<- ggplot(x, aes(x = Time, y= value)) +
#       geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
#       ylim  (ylow,yup)+
#       geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
#     ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/")
#     
#     # Year plot - Black & Red - Flags
#     #          
#     png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
#     mp1<- ggplot(d, aes(x = time, y= value)) +
#       geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
#       ylim  (ylow,yup)+
#       scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#     #print(mp1)
#     mp2<- ggplot(d, aes(x = time, y= value)) +
#       geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#       xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#     
#     p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#     graphics.off()
#     
#     # 2 weeks plot
#     for (j in 1: (length(x$Levelna)/40320)){
#       if (length(x$Levelna) !=0 ){
#         
#         if (j == 1){
#           data <- slice (d,1: 40320)
#         }else { data <- slice(d, (40320*(j-1)):(40320*j))}
#         
#         if (length(data$Levelna) !=0 ){
#           if (all(is.na(data$Levelna)==T)){message(paste("Offline station for",t[j]))
#           }else{
#             
#             #          
#             png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/week/%s_%s_A%s_week_%s.png",station,sensor,k,j),width=1300, height=625)
#             mp1<- ggplot(data, aes(x = time, y= value)) +
#               geom_point(aes(y=Levelna,col=data$Col),size = 1) +
#               ylim  (ylow,yup)+
#               scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
#             #print(mp1)
#             mp2<- ggplot(data, aes(x = time, y= value)) +
#               geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
#               xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
#             
#             p3 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
#             graphics.off()
#           } #closing else of  if (all(is.na(data$Levelna)==T))
#         } #closing if (length(data$Levelna) !=0 )
#         rm(data)} #closing if (length(x$Levelna) !=0 )
#     } #closing for 1:length(levelna)
#     rm(d);rm(p);rm(p2);rm(p3)} # closing if of plots
#   
#   save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   
#   #####
#   
#   # B17 - # Only Spike Module, lunar months
#   #####
#   k=34
#   A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F,filtro=3) # Only Spike Module, lunar months
#   nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
#   nA <- length(nA)  
#   Tab1[i,k+1] <- nA
#   mslA <- mean(A$xQC$Level_clean,na.rm= T)
#   Tab2[i,k+1]<- msl
#   hwlA <- max(A$xQC$Level_clean, na.rm = T)
#   lwlA <- min(A$xQC$Level_clean, na.rm = T)
#   ampA <- abs(hwlA - lwlA)
#   Tab3[i,k+1] <- ampA
#   rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
#   #####
#   
#   A<- QCmodule(x$Level,x$Time,ALL = F,BP = T,ST = F,SC = F,OUT = F,SP = F,lm = T,lp = T, go= F,filtro=3) # Only Spike Module, lunar months
#   
#   bp_level<-A$xQC$Level_clean
# 
#   Df_flags <- data.frame(st_flag,out_cal_flag,sc_cal_flag,sp_flag)
#   flags = apply(Df_flags, 1, prod, na.rm=T)
#   Ballcal<-data.frame(time=x$Time,level=bp_level,flag=flags)
#   
#   Df_flags <- data.frame(st_flag,out_lun_flag,sc_lun_flag,sp_flag)
#   flags = apply(Df_flags, 1, prod, na.rm=T)
#   Balllun<-data.frame(time=x$Time,level=bp_level,flag=flags)
#   
#   Df_flags <- data.frame(st_flag,out_2cal_flag,sc_cal_flag,sp_flag)
#   flags = apply(Df_flags, 1, prod, na.rm=T)
#   Ballcal2<-data.frame(time=x$Time,level=bp_level,flag=flags)
#   
#   Df_flags <- data.frame(st_flag,out_2lun_flag,sc_lun_flag,sp_flag)
#   flags = apply(Df_flags, 1, prod, na.rm=T)
#   
#   Balllun2<-data.frame(time=x$Time,level=bp_level,flag=flags)
#   
#   
#   save(Ballcal,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_ballcal.Rda',station,sensor))
#   save(Ballcal2,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_ballcal2.Rda',station,sensor))
#   save(Balllun,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_balllun.Rda',station,sensor))
#   save(Balllun2,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/clean/%s_%s_balllun2.Rda',station,sensor))
#   
# }# closing initial for loop
# 
# save(Tab1,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/algm/Tab1.Rda')
# save(Tab2,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/algm/Tab2.Rda')
# save(Tab3,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/algm/Tab3.Rda')
# 
# ftime <- Sys.time() 
# print(ftime- stime)
# #6.582588 hours + 8 h
# #####

# NO PLOTS
# rm(list=ls()) 
# source('/Claudio/Documents/Oceans&Lakes/Thesis/Data/1year/x/functions3.R')
# #####
# setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/1year/') 
# filenames <- list.files(".", pattern="*.Rda", full.names=TRUE)
# # Creating Tables to compare different Algorithms
# #####
# fname<- NULL
# for (i in 1:length(filenames)){
#   #  i=1
#   load(filenames[i]) 
#   station <- as.character(x$Station[1]) 
#   sensor <- x$Sensor[1]
#   fname[i]<-paste(station,sensor,sep="_")
# }
# rm(x)
# NRows=length(filenames)
# 
# algm = paste ("A", 0:34, sep="")  #like this there will be no space
# algm[1]<- c("Raw")
# algm[2:35] = paste (rep(LETTERS[1:2],each =17), 1:17, sep="")
# NCols=length(algm) 
# 
# Tab1<-matrix(NA, NRows, NCols)
# 
# colnames(Tab1)<-algm
# rownames(Tab1)<-fname
# 
# Tab2<- Tab1
# Tab3<- Tab2
# #####
setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/1year/') 
stime <- Sys.time()
for(i in 22:length(filenames)){
  load(filenames[i]) 
  station <- as.character(x$Station[1]) 
  sensor <- x$Sensor[1]
  x <- mutate(x,Levelna = ifelse(Level ==-999, NA, Level))
  # Raw
  #####
  k=0
  na <- which(is.na(x$Levelna) == T) #Number of missing values in Raw dataset
  na <- length(na)
  nt <- length(x$Level) # total number of observations
  n <- nt - na # Real number of observations in the Raw Dataset
  Tab1[i,k+1]<- n
  
  msl <- mean(x$Levelna,na.rm= T)
  Tab2[i,k+1]<- msl
  hwl <- max(x$Levelna, na.rm = T)
  lwl <- min(x$Levelna, na.rm = T)
  amp <- abs(hwl - lwl)
  Tab3[i,k+1] <- amp
  #####
  
  # A1 - All modules and filters, calendar months
  #####
  k=1
  A <- QCmodule(x$Level,x$Time, ALL = T) # All modules and filters, calendar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
  
  #####
  
  # B1 - All modules and filters, lunar months
  #####
  k=18
  A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T) # All modules and filters, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A2 - All modules and filters (GO is off), calendar months
  #####
  k=2
  A <- QCmodule(x$Level,x$Time,ALL = T, go= F) # All modules and filters (but the GO in Outlier), calendar months
  
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  #B2 - All modules and filters (GO is off), lunar months
  #####
  k=19
  A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A3 - Without BreakPoint Module, calendar months
  #####
  k=3
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F, go= F) # Without BreakPoint Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA  
  bp_level <- A$XQC$Level_clean
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B3 - Without BreakPoint Module, lunar months
  #####
  k=20
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,lm = T,lp = T, go= F) # Without BreakPoint Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA  
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A4 - Without Stability Module, calendar months
  #####
  k=4
  A <- QCmodule(x$Level,x$Time,ALL = F,ST = F, go= F) # Without Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B4 - Without Stability Module, lunar months
  #####
  k=21
  A <- QCmodule(x$Level,x$Time,ALL = F,ST = F,lm = T,lp = T, go= F) # Without Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A5 - # Without OUT Module, calendar months
  #####
  k=5
  A  <- QCmodule(x$Level,x$Time,ALL = F,OUT = F,lm = T,lp = T, go= F) # Without OUT Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B5 - # Without OUT Module, lunar months
  #####
  k=22
  A  <- QCmodule(x$Level,x$Time,ALL = F,OUT = F,lm = T,lp = T, go= F) # Without OUT Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A6 - Without SC Module, calendar months
  #####
  k=6
  A  <- QCmodule(x$Level,x$Time,ALL = F,SC = F, go= F) # Without SC Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B6 - Without SC Module, lunar months
  #####
  k=23
  A  <- QCmodule(x$Level,x$Time,ALL = F,SC = F,lm = T,lp = T, go= F) # Without SC Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  #A7 - Without SP Module. calendar months
  #####
  k=7
  A <- QCmodule(x$Level,x$Time,ALL = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  #B7 - Without SP Module. Lunarmonths
  #####
  k=24
  A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
   rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A8 - Only Stability Module, calendar months
  #####
  k=8
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = T,SC = F,OUT = F,SP = F, go= F) # Only Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B8 - Only Stability Module, lunar months
  #####
  k=25
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = T,SC = F,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  st_level<-A$xQC$Level_clean
  st_flag<-A$xQC$Flag_final
  
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A9 - # Only Outlier Module (all filters), calendar months
  #####
  k=9
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  out_cal_level<-A$xQC$Level_clean
  out_cal_flag<-A$xQC$Flag_final
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B9 - # Only Outlier Module (all filters), lunar months
  #####
  k=26
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  out_lun_level<-A$xQC$Level_clean
  out_lun_flag<-A$xQC$Flag_final
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A10 - Only Speed of Change Module, calendar months
  #####
  k=10
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = T,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Speed of Change Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  sc_cal_level<-A$xQC$Level_clean
  sc_cal_flag<-A$xQC$Flag_final
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B10 - Only Speed of Change Module, lunar months
  #####
  k=27
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = T,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Speed of Change Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  sc_lun_level<-A$xQC$Level_clean
  sc_lun_flag<-A$xQC$Flag_final
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A11 - # Only Spike Module, calendar months
  #####
  k=11
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
   rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B11 - # Only Spike Module, lunar months
  #####
  k=28
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  sp_level<-A$xQC$Level_clean
  sp_flag<-A$xQC$Flag_final
  
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A12 - # Only Outlier Module with OR and OG, calendar months
  #####
  k=12
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  
  out_2cal_level<-A$xQC$Level_clean
  out_2cal_flag<-A$xQC$Flag_final
  
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B12 - # Only Outlier Module with OR and OG, lunar months
  #####
  k=29
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  out_2lun_level<-A$xQC$Level_clean
  out_2lun_flag<-A$xQC$Flag_final
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A13 - Only Outlier Module with OG filter, calendar months
  #####
  k=13
  A<- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F, go= F, or= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B13 - Only Outlier Module with OG filter, lunar months
  #####
  k=30
  A<- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F, or= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A14 - Only Outlier Module with OR filter, calendar months
  #####
  k=14
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,go= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B14 - Only Outlier Module with OR filter, lunar months
  #####
  k=31
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A15 - Only Outlier Module with GO filter, calendar months
  #####
  k=15
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,or= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B15 - Only Outlier Module with OR filter, lunar months
  #####
  k=32
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, or= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
   rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A16 - # Only Spike Module with Med Filter, lunar months
  #####
  k=16
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F,filtro=2) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
  #####
  
  # B16 - # Only Spike Module with Med Filter, lunar months
  #####
  k=33
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F,filtro=2) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A17 - # Only Spike Module with Spline Filter, lunar months
  #####
  k=17
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F,filtro=3) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
  #####
  
  # B17 - # Only Spike Module, lunar months
  #####
  k=34
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F,filtro=3) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  A<- QCmodule(x$Level,x$Time,ALL = F,BP = T,ST = F,SC = F,OUT = F,SP = F,lm = T,lp = T, go= F,filtro=3) # Only Spike Module, lunar months
  
  bp_level<-A$xQC$Level_clean
  
  Df_flags <- data.frame(st_flag,out_cal_flag,sc_cal_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  Ballcal<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  Df_flags <- data.frame(st_flag,out_lun_flag,sc_lun_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  Balllun<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  Df_flags <- data.frame(st_flag,out_2cal_flag,sc_cal_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  Ballcal2<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  Df_flags <- data.frame(st_flag,out_2lun_flag,sc_lun_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  
  Balllun2<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  
  save(Ballcal,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_ballcal.Rda',station,sensor))
  save(Ballcal2,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_ballcal2.Rda',station,sensor))
  save(Balllun,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_balllun.Rda',station,sensor))
  save(Balllun2,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/%s_%s_balllun2.Rda',station,sensor))
  
}# closing initial for loop

save(Tab1,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/Tab1.Rda')
save(Tab2,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/Tab2.Rda')
save(Tab3,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/new/files/Tab3.Rda')

ftime <- Sys.time() 
print(ftime- stime)
# i = 22, k=2 - time=7.7
#####

#### FULL
######
setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/full2/') 
filenames <- list.files(".", pattern="*.Rda", full.names=TRUE)
# Creating Tables to compare different Algorithms
#####
fname<- NULL
for (i in 1:length(filenames)){
  #  i=1
  load(filenames[i]) 
  station <- as.character(x$Station[1]) 
  sensor <- x$Sensor[1]
  fname[i]<-paste(station,sensor,sep="_")
}
rm(x)
NRows=length(filenames)

algm = paste ("A", 0:34, sep="")  #like this there will be no space
algm[1]<- c("Raw")
algm[2:35] = paste (rep(LETTERS[1:2],each =17), 1:17, sep="")
NCols=length(algm) 

Tab1<-matrix(NA, NRows, NCols)

colnames(Tab1)<-algm
rownames(Tab1)<-fname

Tab2<- Tab1
Tab3<- Tab2
#####
setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/full2/') 
stime2 <- Sys.time()
for(i in 1:length(filenames)){
  load(filenames[i]) 
  station <- as.character(x$Station[1]) 
  sensor <- x$Sensor[1]
  x <- mutate(x,Levelna = ifelse(Level ==-999, NA, Level))
  # Raw
  #####
  k=0
  na <- which(is.na(x$Levelna) == T) #Number of missing values in Raw dataset
  na <- length(na)
  nt <- length(x$Level) # total number of observations
  n <- nt - na # Real number of observations in the Raw Dataset
  Tab1[i,k+1]<- n
  
  msl <- mean(x$Levelna,na.rm= T)
  Tab2[i,k+1]<- msl
  hwl <- max(x$Levelna, na.rm = T)
  lwl <- min(x$Levelna, na.rm = T)
  amp <- abs(hwl - lwl)
  Tab3[i,k+1] <- amp
  #####
  
  # A1 - All modules and filters, calendar months
  #####
  k=1
  A <- QCmodule(x$Level,x$Time, ALL = T) # All modules and filters, calendar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
  
  #####
  
  # B1 - All modules and filters, lunar months
  #####
  k=18
  A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T) # All modules and filters, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A2 - All modules and filters (GO is off), calendar months
  #####
  k=2
  A <- QCmodule(x$Level,x$Time,ALL = T, go= F) # All modules and filters (but the GO in Outlier), calendar months
  
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  #B2 - All modules and filters (GO is off), lunar months
  #####
  k=19
  A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A3 - Without BreakPoint Module, calendar months
  #####
  k=3
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F, go= F) # Without BreakPoint Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA  
  bp_level <- A$XQC$Level_clean
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B3 - Without BreakPoint Module, lunar months
  #####
  k=20
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,lm = T,lp = T, go= F) # Without BreakPoint Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA  
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A4 - Without Stability Module, calendar months
  #####
  k=4
  A <- QCmodule(x$Level,x$Time,ALL = F,ST = F, go= F) # Without Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B4 - Without Stability Module, lunar months
  #####
  k=21
  A <- QCmodule(x$Level,x$Time,ALL = F,ST = F,lm = T,lp = T, go= F) # Without Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A5 - # Without OUT Module, calendar months
  #####
  k=5
  A  <- QCmodule(x$Level,x$Time,ALL = F,OUT = F,lm = T,lp = T, go= F) # Without OUT Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B5 - # Without OUT Module, lunar months
  #####
  k=22
  A  <- QCmodule(x$Level,x$Time,ALL = F,OUT = F,lm = T,lp = T, go= F) # Without OUT Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A6 - Without SC Module, calendar months
  #####
  k=6
  A  <- QCmodule(x$Level,x$Time,ALL = F,SC = F, go= F) # Without SC Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B6 - Without SC Module, lunar months
  #####
  k=23
  A  <- QCmodule(x$Level,x$Time,ALL = F,SC = F,lm = T,lp = T, go= F) # Without SC Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  #A7 - Without SP Module. calendar months
  #####
  k=7
  A <- QCmodule(x$Level,x$Time,ALL = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  #B7 - Without SP Module. Lunarmonths
  #####
  k=24
  A <- QCmodule(x$Level,x$Time,ALL = T,lm = T,lp = T, go= F) # All modules and filters (but the GO in Outlier), lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- mslA
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A8 - Only Stability Module, calendar months
  #####
  k=8
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = T,SC = F,OUT = F,SP = F, go= F) # Only Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B8 - Only Stability Module, lunar months
  #####
  k=25
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = T,SC = F,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Stability Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  st_level<-A$xQC$Level_clean
  st_flag<-A$xQC$Flag_final
  
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A9 - # Only Outlier Module (all filters), calendar months
  #####
  k=9
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  out_cal_level<-A$xQC$Level_clean
  out_cal_flag<-A$xQC$Flag_final
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B9 - # Only Outlier Module (all filters), lunar months
  #####
  k=26
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  out_lun_level<-A$xQC$Level_clean
  out_lun_flag<-A$xQC$Flag_final
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A10 - Only Speed of Change Module, calendar months
  #####
  k=10
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = T,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Speed of Change Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  sc_cal_level<-A$xQC$Level_clean
  sc_cal_flag<-A$xQC$Flag_final
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B10 - Only Speed of Change Module, lunar months
  #####
  k=27
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = T,OUT = F,SP = F,lm = T,lp = T, go= F) # Only Speed of Change Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  sc_lun_level<-A$xQC$Level_clean
  sc_lun_flag<-A$xQC$Flag_final
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A11 - # Only Spike Module, calendar months
  #####
  k=11
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B11 - # Only Spike Module, lunar months
  #####
  k=28
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  sp_level<-A$xQC$Level_clean
  sp_flag<-A$xQC$Flag_final
  
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A12 - # Only Outlier Module with OR and OG, calendar months
  #####
  k=12
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  
  out_2cal_level<-A$xQC$Level_clean
  out_2cal_flag<-A$xQC$Flag_final
  
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B12 - # Only Outlier Module with OR and OG, lunar months
  #####
  k=29
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  out_2lun_level<-A$xQC$Level_clean
  out_2lun_flag<-A$xQC$Flag_final
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A13 - Only Outlier Module with OG filter, calendar months
  #####
  k=13
  A<- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F, go= F, or= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B13 - Only Outlier Module with OG filter, lunar months
  #####
  k=30
  A<- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F, or= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A14 - Only Outlier Module with OR filter, calendar months
  #####
  k=14
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,go= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B14 - Only Outlier Module with OR filter, lunar months
  #####
  k=31
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, go= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A15 - Only Outlier Module with GO filter, calendar months
  #####
  k=15
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,or= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # B15 - Only Outlier Module with OR filter, lunar months
  #####
  k=32
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = T,SP = F,lm = T,lp = T, or= F, og= F) # Only Outlier Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A16 - # Only Spike Module with Med Filter, lunar months
  #####
  k=16
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F,filtro=2) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
  #####
  
  # B16 - # Only Spike Module with Med Filter, lunar months
  #####
  k=33
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F,filtro=2) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  # A17 - # Only Spike Module with Spline Filter, lunar months
  #####
  k=17
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T, go= F,filtro=3) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
  #####
  
  # B17 - # Only Spike Module, lunar months
  #####
  k=34
  A <- QCmodule(x$Level,x$Time,ALL = F,BP = F,ST = F,SC = F,OUT = F,SP = T,lm = T,lp = T, go= F,filtro=3) # Only Spike Module, lunar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  Tab1[i,k+1] <- nA
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  Tab2[i,k+1]<- msl
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  Tab3[i,k+1] <- ampA
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  #####
  
  A<- QCmodule(x$Level,x$Time,ALL = F,BP = T,ST = F,SC = F,OUT = F,SP = F,lm = T,lp = T, go= F,filtro=3) # Only Spike Module, lunar months
  
  bp_level<-A$xQC$Level_clean
  
  Df_flags <- data.frame(st_flag,out_cal_flag,sc_cal_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  Ballcal<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  Df_flags <- data.frame(st_flag,out_lun_flag,sc_lun_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  Balllun<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  Df_flags <- data.frame(st_flag,out_2cal_flag,sc_cal_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  Ballcal2<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  Df_flags <- data.frame(st_flag,out_2lun_flag,sc_lun_flag,sp_flag)
  flags = apply(Df_flags, 1, prod, na.rm=T)
  
  Balllun2<-data.frame(time=x$Time,level=bp_level,flag=flags)
  
  
  save(Ballcal,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_ballcal.Rda',station,sensor))
  save(Ballcal2,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_ballcal2.Rda',station,sensor))
  save(Balllun,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_balllun.Rda',station,sensor))
  save(Balllun2,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/%s_%s_balllun2.Rda',station,sensor))
  
}# closing initial for loop

save(Tab1,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/Tab1.Rda')
save(Tab2,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/Tab2.Rda')
save(Tab3,file='/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/files/Tab3.Rda')

ftime2 <- Sys.time() 
print(ftime2- stime2)
# stopped at i=1,k=15. time=1.4h


####### Only A1 
rm(list=ls())
source('/Claudio/Documents/Oceans&Lakes/Thesis/Data/1year/x/functions3.R')

setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/full2/') 
filenames <- list.files(".", pattern="*.Rda", full.names=TRUE)
stime2 <- Sys.time()
for(i in 1:length(filenames)){
  load(filenames[i]) 
  station <- as.character(x$Station[1]) 
  sensor <- x$Sensor[1]
  x <- mutate(x,Levelna = ifelse(Level ==-999, NA, Level))
  # Raw
  #####
  k=0
  na <- which(is.na(x$Levelna) == T) #Number of missing values in Raw dataset
  na <- length(na)
  nt <- length(x$Level) # total number of observations
  n <- nt - na # Real number of observations in the Raw Dataset

  msl <- mean(x$Levelna,na.rm= T)
  hwl <- max(x$Levelna, na.rm = T)
  lwl <- min(x$Levelna, na.rm = T)
  amp <- abs(hwl - lwl)
  #####
  
  # A1 - All modules and filters, calendar months
  #####
  k=1
  A <- QCmodule(x$Level,x$Time, ALL = T) # All modules and filters, calendar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)

  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/week/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/newf/week/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/data/tide/%s_%s_A%s.Rda',station,sensor,k))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
}
ftime<-Sys.time()
ftime-stime2
# Time difference of 2.956337 hours
#####

# Data of 2018
rm(list=ls())
source('/Claudio/Documents/Oceans&Lakes/Thesis/Data/1year/x/functions3.R')

setwd('/Claudio/Documents/Oceans&Lakes/Thesis/Data/2018/') 
filenames <- list.files(".", pattern="*.Rda", full.names=TRUE)
stime2 <- Sys.time()
for(i in 1:length(filenames)){
  load(filenames[i]) 
  station <- as.character(x$Station[1]) 
  sensor <- x$Sensor[1]
  x <- mutate(x,Levelna = ifelse(Level ==-999, NA, Level))
  # Raw
  #####
  k=0
  na <- which(is.na(x$Levelna) == T) #Number of missing values in Raw dataset
  na <- length(na)
  nt <- length(x$Level) # total number of observations
  n <- nt - na # Real number of observations in the Raw Dataset
  
  msl <- mean(x$Levelna,na.rm= T)
  hwl <- max(x$Levelna, na.rm = T)
  lwl <- min(x$Levelna, na.rm = T)
  amp <- abs(hwl - lwl)
  #####
  
  # A1 - All modules and filters, calendar months
  #####
  k=1
  A <- QCmodule(x$Level,x$Time, ALL = T) # All modules and filters, calendar months
  nA <- which(is.na(A$xQC$Level_clean) == T) #Number of missing values in Raw dataset
  nA <- length(nA)  
  mslA <- mean(A$xQC$Level_clean,na.rm= T)
  hwlA <- max(A$xQC$Level_clean, na.rm = T)
  lwlA <- min(A$xQC$Level_clean, na.rm = T)
  ampA <- abs(hwlA - lwlA)
  
  # PLOTS
  if (any(A$xQC$Level_clean != 0, na.rm = T)){
    
    ylow <- min(lwl,lwlA)
    yup <-max(hwl,hwlA)
    d <- data.frame(Levelna = x$Levelna,level = A$xQC$Level_clean,time = A$xQC$Time, Flag = A$xQC$Flag_final)
    d <- mutate(d, Col = ifelse( Flag == 0, "out", "kept"))
    d$Col <- factor(d$Col, levels = c("kept", "out"))
    
    
    # Year Plot (Grey & Green)
    p<- ggplot(x, aes(x = Time, y= value)) +
      geom_point(aes(y=x$Levelna),size=0.5, color = "grey") +
      ylim  (ylow,yup)+
      geom_point(aes(y = A$xQC$Level_clean),size=0.5,color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Raw X Clean")
    ggsave(sprintf("%s_%s_A%s.png",station,sensor,k), plot = p, device = "png", 
           path = "/Claudio/Documents/Oceans&Lakes/Thesis/Data/2018/fig/")
    
    # Year plot - Black & Red - Flags
    #          
    png(file=sprintf("/Claudio/Documents/Oceans&Lakes/Thesis/Data/2018/fig/%s_%s_A%s_flag.png",station,sensor,k),width=1300, height=625)
    mp1<- ggplot(d, aes(x = time, y= value)) +
      geom_point(aes(y=Levelna,col=d$Col),size = 0.5) +
      ylim  (ylow,yup)+
      scale_colour_manual("legend", values = c( "olivedrab", "firebrick")) +
      xlab("Time ") + ylab("Level (m)") + labs(title="Quality Control")
    #print(mp1)
    mp2<- ggplot(d, aes(x = time, y= value)) +
      geom_line(aes(y=level),size=0.5, color = "aquamarine4") +
      xlab("Time ") + ylab("Level (m)") + labs(title="Clean")
    
    p2 <- grid.arrange(mp1,mp2,ncol=1,nrow=2)
    graphics.off()
    
    rm(d);rm(p);rm(p2)} # closing if of plots
  
  
  save(A,file=sprintf('/Claudio/Documents/Oceans&Lakes/Thesis/Data/2018/clean/%s_%s_18.Rda',station,sensor))
  rm(A);rm(mslA);rm(ampA);rm(hwlA);rm(lwlA)
  
}
ftime<-Sys.time()
ftime-stime2
