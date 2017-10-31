outlier_sds = 3

setwd("~/Documents/GitHub/Buttons_Task/")

#setwd("C:/Users/Prasannajeet/Desktop/Buttons_Task");


library("lsr")
library("ggplot2")
library("pwr")


IN.FILES=  list.files(c("Individual"), pattern="*.csv")
DU.FILES= list.files(c("Dual1"), pattern="*.csv")
DI.FILES= list.files(c("Dual2"), pattern="*.csv")

summary.vesper<- data.frame(PP=c(rep(as.numeric(""),length(IN.FILES))),
                          IN.M=c(rep(as.numeric(""),length(IN.FILES))),
                          IN.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          IN.C.M=c(rep(as.numeric(""),length(IN.FILES))),
                          IN.C.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          IN.I.M=c(rep(as.numeric(""),length(IN.FILES))),
                          IN.I.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.C.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.C.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.I.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.I.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.C.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.C.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.I.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.I.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.ASY.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.ASY.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.ASY.C.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.ASY.C.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.ASY.I.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DU.ASY.I.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.ASY.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.ASY.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.ASY.C.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.ASY.C.SD=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.ASY.I.M=c(rep(as.numeric(""),length(IN.FILES))),
                          DI.ASY.I.SD=c(rep(as.numeric(""),length(IN.FILES))),
                                                    
                          DIFF.IN.DU.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DI.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.DU.DI.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DU.SD = c(rep(as.numeric(""),length(IN.FILES))),  
                          DIFF.IN.DI.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.DU.DI.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DU.C.M = c(rep(as.numeric(""),length(IN.FILES))),                          
                          DIFF.IN.DI.C.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.DU.DI.C.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DU.C.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DI.C.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.DU.DI.C.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DU.I.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DI.I.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.DU.DI.I.M = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DU.I.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.IN.DI.I.SD = c(rep(as.numeric(""),length(IN.FILES))),
                          DIFF.DU.DI.I.SD = c(rep(as.numeric(""),length(IN.FILES))))

IN.FILES=  list.files(c("Individual"), pattern="*.csv")
DU.FILES= list.files(c("Dual1"), pattern="*.csv")
DI.FILES= list.files(c("Dual2"), pattern="*.csv")


for (i in 1: length(IN.FILES)){

  pp<-as.character(IN.FILES[i])
  summary.vesper$PP[i]= gsub(pattern = "\\.csv$", "", pp)
 
 ind.data<- read.csv(paste("Individual/",IN.FILES[i],sep = ""))
 ind.data<- ind.data[(ind.data$Running.SubTrial.=="b1trails1" | ind.data$Running.SubTrial. == "List2") & ind.data$Subject1ACC==1,]
 # ind.data$Subject1RT<- as.integer(ind.data$Subject1RT)
 
 '
 foo = ind.data$Subject1RT
 bar = as.numeric(foo)
 ind.data$Subject1RT = bar
 '
 
ind.data$Subject1RT<- as.numeric(as.character(ind.data$Subject1RT))
 
 
 ind.data$Type<- ifelse((ind.data$Running.SubTrial.=="b1trails1" & ind.data$Screen1_Obj_Colour=="blue" & ind.data$Screen_1_Obj_Pos== "e") | 
                        (ind.data$Running.SubTrial.=="b1trails1" &  ind.data$Screen1_Obj_Colour=="red" & ind.data$Screen_1_Obj_Pos== "w") |
                        (ind.data$Running.SubTrial.=="List2" & ind.data$Screen1_Obj_Colour=="blue" & ind.data$Screen_1_Obj_Pos== "w") | 
                        (ind.data$Running.SubTrial.=="List2" &  ind.data$Screen1_Obj_Colour=="red" & ind.data$Screen_1_Obj_Pos== "e"), "C","I")
 m<-mean(ind.data$Subject1RT)
 s<-sd(na.omit(ind.data$Subject1RT))
 summary.vesper$IN.M[i] <- mean(ind.data$Subject1RT[ind.data$Subject1RT> m-outlier_sds*s &
                                                    ind.data$Subject1RT< m+outlier_sds*s])
 summary.vesper$IN.SD[i] <- sd(na.omit(ind.data$Subject1RT[ind.data$Subject1RT> m-outlier_sds*s &
                                                           ind.data$Subject1RT< m+outlier_sds*s]))
 summary.vesper$IN.C.M[i]<- mean(na.omit(ind.data$Subject1RT[ind.data$Type=="C" & 
                                                             ind.data$Subject1RT> m-outlier_sds*s &
                                                             ind.data$Subject1RT< m+outlier_sds*s]))
 summary.vesper$IN.C.SD[i]<- sd(na.omit(ind.data$Subject1RT[ind.data$Type=="C" &
                                                            ind.data$Subject1RT> m-outlier_sds*s &
                                                            ind.data$Subject1RT< m+outlier_sds*s]))
 summary.vesper$IN.I.M[i]<- mean(na.omit(ind.data$Subject1RT[ind.data$Type=="I" &
                                                             ind.data$Subject1RT> m-outlier_sds*s &
                                                             ind.data$Subject1RT< m+outlier_sds*s]))
 summary.vesper$IN.I.SD[i]<- sd(na.omit(ind.data$Subject1RT[ind.data$Type=="I" &
                                                            ind.data$Subject1RT> m-outlier_sds*s &
                                                            ind.data$Subject1RT< m+outlier_sds*s]))
 
}


for (i in 1:length(DU.FILES)){

dualu.data<- read.csv(paste("Dual1/",DU.FILES[i],sep = ""))

name<- gsub("[^[:digit:]]", "", DU.FILES[i])


x<-as.numeric(match(substr(name,1,3), summary.vesper$PP))
y<-as.numeric(match(substr(name,4,6), summary.vesper$PP))

dualu.data<- dualu.data[(dualu.data$Running.SubTrial.=="CM1Trails" | dualu.data$Running.SubTrial. == "CM2Trials") & dualu.data$Subject1ACC==1 & dualu.data$Subject2ACC==1,]


dualu.data$Subject1RT<- as.numeric(as.character(dualu.data$Subject1RT))
dualu.data$Subject2RT<- as.numeric(as.character(dualu.data$Subject2RT))

dualu.data$Type1<- ifelse((dualu.data$Running.SubTrial.=="CM1Trails" & dualu.data$Screen1_Obj_Colour=="blue" & dualu.data$Screen_1_Obj_Pos== "e") | 
                         (dualu.data$Running.SubTrial.=="CM1Trails" &  dualu.data$Screen1_Obj_Colour=="red" & dualu.data$Screen_1_Obj_Pos== "w") |
                         (dualu.data$Running.SubTrial.=="CM2Trials" & dualu.data$Screen1_Obj_Colour=="blue" & dualu.data$Screen_1_Obj_Pos== "w") | 
                         (dualu.data$Running.SubTrial.=="CM2Trials" &  dualu.data$Screen1_Obj_Colour=="red" & dualu.data$Screen_1_Obj_Pos== "e"), "C","I")

dualu.data$Type2<- ifelse((dualu.data$Running.SubTrial.=="CM1Trails" & dualu.data$Screen2_Obj_Colour=="blue" & dualu.data$Screen_2_Obj_Pos== "e") | 
                            (dualu.data$Running.SubTrial.=="CM1Trails" &  dualu.data$Screen2_Obj_Colour=="red" & dualu.data$Screen_2_Obj_Pos== "w") |
                            (dualu.data$Running.SubTrial.=="CM2Trials" & dualu.data$Screen2_Obj_Colour=="blue" & dualu.data$Screen_2_Obj_Pos== "w") | 
                            (dualu.data$Running.SubTrial.=="CM2Trials" &  dualu.data$Screen2_Obj_Colour=="red" & dualu.data$Screen_2_Obj_Pos== "e"), "C","I")

m<-mean(dualu.data$Subject1RT)
s<-sd(na.omit(dualu.data$Subject1RT))
m1<-mean(dualu.data$Subject2RT)
s1<-sd(na.omit(dualu.data$Subject2RT))
   
   summary.vesper$DU.M[x] <- mean(dualu.data$Subject1RT[dualu.data$Subject1RT> m-outlier_sds*s &
                                                        dualu.data$Subject1RT< m+outlier_sds*s])
   summary.vesper$DU.SD[x] <- sd(na.omit(dualu.data$Subject1RT[dualu.data$Subject1RT> m-outlier_sds*s &
                                                               dualu.data$Subject1RT< m+outlier_sds*s]))
   summary.vesper$DU.M[y] <- mean(dualu.data$Subject2RT[dualu.data$Subject2RT> m1-outlier_sds*s1 &
                                                        dualu.data$Subject2RT< m1+outlier_sds*s1])
   summary.vesper$DU.SD[y] <- sd(na.omit(dualu.data$Subject2RT[dualu.data$Subject2RT> m1-outlier_sds*s1 &
                                                               dualu.data$Subject2RT< m1+outlier_sds*s1]))
   summary.vesper$DU.C.M[x]<- mean(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="C" &
                                                                   dualu.data$Subject1RT> m-outlier_sds*s &
                                                                   dualu.data$Subject1RT< m+outlier_sds*s]))
   summary.vesper$DU.C.SD[x]<- sd(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="C" &
                                                                  dualu.data$Subject1RT> m-outlier_sds*s &
                                                                  dualu.data$Subject1RT< m+outlier_sds*s]))
   summary.vesper$DU.C.M[y]<- mean(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="C" &
                                                                 dualu.data$Subject2RT> m1-outlier_sds*s1 &
                                                                 dualu.data$Subject2RT< m1+outlier_sds*s1]))
   summary.vesper$DU.C.SD[y]<- sd(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="C" &
                                                                dualu.data$Subject2RT> m1-outlier_sds*s1 &
                                                                dualu.data$Subject2RT< m1+outlier_sds*s1]))
   summary.vesper$DU.I.M[x]<- mean(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="I" &
                                                                   dualu.data$Subject1RT> m-outlier_sds*s &
                                                                   dualu.data$Subject1RT< m+outlier_sds*s]))
   summary.vesper$DU.I.SD[x]<- sd(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="I" &
                                                                  dualu.data$Subject1RT> m-outlier_sds*s &
                                                                  dualu.data$Subject1RT< m+outlier_sds*s]))
   summary.vesper$DU.I.M[y]<- mean(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="I" &
                                                                 dualu.data$Subject2RT> m1-outlier_sds*s1 &
                                                                 dualu.data$Subject2RT< m1+outlier_sds*s1]))
   summary.vesper$DU.I.SD[y]<- sd(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="I" &
                                                                dualu.data$Subject2RT> m1-outlier_sds*s1 &
                                                                dualu.data$Subject2RT< m1+outlier_sds*s1]))
 
}


for (i in 1:length(DI.FILES)){
  
  duali.data<- read.csv(paste("Dual2/",DI.FILES[i],sep = ""))
  
  name<- gsub("[^[:digit:]]", "", DI.FILES[i])
  
  
  x<-as.numeric(match(substr(name,1,3), summary.vesper$PP))
  y<-as.numeric(match(substr(name,4,6), summary.vesper$PP))
  
  duali.data<- duali.data[(duali.data$Running.SubTrial.=="CM1Trails" | duali.data$Running.SubTrial. == "CM2Trials") & duali.data$Subject1ACC==1 & duali.data$Subject2ACC==1,]
  duali.data$Subject1RT<- as.numeric(as.character(duali.data$Subject1RT))
  duali.data$Subject2RT<- as.numeric(as.character(duali.data$Subject2RT))
  
  duali.data$Type1<- ifelse((duali.data$Running.SubTrial.=="CM1Trails" & duali.data$Screen1_Obj_Colour=="blue" & duali.data$Screen_1_Obj_Pos== "e") | 
                              (duali.data$Running.SubTrial.=="CM1Trails" &  duali.data$Screen1_Obj_Colour=="red" & duali.data$Screen_1_Obj_Pos== "w") |
                              (duali.data$Running.SubTrial.=="CM2Trials" & duali.data$Screen1_Obj_Colour=="blue" & duali.data$Screen_1_Obj_Pos== "w") | 
                              (duali.data$Running.SubTrial.=="CM2Trials" &  duali.data$Screen1_Obj_Colour=="red" & duali.data$Screen_1_Obj_Pos== "e"), "C","I")
  
  duali.data$Type2<- ifelse((duali.data$Running.SubTrial.=="CM1Trails" & duali.data$Screen2_Obj_Colour=="blue" & duali.data$Screen_2_Obj_Pos== "e") | 
                              (duali.data$Running.SubTrial.=="CM1Trails" &  duali.data$Screen2_Obj_Colour=="red" & duali.data$Screen_2_Obj_Pos== "w") |
                              (duali.data$Running.SubTrial.=="CM2Trials" & duali.data$Screen2_Obj_Colour=="blue" & duali.data$Screen_2_Obj_Pos== "w") | 
                              (duali.data$Running.SubTrial.=="CM2Trials" &  duali.data$Screen2_Obj_Colour=="red" & duali.data$Screen_2_Obj_Pos== "e"), "C","I")
  
  m<-mean(duali.data$Subject1RT)
  s<-sd(na.omit(duali.data$Subject1RT))
  m1<-mean(duali.data$Subject2RT)
  s1<-sd(na.omit(duali.data$Subject2RT))
  
  
  summary.vesper$DI.M[x] <- mean(duali.data$Subject1RT[duali.data$Subject1RT> m-outlier_sds*s &
                                                         duali.data$Subject1RT< m+outlier_sds*s])
  summary.vesper$DI.SD[x] <- sd(na.omit(duali.data$Subject1RT[duali.data$Subject1RT> m-outlier_sds*s &
                                                                duali.data$Subject1RT< m+outlier_sds*s]))
  summary.vesper$DI.M[y] <- mean(duali.data$Subject2RT[duali.data$Subject2RT> m1-outlier_sds*s1 &
                                                         duali.data$Subject2RT< m1+outlier_sds*s1])
  summary.vesper$DI.SD[y] <- sd(na.omit(duali.data$Subject2RT[duali.data$Subject2RT> m1-outlier_sds*s1 &
                                                                duali.data$Subject2RT< m1+outlier_sds*s1]))
  summary.vesper$DI.C.M[x]<- mean(na.omit(duali.data$Subject1RT[duali.data$Type1=="C" &
                                                                  duali.data$Subject1RT> m-outlier_sds*s &
                                                                  duali.data$Subject1RT< m+outlier_sds*s]))
  summary.vesper$DI.C.SD[x]<- sd(na.omit(duali.data$Subject1RT[duali.data$Type1=="C" &
                                                                 duali.data$Subject1RT> m-outlier_sds*s &
                                                                 duali.data$Subject1RT< m+outlier_sds*s]))
  summary.vesper$DI.C.M[y]<- mean(na.omit(duali.data$Subject2RT[duali.data$Type2=="C" &
                                                                  duali.data$Subject2RT> m1-outlier_sds*s1 &
                                                                  duali.data$Subject2RT< m1+outlier_sds*s1]))
  summary.vesper$DI.C.SD[y]<- sd(na.omit(duali.data$Subject2RT[duali.data$Type2=="C" &
                                                                 duali.data$Subject2RT> m1-outlier_sds*s1 &
                                                                 duali.data$Subject2RT< m1+outlier_sds*s1]))
  summary.vesper$DI.I.M[x]<- mean(na.omit(duali.data$Subject1RT[duali.data$Type1=="I" &
                                                                  duali.data$Subject1RT> m-outlier_sds*s &
                                                                  duali.data$Subject1RT< m+outlier_sds*s]))
  summary.vesper$DI.I.SD[x]<- sd(na.omit(duali.data$Subject1RT[duali.data$Type1=="I" &
                                                                 duali.data$Subject1RT> m-outlier_sds*s &
                                                                 duali.data$Subject1RT< m+outlier_sds*s]))
  summary.vesper$DI.I.M[y]<- mean(na.omit(duali.data$Subject2RT[duali.data$Type2=="I" &
                                                                  duali.data$Subject2RT> m1-outlier_sds*s1 &
                                                                  duali.data$Subject2RT< m1+outlier_sds*s1]))
  summary.vesper$DI.I.SD[y]<- sd(na.omit(duali.data$Subject2RT[duali.data$Type2=="I" &
                                                                 duali.data$Subject2RT> m1-outlier_sds*s1 &
                                                                 duali.data$Subject2RT< m1+outlier_sds*s1]))
  
  summary.vesper$DIFF.DU.DI.M[x]= summary.vesper$DU.M[x] - summary.vesper$DI.M[x]
  summary.vesper$DIFF.DU.DI.SD[x]= summary.vesper$DU.SD[x] - summary.vesper$DI.SD[x]
  summary.vesper$DIFF.DU.DI.M[y]= summary.vesper$DU.M[y] - summary.vesper$DI.M[y]
  summary.vesper$DIFF.DU.DI.SD[y]= summary.vesper$DU.SD[y] - summary.vesper$DI.SD[y]
  summary.vesper$DIFF.DU.DI.C.M[x]= summary.vesper$DU.C.M[x] - summary.vesper$DI.C.M[x]
  summary.vesper$DIFF.DU.DI.C.SD[x]= summary.vesper$DU.C.SD[x] - summary.vesper$DI.C.SD[x]
  summary.vesper$DIFF.DU.DI.C.M[y]= summary.vesper$DU.C.M[y] - summary.vesper$DI.C.M[y]
  summary.vesper$DIFF.DU.DI.C.SD[y]= summary.vesper$DU.C.SD[y] - summary.vesper$DI.C.SD[y]
  summary.vesper$DIFF.DU.DI.I.M[x]= summary.vesper$DU.I.M[x] - summary.vesper$DI.I.M[x]
  summary.vesper$DIFF.DU.DI.I.SD[x]= summary.vesper$DU.I.SD[x] - summary.vesper$DI.I.SD[x]
  summary.vesper$DIFF.DU.DI.I.M[y]= summary.vesper$DU.I.M[y] - summary.vesper$DI.I.M[y]
  summary.vesper$DIFF.DU.DI.I.SD[y]= summary.vesper$DU.I.SD[y] - summary.vesper$DI.I.SD[y]
  
  summary.vesper$DIFF.IN.DU.M[x]= summary.vesper$IN.M[x] - summary.vesper$DU.M[x]
  summary.vesper$DIFF.IN.DU.SD[x]= summary.vesper$IN.SD[x] - summary.vesper$DU.SD[x]
    summary.vesper$DIFF.IN.DI.M[x]= summary.vesper$IN.M[x] - summary.vesper$DI.M[x]
  summary.vesper$DIFF.IN.DI.SD[x]= summary.vesper$IN.SD[x] - summary.vesper$DI.SD[x]
  
  summary.vesper$DIFF.IN.DU.M[y]= summary.vesper$IN.M[y] - summary.vesper$DU.M[y]
  summary.vesper$DIFF.IN.DU.SD[y]= summary.vesper$IN.SD[y] - summary.vesper$DU.SD[y]
  
  summary.vesper$DIFF.IN.DI.M[y]= summary.vesper$IN.M[y] - summary.vesper$DI.M[y]
  summary.vesper$DIFF.IN.DI.SD[y]= summary.vesper$IN.SD[y] - summary.vesper$DI.SD[y]
  
  summary.vesper$DIFF.IN.DU.C.M[x]= summary.vesper$IN.C.M[x] - summary.vesper$DU.C.M[x]
  summary.vesper$DIFF.IN.DU.C.SD[x]= summary.vesper$IN.C.SD[x] - summary.vesper$DU.C.SD[x]
  summary.vesper$DIFF.IN.DI.C.M[x]= summary.vesper$IN.C.M[x] - summary.vesper$DI.C.M[x]
  summary.vesper$DIFF.IN.DI.C.SD[x]= summary.vesper$IN.C.SD[x] - summary.vesper$DI.C.SD[x]
  summary.vesper$DIFF.IN.DI.C.M[y]= summary.vesper$IN.C.M[y] - summary.vesper$DI.C.M[y]
  summary.vesper$DIFF.IN.DI.C.SD[y]= summary.vesper$IN.C.SD[y] - summary.vesper$DI.C.SD[y]
  summary.vesper$DIFF.IN.DU.C.M[y]= summary.vesper$IN.C.M[y] - summary.vesper$DU.C.M[y]
  summary.vesper$DIFF.IN.DU.C.SD[y]= summary.vesper$IN.C.SD[y] - summary.vesper$DU.C.SD[y]
  
  summary.vesper$DIFF.IN.DI.C.M[y]= summary.vesper$IN.C.M[y] - summary.vesper$DI.C.M[y]
  summary.vesper$DIFF.IN.DI.C.SD[y]= summary.vesper$IN.C.SD[y] - summary.vesper$DI.C.SD[y]
  summary.vesper$DIFF.IN.DU.I.M[x]= summary.vesper$IN.I.M[x] - summary.vesper$DU.I.M[x]
  summary.vesper$DIFF.IN.DU.I.SD[x]= summary.vesper$IN.I.SD[x] - summary.vesper$DU.I.SD[x]
  summary.vesper$DIFF.IN.DI.I.M[x]= summary.vesper$IN.I.M[x] - summary.vesper$DI.I.M[x]
  summary.vesper$DIFF.IN.DI.I.SD[x]= summary.vesper$IN.I.SD[x] - summary.vesper$DI.I.SD[x]
  
  summary.vesper$DIFF.IN.DU.I.M[y]= summary.vesper$IN.I.M[y] - summary.vesper$DU.I.M[y]
  summary.vesper$DIFF.IN.DU.I.SD[y]= summary.vesper$IN.I.SD[y] - summary.vesper$DU.I.SD[y]
  
  
  summary.vesper$DIFF.IN.DI.I.M[y]= summary.vesper$IN.I.M[y] - summary.vesper$DI.I.M[y]
  summary.vesper$DIFF.IN.DI.I.SD[y]= summary.vesper$IN.I.SD[y] - summary.vesper$DI.I.SD[y]
  
  
  summary.vesper$DU.ASY.M[x]= summary.vesper$DU.M[x] - summary.vesper$DU.M[y]
  summary.vesper$DU.ASY.SD[y]= summary.vesper$DU.SD[y] - summary.vesper$DU.SD[x]
  
  summary.vesper$DU.ASY.C.M[x]= summary.vesper$DU.C.M[x] - summary.vesper$DU.C.M[y]
  summary.vesper$DU.ASY.C.M[y]= summary.vesper$DU.C.M[y] - summary.vesper$DU.C.M[x]
  
  summary.vesper$DU.ASY.C.SD[x]= summary.vesper$DU.C.SD[x] - summary.vesper$DU.C.SD[y]
  summary.vesper$DU.ASY.C.SD[y]= summary.vesper$DU.C.SD[y] - summary.vesper$DU.C.SD[x]
  
  summary.vesper$DU.ASY.I.M[x]= summary.vesper$DU.I.M[x] - summary.vesper$DU.I.M[y]
  summary.vesper$DU.ASY.I.SD[y]= summary.vesper$DU.I.SD[y] - summary.vesper$DU.I.SD[x]
  
  
  summary.vesper$DI.ASY.M[x]= summary.vesper$DI.M[x] - summary.vesper$DI.M[y]
  summary.vesper$DI.ASY.SD[y]= summary.vesper$DI.SD[y] - summary.vesper$DI.SD[x]
  
  summary.vesper$DI.ASY.C.M[x]= summary.vesper$DI.C.M[x] - summary.vesper$DI.C.M[y]
  summary.vesper$DI.ASY.C.M[y]= summary.vesper$DI.C.M[y] - summary.vesper$DI.C.M[x]
  
  summary.vesper$DI.ASY.C.SD[x]= summary.vesper$DI.C.SD[x] - summary.vesper$DI.C.SD[y]
  summary.vesper$DI.ASY.C.SD[y]= summary.vesper$DI.C.SD[y] - summary.vesper$DI.C.SD[x]
  
  summary.vesper$DI.ASY.I.M[x]= summary.vesper$DI.I.M[x] - summary.vesper$DI.I.M[y]
  summary.vesper$DI.ASY.I.SD[y]= summary.vesper$DI.I.SD[y] - summary.vesper$DI.I.SD[x]
  
  
  
  
  summary.vesper$DU.ASY.M[y]= summary.vesper$DU.M[y] - summary.vesper$DU.M[x]
  summary.vesper$DU.ASY.SD[x]= summary.vesper$DU.SD[x] - summary.vesper$DU.SD[y]
  
  summary.vesper$DU.ASY.C.M[y]= summary.vesper$DU.C.M[y] - summary.vesper$DU.C.M[x]
  summary.vesper$DU.ASY.C.M[x]= summary.vesper$DU.C.M[x] - summary.vesper$DU.C.M[y]
  
  summary.vesper$DU.ASY.C.SD[y]= summary.vesper$DU.C.SD[y] - summary.vesper$DU.C.SD[x]
  summary.vesper$DU.ASY.C.SD[x]= summary.vesper$DU.C.SD[x] - summary.vesper$DU.C.SD[y]
  
  summary.vesper$DU.ASY.I.M[y]= summary.vesper$DU.I.M[y] - summary.vesper$DU.I.M[x]
  summary.vesper$DU.ASY.I.SD[x]= summary.vesper$DU.I.SD[x] - summary.vesper$DU.I.SD[y]
  
  
  summary.vesper$DI.ASY.M[y]= summary.vesper$DI.M[y] - summary.vesper$DI.M[x]
  summary.vesper$DI.ASY.SD[x]= summary.vesper$DI.SD[x] - summary.vesper$DI.SD[y]
  
  summary.vesper$DI.ASY.C.M[y]= summary.vesper$DI.C.M[y] - summary.vesper$DI.C.M[x]
  summary.vesper$DI.ASY.C.M[x]= summary.vesper$DI.C.M[x] - summary.vesper$DI.C.M[y]
  
  summary.vesper$DI.ASY.C.SD[y]= summary.vesper$DI.C.SD[y] - summary.vesper$DI.C.SD[x]
  summary.vesper$DI.ASY.C.SD[x]= summary.vesper$DI.C.SD[x] - summary.vesper$DI.C.SD[y]
  
  summary.vesper$DI.ASY.I.M[y]= summary.vesper$DI.I.M[y] - summary.vesper$DI.I.M[x]
  summary.vesper$DI.ASY.I.SD[x]= summary.vesper$DI.I.SD[x] - summary.vesper$DI.I.SD[y]
  
  
  

  
}



t.test(summary.vesper$IN.C.M,summary.vesper$IN.I.M,paired=T)
t.test(summary.vesper$DU.C.M,summary.vesper$DU.I.M,paired=T)
t.test(summary.vesper$DI.C.M,summary.vesper$DI.I.M,paired=T)
t.test(summary.vesper$IN.SD,summary.vesper$DU.SD,paired=T)
t.test(summary.vesper$IN.SD,summary.vesper$DI.SD,paired=T)

cor.test(summary.vesper$DU.M, summary.vesper$DU.SD, method = c("pearson"))
plot(summary.vesper$DU.C.M,summary.vesper$DU.C.SD)
abline(lm(summary.vesper$DU.C.SD~summary.vesper$DU.C.M))


cor.test(summary.vesper$DU.SD, summary.vesper$DU.ASY.M, method = c("pearson"))
plot(summary.vesper$DU.ASY.M,summary.vesper$DU.SD)
abline(lm(summary.vesper$DU.SD~summary.vesper$DU.ASY.M))


cor.test(summary.vesper$DI.M, summary.vesper$DI.SD, method = c("pearson"))
plot(summary.vesper$DI.M,summary.vesper$DI.SD)
abline(lm(summary.vesper$DI.SD~summary.vesper$DI.M))

cor.test(summary.vesper$DU.C.M, summary.vesper$DI.C.SD, method = c("pearson"))
cor.test(summary.vesper$DU.I.M, summary.vesper$DI.I.SD, method = c("pearson"))
cor.test(summary.vesper$DU.C.M, summary.vesper$DI.C.SD, method = c("pearson"))



cor.test(summary.vesper$IN.C.M, summary.vesper$DI.C.M, method = c("pearson"))
                                      

DF.DU<- summary.vesper[, c(8,9,20)]
pcor(DF.DU, method="pearson")

DF.DI<- summary.vesper[, c(14,15,26)]
pcor(DF.DI, method="pearson")
