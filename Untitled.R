setwd("~/Documents/GitHub/Buttons_Task/")

library("lsr")
library("ggplot2")
library("pwr")

summary.vesper<- data.frame(PP=c(rep(as.numeric(""),6)),
                          IN.M=c(rep(as.numeric(""),6)),
                          IN.SD=c(rep(as.numeric(""),6)),
                          IN.C.M=c(rep(as.numeric(""),6)),
                          IN.C.SD=c(rep(as.numeric(""),6)),
                          IN.I.M=c(rep(as.numeric(""),6)),
                          IN.I.SD=c(rep(as.numeric(""),6)),
                          DU.M=c(rep(as.numeric(""),6)),
                          DU.SD=c(rep(as.numeric(""),6)),
                          DU.C.M=c(rep(as.numeric(""),6)),
                          DU.C.SD=c(rep(as.numeric(""),6)),
                          DU.I.M=c(rep(as.numeric(""),6)),
                          DU.I.SD=c(rep(as.numeric(""),6)),
                          DI.M=c(rep(as.numeric(""),6)),
                          DI.SD=c(rep(as.numeric(""),6)),
                          DI.C.M=c(rep(as.numeric(""),6)),
                          DI.C.SD=c(rep(as.numeric(""),6)),
                          DI.I.M=c(rep(as.numeric(""),6)),
                          DI.I.SD=c(rep(as.numeric(""),6)))

IN.FILES=  list.files(c("Individual"), pattern="*.csv")
DU.FILES= list.files(c("Dual1"), pattern="*.csv")
DI.FILES= list.files(c("Dual2"), pattern="*.csv")


for (i in 1: length(IN.FILES)){

 summary.vesper$PP[i]= as.character(IN.FILES[i])
 
 ind.data<- read.csv(paste("Individual/",IN.FILES[i],sep = ""))
 ind.data<- ind.data[(ind.data$Running.SubTrial.=="b1trails1" | ind.data$Running.SubTrial. == "List2") & ind.data$Subject1ACC==1,]
 ind.data$Subject1RT<- as.numeric(ind.data$Subject1RT)
 
 ind.data$Type<- ifelse((ind.data$Running.SubTrial.=="b1trails1" & ind.data$Screen1_Obj_Colour=="blue" & ind.data$Screen_1_Obj_Pos== "e") | 
                        (ind.data$Running.SubTrial.=="b1trails1" &  ind.data$Screen1_Obj_Colour=="red" & ind.data$Screen_1_Obj_Pos== "w") |
                        (ind.data$Running.SubTrial.=="List2" & ind.data$Screen1_Obj_Colour=="blue" & ind.data$Screen_1_Obj_Pos== "w") | 
                        (ind.data$Running.SubTrial.=="List2" &  ind.data$Screen1_Obj_Colour=="red" & ind.data$Screen_1_Obj_Pos== "e"), "C","I")
 
 summary.vesper$IN.M[i] <- mean(ind.data$Subject1RT)
 summary.vesper$IN.SD[i] <- sd(na.omit(ind.data$Subject1RT))
 summary.vesper$IN.C.M[i]<- mean(na.omit(ind.data$Subject1RT[ind.data$Type=="C"]))
 summary.vesper$IN.C.SD[i]<- sd(na.omit(ind.data$Subject1RT[ind.data$Type=="C"]))
 summary.vesper$IN.I.M[i]<- mean(na.omit(ind.data$Subject1RT[ind.data$Type=="I"]))
 summary.vesper$IN.I.SD[i]<- sd(na.omit(ind.data$Subject1RT[ind.data$Type=="I"]))
 
}

dualu.data<- read.csv(paste("Dual1/",DU.FILES[1],sep = ""))
dualu.data<- dualu.data[(dualu.data$Running.SubTrial.=="CM1Trails" | dualu.data$Running.SubTrial. == "CM2Trials") & dualu.data$Subject1ACC==1 & dualu.data$Subject2ACC==1,]
dualu.data$Subject1RT<- as.numeric(dualu.data$Subject1RT)
dualu.data$Subject2RT<- as.numeric(dualu.data$Subject2RT)

dualu.data$Type1<- ifelse((dualu.data$Running.SubTrial.=="CM1Trails" & dualu.data$Screen1_Obj_Colour=="blue" & dualu.data$Screen_1_Obj_Pos== "e") | 
                         (dualu.data$Running.SubTrial.=="CM1Trails" &  dualu.data$Screen1_Obj_Colour=="red" & dualu.data$Screen_1_Obj_Pos== "w") |
                         (dualu.data$Running.SubTrial.=="CM2Trials" & dualu.data$Screen1_Obj_Colour=="blue" & dualu.data$Screen_1_Obj_Pos== "w") | 
                         (dualu.data$Running.SubTrial.=="CM2Trials" &  dualu.data$Screen1_Obj_Colour=="red" & dualu.data$Screen_1_Obj_Pos== "e"), "C","I")

dualu.data$Type2<- ifelse((dualu.data$Running.SubTrial.=="CM1Trails" & dualu.data$Screen2_Obj_Colour=="blue" & dualu.data$Screen_2_Obj_Pos== "e") | 
                            (dualu.data$Running.SubTrial.=="CM1Trails" &  dualu.data$Screen2_Obj_Colour=="red" & dualu.data$Screen_2_Obj_Pos== "w") |
                            (dualu.data$Running.SubTrial.=="CM2Trials" & dualu.data$Screen2_Obj_Colour=="blue" & dualu.data$Screen_2_Obj_Pos== "w") | 
                            (dualu.data$Running.SubTrial.=="CM2Trials" &  dualu.data$Screen2_Obj_Colour=="red" & dualu.data$Screen_2_Obj_Pos== "e"), "C","I")


   
   summary.vesper$DU.M[1] <- mean(dualu.data$Subject1RT)
   summary.vesper$DU.SD[1] <- sd(na.omit(dualu.data$Subject1RT))
   summary.vesper$DU.M[2] <- mean(dualu.data$Subject2RT)
   summary.vesper$DU.SD[2] <- sd(na.omit(dualu.data$Subject2RT))
   summary.vesper$DU.C.M[1]<- mean(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="C"]))
   summary.vesper$DU.C.SD[1]<- sd(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="C"]))
   summary.vesper$DU.C.M[2]<- mean(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="C"]))
   summary.vesper$DU.C.SD[2]<- sd(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="C"]))
   summary.vesper$DU.I.M[1]<- mean(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="I"]))
   summary.vesper$DU.I.SD[1]<- sd(na.omit(dualu.data$Subject1RT[dualu.data$Type1=="I"]))
   summary.vesper$DU.I.M[2]<- mean(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="I"]))
   summary.vesper$DU.I.SD[2]<- sd(na.omit(dualu.data$Subject2RT[dualu.data$Type2=="I"]))
 
