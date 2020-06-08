setwd("/home/rita/Documents/Universita/Stage/Projet\ Jumeau\ Numérique/")
load("~/Documents/Universita/Stage/Projet Jumeau Numérique/myData.RData")


load("~/Documents/Universita/Stage/Projet Jumeau Numérique/Analyse_18_19.RData")

library(readxl)
library(chron)
library(dplyr)

#Converting char to num type in the dataframe
myData <- subset(Data, select = -c(Colonne17, Colonne22, Colonne23, Colonne34, Colonne35, Colonne44, Classification, EvtTraite, CatTurb, Commentaire, Configuration))

data <- read_xlsx("~/Documenti/Universita/Stage/Projet Jumeau Numérique/SampleDataSolPropre.xlsx")
#taking off useless columns
SampleData <- subset(data, select = -c(Motorisation, Pai, Colonne17, Colonne22, Colonne23, Colonne34, Colonne35, Colonne44, Classification, EvtTraite, CatTurb, ZonesDegivrage, TempsDegivrage, Commentaire, Configuration))
#taking off NA rows
SampleData <- SampleData[!is.na(SampleData$Mouvement), ]

#New Variables-----------------

#Parking area
myData$ParkingArea<-sub("^([[:alpha:]]*).*", "\\1", myData$Porte)

#Compagnie
grep("@",Data$Indicatif, value=TRUE)
myData$Indicatif <- gsub("@", "", myData$Indicatif)
myData$Compagnie <- substr(myData$Indicatif,1,3)

#Time slot

TimeDivision=quantile(myData$HeureTu, probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),na.rm=TRUE)

myData<-myData%>%mutate(timeSlot=case_when(HeureTu<TimeDivision[[1]]~1,
                                   HeureTu<TimeDivision[[2]]~2,
                                   HeureTu<TimeDivision[[3]]~3,
                                   HeureTu<TimeDivision[[4]]~4,
                                   HeureTu<TimeDivision[[5]]~5,
                                   HeureTu<TimeDivision[[6]]~6,
                                   HeureTu<TimeDivision[[7]]~7,
                                   HeureTu<TimeDivision[[8]]~8,
                                   HeureTu<TimeDivision[[9]]~9,
                                   ))
  


#division by hour
myData$hour<-round(as.numeric(myData$HeureTu)/3600)
myData$hour[myData$hour==24]<-23

myData$nightTime<- rep(FALSE, nrow(myData))
myData$nightTime[myData$hour %in% c(22,23,0,1,2,3, 4, 5, 6)]<-TRUE

#Group the same runway 
myData<-myData %>% mutate(jointRunway= case_when(Qfu=="26L" | Qfu=="08R" ~"SA",
                                               Qfu=="26R" | Qfu=="08L" ~"SD",
                                               Qfu=="27L" | Qfu=="09R" ~"ND",
                                               Qfu=="27R" | Qfu=="09L" ~"NA"
                                               ))
#Closed runway

myData$closedNA=rep(FALSE, nrow(myData))
myData$closedND=rep(FALSE, nrow(myData))
myData$closedSA=rep(FALSE, nrow(myData))
myData$closedSD=rep(FALSE, nrow(myData))

#Runway of departure used for arrivals and viceversa
myData$oppositeRunway<-rep(FALSE, nrow(myData))
myData$oppositeRunway[myData$Mouvement=="Atterrissage" & myData$Qfu %in% c("09R", "27L", "08L","26R")]<-TRUE
myData$oppositeRunway[myData$Mouvement!="Atterrissage" & myData$Qfu %in% c("09L", "27R", "08R","26L")]<-TRUE
myData$groupedRunway<-myData$Qfu
myData$groupedRunway[!myData$Qfu %in% c("NORD", "SUD")]<-gsub('.{1}$', '', myData$Qfu[!myData$Qfu %in% c("NORD", "SUD")])
unique(myData$groupedRunway)


#runway closed for more than 1 day
myData$longClosure=rep(FALSE, nrow(myData))
myData[myData$TimeMovement>chron(dates = "08/07/18", times = "20:00:00", format = c(dates = "d/m/y", times = "h:m:s")) & myData$TimeMovement<chron(dates = "09/10/18", times = "15:00:00", format = c(dates = "d/m/y", times = "h:m:s")),]$longClosure<-TRUE
myData[myData$TimeMovement>chron(dates = "13/11/19", times = "21:15:00", format = c(dates = "d/m/y", times = "h:m:s")) & myData$TimeMovement<chron(dates = "18/11/19", times = "04:30:00", format = c(dates = "d/m/y", times = "h:m:s")),]$longClosure<-TRUE



#Congestion 1 day------------------------------------------------------

#converting "char" into "time" type
SampleData[["HeureTu"]] <- chron(times=SampleData[["HeureTu"]])
SampleData[["HeureTuDebutDecollage"]] <- chron(times=SampleData[["HeureTuDebutDecollage"]])
mydates <- dates(SampleData[["DateTu"]], format = "d/m/y")
SampleData[["DateTu"]] <- chron(dates = mydates, format = "d/m/y")

#creating time column
SampleData["TimeMovement"]=chron(dates = SampleData[["DateTu"]], times = SampleData[["HeureTu"]], format = c(dates = "d/m/y", times = "h:m:s"))
SampleData["TimeStartTakeOff"]=chron(dates = SampleData[["DateTu"]], times = SampleData[["HeureTuDebutDecollage"]], format =  c(dates = "d/m/y", times = "h:m:s"))


#functions to fill the lists t_in e t_out
update_t_in_dec <- function(HeureTuDebutDecollage, TempsRoulage){
  return(times=HeureTuDebutDecollage-TempsRoulage/86400)
}

update_t_out_dec <- function(HeureTuDebutDecollage){
  return(HeureTuDebutDecollage)
}

update_t_in_att <- function(HeureTu, Top){
  return(HeureTu+Top/86400)
}

update_t_out_att <- function(HeureTu, Top, TempsRoulage){
  return(HeureTu+Top/86400+TempsRoulage/86400)
}


add_t_in <- function(t_in){
  t_in = c(t_in, mapply(update_t_in_dec, SampleData[SampleData[, "Mouvement"]=="Décollage",][-1,"TimeStartTakeOff"], SampleData[SampleData[, "Mouvement"]=="Décollage",][-1,"TempsRoulage"]))
  t_in = c(t_in, mapply(update_t_in_att, SampleData[SampleData[, "Mouvement"]=="Atterrissage",][["TimeMovement"]], SampleData[SampleData[, "Mouvement"]=="Atterrissage",][["Top"]]))
  return(t_in)
}

add_t_out<-function(t_out){
  t_out = c(t_out, mapply(update_t_out_dec, SampleData[SampleData[, "Mouvement"]=="Décollage",][-1,"TimeStartTakeOff"]))
  t_out= c(t_out, mapply(update_t_out_att, SampleData[SampleData[, "Mouvement"]=="Atterrissage",][["TimeMovement"]], SampleData[SampleData[, "Mouvement"]=="Atterrissage",][["Top"]],SampleData[SampleData[, "Mouvement"]=="Atterrissage",][["TempsRoulage"]]))
  return(t_out)
}

set_t_in<-function(TimeStartTakeOff,TempsRoulage){
  t_in=TimeStartTakeOff-TempsRoulage/86400
  t_in=add_t_in(t_in)
  return(t_in)
}

set_t_out<-function(TimeStartTakeOff){
  t_out=TimeStartTakeOff
  t_out=add_t_out(t_out)
  return(t_out)
}

#initialising the lists of t_in and t_out
t_in<-set_t_in(SampleData[SampleData[, "Mouvement"]=="Décollage",][["TimeStartTakeOff"]][1],SampleData[SampleData[, "Mouvement"]=="Décollage",][["TempsRoulage"]][1])
t_out<-set_t_out(SampleData[SampleData[, "Mouvement"]=="Décollage",][["TimeStartTakeOff"]][1])

dftimes <- data.frame("time"=c(t_in, t_out), "add"=c(rep(1,length(t_in)), rep(-1,length(t_out))))
dftimes <- dftimes[order(dftimes["time"]),]
row.names(dftimes) <- NULL

nAircrafts <- rep(0, nrow(dftimes))
for(i in 1:nrow(dftimes)){
  nAircrafts[i] <- sum(head(dftimes["add"],i))
}

dftimes["nAircrafts"]=nAircrafts

plot(dftimes[["time"]], dftimes[["nAircrafts"]], "l", xlab="time", ylab="N airctafts on taxiway")
max(dftimes["nAircrafts"])



#plot taxi time histogram for 6 companies------------
table(myData$Compagnie)[order()]

myData$Compagnie[myData$Compagnie=="EJU"]<-"EZY"

par(mfrow=c(3,2))
for(i in c("AFR","EZY","FDX","VLG","DLH")){
hist(myData[myData$Compagnie==i & myData$TempsRoulage<1500,]$TempsRoulage, breaks=50, main= i, xlab = "Taxi Time")
}
library(dplyr)
companyMeans<-as.data.frame(myData %>%
                group_by(Compagnie) %>%
                summarize(n=n(),percentage=n()/nrow(myData),mean = mean(TempsRoulage, na.rm = TRUE), sd=sd(TempsRoulage), meanD=mean(DistanceRoulage, na.rm=TRUE)))

linearRegression<-lm(formula = companyMeans[companyMeans$n>3,]$mean ~ companyMeans[companyMeans$n>3,]$meanD, x=TRUE, y=TRUE)
summary(linearRegression)


AnovaCompagnie<-aov(myData[myData$Compagnie %in% c("AFR","EZY"),]$TempsRoulage~myData[myData$Compagnie %in% c("AFR","EZY"),]$Compagnie)
summary(AnovaCompagnie)

#plot pairwise comparison
plot(TukeyHSD(ANOVA_FDX_Qfu))

#Sample AirFrance flights with the same hours as the other companies ---------------
library(frequency)

frequencies<-as.data.frame(table(myData$Compagnie))
frequencies$Percentage<-frequencies$Freq *100 / nrow(myData)

majorComp<-frequencies[frequencies$Percentage>1,]

#Time distribution of flights for EZY
breaks<-900*(0:(24*4)) #bin = a quarter of hour

hoursEZY<-myData[myData$Compagnie=="EZY" | myData$Compagnie=="EJU",][["HeureTu"]]
hoursEZY<-as.vector(hoursEZY, mode = "integer")
histEZY<-hist(hoursEZY, breaks)

#Time distribution of flights for FDX
hoursFDX<-myData[myData$Compagnie=="FDX",][["HeureTu"]]
hoursFDX<-as.vector(hoursFDX, mode = "integer")
histFDX<-hist(hoursFDX, breaks)

#Time distribution of flights for AFR
hoursAFR<-myData[myData$Compagnie=="AFR",][["HeureTu"]]
hoursAFR<-as.vector(hoursAFR, mode = "integer")
hoursAFR<-sort(hoursAFR)

histAFR<-hist(hoursAFR, breaks)

#the target probability is P_EZY(x)=P(x)*P_AFR(x) -> P(x)=P_EZY(x)/P_AFR(x)
#resampling AFR flights with time distribution of flights EZY
AFRordered<-myData[myData$Compagnie=="AFR",][order(myData[myData$Compagnie=="AFR",]$HeureTu),]

auxiliaryProbabilityEZY <-histEZY$density/histAFR$density
probabilitiesEZY<-rep(auxiliaryProbabilityEZY , histAFR$count)
sampleEZYIndexesAFR<-sample(1:nrow(myData[myData$Compagnie=="AFR",]), size=length(hoursEZY), prob=probabilitiesEZY, replace=FALSE)
sampleAFR_EZY<-as.vector(AFRordered[sampleEZYIndexesAFR,][["HeureTu"]], mode = "integer")
histAFRsampleEZY<-hist(sampleAFR_EZY, breaks)

#resampling AFR flights with time distribution of flights FDX

auxiliaryProbabilityFDX <-histFDX$density/histAFR$density
probabilitiesFDX<-rep(auxiliaryProbabilityFDX , histAFR$count)
sampleFDXIndexesAFR<-sample(1:nrow(myData[myData$Compagnie=="AFR",]), size=length(hoursFDX), prob=probabilitiesFDX, replace=FALSE)
sampleAFR_FDX<-as.vector(AFRordered[sampleFDXIndexesAFR,][["HeureTu"]], mode = "integer")
histAFRsampleFDX<-hist(sampleAFR_FDX, breaks)


#histograms of the Taxiing Time
Breaks_Roulage = (0:(1500*1.1/30))*30
par(mfrow=c(3,2))
histTDR_AFR_FDX<-hist(AFRordered[sampleFDXIndexesAFR,][AFRordered[sampleFDXIndexesAFR,]$TempsRoulage<1500,]$TempsRoulage, Breaks_Roulage, main = "histogram of AFR_FDX", xlab = "Taxi Time")
histTDR_FDX<-hist(myData[myData$Compagnie=="FDX" & myData$TempsRoulage<1500,]$TempsRoulage, Breaks_Roulage, main = "histogram of FDX", xlab = "Taxi Time")
histTDR_AFR_EZY<-hist(AFRordered[sampleEZYIndexesAFR,][AFRordered[sampleEZYIndexesAFR,]$TempsRoulage<1500,]$TempsRoulage, Breaks_Roulage, main = "histogram of AFR_EZY", xlab = "Taxi Time")
histTDR_EZY<-hist(myData[myData$Compagnie=="EZY" & myData$TempsRoulage<1500,]$TempsRoulage, Breaks_Roulage, main = "histogram of EZY", xlab = "Taxi Time")
histTDR_AFR<-hist(AFRordered[AFRordered$TempsRoulage<1500,]$TempsRoulage, Breaks_Roulage, main = "histogram of AFR", xlab = "Taxi Time")

histTDR_AFR$mids[which.max(histTDR_AFR$count)]
histTDR_EZY$mids[which.max(histTDR_EZY$count)]

hist(myData[myData$Compagnie=="FDX" & myData$TempsRoulage<1500 & myData$Mouvement=="Atterrissage",]$TempsRoulage, Breaks_Roulage, main = "histogram of FDX Atterrissage", xlab = "Taxi Time")
hist(myData[myData$Compagnie=="FDX" & myData$TempsRoulage<1500 & myData$HeureTu>20000,]$TempsRoulage, Breaks_Roulage, main = "histogram of FDX", xlab = "Taxi Time")
hist(myData[myData$Compagnie=="FDX" & myData$TempsRoulage<1500 & myData$HeureTu<700000,]$TempsRoulage, Breaks_Roulage, main = "histogram of FDX", xlab = "Taxi Time")


unique(dataFDX$TypeAvion)


# fit gaussian
library(MASS)
fitdistr(sampleAFR$TempsRoulage, "gamma")
fitdistr(myData[myData$Compagnie=="EZY",]$TempsRoulage, "gamma")


#TEMPS DE ROULAGE OF FDX: isolate dependences----------------

#Qfu ---------

#Histograms and boxplot
dataFDX<-myData[myData$Compagnie=="FDX",]
dataFDX <- dataFDX[dataFDX$Qfu != "NORD", ]

boxplot(dataFDX$TempsRoulage~dataFDX$Qfu, main= "Boxplot FedEx", xlab="Runway", ylab="Taxi Time", outline=FALSE)

par(mfrow=c(2,2))

for (i in c("26R", "08L", "26L", "08R")){
hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == i,]$TempsRoulage, breaks=60, xlab=paste("Taxi time",i, "for FDX"), main=paste("Taxi time",i, "for FDX"))
  }
par(mfrow=c(2,2))

for (i in c("27L", "09R", "27R", "09L")){
hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == i,]$TempsRoulage, breaks=60, xlab=paste("Taxi time",i, "for FDX"), main=paste("Taxi time",i, "for FDX"))
}


#Analysis of Variance (ANOVA)
ANOVA_FDX_Qfu<-aov(dataFDX$TempsRoulage~dataFDX$Qfu)
summary(ANOVA_FDX_Qfu)
TukeyHSD(ANOVA_FDX_Qfu)$diff
TukeyHSD(ANOVA_FDX_Qfu)[order(diff),] 
#plot pairwise comparison
plot(TukeyHSD(ANOVA_FDX_Qfu))

#Mean TempsRoulage grouped by runway
mean_Qfu_FDX<-setNames(data.frame(matrix(ncol = 2, nrow =0 )), c("Qfu","Mean"))

for (i in unique(dataFDX$Qfu)){
  mean_Qfu_FDX[i,1]<-i
  mean_Qfu_FDX[i,2]<-mean(dataFDX[dataFDX$Qfu == i,]$TempsRoulage)
}
par(mfrow=c(1,1))
mean_Qfu_FDX<-mean_Qfu_FDX[order(mean_Qfu_FDX$Mean),]


library(dplyr)

dataFDX<-myData[myData$Compagnie =="FDX" | myData$Compagnie=="ABR" & myData$ParkingArea=="I" ,]
dataFDX <- dataFDX[dataFDX$Qfu != "NORD", ]
unique(dataFDX$ParkingArea)
groupby_Parking_Qfu_FDX<-as.data.frame(dataFDX %>%
                             group_by(ParkingArea,Qfu) %>%
                             summarize(mean = mean(TempsRoulage, na.rm = TRUE)))

FDX_mean_sd<-as.data.frame(dataFDX %>%
                group_by(Qfu) %>%
                summarize(mean = mean(TempsRoulage, na.rm = TRUE), sd = sd(TempsRoulage, na.rm = TRUE)))
FDX_mean_sd$count<-as.data.frame(dataFDX %>%count(Qfu))$n
FDX_mean_sd$relFreq<-round(FDX_mean_sd$count*100/sum(FDX_mean_sd$count))

FDX_mean_sd[order(FDX_mean_sd$mean),]

#TEMPS DE ROULAGE FOR AIRFRANCE: isolate dependences----------------

#Histograms
dataAFR<-myData[myData$Compagnie=="AFR",]
dataAFR <- dataAFR[dataAFR$Qfu != "NORD", ]
dataAFR <- dataAFR[dataAFR$Qfu != "SUD", ]


new_order <- with( dataAFR, reorder(variety , note, mean , na.rm=T))

boxplot(dataAFR$TempsRoulage~dataAFR$Qfu, main="Differences in taxi times for Air France", xlab="Runway", ylab="Taxi Time", outline=FALSE)
par(mfrow=c(2,2))
for (i in c("26R", "08L", "26L", "08R")){
  hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == i,]$TempsRoulage, breaks=60, xlab=paste("Taxi time",i), main=paste("Taxi time",i, "for Air France"))
}
par(mfrow=c(2,2))

for (i in c("27L", "09R", "27R", "09L")){
  hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == i,]$TempsRoulage, breaks=60, xlab=paste("Taxi time",i), main=paste("Taxi time",i, "for Air France"))
}
#Analysis of Variance (ANOVA)
ANOVA_AFR_Qfu<-aov(dataAFR$TempsRoulage~dataAFR$Qfu)
summary(ANOVA_AFR_Qfu)
TukeyHSD(ANOVA_AFR_Qfu)$diff
TukeyHSD(ANOVA_AFR_Qfu)[order(diff),] 
plot(TukeyHSD(ANOVA_AFR_Qfu))

#comparer Atterrissage et Décollage
par(mfrow=c(2,1))
hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "08L" & dataAFR$Mouvement == "Atterrissage",]$TempsRoulage, breaks=60, xlab="Taxi time 08L", main="Taxi time 08L")
hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "08L" & dataAFR$Mouvement != "Atterrissage",]$TempsRoulage, breaks=60, xlab="Taxi time 08L", main="Taxi time 08L")

#Mean TempsRoulage grouped by runway
library(dplyr)

mean_Qfu_AFR<-as.data.frame(dataAFR %>%
                               group_by(Qfu) %>%
                               summarize(mean = mean(TempsRoulage, na.rm = TRUE)))

#group a subsample of AFR flights with the same time distribution as FDX flights

sampleAFR_FDX<-AFRordered[sampleFDXIndexesAFR,]

as.data.frame(sampleAFR_FDX %>%
                 group_by(Qfu) %>%
                 summarize(mean = mean(TempsRoulage, na.rm = TRUE)))

#group by Parking Area and Qfu
unique()
groupby_Parking_Qfu_AFR<-as.data.frame(dataAFR %>%
                                         group_by(ParkingArea,Qfu) %>%
                                         summarize(mean = mean(TempsRoulage, na.rm = TRUE)))
P<-
  dataAFR[dataAFR$ParkingArea=="G" & !is.na(dataAFR$ParkingArea),]$ParkingArea
dataAFR$Porte[is.na(dataAFR$ParkingArea)]

#mean and standard dev
AFR_mean_sd<-as.data.frame(dataAFR %>%
                             group_by(Qfu) %>%
                             summarize(mean = mean(TempsRoulage, na.rm = TRUE), sd = sd(TempsRoulage, na.rm = TRUE)))
AFR_mean_sd$count<-as.data.frame(dataAFR %>%count(Qfu))$n
AFR_mean_sd$relFreq<-round(AFR_mean_sd$count*100/sum(AFR_mean_sd$count))
AFR_mean_sd[order(AFR_mean_sd$mean),]

#Cheminement--------

#Add Variable -> Cheminement without the gate
myData$Cheminement_Reduced<-myData$Cheminement
myData[myData$Mouvement=="Atterrissage",]$Cheminement_Reduced <- sub("-[^-]+$", "", myData[myData$Mouvement=="Atterrissage",]$Cheminement_Reduced)
myData[myData$Mouvement!="Atterrissage",]$Cheminement_Reduced <- gsub("^T2-.*?-","",myData[myData$Mouvement!="Atterrissage",]$Cheminement_Reduced)


#Standard Cheminement pour Départure/Atterrissage Face Est/Ouest piste Nord/Sud
as.data.frame(table(myData[myData$Mouvement=="Atterrissage",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement=="Atterrissage",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Dep_FaceEst_Nord<-as.data.frame(table(myData[myData$Mouvement!="Atterrissage" & myData$Qfu=="09R",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement!="Atterrissage"& myData$Qfu=="09R",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Dep_FaceOuest_Nord<-as.data.frame(table(myData[myData$Mouvement!="Atterrissage" & myData$Qfu=="27L",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement!="Atterrissage"& myData$Qfu=="27L",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Att_FaceEst_Nord<-as.data.frame(table(myData[myData$Mouvement=="Atterrissage" & myData$Qfu=="09L",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement=="Atterrissage"& myData$Qfu=="09L",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Att_FaceOuest_Nord<-as.data.frame(table(myData[myData$Mouvement=="Atterrissage" & myData$Qfu=="27R",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement=="Atterrissage"& myData$Qfu=="27R",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]

StCh_Dep_FaceEst_Sud<-as.data.frame(table(myData[myData$Mouvement!="Atterrissage" & myData$Qfu=="08L",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement!="Atterrissage"& myData$Qfu=="08L",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Dep_FaceOuest_Sud<-as.data.frame(table(myData[myData$Mouvement!="Atterrissage" & myData$Qfu=="26R",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement!="Atterrissage"& myData$Qfu=="27L",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Att_FaceEst_Sud<-as.data.frame(table(myData[myData$Mouvement=="Atterrissage" & myData$Qfu=="08R",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement=="Atterrissage"& myData$Qfu=="09L",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]
StCh_Att_FaceOuest_Sud<-as.data.frame(table(myData[myData$Mouvement=="Atterrissage" & myData$Qfu=="26L",]$Cheminement_Reduced))[order(as.data.frame(table(myData[myData$Mouvement=="Atterrissage"& myData$Qfu=="27R",]$Cheminement_Reduced))$Freq, decreasing=TRUE),]

length(unique(myData$Cheminement_Reduced))
library(chron)


length(unique(myData[!myData$longClosure & !myData$nightTime,]$CheminementClean))

# list of all taxiways in Cheminement

library(tm)
taxiways<- gsub("-", " ", myData$Cheminement)
corpus<-VCorpus(VectorSource(taxiways))
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths=c(1,Inf)))

allTaxiways<-colnames(dtm)
allTaxiways<-toupper(allTaxiways)
allTaxiways<-allTaxiways[allTaxiways != "MIDDLE1"& allTaxiways != "MIDDLE2" & allTaxiways !="MIDDLE3" & allTaxiways !="MIDDLE4"]
allTaxiways<-append(allTaxiways, c("Middle1", "Middle2", "Middle3", "Middle4"))
allTaxiways<-allTaxiways[allTaxiways != "?"]


#add path extremes to the dataset
myData$CheminementExtremes<-rep("not assigned", nrow(myData))
chemExtr<-function(x){
  return(paste(sub("-.*", "", x), "-", sub('.*-', '', x)))
}
myData$CheminementExtremes<-chemExtr(myData$Cheminement)

myData$Extreme1<-rep("not assigned", nrow(myData))
myData$Extreme1<-sub("-.*", "", myData$Cheminement)
myData$Extreme2<-rep("not assigned", nrow(myData))
myData$Extreme2<-sub('.*-', '', myData$Cheminement)


#clean cheminement

CheminementClean2<-myData$Cheminement

CheminementClean2<-gsub(pattern = "Middle1" , replacement= "N", CheminementClean2)
CheminementClean2<-gsub(pattern = "Middle2" , replacement= "F", CheminementClean2)
CheminementClean2<-gsub(pattern = "Middle3" , replacement= "B", CheminementClean2)
CheminementClean2<-gsub(pattern = "Middle4" , replacement= "Q", CheminementClean2)

#take off taxiways that repeat like B-B-
for(i in allTaxiways){
  CheminementClean2<-gsub(pattern = paste(i,"-", "(",i,"-)+", sep="") , replacement= paste(i,"-", sep=""), CheminementClean2)
}
myData$CheminementClean<-CheminementClean2
tableChem<-as.data.frame(table(myData$CheminementClean))

#Clean Cheminement
myData$CheminementClean[myData$Extreme2=="T3" & myData$Mouvement=="Atterrissage"]<-sub('-[^-]*$', '', myData$CheminementClean[myData$Extreme2=="T3" & myData$Mouvement=="Atterrissage"])
myData$CheminementClean[myData$Extreme1=="T3" & myData$Mouvement!="Atterrissage"]<-sub("^[^-]*-", '', myData$CheminementClean[myData$Extreme1=="T3" & myData$Mouvement!="Atterrissage"])
myData[grep("T2-|T1-", myData$Pai),]$CheminementClean<-as.vector(mapply(sub, pattern = myData[grep("T2-|T1-", myData$Pai),]$Pai, replacement= "", x = myData[grep("T2-|T1-", myData$Pai),]$CheminementClean))

myData$CheminementClean <- sub("^-", "", myData$CheminementClean)
myData$CheminementClean <- sub("-$", "", myData$CheminementClean)

for(j in c("W","V","S","T","R","Z","K","Y", "Q","D")){
  myData$CheminementClean<-sub( paste(j,"[0-9]+", sep=""),paste(j,"i", sep=""), myData$CheminementClean)
}


#remove entree sortie piste
myData$CheminementClean2<-myData$CheminementClean
myData$CheminementClean2[grep("-", myData$CheminementClean2)]<-sub("Wi|Vi|Si|Ti|Ri|Zi|Ki|Yi|Qi|Vi$|Di", "", myData$CheminementClean2[grep("-", myData$CheminementClean2)])

#myData$CheminementClean2[grep("Vi$", myData$CheminementClean2)]

for(i in 1:3){
  myData$CheminementClean2 <- sub("^-", "", myData$CheminementClean2)
  myData$CheminementClean2 <- sub("-$", "", myData$CheminementClean2)
}

myData$CheminementClean2[grep("--", myData$CheminementClean2)]<-sub("Vi$|Zi$", "", myData$CheminementClean2[grep("--", myData$CheminementClean2)])
myData$CheminementClean2[grep("-", myData$CheminementClean2)]<-sub("^Wi|^Vi|^Si|^Ti|^Ri|^Zi|^Ki|^Yi|^Qi|^Di", "",myData$CheminementClean2[grep("-", myData$CheminementClean2)])

for(i in 1:3){
  myData$CheminementClean2 <- sub("^-", "", myData$CheminementClean2)
  myData$CheminementClean2 <- sub("-$", "", myData$CheminementClean2)
}

myData$CheminementClean2[grep("--", myData$CheminementClean2)]

length(unique(myData$CheminementClean2))

myData$CheminementClean<-myData$CheminementClean2

unique(myData$CheminementClean2[-grep("\\?", myData$CheminementClean2)])


#make a table
secondFrequentChem <-function(x){
  if(nrow(x)>1){
    return(x[2,1])
  }else{
    return(NA)
  }
}

secondFrequentFreq <-function(x){
  if(nrow(x)>1){
    return(x[2,2])
  }else{
    return(NA)
  }
}

thirdFrequentChem <-function(x){
  if(nrow(x)>2){
    return(x[3,1])
  }else{
    return(NA)
  }
}
thirdFrequentFreq <-function(x){
  if(nrow(x)>2){
    return(x[3,2])
  }else{
    return(NA)
  }
}

autresChemin<-function(chems, chem1, chem2, chem3){
  ch <- strsplit(chems, split=", ")[[1]]
  
  if(length(ch)>3){
    return( str_c(ch[!ch %in% c(as.character(chem1),as.character(chem2),as.character(chem3))],  sep=" ", collapse = ", ") )
  }else{
    return("none")
  }
}

prova<-myData[grep("\\?", myData$CheminementClean2),]
prova<-prova[c("Cheminement","Pai")]

length(unique(myData$CheminementClean2))
dfCheminements<-myData[-grep("\\?", myData$CheminementClean2),]
dfCheminements<-dfCheminements[!dfCheminements$nightTime,]

library(stringr)
library(dplyr)

groupby_Extremes<-as.data.frame(dfCheminements %>%
                                  group_by(Mouvement, Pai, groupedRunway) %>%
                                  summarize(Porte = str_c(unique(ParkingArea[!is.na(ParkingArea)], na.rm=TRUE), sep=" ", collapse = ", "),
                                            n = n(),
                                            Freq_1 = paste(round(as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),][1,"Freq"]/n()*100),"%"),
                                            freq_1= as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),][1,"Freq"],
                                            Freq_2 = paste(round(secondFrequentFreq(as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),])/n()*100),"%"),
                                            freq_2= secondFrequentFreq(as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),]),
                                            Freq_3 = paste(round(thirdFrequentFreq(as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),])/n()*100),"%"),
                                            freq_3 = thirdFrequentFreq(as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),]),
                                            cheminement_standard = as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),][1,1],
                                            cheminement_2 = secondFrequentChem( as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),] ),
                                            cheminement_3 = thirdFrequentChem( as.data.frame(table(CheminementClean2))[order( - as.data.frame(table(CheminementClean2))$Freq),] ),
                                            n_chem = length(unique(CheminementClean2)) ,
                                            chemin = str_c(unique(CheminementClean2), sep=" ", collapse = ", ") )
)

groupby_Extremes$autresChemin <-  as.vector(mapply(autresChemin, groupby_Extremes$chemin, groupby_Extremes$cheminement_standard, groupby_Extremes$cheminement_2, groupby_Extremes$cheminement_3 ))

groupby_Extremes<-groupby_Extremes[,names(groupby_Extremes) != "chemin", drop = F]


unique(myData$CheminementClean)

ambiguous<-groupby_Extremes_night[groupby_Extremes_night$freq_2/groupby_Extremes_night$freq_1>0.5,]
ambiguous<-ambiguous[!is.na(ambiguous$Mouvement),]
ambiguous<-ambiguous[ambiguous$n>10,]
nrow(ambiguous)
length(unique(myData$CheminementClean2))



# all taxiways without runway entrance
allTaxiwayClean<-allTaxiways
for(j in c("W","V","S","T","R","Z","K","Y", "Q","D")){
  allTaxiwayClean<-sub( paste(j,"[0-9]+", sep=""),paste(j,"i", sep=""), allTaxiwayClean)
}
unique(allTaxiwayClean)
allTaxiwayClean<-allTaxiwayClean[- which(allTaxiwayClean %in% c("Wi","Vi","Si","Ti","Ri","Zi","Ki","Yi", "Qi","Di") ) ]

unique(myData[myData$ParkingArea=="C",]$Pai)


unique(myData$CheminementClean2[grep("Si", myData$CheminementClean2)])

myData[myData$CheminementClean2=="D-A-E",]$Cheminement


tablePai<-as.data.frame(table(myData$Pai))
tablePaiNight<-as.data.frame(table(myData[myData$nightTime,]$Pai))
tableParkArea<-as.data.frame(table(myData$ParkingArea))

#PAI---------
unique(myData$Pai)
par(mfrow=c(1,1))
boxplot(myData$TempsRoulage~myData$Pai, xlab="Pai", ylab="Taxi Time", outline=FALSE)

library("dplyr")    

groupby_Pai<-as.data.frame(myData %>%
                             group_by(Pai) %>%
                             summarize(mean = mean(TempsRoulage, na.rm = TRUE)))

groupby_Pai$sd<-as.data.frame(myData %>%
                                group_by(Pai) %>%
                                summarize(sd = sd(TempsRoulage, na.rm = TRUE)))$sd

groupby_Pai$count<-as.data.frame(myData %>%count(Pai))$n

groupby_Pai<-groupby_Pai[order(groupby_Pai$mean),]

groupby_Pai[order(groupby_Pai$count),]


#Plot mean variables vs time-------------

myData$HeureStart[myData$Mouvement == "Atterrissage"] <- myData$HeureTu[myData$Mouvement == "Atterrissage"]
myData$HeureStart[myData$Mouvement != "Atterrissage"] <- 
  myData$HeureTu[myData$Mouvement != "Atterrissage"] - (myData$Top[myData$Mouvement != "Atterrissage"] + myData$TempsAttente1[myData$Mouvement != "Atterrissage"]
                                                        + myData$TempsAttente2[myData$Mouvement != "Atterrissage"] + myData$TempsAttente3[myData$Mouvement != "Atterrissage"]
                                                        + myData$TempsArretTotal[myData$Mouvement != "Atterrissage"] + myData$TempsAttentePiste[myData$Mouvement != "Atterrissage"]
                                                        + myData$TempsRoulage[myData$Mouvement != "Atterrissage"] + myData$TempsTraversee[myData$Mouvement != "Atterrissage"])


myData$HeureStop[myData$Mouvement == "Atterrissage"] <- 
  myData$HeureTu[myData$Mouvement == "Atterrissage"] + (myData$Top[myData$Mouvement == "Atterrissage"] + myData$TempsTraversee[myData$Mouvement == "Atterrissage"]
                                                        + myData$TempsAttente1[myData$Mouvement == "Atterrissage"] + myData$TempsAttente2[myData$Mouvement == "Atterrissage"]
                                                        + myData$TempsAttente3[myData$Mouvement == "Atterrissage"] + myData$TempsArretTotal[myData$Mouvement == "Atterrissage"]
                                                        + myData$TempsRoulage[myData$Mouvement == "Atterrissage"] )

myData$HeureStop[myData$Mouvement != "Atterrissage"] <- myData$HeureTu[myData$Mouvement != "Atterrissage"]


AttenteMoy<- function(Data,TimeStep,Start = 0, Stop = 86400, type = "smooth"){
  NbSteps = (Stop-Start)/TimeStep
  TpsAttenteMoy <- data.frame(1:NbSteps,1:NbSteps)
  names(TpsAttenteMoy) <- c("Heure","TempsAttenteMoy")
  for (i in 0:(NbSteps-1)){
    min = Start + i*TimeStep
    max = min + TimeStep 
    TpsAttenteMoy$Heure[i+1] <- (min+max)/2
    if (type == "smooth"){
      avionsvoies <- Data$Indicatif[(Data$HeureStart < min  &  max < Data$HeureStop) 
                                    | (min < Data$HeureStart & Data$HeureStart < max) 
                                    | (min < Data$HeureStop & Data$HeureStop < max)]
    } 
    else {
      avionsvoies <- Data$Indicatif[min<Data$HeureTu & Data$HeureTu< max]  
    }
    TpsAttenteMoy$TempsAttenteMoy[i+1] <- mean((Data$TempsAttentePiste + Data$TempsArretTotal + Data$TempsAttente1 + Data$TempsAttente2+ Data$TempsAttente3)[Data$Indicatif %in% avionsvoies])
  }
  return(TpsAttenteMoy)
}

RoulageMoy<- function(myData,TimeStep,Start = 0, Stop = 86400, type = "smooth"){
  NbSteps = (Stop-Start)/TimeStep
  RoulageMoy <- data.frame(1:NbSteps,1:NbSteps)
  names(RoulageMoy) <- c("Heure","RoulageMoy")
  for (i in 0:(NbSteps-1)){
    min = Start + i*TimeStep
    max = min + TimeStep 
    RoulageMoy$Heure[i+1] <- (min+max)/2
    if (type == "smooth"){
      avionsvoies <- myData$Indicatif[(myData$HeureStart < min  &  max < myData$HeureStop) 
                                      | (min < myData$HeureStart & myData$HeureStart < max) 
                                      | (min < myData$HeureStop & myData$HeureStop < max)]
    } 
    else {
      avionsvoies <- myData$Indicatif[min<myData$HeureTu & myData$HeureTu< max]  
    }
    RoulageMoy$RoulageMoy[i+1] <- mean((myData$TempsRoulage)[myData$Indicatif %in% avionsvoies])
  }
  return(RoulageMoy)
}

DistanceMoy<- function(myData,TimeStep,Start = 0, Stop = 86400, type = "smooth"){
  NbSteps = (Stop-Start)/TimeStep
  DistanceMoy <- data.frame(1:NbSteps,1:NbSteps)
  names(DistanceMoy) <- c("Heure","DistanceMoy")
  for (i in 0:(NbSteps-1)){
    min = Start + i*TimeStep
    max = min + TimeStep 
    DistanceMoy$Heure[i+1] <- (min+max)/2
    if (type == "smooth"){
      avionsvoies <- myData$Indicatif[(myData$HeureStart < min  &  max < myData$HeureStop) 
                                      | (min < myData$HeureStart & myData$HeureStart < max) 
                                      | (min < myData$HeureStop & myData$HeureStop < max)]
    } 
    else {
      avionsvoies <- myData$Indicatif[min<myData$HeureTu & myData$HeureTu< max]  
    }
    DistanceMoy$DistanceMoy[i+1] <- mean((myData$DistanceRoulage)[myData$Indicatif %in% avionsvoies])
  }
  return(DistanceMoy)
}

VitesseMoy<- function(myData,TimeStep,Start = 0, Stop = 86400, type = "smooth"){
  NbSteps = (Stop-Start)/TimeStep
  VitesseMoy <- data.frame(1:NbSteps,1:NbSteps)
  names(VitesseMoy) <- c("Heure","VitesseMoy")
  for (i in 0:(NbSteps-1)){
    min = Start + i*TimeStep
    max = min + TimeStep 
    VitesseMoy$Heure[i+1] <- (min+max)/2
    if (type == "smooth"){
      avionsvoies <- myData$Indicatif[(myData$HeureStart < min  &  max < myData$HeureStop) 
                                      | (min < myData$HeureStart & myData$HeureStart < max) 
                                      | (min < myData$HeureStop & myData$HeureStop < max)]
    } 
    else {
      avionsvoies <- myData$Indicatif[min<myData$HeureTu & myData$HeureTu< max]  
    }
    VitesseMoy$VitesseMoy[i+1] <- mean((myData$VitesseMoyRoulage)[myData$Indicatif %in% avionsvoies])
  }
  return(VitesseMoy)
}


VitesseMoy <- VitesseMoy(myData, 60)
TempsRoulageMoy <- RoulageMoy(myData,60)
TempsMoyAttente <- AttenteMoy(myData,60)
DistanceMoy<-DistanceMoy(myData,60)
AvionsVoies <- NbAvionsVoies(myData,60)


par(mar = c(5, 4, 4, 5))
plot(TempsMoyAttente$Heure,TempsMoyAttente$TempsAttenteMoy, ylab = "Temps attente moyen", xlab = "Heure", ylim=c(0, 280))
par(new = TRUE)
plot(TempsRoulageMoy$Heure,TempsRoulageMoy$RoulageMoy, type = "p", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red", ylim=c(350, 630))
axis(side = 4, col = "red", col.axis = "red")
mtext("Temps de roulage moyen(s)", side=4, line=2.5, col = "red")


par(mar = c(5, 4, 4, 5))
plot(DistanceMoy$Heure,DistanceMoy$DistanceMoy, ylab = "Distance moyenne", xlab = "Heure")
par(new = TRUE)
plot(TempsRoulageMoy$Heure,TempsRoulageMoy$RoulageMoy, type = "p", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red")
axis(side = 4, col = "red", col.axis = "red")
mtext("Temps de roulage moyen(s)", side=4, line=2.5, col = "red")


par(mar = c(5, 4, 4, 5))
plot(VitesseMoy$Heure,-(VitesseMoy$VitesseMoy), ylab = "-Vitesse moyenne", xlab = "Heure")
par(new = TRUE)
plot(TempsRoulageMoy$Heure,TempsRoulageMoy$RoulageMoy, type = "p", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red")
axis(side = 4, col = "red", col.axis = "red")
mtext("Temps de roulage moyen(s)", side=4, line=2.5, col = "red")

par(mar = c(5, 4, 4, 5))
plot(AvionsVoies$Heure,AvionsVoies$Nb , ylab = "n° Avions", xlab = "Heure")
par(new = TRUE)
plot(TempsRoulageMoy$Heure,TempsRoulageMoy$RoulageMoy, type = "p", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red")
axis(side = 4, col = "red", col.axis = "red")
mtext("Temps de roulage moyen(s)", side=4, line=2.5, col = "red")


?plot

#Peak of 3 a.m. ------------------
myData[myData$HeureTu<13000 & myData$HeureTu>5800,]

#dataframe with Company and its frequency between 1:40 and 3:40
frequencyComp<-as.data.frame(table(myData[myData$HeureTu<13300 & myData$HeureTu>5800,]$Compagnie))
frequencyComp = frequencyComp[frequencyComp$Freq >10,]
frequencyComp = frequencyComp[order(frequencyComp$Freq, decreasing=TRUE),]
frequencyComp$RelFreq<-frequencyComp$Freq /sum(frequencyComp$Freq)

#dataframe with Company and its frequency whole day
frequencyCompAll<-as.data.frame(table(myData$Compagnie))[order(as.data.frame(table(myData$Compagnie))$Freq, decreasing=TRUE),]
frequencyCompAll$RelFreq<-frequencyCompAll$Freq /sum(frequencyCompAll$Freq)

#dataframe with runway and its frequency between 1:40 and 3:40
frequencyQfu<-as.data.frame(table(myData[myData$HeureTu<13300 & myData$HeureTu>5800,]$Qfu))
frequencyQfu$RelFreq<-frequencyQfu$Freq /sum(frequencyQfu$Freq)
frequencyQfu[order(frequencyQfu$Freq, decreasing=TRUE),]

#dataframe with runway and its frequency whole day
frequencyQfuAll<-as.data.frame(table(myData$Qfu))
frequencyQfuAll$RelFreq<-frequencyQfuAll$Freq /sum(frequencyQfuAll$Freq)
frequencyQfuAll[order(frequencyQfuAll$Freq, decreasing=TRUE),]

frequencyPai<-as.data.frame(table(myData[myData$HeureTu<13300 & myData$HeureTu>5800,]$Pai))
frequencyPai[order(frequencyPai$Freq, decreasing=TRUE),]


library("dplyr")    

groupby_Qfu<-as.data.frame(myData[myData$HeureTu<13000 & myData$HeureTu>5800 & myData$Pai=="FDX",] %>%
  group_by(Qfu) %>%
  summarize(mean = mean(DistanceRoulage, na.rm = TRUE)))
groupby_Qfu$Freq<-as.data.frame(myData[myData$Pai=="FDX",] %>%
                                  group_by(Qfu) %>% count())$n
groupby_Qfu$RelFreq<-groupby_Qfu$Freq*100 /sum(groupby_Qfu$Freq)

groupby_Qfu[order(groupby_Qfu$mean),]


groupby_Qfu_All<-as.data.frame(myData[myData$Pai=="FDX",] %>%
                             group_by(Qfu) %>%
                             summarize(mean = mean(DistanceRoulage, na.rm = TRUE)))
groupby_Qfu_All$Freq<-as.data.frame(myData[myData$Pai=="FDX",] %>%
                                  group_by(Qfu) %>% count())$n
groupby_Qfu_All$RelFreq<-groupby_Qfu_All$Freq*100 /sum(groupby_Qfu_All$Freq)

groupby_Qfu_All[order(groupby_Qfu_All$mean),]


groupby_Pai<-as.data.frame(myData[myData$HeureTu<13000 & myData$HeureTu>5800,] %>%
                             group_by(Pai) %>%
                             summarize(mean = mean(DistanceRoulage, na.rm = TRUE)))

groupby_Pai[order(groupby_Pai$mean),]


groupby_Pai_all<-as.data.frame(myData %>%
                             group_by(Pai) %>%
                             summarize(mean = mean(DistanceRoulage, na.rm = TRUE)))

groupby_Pai_all[order(groupby_Pai_all$mean),]



#LIFO and FIFO analysis--------------------------------------
LIFOdata<-read.csv(file = 'LIFO.csv')
FIFOdata<-read.csv(file='FIFO.csv')
LIFOnomax<-read.csv(file = 'LIFOnoMax.csv')

histLIFO500<-hist(log(LIFOdata$TempsAttentePiste), breaks = 50)
par(mfrow=c(1,2))
histFIFO<-hist(log(FIFOdata$TempsAttentePiste), breaks = 50,main = "FIFO waiting time (log scale)", xlab = "Waiting Time" )
histLIFO<-hist(log(LIFOnomax$TempsAttentePiste), breaks = 40, main = "LIFO waiting time (log scale)", xlab= "Waiting Time")
par(mfrow=c(1,2))
histFIFO<-hist(FIFOdata$TempsAttentePiste, breaks = 25,main = "FIFO waiting time", xlab = "Waiting Time" )
histLIFO<-hist(LIFOnomax$TempsAttentePiste, breaks = 25, main = "LIFO waiting time", xlab= "Waiting Time")
plot(histLIFO$mids,histLIFO$count, log="y", col="blue",main="Log-normal distribution",xlab="Waiting Time", ylab="Frequency")

mean(LIFOdata$TempsAttentePiste)
mean(FIFOdata$TempsAttentePiste)
mean(LIFOnomax$TempsAttentePiste)

max(LIFOdata$TempsAttentePiste)
max(FIFOdata$TempsAttentePiste)
max(LIFOnomax$TempsAttentePiste)

hist(LIFOdata$TempsAttentePiste, breaks = 100)
hist(FIFOdata$TempsAttentePiste, breaks = 100)
hist(LIFOnomax$TempsAttentePiste, breaks = 100)

exp(histLIFO$mids[which.max(histLIFO$count)])


LIFOnomax<-LIFOnomax[!is.na(LIFOnomax$TempsAttentePiste), ]

LIFOnomax$TempsAttentePiste[is.na(LIFOnomax$TempsAttentePiste)]






#Detecting non-standard procedures--------------------
#Distribution of the time interval between two consecutive flghts
as.data.frame(table(myData[myData$Mouvement!="Atterrissage",]$Qfu))[order(as.data.frame(table(myData[myData$Mouvement!="Atterrissage",]$Qfu))["Freq"]),]
as.data.frame(table(myData[myData$Mouvement=="Atterrissage",]$Qfu))[order(as.data.frame(table(myData[myData$Mouvement=="Atterrissage",]$Qfu))["Freq"]),]

library(lubridate)
library(chron)
library(dplyr)

#Creation of variable expected runway, Orientation of runway and Direction of runway, time interval between flights and between those in the same runway-------

#create time of flight and order data
myData["TimeMovement"]=chron(dates = myData[["DateTu"]], times = hms::as_hms(myData[["HeureTu"]]), format = c(dates = "d/m/y", times = "h:m:s"))
myData<-myData[order(myData$TimeMovement),]

#create time interval between flights in the same runway----------
coupledRunways<-list()
coupledRunways[[1]]<-c("09L", "27R")
coupledRunways[[2]]<-c("09R", "27L")
coupledRunways[[3]]<-c("08L", "26R")
coupledRunways[[4]]<-c("08R", "26L")

myData$IntervalRunway=rep(0, nrow(myData))
for (i in coupledRunways){
  myData[myData$Qfu %in% i,]$IntervalRunway=as.numeric(myData[myData$Qfu%in%i,]$TimeMovement)- as.numeric(lag(myData[myData$Qfu%in%i,]$TimeMovement))
}
myData$IntervalRunway<-myData$IntervalRunway*24*60

#plot
par(mfrow=c(2,2))
for (i in coupledRunways){
  assign(paste("interval",i[1], sep=""), hist( myData[myData$Qfu%in%i,]$IntervalRunway, breaks = c(0, exp(0.5*(0:(round(2*log(max(myData[myData$Qfu%in%i,]$IntervalRunway, na.rm=TRUE )))+1) ))), main=i[1], xlab = paste("time interval for runway", i[1], "(s)")))
}

#a<-as.data.frame(get(paste("interval",i[1], sep=""))$count)
#a$mids <- round(get(paste("interval",i[1], sep=""))$mids)
#a$mids<-as.character(a$mids)
#plot(a$mids,a$get(paste("interval",i[1], sep=""))$count)

#create orientation-------------------------
myData<-myData %>% mutate(Orientation = case_when(myData$Qfu %in% c("08L", "09L", "08R", "09R") ~ "East", 
                                                  myData$Qfu %in% c("27L", "26L", "27R", "26R") ~ "West"))
#create direction---------------------
myData<-myData %>% mutate(Direction = case_when(myData$Qfu %in% c("08L", "26L", "08R", "26R", "SUD") ~ "Sud", 
                                                myData$Qfu %in% c("27L", "09L", "27R", "09R", "NORD") ~ "Nord"))


#create expeted runway---------------

Destinations<-data.frame(name= unique(na.omit(myData$TerrainArrivee))[unique(na.omit(myData$TerrainArrivee))!="LFPG"])
Destinations$percentage<-rep(0,nrow(Destinations)) 
Destinations$percentageDay<-rep(0,nrow(Destinations)) 
Destinations$direction<-rep("Unknown",nrow(Destinations)) 
Destinations<-Destinations[!is.na(Destinations$name),]

Origin<-data.frame(name= unique(na.omit(myData$TerrainDepart))[unique(na.omit(myData$TerrainArrivee))!="LFPG"])
Origin$percentage<-rep(0,nrow(Origin)) 
Origin$percentageDay<-rep(0,nrow(Origin)) 
Origin$direction<-rep("Unknown",nrow(Origin))
Origin<-Origin[!is.na(Origin$name),]

myData$ExpDirection=rep("NotAssigned", nrow(myData))


mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


for(i in Destinations$name ){
  Destinations[Destinations$name==i & !is.na(Destinations$name),]$percentage = nrow(myData[myData$TerrainArrivee == i & !is.na(myData$TerrainArrivee) & myData$Direction==mode(myData[myData$TerrainArrivee==i & !is.na(myData$TerrainArrivee),]$Direction) ,]) / nrow(myData[myData$TerrainArrivee==i & !is.na(myData$TerrainArrivee),])
  Destinations[Destinations$name==i & !is.na(Destinations$name),]$percentageDay = nrow(myData[myData$TerrainArrivee == i & !is.na(myData$TerrainArrivee) & myData$Direction==mode(myData[myData$TerrainArrivee==i & !is.na(myData$TerrainArrivee),]$Direction) & !myData$nightTime,]) / nrow(myData[myData$TerrainArrivee==i & !is.na(myData$TerrainArrivee)  & !myData$nightTime,])
  Destinations[Destinations$name==i & !is.na(Destinations$name),]$direction <- mode(myData[myData$TerrainArrivee==i & !is.na(myData$TerrainArrivee),]$Direction)
  Destinations[Destinations$percentage<0.8,]$direction="Unknown"
  myData$ExpDirection[myData$Mouvement!="Atterrissage" & myData$TerrainArrivee==i & !is.na(myData$TerrainArrivee)] = Destinations[Destinations$name==i & !is.na(Destinations$name),]$direction
}

for(i in Origin$name){
  Origin[Origin$name==i & !is.na(Origin$name),]$percentage = nrow(myData[myData$TerrainDepart == i & !is.na(myData$TerrainDepart) & myData$Direction==mode(myData[myData$TerrainDepart==i & !is.na(myData$TerrainDepart),]$Direction) ,]) / nrow(myData[myData$TerrainDepart==i & !is.na(myData$TerrainDepart),])
  Origin[Origin$name==i & !is.na(Origin$name),]$percentageDay = nrow(myData[myData$TerrainDepart == i & !is.na(myData$TerrainDepart) & myData$Direction==mode(myData[myData$TerrainDepart==i & !is.na(myData$TerrainDepart),]$Direction)  & !myData$nightTime,]) / nrow(myData[myData$TerrainDepart==i & !is.na(myData$TerrainDepart) & !myData$nightTime,])
  Origin[Origin$name==i & !is.na(Origin$name),]$direction <- mode(myData[myData$TerrainDepart==i & !is.na(myData$TerrainDepart),]$Direction)
  Origin[Origin$percentage<0.8,]$direction="Unknown"
  myData$ExpDirection[myData$Mouvement=="Atterrissage" & myData$TerrainDepart==i & !is.na(myData$TerrainDepart)] = Origin[Origin$name==i & !is.na(Origin$name),]$direction
}

nrow(myData[myData$ExpDirection=="Unknown" & !myData$Mouvement=="Atterrissage",])/nrow(myData[!myData$Mouvement=="Atterrissage",])
par(mfrow=c(1,1))
hist(Destinations$percentage, freq=FALSE)

myData$ExpRunway=rep("Unknown",nrow(myData))

myData<-myData %>% mutate(ExpRunway = case_when(myData$ExpDirection == "Nord" & myData$Orientation == "West" & myData$Mouvement=="Atterrissage"~ "27R", 
                                                myData$ExpDirection == "Nord" & myData$Orientation == "West" & !myData$Mouvement=="Atterrissage"~ "27L", 
                                                myData$ExpDirection == "Nord" & myData$Orientation == "East" & myData$Mouvement=="Atterrissage"~ "09L", 
                                                myData$ExpDirection == "Nord" & myData$Orientation == "East" & !myData$Mouvement=="Atterrissage"~ "09R", 
                                                myData$ExpDirection == "Sud" & myData$Orientation == "West" & myData$Mouvement=="Atterrissage"~ "26L", 
                                                myData$ExpDirection == "Sud" & myData$Orientation == "West" & !myData$Mouvement=="Atterrissage"~ "26R", 
                                                myData$ExpDirection == "Sud" & myData$Orientation == "East" & myData$Mouvement=="Atterrissage"~ "08R", 
                                                myData$ExpDirection == "Sud" & myData$Orientation == "East" & !myData$Mouvement=="Atterrissage"~ "08L", 
))


#Presentation of results : create a dataframe with hour, runway, meanIntervalRunway, sdRunway, quantileRunway--------------------

meanInterval<-as.data.frame(myData[myData$IntervalRunway<1,]%>% 
                group_by(hour) %>%
                summarize(meanRunway = mean(IntervalRunway, na.rm = TRUE)*24*60, sdRunway= sd(IntervalRunway, na.rm = TRUE)*24*60, quantile= quantile(IntervalRunway, probs = 0.8, na.rm=TRUE)*24*60, count=n() ))
meanInterval<-meanInterval[!is.na(meanInterval$meanRunway),]
meanInterval

#plots

hist(myData[!myData$nightTime & myData$IntervalRunway<0.033,]$IntervalRunway*24*60, xlab="Time interval between consecutive flights (min)", main ="From 05:00 to 23:00" )
hist(myData[myData$nightTime & myData$IntervalRunway<0.033,]$IntervalRunway*24*60, xlab="Time interval between consecutive flights (min)", main ="From 23:00 to 5:00" )

hist(myData[myData$timeSlot==1 & myData$IntervalRunway<0.022,]$IntervalRunway*24*60, xlab=1 )
hist(myData[myData$timeSlot!=1 & myData$IntervalRunway<0.022,]$IntervalRunway*24*60, xlab="others" )

par(mfrow=c(1,1))
for(i in 2:12){
hist(as.numeric(myData[myData$IntervalRunway>(i-1)/24 ,]$HeureTu)/3600, breaks = 24, xlab="hour",main=paste("Interval of",i-1, "hrs or more between two flights"))
}


#table
tableInterval<-data.frame(hour=c(1:7, 24), Ndays= rep(0,8), freq=rep(0,8))
for(i in tableInterval$hour){
tableInterval[which(tableInterval$hour == i),"Ndays"] <- length(unique(myData[myData$IntervalRunway>i/24 & !myData$nightTime,]$DateTu))-1
tableInterval[which(tableInterval$hour == i),"freq"]<-tableInterval[which(tableInterval$hour == i),"Ndays"]/790
}


longClosure<-myData[myData$IntervalRunway>1 & !is.na(myData$IntervalRunway),]

table(myData[myData$IntervalRunway>4/24 & !myData$nightTime ,]$jointRunway)


#labelling flights with closed runway-------------------
myData$closedNA=rep(FALSE, nrow(myData))
myData$closedND=rep(FALSE, nrow(myData))
myData$closedSA=rep(FALSE, nrow(myData))
myData$closedSD=rep(FALSE, nrow(myData))



myData$IntervalRunway<-myData$IntervalRunway/(24*60) 

myData<-myData%>%mutate(closedNA=case_when(IntervalRunway>4/24 & jointRunway == "NA" & hour %in% 0:5 ~ TRUE,
                                           IntervalRunway>0.5/24 & jointRunway == "NA" & hour %in% 6:21 ~ TRUE,
                                           TRUE~FALSE))

myData<-myData%>%mutate(closedND=case_when(IntervalRunway>4/24 & jointRunway == "ND" & hour %in% 0:5~ TRUE,
                                           IntervalRunway>0.5/24 & jointRunway == "ND" & hour %in% 6:23~ TRUE,
                                           TRUE~FALSE))

myData<-myData%>%mutate(closedSA=case_when(IntervalRunway>4/24 & jointRunway == "SA" & hour %in% 0:5 ~ TRUE,
                                           IntervalRunway>0.5/24 & jointRunway == "SA" & hour %in% 6:23 ~ TRUE,
                                           TRUE~FALSE))

myData<-myData%>%mutate(closedSD=case_when(IntervalRunway>4/24 & jointRunway == "SD" & hour %in% 0:5 ~ TRUE,
                                           IntervalRunway>0.5/24 & jointRunway == "SD" & hour %in% 6:23 ~ TRUE,
                                           TRUE~FALSE))

closed_set<-myData[myData$closedNA | myData$closedSA | myData$closedND | myData$closedSD ,]
closed_set<-closed_set[!closed_set$longClosure,]
length(unique(closed_set[closed_set$hour %in% c(8,9,10,11,12,13,14,15,16,17,18,19,20),]$DateTu))

hist(closed_set[closed_set$hour %in% c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),]$hour, main= "Time when a runway is closed more than 30min (from 7 to 21)", xlab="hour")

mean(myData[myData$hour==23,]$IntervalRunway*24*60, na.rm = TRUE)

sd(myData[myData$hour==23 & myData$IntervalRunway<180,]$IntervalRunway, na.rm = TRUE)

#not working creation of dataframe with closed runways only--------------------
firstOpen<-myData[!is.na(myData$closedRunway) & !is.na(myData$closedRunway),]
firstOpen<-firstOpen[!is.na(firstOpen$Mouvement),]

for (i in which(myData$closedNA) ){
  if(length(myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedNA)>15){
    myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedNA= TRUE
    }else {
      myData$closedNA[i]==FALSE
    }
}

for (i in which(myData$closedND) ){
  if(length(myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedND)>15){
    myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedND= TRUE
  }else {
    myData$closedND[i]==FALSE
  }
}

for (i in which(myData$closedSA) ){
  if(length(myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedSA)>15){
    myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedSA= TRUE
  }else {
    myData$closedSA[i]==FALSE
  }
}

for (i in which(myData$closedSD) ){
  if(length(myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedSD)>15){
    myData[myData$TimeMovement<myData$TimeMovement[i] & myData$TimeMovement>myData$TimeMovement[i]-myData$IntervalRunway[i] & !is.na(myData$IntervalRunway),]$closedSD= TRUE
  }else {
    myData$closedSD[i]==FALSE
  }
}

a<-myData[myData$IntervalRunway>2/24 & myData$closedND & myData$jointRunway == "ND" & myData$hour %in% c(23,0,1) &!is.na(myData$jointRunway),]

myData[myData$IntervalRunway>1/24 & myData$jointRunway == "NA" & myData$hour %in% 7:22 &!is.na(myData$jointRunway),]

#Distribution of time interval between two changes in runway orientation-------------------------
myData$ChangeOrientation=rep(FALSE, nrow(myData))

myData$ChangeOrientation[which(myData$Orientation != lag(myData$Orientation) & !is.na(myData$Orientation))]<-TRUE

a<-myData[myData$Orientation!= lag(myData$Orientation) & !is.na(myData$Orientation),]
b<-myData[myData$Orientation!= lead(myData$Orientation) & !is.na(myData$Orientation),]

hist(log(a[a$IntervalRunway<5,]$IntervalRunway*24*60), breaks =0.5* (-8:14))
hist(log(b[b$IntervalRunway<5,]$IntervalRunway*24*60), breaks =0.5* (-8:14))
hist(log(myData$IntervalRunway*24*60))

a<-myData[myData$ChangeOrientation,]$TimeMovement-lag(myData[myData$ChangeOrientation,]$TimeMovement)
a<-as.numeric(a)
hist(a, breaks=0:21, main="Time interval between two orientation change", xlab="time interval between orientation change (days)", ylab="Frequency" )
mean(a, na.rm=TRUE)
hist(a[a<1]*24, main="Time interval between two orientation change \n for less than 1 day intervals", xlab="time interval between orientation change (hours)", ylab="Frequency" )

ANOVA_orientation<-aov(myData$TempsRoulage~myData$ChangeOrientation)
summary(ANOVA_orientation) #significally different!


#inspect regular flights-------------
library(dplyr)

length(unique(myData$Indicatif))
indicatifs<-sort(table(myData$Indicatif), decreasing=TRUE)

Indicatifs<-as.data.frame(myData %>%
                          group_by(Indicatif, TerrainArrivee, TerrainDepart) %>%
                          summarize(meanHour = mean(HeureTu, na.rm = TRUE), sdHeure = sd(HeureTu, na.rm = TRUE), count = n())
                          )
nrow(Indicatifs[Indicatifs$sdHeure>60*60 & !is.na(Indicatifs$sdHeure) & Indicatifs$count>30 ,])

RecurrentFlights<-Indicatifs[Indicatifs$Indicatif %in% Indicatifs[duplicated(Indicatifs$Indicatif) & Indicatifs$count>30,]$Indicatif,]
RecurrentFlights<-RecurrentFlights[RecurrentFlights$count>10,]
RecurrentFlights<-RecurrentFlights[!(RecurrentFlights$Indicatif %in% RecurrentFlights[duplicated(RecurrentFlights$Indicatif),]$Indicatif),]
RecurrentFlights[order(RecurrentFlights$count, decreasing = TRUE),]
length(RecurrentFlights$count)
RecurrentFlights<-RecurrentFlights[RecurrentFlights$TerrainDepart=="LFPG",]
a<-myData[myData$Indicatif=="AFR276",]
RecurrentFlights$meanHour<-hms::as.hms(round(RecurrentFlights$meanHour))
RecurrentFlights$sdHeure<-hms::as.hms(round(RecurrentFlights$sdHeure))

nrow(RecurrentFlights[as.numeric(RecurrentFlights$sdHeure)<2000 & RecurrentFlights$count>140,])

#inspect long taxi time-----------------

mean(myData$TempsRoulage)
hist(myData[myData$TempsRoulage>2000,]$TempsRoulage)
a<-myData[myData$TempsRoulage>2000,]

a$IntervalRunway=rep(NA, nrow(a))

for (i in coupledRunways){
  a[a$Qfu %in% i,]$IntervalRunway=a[a$Qfu%in%i,]$TimeMovement- lag(a[a$Qfu%in%i,]$TimeMovement)
}

a$IntervalRunway<-a$IntervalRunway*60*24

hist(a[a$IntervalRunway<20000,]$IntervalRunway, breaks = 20)
hist(a[a$IntervalRunway<100,]$IntervalRunway, breaks=25)

length(unique(a$DateTu))
hist(as.numeric(a$hour), breaks=0:23)

c<-myData[myData$TempsRoulage>2000 & myData$TempsArretTotal==0 & is.na(myData$TempsDegivrage),]

b<-a[a$IntervalRunway<40,]

criticalDates<- as.data.frame(a %>% group_by(DateTu) %>% tally())

