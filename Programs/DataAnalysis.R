setwd("/home/rita/Documenti/Universita/Stage/Projet\ Jumeau\ Numérique/")
load("~/Documenti/Universita/Stage/Projet Jumeau Numérique/Analyse_18_19.RData")

library(readxl)
library(chron)

#Converting char to num type in the dataframe
myData <- subset(Data, select = -c(Pai, Colonne17, Colonne22, Colonne23, Colonne34, Colonne35, Colonne44, Classification, EvtTraite, CatTurb, ZonesDegivrage, TempsDegivrage, Commentaire, Configuration))

data <- read_xlsx("~/Documenti/Universita/Stage/Projet Jumeau Numérique/SampleDataSolPropre.xlsx")
#taking off useless columns
SampleData <- subset(data, select = -c(Motorisation, Pai, Colonne17, Colonne22, Colonne23, Colonne34, Colonne35, Colonne44, Classification, EvtTraite, CatTurb, ZonesDegivrage, TempsDegivrage, Commentaire, Configuration))
#taking off NA rows
SampleData <- SampleData[!is.na(SampleData$Mouvement), ]


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


#Sample AirFrance flights with the same hours as the other companies ---------------
library(frequency)

#Creation of Company column
grep("@",Data$Indicatif, value=TRUE)
myData$Indicatif <- gsub("@", "", myData$Indicatif)
myData$Compagnie <- substr(myData$Indicatif,1,3)

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
dataFDX<-myData[myData$Compagnie=="FDX",]

unique(dataFDX$TypeAvion)


# fit gaussian
library(MASS)
fitdistr(sampleAFR$TempsRoulage, "gamma")
fitdistr(myData[myData$Compagnie=="EZY",]$TempsRoulage, "gamma")


#TEMPS DE ROULAGE OF FDX: isolate dependences----------------

#Qfu---------

dataFDX$Qfu[dataFDX$Qfu == "08L"] <- "26R"
dataFDX$Qfu[dataFDX$Qfu == "08R"] <- "26L"
dataFDX$Qfu[dataFDX$Qfu == "09L"] <- "27R"
dataFDX$Qfu[dataFDX$Qfu == "09R"] <- "27L"
#removing temps de roulage too high to have more readable boxplots

dataFDX <- dataFDX[dataFDX$TempsRoulage < 4000, ]
dataFDX <- dataFDX[dataFDX$Qfu != "NORD", ]
#boxplot
boxplot(dataFDX$TempsRoulage~dataFDX$Qfu, xlab="runway", ylab="Taxi Time", outline=FALSE)

par(mfrow=c(2,2))

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "26R",]$TempsRoulage, breaks=60, xlab="Taxi time 26R joint to 08L", main="Taxi time 26R joint to 08L")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "26L",]$TempsRoulage, breaks=60, xlab="Taxi time 26L joint to 08R", main="Taxi time 26L joint to 08R")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "27L",]$TempsRoulage, breaks=60, xlab="Taxi time 27L joint to 09R", main="Taxi time 27L joint to 09R")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "27R",]$TempsRoulage, breaks=60, xlab="Taxi time 27R joint to 09L", main="Taxi time 27R joint to 09L")

#Histograms
dataFDX<-myData[myData$Compagnie=="FDX",]
dataFDX <- dataFDX[dataFDX$Qfu != "NORD", ]

boxplot(dataFDX$TempsRoulage~dataFDX$Qfu, xlab="runway", ylab="Taxi Time", outline=FALSE)

par(mfrow=c(2,2))

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "26R",]$TempsRoulage, breaks=60, xlab="Taxi time 26R", main="Taxi time 26R")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "08L",]$TempsRoulage, breaks=60, xlab="Taxi time 08L", main="Taxi time 08L")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "26L",]$TempsRoulage, breaks=60, xlab="Taxi time 26L", main="Taxi time 26L")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "08R",]$TempsRoulage, breaks=60, xlab="Taxi time 08R", main="Taxi time 08R")

par(mfrow=c(2,2))

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "27L",]$TempsRoulage, breaks=60, xlab="Taxi time 27L", main="Taxi time 27L")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "09R",]$TempsRoulage, breaks=60, xlab="Taxi time 09R", main="Taxi time 09R")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "27R",]$TempsRoulage, breaks=60, xlab="Taxi time 27R", main="Taxi time 27R")

hist(dataFDX[dataFDX$TempsRoulage<1500 & dataFDX$Qfu == "09L",]$TempsRoulage, breaks=60, xlab="Taxi time 09L", main="Taxi time 09L")

#Analysis of Variance (ANOVA)
ANOVA_FDX_Qfu<-aov(dataFDX$TempsRoulage~dataFDX$Qfu)
summary(ANOVA_FDX_Qfu)
TukeyHSD(ANOVA_FDX_Qfu)$diff
TukeyHSD(ANOVA_FDX_Qfu)[order(diff),] 
plot(TukeyHSD(ANOVA_FDX_Qfu))


#TEMPS DE ROULAGE FOR AIRFRANCE: isolate dependences----------------

dataAFR<-myData[myData$Compagnie=="AFR",]

dataAFR$Qfu[dataAFR$Qfu == "08L"] <- "26R"
dataAFR$Qfu[dataAFR$Qfu == "08R"] <- "26L"
dataAFR$Qfu[dataAFR$Qfu == "09L"] <- "27R"
dataAFR$Qfu[dataAFR$Qfu == "09R"] <- "27L"
#removing temps de roulage too high to have more readable boxplots

dataAFR <- dataAFR[dataAFR$Qfu != "NORD", ]
dataAFR <- dataAFR[dataAFR$Qfu != "SUD", ]

#boxplot
boxplot(dataAFR$TempsRoulage~dataAFR$Qfu, xlab="runway", ylab="Taxi Time", outline=FALSE)

par(mfrow=c(2,2))

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "26R",]$TempsRoulage, breaks=60, xlab="Taxi time 26R joint to 08L", main="Taxi time 26R joint to 08L")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "26L",]$TempsRoulage, breaks=60, xlab="Taxi time 26L joint to 08R", main="Taxi time 26L joint to 08R")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "27L",]$TempsRoulage, breaks=60, xlab="Taxi time 27L joint to 09R", main="Taxi time 27L joint to 09R")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "27R",]$TempsRoulage, breaks=60, xlab="Taxi time 27R joint to 09L", main="Taxi time 27R joint to 09L")

#Histograms
dataAFR<-myData[myData$Compagnie=="AFR",]
dataAFR <- dataAFR[dataAFR$Qfu != "NORD", ]
dataAFR <- dataAFR[dataAFR$Qfu != "SUD", ]

boxplot(dataAFR$TempsRoulage~dataAFR$Qfu, xlab="runway", ylab="Taxi Time", outline=FALSE)

par(mfrow=c(2,2))

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "26R",]$TempsRoulage, breaks=60, xlab="Taxi time 26R", main="Taxi time 26R")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "08L",]$TempsRoulage, breaks=60, xlab="Taxi time 08L", main="Taxi time 08L")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "26L",]$TempsRoulage, breaks=60, xlab="Taxi time 26L", main="Taxi time 26L")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "08R",]$TempsRoulage, breaks=60, xlab="Taxi time 08R", main="Taxi time 08R")

par(mfrow=c(2,2))

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "27L",]$TempsRoulage, breaks=60, xlab="Taxi time 27L", main="Taxi time 27L")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "09R",]$TempsRoulage, breaks=60, xlab="Taxi time 09R", main="Taxi time 09R")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "27R",]$TempsRoulage, breaks=60, xlab="Taxi time 27R", main="Taxi time 27R")

hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "09L",]$TempsRoulage, breaks=60, xlab="Taxi time 09L", main="Taxi time 09L")

#Analysis of Variance (ANOVA)
ANOVA_AFR_Qfu<-aov(dataAFR$TempsRoulage~dataAFR$Qfu)
summary(ANOVA_AFR_Qfu)
TukeyHSD(ANOVA_AFR_Qfu)$diff
TukeyHSD(ANOVA_AFR_Qfu)[order(diff),] 
plot(TukeyHSD(ANOVA_AFR_Qfu))

par(mfrow=c(2,1))
hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "08L" & dataAFR$Mouvement == "Atterrissage",]$TempsRoulage, breaks=60, xlab="Taxi time 08L", main="Taxi time 08L")
hist(dataAFR[dataAFR$TempsRoulage<1500 & dataAFR$Qfu == "08L" & dataAFR$Mouvement != "Atterrissage",]$TempsRoulage, breaks=60, xlab="Taxi time 08L", main="Taxi time 08L")

class(TukeyHSD(ANOVA_AFR_Qfu))
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








