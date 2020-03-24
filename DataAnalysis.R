load("~/Documenti/Universita/Stage/Projet Jumeau Numérique/Analyse_18_19.RData")

library(readxl)
library(chron)

#Converting char to num type in the dataframe
myData <- subset(Data, select = -c(Motorisation, Pai, Colonne17, Colonne22, Colonne23, Colonne34, Colonne35, Colonne44, Classification, EvtTraite, CatTurb, ZonesDegivrage, TempsDegivrage, Commentaire, Configuration))

data <- read_xlsx("~/Documenti/Universita/Stage/Projet Jumeau Numérique/SampleDataSolPropre.xlsx")
#taking off useless columns
SampleData <- subset(data, select = -c(Motorisation, Pai, Colonne17, Colonne22, Colonne23, Colonne34, Colonne35, Colonne44, Classification, EvtTraite, CatTurb, ZonesDegivrage, TempsDegivrage, Commentaire, Configuration))
#taking off NA rows
SampleData <- SampleData[!is.na(SampleData$Mouvement), ]

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