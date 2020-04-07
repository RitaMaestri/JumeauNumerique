load("~/Documenti/Universita/Stage/Projet Jumeau Numérique/Analyse_18_19.RData")

library(readxl)
library(cluster)
library(ggplot2)
library(expss)
library(factoextra)
fileEncoding = "utf-8"
data <- read_xlsx("SampleDataSolPropre.xlsx")


# Travaux Préliminaires USELESS -----------------------------------------------------------


breaks <- (0:30)*100
count <- table(data$TempsRoulage, data$VoieEntreeSortiePiste) 
maxFreq <- 100
maxRoulage <- round(max(data$TempsRoulage, na.rm=TRUE))
Voies <- unique(data$VoieEntreeSortiePiste)
NbVoies <- length(Voies)

par(mfrow=c(5,4))
for ( i in Voies[1:20]) 
{hist(data$TempsRoulage[data$VoieEntreeSortiePiste == i], breaks = 10, 
main = paste("Histogram of" , i),xlab = paste("Temps de roulage sur",i),
xlim = c(0,maxRoulage), ylim = c(0,30), freq = TRUE)}

VolsAF <- grep("^AF",data$Indicatif)
Vols <- 1:1361
VolsNonAF <- Vols[-VolsAF]

Piste27L <- grep("27L",data$Qfu)
Piste27R <- grep("27R",data$Qfu)
Pistdatae26L <- grep("26L",data$Qfu)
Piste26R <- grep("26R",data$Qfu)

#group the flights by hour -> VolsXXh returns the indexes

Vols00h <- grep("^00",data$HeureTu)
Vols01h <- grep("^01",data$HeureTu)
Vols02h <- grep("^02",data$HeureTu)
Vols03h <- grep("^03",data$HeureTu)
Vols04h <- grep("^04",data$HeureTu)
Vols05h <- grep("^05",data$HeureTu)
Vols06h <- grep("^06",data$HeureTu)
Vols07h <- grep("^07",data$HeureTu)
Vols08h <- grep("^08",data$HeureTu)
Vols09h <- grep("^09",data$HeureTu)
Vols10h <- grep("^10",data$HeureTu)
Vols11h <- grep("^11",data$HeureTu)
Vols12h <- grep("^12",data$HeureTu)
Vols13h <- grep("^13",data$HeureTu)
Vols14h <- grep("^14",data$HeureTu)
Vols15h <- grep("^15",data$HeureTu)
Vols16h <- grep("^16",data$HeureTu)
Vols17h <- grep("^17",data$HeureTu)
Vols18h <- grep("^18",data$HeureTu)
Vols19h <- grep("^19",data$HeureTu)
Vols20h <- grep("^20",data$HeureTu)
Vols21h <- grep("^21",data$HeureTu)
Vols22h <- grep("^22",data$HeureTu)
Vols23h <- grep("^23",data$HeureTu)

VolsHoraires <- list(Vols00h,Vols01h,Vols02h,Vols03h,Vols04h,Vols05h,Vols06h,
Vols07h,Vols08h,Vols09h,Vols10h,Vols11h,Vols12h,Vols13h,Vols14h,Vols15h,Vols16h
,Vols17h,Vols18h,Vols19h,Vols20h,Vols21h,Vols22h,Vols23h)


#plot Temps Roulage Moyen by hour
MoyTpsHeures <- mean(data$TempsRoulage[Vols00h])
for (Vols in VolsHoraires[2:24])
{MoyTpsHeures <- c(MoyTpsHeures,mean(data$TempsRoulage[Vols]))}
plot(0:23,MoyTpsHeures,"s",xlab = "Heure", ylab = "Temps de roulage moyen (s)")


#plot number of flights by hour
NVols <- length(Vols00h)
for (Vols in VolsHoraires[2:24])
{NVols <- c(NVols,length(Vols))}
plot(0:23,NVols,"s",xlab = "Heure", ylab = "Nombre vols")


#plot of 15 distribution for temps de roulage of AirFrance flights for 15 different time intervals
par(mfrow=c(4,4))
for ( i in VolsHoraires[5:21]) 
{hist(data$TempsRoulage[intersect(VolsAF,i)], breaks = breaks,xlab = paste("Temps de roulage sur",i), xlim = c(0,maxRoulage), ylim = c(0,30))}


#plot of 6 Distributions of temps de roulage for 6 different motorisation
Motorisations <-unique(data$Motorisation, data)
par(mfrow=c(3,2))
for (Motorisation in Motorisations){
hist(data$TempsRoulage[data$Motorisation == Motorisation], breaks = breaks, 
main = paste("Distribution des temps de roulage", Motorisation),
xlab = "Temps de roulage(s)")}


#plot of the distributions of temps de roulage for the take off and the landing
Mouvements <- unique(data$Mouvement, data)
par(mfrow = c(2,2))
for (Mouvement in Mouvements){
hist(data$TempsRoulage[data$Mouvement == Mouvement], breaks = breaks, ylim = c(0,0.0025),
main = paste("Distribution des temps de roulage", Mouvement),
xlab = "Temps de roulage (s)", freq = FALSE)
plot.new()
summ <- capture.output(summary(data$TempsRoulage[data$Mouvement == Mouvement]))
text(0.30,0.5,summ[1])
text(0.30,0.4,summ[2])
}


VoiesEntreeSortiePiste <- unique(data$VoieEntreeSortiePiste,data)

NbArretsPiste <- c()
for (Voie in VoiesEntreeSortiePiste){
NbArretsPiste <- c(NbArretsPiste,sum(data$NbArrets[data$VoieEntreeSortiePiste == Voie]))}
print(NbArretsPiste)


NbVoies <- length(VoieEntreeSortiePiste) 
NbArretsPiste <- c()
count <- table(data$NbArrets,data$VoieEntreeSortiePiste)
Arrets <- as.numeric(rownames(count))
NbArrets <- length(Arrets)

for (Voie in 1:NbVoies) {
ArretsPiste <- 0
for ( Arret in 1:NbArrets){
ArretsPiste <- ArretsPiste + Arrets[Arret]*count[Arret,Voies]}
NbArretsPiste <- c(NbArretsPiste,ArretsPiste)}

barplot(NbArretsPiste,names.arg = VoieEntreeSortiePiste,add = TRUE, col = "blue")

TpsMoyVoies <- c()

for (Voie in VoiesEntreeSortiePiste){
TpsMoyVoies <- c(TpsMoyVoies,mean(data$RoulageAvecArret[VoieEntreeSortiePiste == Voie]))
}
barplot(TpsMoyVoies,names.arg = VoieEntreeSortiePiste, col = "red")


# Distributions -----------------------------------------------------------
Breaks_Roulage = (0:(max(data$RoulageAvecArret, na.rm = TRUE)*1.1/30))*30
Breaks_Arrets = (0:(max(data$TempsArrets, na.rm = TRUE)*1.1/15))*15
Breaks_Decollage = (0:(max(na.omit(data$TempsDecollage))*1.1/5))*5
Breaks_Top = (0:(max(na.omit(data$Top))*1.1/10))*10
data$Top <- as.numeric(data$Top)
data$TempsDecollage <- as.numeric(data$TempsDecollage)

#Creation of Compagnie
data$Compagnie<-substr(data$Indicatif, 1, 3)
data$RoulageAvecArret <- data$TempsRoulage

par(mfcol=c(2,4))
hist(data$RoulageAvecArret[data$Compagnie == "AFR"], breaks = Breaks_Roulage,
xlab = "Temps roulage (s)", main = "Distribution des temps de roulage\n des avions Air France",
col = "blue")
hist(data$RoulageAvecArret[data$Compagnie != "AFR"], breaks = Breaks_Roulage,
xlab = "Temps roulage (s)", main = "Distribution des temps de roulage\n des avions d'une autre compagnie",
col = "red")

hist(data$TempsArrets[data$Compagnie == "AFR"], breaks = Breaks_Arrets,
xlab = "Temps d'arrêt (s)", main = "Distribution des temps d'arrêt\n des avions Air France",
col = "blue")
hist(data$TempsArrets[data$Compagnie != "AFR"], breaks = Breaks_Arrets,
xlab = "Temps d'arrêt (s)", main = "Distribuion des temps d'arrêt\n des avions d'une autre compagnie",
col = "red")

hist(data$TempsDecollage[data$Compagnie == "AFR"], breaks = Breaks_Decollage,
xlab = "Temps de décollage (s)", main = "Distribution des temps de décollage\n des avions Air France",
col = "blue",ylim = c(0,150))
hist(data$TempsDecollage[data$Compagnie != "AFR"], breaks = Breaks_Decollage,
xlab = "Temps de décollage (s)", main = "Distribuion des temps de décollage\n des avions d'une autre compagnie",
col = "red", ylim = c(0,150))

hist(data$Top[data$Compagnie == "AFR"], breaks = Breaks_Top,
xlab = "Temps d'occupation de la piste (s)", main = "Distribution des temps d'occupation de la piste\n des avions Air France",
col = "blue",ylim = c(0,200))
hist(data$Top[data$Compagnie != "AFR"], breaks = Breaks_Top,
xlab = "Temps d'occupation de la piste (s)", main = "Distribuion des temps d'occupation de la piste\n des avions d'une autre compagnie",
col = "red", ylim = c(0,200))


# Polycor USELESS -----------------------------------------------------------------
library(polycor)
polyserial(data$RoulageAvecArret,data$Compagnie)

Variables = c("VoieEntreeSortiePiste", "Compagnie", "Morisation","Qfu",
"Mouvement","Heures","NbArrets","TypeAvion","TempsArrets",
"RoulageSansArret","Pai") 

RoulageAvecArret = c(polyserial(data$RoulageAvecArret,data$VoieEntreeSortiePiste),
polyserial(data$RoulageAvecArret,data$Compagnie),
polyserial(data$RoulageAvecArret,data$Motorisation),
polyserial(data$RoulageAvecArret,data$Qfu),
polyserial(data$RoulageAvecArret,data$Mouvement),
polyserial(data$RoulageAvecArret,data$Heures),
polyserial(data$RoulageAvecArret,data$NbArrets),
polyserial(data$RoulageAvecArret,data$TypeAvion),
polyserial(data$RoulageAvecArret,data$TempsArrets),
polyserial(data$RoulageAvecArret,data$RoulageSansArret),
polyserial(data$RoulageAvecArret,data$Pai))

RoulageSansArret = c(polyserial(data$RoulageSansArret,data$VoieEntreeSortiePiste),
polyserial(data$RoulageSansArret,data$Compagnie),
polyserial(data$RoulageSansArret,data$Motorisation),
polyserial(data$RoulageSansArret,data$Qfu),
polyserial(data$RoulageSansArret,data$Mouvement),
polyserial(data$RoulageSansArret,data$Heures),
polyserial(data$RoulageSansArret,data$NbArrets),
polyserial(data$RoulageSansArret,data$TypeAvion),
polyserial(data$RoulageSansArret,data$TempsArrets),
polyserial(data$RoulageSansArret,data$RoulageSansArret),
polyserial(data$RoulageSansArret,data$Pai))

TempsArrets = c(polyserial(data$TempsArrets,data$VoieEntreeSortiePiste),
polyserial(data$TempsArrets,data$Compagnie),
polyserial(data$TempsArrets,data$Motorisation),
polyserial(data$TempsArrets,data$Qfu),
polyserial(data$TempsArrets,data$Mouvement),
polyserial(data$TempsArrets,data$Heures),
polyserial(data$TempsArrets,data$NbArrets),
polyserial(data$TempsArrets,data$TypeAvion),
polyserial(data$TempsArrets,data$TempsArrets),
polyserial(data$TempsArrets,data$RoulageSansArret),
polyserial(data$TempsArrets,data$Pai))

NbArrets = c(polyserial(data$NbArrets,data$VoieEntreeSortiePiste),
polyserial(data$NbArrets,data$Compagnie),
polyserial(data$NbArrets,data$Motorisation),
polyserial(data$NbArrets,data$Qfu),
polyserial(data$NbArrets,data$Mouvement),
polyserial(data$NbArrets,data$Heures),
polyserial(data$NbArrets,data$NbArrets),
polyserial(data$NbArrets,data$TypeAvion),
polyserial(data$NbArrets,data$TempsArrets),
polyserial(data$NbArrets,data$RoulageSansArret),
polyserial(data$NbArrets,data$Pai))


PolyCorr = data.frame(Variables, RoulageAvecArret,RoulageSansArret, TempsArrets,NbArrets)


# Nouvelles Variables ------------------------------------------------------
Data$Compagnie <- substr(Data$Indicatif,1,3)

Data$AF <- NA
for (i in 1:length(Data$Compagnie)){
if (Data$Compagnie[i] == "AFR"){
Data$AF[i] <- TRUE}
else {
Data$AF[i] <- FALSE}
}


Arret <- c()
for (i in 1:length(data$NbArrets)){
if (data$NbArrets[i] > 0){
Arret <- c(Arret,TRUE)}
else {
Arret <- c(Arret,FALSE)}
}
data$Arret <- Arret

data$NbMoteurs <- substr(data$Motorisation,2,2)
data$Piste <- substr(data$Qfu,1,2)
data$Orientation <- substr(data$Qfu,3,3)
data$Heures <- substr(data$HeureTu,1,2)

TempsArrets <- c()
NbVols <- length(data$Indicatif)
for (Vol in 1:NbVols){
    TempsArretsVol <- 0
    if (data$NbArrets[Vol] != 0){
        for (IndiceArret in 1:data$NbArrets[Vol]){
            Arret <- 49 + 3*(IndiceArret-1)
            TempsArretsVol <- TempsArretsVol + as.numeric(data[Vol,Arret])}}
    TempsArrets <- c(TempsArrets,TempsArretsVol)}
data$TempsArrets <- TempsArrets
data$RoulageAvecArret <- as.numeric(data$RoulageSansArret) + data$TempsArrets


# Premières Statistiques --------------------------------------------------------
data = apply_labels(data, 
AF = c("Air France" = TRUE, "Autres compagnies" = FALSE),
RoulageSansArret = "Temps de roulage sans arr?t(s)",
RoulageAvecArret = "Temps de roulage avec arr?t(s)",
NbArrets = "Nombre d'arr?ts",
TempsArrets = "Temps d'arr?t (s)",
DistanceRoulage = "Distance de roulage (NM)",
VitesseMoyRoulage = "Vitesse moyenne de roulage (nd)",
Top = "Temps d'occupation de la piste (s)", 
Arret = c("Sans Arret" = FALSE, "Avec Arret" = TRUE)
)

data %>% 
    tab_cells(data$Top,data$RoulageSansArret, data$RoulageAvecArret, data$TempsArrets, data$NbArrets,  data$DistanceRoulage,data$VitesseMoyRoulage) %>%
    tab_cols(total(label = "#Total| |"), data$AF) %>% 
    tab_stat_fun(Moyenne = w_mean, "M?diane" = w_median, "Min" = w_min, "Max" = w_max, "Std. dev." = w_sd, "Effectif" = w_n, method = list) %>%
    tab_pivot()

data %>% 
    tab_cells(data$Top,data$RoulageSansArret, data$RoulageAvecArret, data$TempsArrets, data$NbArrets,  data$DistanceRoulage,data$VitesseMoyRoulage) %>%
    tab_cols(total(label = "#Total| |"), data$Arret) %>% 
    tab_stat_fun(Moyenne = w_mean, "M?diane" = w_median, "Min" = w_min, "Max" = w_max, "Std. dev." = w_sd, "Effectif" = w_n, method = list) %>%
    tab_pivot()

dataArret %>% 
    tab_cells(dataArret$Top,dataArret$RoulageSansArret, dataArret$RoulageAvecArret, dataArret$TempsArrets, dataArret$NbArrets,  dataArret$DistanceRoulage,dataArret$VitesseMoyRoulage) %>%
    tab_cols(total(label = "#Total| |"), dataArret$AF) %>% 
    tab_stat_fun(Moyenne = w_mean, "M?diane" = w_median, "Min" = w_min, "Max" = w_max, "Std. dev." = w_sd, "Effectif" = w_n, method = list) %>%
    tab_pivot()

dataSansArret %>% 
    tab_cells(dataSansArret$Top,dataSansArret$RoulageSansArret, dataSansArret$RoulageAvecArret, dataSansArret$TempsArrets, dataSansArret$NbArrets,  dataSansArret$DistanceRoulage,dataSansArret$VitesseMoyRoulage) %>%
    tab_cols(total(label = "#Total| |"), dataSansArret$AF) %>% 
    tab_stat_fun(Moyenne = w_mean, "M?diane" = w_median, "Min" = w_min, "Max" = w_max, "Std. dev." = w_sd, "Effectif" = w_n, method = list) %>%
    tab_pivot()


data$DateHeure <- strptime(paste(data$DateTu,data$HeureTu, sep = ":"),
format='%d/%m/%Y:%H:%M:%S')
hist(data$DateHeure, "hours", freq = TRUE, main = "Distribution temporelle des mouvements")

# Vols avec le même indicatif -------------------------------------------

Vols_Double <- c("ABR7CK", "AFR584","AFR454","FDX5124", "FDX5393", "FDX7", "PIA750", "SRR6315", "SRR6316", "TAY826H", "UPS218", "UPS219")

for (Vol in Vols_Double){
Deja_vu <- FALSE
Premier_vol <- FALSE
i <- 1
while (Deja_vu == FALSE){
if (data$Indicatif[i] == Vol){
if (Premier_vol == FALSE){
Premier_vol <- TRUE}
else{
Deja_vu <- TRUE
data$Indicatif[i] <- paste(data$Indicatif[i],"-", sep = "")}
}
i <- i+1}
}

# Fonction "Statistiques" ---------------------------------------------------
Statistiques <- function(Data,Pivot,Variables = names(Data)){
Valeurs_Pivot <- unique(Data[[Pivot]])
List_Labels <- Labels(Data,Variables)
for (Val_Pivot in Valeurs_Pivot){
cat("\n","Statistiques pour",var_lab(Data[[Pivot]]),"= ", Val_Pivot,"(effectif = ",length(Data[[Variables[1]]][Data[[Pivot]]== Val_Pivot]),")","\n")
moyenne <- c()
std <- c()
fd <- c()
fq <- c()
mediane <- c()
lq <- c()
ld <- c()
for (Variable in Variables){
moyenne <- c(moyenne,mean(Data[[Variable]][Data[[Pivot]]== Val_Pivot]))
std <- c(std,sd(Data[[Variable]][Data[[Pivot]]== Val_Pivot]))
fd <- c(fd,quantile(Data[[Variable]][Data[[Pivot]]== Val_Pivot],.1))
fq <- c(fq,quantile(Data[[Variable]][Data[[Pivot]]== Val_Pivot],.25))
mediane <- c(mediane,median(Data[[Variable]][Data[[Pivot]]== Val_Pivot]))
lq <- c(lq,quantile(Data[[Variable]][Data[[Pivot]]== Val_Pivot],.75))
ld <- c(ld,quantile(Data[[Variable]][Data[[Pivot]]== Val_Pivot],.9))
}
Stats <- data.frame(moyenne,std,fd,fq,mediane,lq,ld)
names(Stats) <- c("Moyenne","Ecart type", "Premier décile", "Premier quartile",
"Médiane", "Troisième quartile", "Neuvième décile")
row.names(Stats) <- List_Labels
print(Stats)
}
}
# DataClustering ----------------------------------------------------------
DataClustering <- data.frame(as.numeric(data$TempsRoulage),data$AF,data$TempsArrets,
as.numeric(data$NbArrets),as.numeric(data$NbMoteurs), 
as.numeric(data$DistanceRoulage), as.numeric(data$VitesseMoyRoulage),
as.numeric(data$Top), data$Arret, as.numeric(data$Heures), 
data$Orientation, data$Piste)

names(DataClustering) <- c("TempsRoulage","AF","TempsArrets","NbArrets",
"NbMoteurs","DistanceRoulage","VitesseMoyRoulage","Top","Arret","Heures",
"Orientation","Piste")


DataClustering$AF <- as.numeric(DataClustering$AF)
DataClustering$Arret <- as.numeric(DataClustering$Arret)
DataClustering$Orientation[data$Orientation == "L"] <- -1
DataClustering$Orientation[data$Orientation == "R"] <- 1
DataClustering$Orientation <- as.numeric(DataClustering$Orientation)
DataClustering$Piste[data$Piste == "26"] <- -1
DataClustering$Piste[data$Piste == "27"] <- 1
DataClustering$Piste <- as.numeric(DataClustering$Piste)


Compagnies <- unique(data$Compagnie)
i <- 1
Num_Rand <- sample(1:length(Compagnies),length(Compagnies))
CompagnieNum <- c(1:length(data$Compagnie))
for (Compagnie in Compagnies){
CompagnieNum[data$Compagnie == Compagnie] <- Num_Rand[i]
i <- i+1
}
DataClustering$CompagnieNum <- CompagnieNum

DataClustering = apply_labels(DataClustering, 
AF = "Air France?",
AF = c("Air France" = 1, "Autres compagnies" = 0),
TempsRoulage = "Temps de roulage sans arrêt(s)",
TempsArrets = "Temps d'arrêt(s)",
NbArrets = "Nombre d'arrêts",
NbMoteurs = "Nombre de moteurs",
DistanceRoulage = "Distance de roulage (NM)",
VitesseMoyRoulage = "Vitesse moyenne de roulage (nd)",
Top = "Temps d'occupation de la piste (s)", 
Arret = "Arrêt?",
Arret = c("Sans Arrêt" = 0, "Avec Arrêt" = 1),
Heures = "Heure du mouvement",
Orientation = "Orientation",
Orientation = c("L" = -1,"R" = 1),
Piste = "Piste utilisée",
Piste = c("26" = -1, "27" = 1),
CompagnieNum = "Code numérique Compagnie"
)
row.names(DataClustering) <- data$Indicatif

# Fonction "Labels" ---------------------------------------------------------
Labels <- function(Data,Variables){
List_Labels <- c()
for (i in Variables){
List_Labels <- c(List_Labels, var_lab(Data[[i]]))
}
return (List_Labels)
}

# Importation Fichiers ----------------------------------------------------
setwd("~/Projet Jumeau Numérique/export/export")
library(readr)
dir <- getwd()

List_days <- list.files(path = dir, pattern = "2018")
Data_2018 <- read_tsv(file = List_days[1], skip = 1)
for (Day in 2:length(List_days)){
    Data_Day <- read_tsv(file = List_days[Day], skip = 1)
    Data_2018 <- rbind(Data_2018, Data_Day)
}

List_days <- list.files(path = dir, pattern = ".sol")
Data <- read_tsv(file = List_days[1], skip = 1)
for (Day in 2:length(List_days)){
    Data_Day <- read_tsv(file = List_days[Day], skip = 1)
    Data <- rbind(Data, Data_Day)
}

# Nettoyage des données ---------------------------------------------------
Data_2018$TempsRoulage[Data_2018$TempsRoulage > 30000] <- 86400 - Data_2018$TempsRoulage[Data_2018$TempsRoulage > 30000]
Data_2018$Top[Data_2018$Top > 30000] <- 86400 - Data_2018$Top[Data_2018$Top > 30000]
Data_2018$TempsArretTotal[Data_2018$TempsArretTotal < 0] <- Data_2018$TempsArretTotal[Data_2018$TempsArretTotal < 0] + 86400



Data_2018 = apply_labels(Data_2018, 
                              AF = "Air France",
                              AF = c("Air France" = TRUE, "Autres compagnies" = FALSE),
                              TempsRoulage = "Temps de roulage sans arrêt(s)",
                              NbArrets = "Nombre d'arrêts",
                              DistanceRoulage = "Distance de roulage (NM)",
                              VitesseMoyRoulage = "Vitesse moyenne de roulage (nd)",
                              Top = "Temps d'occupation de la piste (s)",
                              TempsArretTotal = "Temps total d'arrêt (s)"
)


Data$TempsRoulage[Data$TempsRoulage > 30000] <- 86400 - Data$TempsRoulage[Data$TempsRoulage > 30000]
Data$Top[Data$Top > 30000] <- 86400 - Data$Top[Data$Top > 30000]
Data$TempsArretTotal[Data$TempsArretTotal < 0] <- Data$TempsArretTotal[Data$TempsArretTotal < 0] + 86400
Data$TempsAttentePiste[Data$TempsAttentePiste<0] <- Data$TempsAttentePiste[Data$TempsAttentePiste<0] + 86400

Data = apply_labels(Data, 
                         AF = "Air France",
                         AF = c("Air France" = TRUE, "Autres compagnies" = FALSE),
                         TempsRoulage = "Temps de roulage (s)",
                         NbArrets = "Nombre d'arrêts",
                         DistanceRoulage = "Distance de roulage (NM)",
                         VitesseMoyRoulage = "Vitesse moyenne de roulage (nd)",
                         Top = "Temps d'occupation de la piste (s)",
                         TempsArretTotal = "Temps total d'arrêt (s)"
)


# Heures ------------------------------------------------------------------
Heure = as.difftime("07:00:00")
Data_HEURE = subset.data.frame(Data, subset = Data$HeureTu < Heure)
# Plot 2D -----------------------------------------------------------------
d <- ggplot(Data, aes(HeureTu,TempsAttentePiste))

d + geom_bin2d(binwidth = c(1,10)) + scale_fill_continuous(type = "viridis") + theme_bw()
d + stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")



# Avions sur les voies ----------------------------------------------------

Data$HeureStart[Data$Mouvement == "Atterrissage"] <- Data$HeureTu[Data$Mouvement == "Atterrissage"]
Data$HeureStart[Data$Mouvement != "Atterrissage"] <- 
    Data$HeureTu[Data$Mouvement != "Atterrissage"] - (Data$Top[Data$Mouvement != "Atterrissage"] + Data$TempsAttente1[Data$Mouvement != "Atterrissage"]
                                                      + Data$TempsAttente2[Data$Mouvement != "Atterrissage"] + Data$TempsAttente3[Data$Mouvement != "Atterrissage"]
                                                      + Data$TempsArretTotal[Data$Mouvement != "Atterrissage"] + Data$TempsAttentePiste[Data$Mouvement != "Atterrissage"]
                                                      + Data$TempsRoulage[Data$Mouvement != "Atterrissage"] + Data$TempsTraversee[Data$Mouvement != "Atterrissage"])
                                                      

Data$HeureStop[Data$Mouvement == "Atterrissage"] <- 
    Data$HeureTu[Data$Mouvement == "Atterrissage"] + (Data$Top[Data$Mouvement == "Atterrissage"] + Data$TempsTraversee[Data$Mouvement == "Atterrissage"]
                                                      + Data$TempsAttente1[Data$Mouvement == "Atterrissage"] + Data$TempsAttente2[Data$Mouvement == "Atterrissage"]
                                                      + Data$TempsAttente3[Data$Mouvement == "Atterrissage"] + Data$TempsArretTotal[Data$Mouvement == "Atterrissage"]
                                                      + Data$TempsRoulage[Data$Mouvement == "Atterrissage"] )

Data$HeureStop[Data$Mouvement != "Atterrissage"] <- Data$HeureTu[Data$Mouvement != "Atterrissage"]

NbAvionsVoies<- function(Data,TimeStep,Start = 0, Stop = 86400){
    NbSteps = (Stop-Start)/TimeStep
    NbAvions <- data.frame(1:NbSteps,1:NbSteps)
    names(NbAvions) <- c("Heure","Nb")
    for (i in 0:(NbSteps-1)){
        min = Start + i*TimeStep
        max = min + TimeStep 
        NbAvions$Heure[i+1] <- (min+max)/2
        avionsvoies <- Data$Indicatif[(Data$HeureStart < min  &  max < Data$HeureStop) 
                                      | (min < Data$HeureStart & Data$HeureStart < max) 
                                      | (min < Data$HeureStop & Data$HeureStop < max)]
        NbAvions$Nb[i+1] <- length(avionsvoies)
    }
    return(NbAvions)
}

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



# Plot Congestion--------------------------------------------------------------------
AvionsVoies <- NbAvionsVoies(Data,60)
TempsMoyAttente <- AttenteMoy(Data,60)
TempsMoyPiste <- aggregate(Data$TempsAttentePiste, by = list(as.integer(Data$HeureTu/60)),mean)
names(TempsMoyPiste) <- c("Heure", "TempsMoy")
TempsMoyPiste$Heure <- TempsMoyPiste$Heure*60

par(mar = c(5, 4, 4, 5))
plot(AvionsVoies$Heure,AvionsVoies$Nb/790, ylab = "Nombre moyen d'avions sur les voies", xlab = "Heure")
par(new = TRUE)
plot(TempsMoyAttente$Heure,TempsMoyAttente$TempsAttenteMoy, type = "p", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red")
axis(side = 4, col = "red", col.axis = "red")
mtext("Temps d'attente moyen (s)", side=4, line=2.5, col = "red")

