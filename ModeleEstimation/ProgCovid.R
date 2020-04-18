#' Titre: Modélisation de la dynamique épidémique du COVID-19 : Estimations des taux d’infection des populations et des dates de fin des vagues épidémiques dans différents pays
#' Auteurs: Patrick Gasqui, Maude Jacquot
#' Créé le 14 Avril 2020
#' Derniere modification le 18 Avril 2020
#' 
#' Source de données: https://github.com/CSSEGISandData/COVID-19
#' Période considérée: du 22/01/2020 au 13/04/2020
#' 
#' Variables clés:
#'  cas confirmés:	xcasconf
#'  cas décédés:		xcasdece
#'  cas guéris:		  xcasguer


####
# Chargement des packages et fonctions
####
require(deSolve)
source("FunctionCovidModelEstim.R")

############################################
# Modélisation
############################################

# Création du fichier de sortie résumé
nomFicOutResnum <- paste("ResNumParPays",".txt",sep="")
sink(nomFicOutResnum,append=FALSE)
cat("\n")
sink()

# Intinialisation de la date de fin de l'analyse
vdatFic <- c("1304")
vdatfin <- c("13-04-2020")

# Initialisation du nombre de jour a prendre en compte pour les estimations (vague épidémique incomplete)
vnjestim <- 10 # les 10 derniers jours

####
## Analyse pour l'Espagne
####

vpays   <- c("Spain")
vregion <- c("")
vindJdebut <- 42
vindJfin <- 200
vtaillepop <- 49.3	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta)

# Ajout des résultats au fichier de sortie
  sink(nomFicOutResnum,append=TRUE)
  cat(vpays,"\n")
  cat("\t",res[[1]][[1]],"\t","\t",paste(res[[1]][[2]],sep=""),"\n")
  cat("\t",res[[2]][[1]],"\t","\t",paste(res[[2]][[2]],sep=""),"\n")
  cat("\t",res[[3]][[1]],"\t","\t",paste(res[[3]][[2]],sep=""),"\t",res[[3]][[3]],"\n")
  cat("\t",res[[4]][[1]],"\t","\t",paste(res[[4]][[2]],sep=""),"\t",res[[4]][[3]],"\n")
  cat("\t",paste(round(as.numeric(res[[5]][1])*1000)/1000,sep=""),"\t","\t",paste(round(as.numeric(res[[5]][2])*1000)/1000,sep=""),"\n")
  cat("\t",paste(round(as.numeric(res[[6]][1])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[6]][2])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[6]][3])*1000)/1000,sep=""),"\n")
  cat("\t",paste(round(as.numeric(res[[7]][[1]][1])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[7]][[1]][2])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[7]][[1]][3])*1000)/1000,sep=""),"\n")
  cat("\n")
  sink()

####
## Analyse pour l'Italie
####

vpays   <- c("Italy")
vregion <- c("")
vindJdebut <- 20
vindJfin <- 300
vtaillepop <- 60.4	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta)

# Ajout des résultats au fichier de sortie
  sink(nomFicOutResnum,append=TRUE)
  cat(vpays,"\n")
  cat("\t",res[[1]][[1]],"\t","\t",paste(res[[1]][[2]],sep=""),"\n")
  cat("\t",res[[2]][[1]],"\t","\t",paste(res[[2]][[2]],sep=""),"\n")
  cat("\t",res[[3]][[1]],"\t","\t",paste(res[[3]][[2]],sep=""),"\t",res[[3]][[3]],"\n")
  cat("\t",res[[4]][[1]],"\t","\t",paste(res[[4]][[2]],sep=""),"\t",res[[4]][[3]],"\n")
  cat("\t",paste(round(as.numeric(res[[5]][1])*1000)/1000,sep=""),"\t","\t",paste(round(as.numeric(res[[5]][2])*1000)/1000,sep=""),"\n")
  cat("\t",paste(round(as.numeric(res[[6]][1])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[6]][2])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[6]][3])*1000)/1000,sep=""),"\n")
  cat("\t",paste(round(as.numeric(res[[7]][[1]][1])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[7]][[1]][2])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[7]][[1]][3])*1000)/1000,sep=""),"\n")
  cat("\n")
  sink()

####
## Analyse pour la France
####
  
vpays   <- c("France")
vregion <- c("")
vindJdebut <- 35
vindJfin <- 250
vtaillepop <- 67.1	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta)

# Ajout des résultats au fichier de sortie
  sink(nomFicOutResnum,append=TRUE)
  cat(vpays,"\n")
  cat("\t",res[[1]][[1]],"\t","\t",paste(res[[1]][[2]],sep=""),"\n")
  cat("\t",res[[2]][[1]],"\t","\t",paste(res[[2]][[2]],sep=""),"\n")
  cat("\t",res[[3]][[1]],"\t","\t",paste(res[[3]][[2]],sep=""),"\t",res[[3]][[3]],"\n")
  cat("\t",res[[4]][[1]],"\t","\t",paste(res[[4]][[2]],sep=""),"\t",res[[4]][[3]],"\n")
  cat("\t",paste(round(as.numeric(res[[5]][1])*1000)/1000,sep=""),"\t","\t",paste(round(as.numeric(res[[5]][2])*1000)/1000,sep=""),"\n")
  cat("\t",paste(round(as.numeric(res[[6]][1])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[6]][2])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[6]][3])*1000)/1000,sep=""),"\n")
  cat("\t",paste(round(as.numeric(res[[7]][[1]][1])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[7]][[1]][2])*10000)/10000,sep=""),"\t",paste(round(as.numeric(res[[7]][[1]][3])*1000)/1000,sep=""),"\n")
  cat("\n")
  sink()

