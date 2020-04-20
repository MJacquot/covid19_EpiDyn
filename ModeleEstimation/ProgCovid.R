# Titre: Modélisation de la dynamique épidémique du COVID-19 : 
# Estimations des taux d'infection des populations et 
# des dates de fin des vagues épidémiques dans différents pays
# Auteurs: Patrick Gasqui, Maude Jacquot
# Créé le 14 Avril 2020
# Dernière modification le 20 Avril 2020
# 
# Source de données: https://github.com/CSSEGISandData/COVID-19
# Période considérée: du 22/01/2020 au 18/04/2020
# 
# Variables clés:
#  cas confirmés:	xcasconf
#  cas décédés:		xcasdece
#  cas guéris:		xcasguer


####
# Chargement des packages et fonctions
####
require(deSolve)
source("FunctionCovidModelEstim.R")

############################################
# Modélisation
############################################

# Création du fichier de sortie résumé
vnomFicOutResnum <- paste("ResNumParPays",".txt",sep="")
sink(vnomFicOutResnum,append=FALSE)
cat("Pays",";","Region")
cat(";","jour debut",";","date debut")
cat(";","jour crois",";","date crois")
cat(";","jour Za0.01",";","date Za0.01",";","duree epid. en jour")
cat(";","jour Za0.001",";","date Za0.001",";","duree epid. en jour")
cat(";","prop. act. deces",";","prop. fin. deces")
cat(";","taux deces p1",";","taux guerison p2",";","prop. cas confirmes/pop.tot. en pour mille")
cat(";","taux inf. pop. en % pour TDR=0.01",";","taux inf. pop. en % pour TDR=0.02",";","taux inf. pop. en % pour TDR=0.03")
cat("\n")
sink()

# Intinialisation de la date de fin de l'analyse
vdatFic <- c("1804")
vdatfin <- c("18-04-2020")

# Initialisation du nombre de jour a prendre en compte pour les estimations (vague épidémique incomplète)
vnjestim <- 10 # les 10 derniers jours

####
## Analyse pour l'Autriche
####

vpays   <- c("Austria")
vregion <- c("")
vindJdebut <- 45	# 55 	
vindJfin <- 200
vtaillepop <- 8.8	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour l'Espagne
####

vpays   <- c("Spain")
vregion <- c("")
vindJdebut <- 35	# 40
vindJfin <- 200
vtaillepop <- 49.3	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour l'Italie
####

vpays   <- c("Italy")
vregion <- c("")
vindJdebut <- 15	# 20
vindJfin <- 350
vtaillepop <- 60.4	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour la France
####

vpays   <- c("France")
vregion <- c("")
vindJdebut <- 35	# 45
vindJfin <- 250
vtaillepop <- 67.1	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour l'Allemagne
####

vpays   <- c("Germany")
vregion <- c("")
vindJdebut <- 35
vindJfin <- 200
vtaillepop <- 80.5	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour la Belgique
####

vpays   <- c("Belgium")
vregion <- c("")
vindJdebut <- 45	# 40
vindJfin <- 300	# 200
vtaillepop <- 11.6	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour la Suisse
####

vpays   <- c("Switzerland")
vregion <- c("")
vindJdebut <- 45	# 50
vindJfin   <- 200
vtaillepop <- 8.3	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour le Danemark
####

vpays   <- c("Denmark")
vregion <- c("")
vindJdebut <- 38
vindJfin   <- 200
vtaillepop <- 5.8	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour la Suède
####

vpays   <- c("Sweden")
vregion <- c("")
vindJdebut <- 50	
vindJfin   <- 500
vtaillepop <- 10.0	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour les Etats-Unis
####

vpays   <- c("US")
vregion <- c("")
vindJdebut <- 50
vindJfin   <- 300	# 500
vtaillepop <- 329.3	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour le Portugal
####

vpays   <- c("Portugal")
vregion <- c("")
vindJdebut <- 41
vindJfin   <- 500
vtaillepop <- 10.4	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour le Canada
####

vpays   <- c("Canada")
vregion <- c("")
vindJdebut <- 40	# 55
vindJfin <- 200
vtaillepop <- 35.9	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour les Pays-Bas 
## Problème des guérisons ???
####

vpays   <- c("Netherlands")
vregion <- c("")
vindJdebut <- 45	
vindJfin   <- 500
vtaillepop <- 17.2	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

####
## Analyse pour le Royaume-Uni
## Problème des guérisons ???
####

vpays   <- c("United Kingdom")
vregion <- c("")
vindJdebut <- 42
vindJfin   <- 500
vtaillepop <- 65.1	# en million d'habitants
vdelta <- 0
res <- FunctionRCovidModelEstim(vpays,vregion,vdatfin,vindJdebut,vindJfin,vnjestim,vtaillepop,vdatFic,vdelta,vnomFicOutResnum)

