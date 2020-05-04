# Titre: Modelisation de la dynamique epidemique du COVID-19 : 
# Estimations des taux d'infection des populations et 
# des dates de fin des vagues epidemiques dans differents pays
# Auteurs: Patrick Gasqui, Maude Jacquot
# Cree le 14 Avril 2020
# Derniere modification le 04 Mai 2020
# 
# Source de donnees: https://github.com/CSSEGISandData/COVID-19
# Periode consideree: du 22/01/2020 au 03/05/2020
# 
# Variables cles:
#  cas confirmes:	xcasconf
#  cas decedes:		xcasdece
#  cas gueris:		xcasguer


####
# Chargement des packages et fonctions
####
#require(deSolve)
source("FunctionCovidVisuMonde.R")
source("FunctionCovidModelEstim.R")

# Intinialisation de la date de fin de l'analyse
vdatFic <- c("0305")
vdatfin <- c("03-05-2020")

# paramètre d'affichage
options(warn=-1)

############################################
# Visualisation des observations pour le Monde
############################################

vpays   <- c("Monde")
vtaillepop <- 7530.0	# en million d'habitants
FctVisuCovidMonde(vpays,vdatfin,vtaillepop,vdatFic)

############################################
# Modelisation
############################################

# Creation du fichier de sortie resume
vnomFicOutResnum <- paste("ResNumParPays",".txt",sep="")
sink(vnomFicOutResnum,append=FALSE)
cat("Pays")
cat(";","jour debut",";","date debut")
cat(";","jour crois",";","date crois")
cat(";","jour Za0.01",";","date Za0.01",";","duree epid. en jour")
cat(";","jour Za0.001",";","date Za0.001",";","duree epid. en jour")
cat(";","prop. act. deces",";","prop. fin. deces")
cat(";","taux deces p1",";","taux guerison p2",";","prop. cas confirmes/pop.tot. en pour mille")
cat(";","taux inf. pop. en % pour TDR=0.005",";","taux inf. pop. en % pour TDR=0.010",";","taux inf. pop. en % pour TDR=0.020")
cat("\n")
sink()

# Initialisation du nombre de jour a prendre en compte pour les estimations (vague epidemique incomplete)
vnjestim <- 10 # les 10 derniers jours

####
## Analyse pour l'Autriche
####

vpays      <- c("Austria")
vregion    <- c("")
vindJfin   <- 200
vtaillepop <- 8.8	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour l'Espagne
####

vpays      <- c("Spain")
vregion    <- c("")
vindJfin   <- 250
vtaillepop <- 49.3	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour l'Italie
####

vpays      <- c("Italy")
vregion    <- c("")
vindJfin   <- 350
vtaillepop <- 60.4	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour la France
####

vpays      <- c("France")
vregion    <- c("")
vindJfin   <- 300
vtaillepop <- 67.1	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour l'Allemagne
####

vpays      <- c("Germany")
vregion    <- c("")
vindJfin   <- 200
vtaillepop <- 80.5	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour la Belgique
####

vpays      <- c("Belgium")
vregion    <- c("")
vindJfin   <- 300
vtaillepop <- 11.6	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour la Suisse
####

vpays      <- c("Switzerland")
vregion    <- c("")
vindJfin   <- 200
vtaillepop <- 8.3	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour le Danemark
####

vpays      <- c("Denmark")
vregion    <- c("")
vindJfin   <- 200
vtaillepop <- 5.8	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour le Canada
####

vpays      <- c("Canada")
vregion    <- c("")
vindJfin   <- 200
vtaillepop <- 35.9	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour les Etats-Unis
####

vpays      <- c("US")
vregion    <- c("")
vindJfin   <- 300
vtaillepop <- 329.3	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour le Portugal
####

vpays      <- c("Portugal")
vregion    <- c("")
vindJfin   <- 500
vtaillepop <- 10.4	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour la Suede
####

vpays      <- c("Sweden")
vregion    <- c("")
vindJfin   <- 550
vtaillepop <- 10.0	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour les Pays-Bas 
## Probleme des guerisons ???
####

vpays      <- c("Netherlands")
vregion    <- c("")
vindJfin   <- 500
vtaillepop <- 17.2	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)

####
## Analyse pour la Grande-Bretagne 
## Probleme des guerisons ???
####

vpays      <- c("United Kingdom")
vregion    <- c("")
vindJfin   <- 500
vtaillepop <- 65.1	# en million d'habitants
res <- FunctionRCovidModelEstim(vpays,vregion,vdatvindJfin,vnjestim,vtaillepop,vdatFic,vnomFicOutResnum)


