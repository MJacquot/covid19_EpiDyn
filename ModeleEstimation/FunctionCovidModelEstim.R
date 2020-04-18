#' Fonction necessaire a l'estimation des parametres du modele covid-19
#' Auteurs: Patrick Gasqui, Maude Jacquot
#' Créé le 10 Avril 2020
#' Derniere modification le 18 Avril 2020

# Estimation des parametres du modele et visualisation des résultats et des données pour un pays donn
FunctionRCovidModelEstim <- function(vxpays,vxregion,vxdatfin,vxindJdebut,vxindJfin,vxnjestim,vxtaillepop,vxdatFic,vxdelta) {
  
  # Initialisation
  vpays <- vxpays
  if (vpays=="Spain") {nom_pays="l'Espagne"}
  else if (vpays=="Italy") {nom_pays="l'Italie"}
  else if (vpays=="France") {nom_pays="la France"}
  else { nom_pays=vpays }
  vregion <- vxregion
  vdatfin <- vxdatfin
  vindJdebut <- vxindJdebut
  vindJfin <- vxindJfin
  vnjestim <- vxnjestim
  vtaillepop <- vxtaillepop
  vdatFic <- vxdatFic 
  vdelta <- vxdelta
  vdatdeb <- c("22-01-2020")
  vlegend <- c("Confirmés","Décédés","Guéris","Actifs")
  
  # chargement du nombre de cas confirmés cumulés par jour 
  nomFicIn <- paste("covidCCO",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  #dim(xdatCSV)						# 253  71
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  selj <- ((xdatCSV[,2]==vpays)&(xdatCSV[,1]==vregion))
  xcasconf <- as.numeric(xdatCSV[selj,c(5:length(nomCol))])
  #length(xcasconf)					# 67

  # rapport du nombre de "cas confirmés" sur la taille de la population (exprimée en million) en % pour 1000 habitants
  xpropnbrepopulation <- (xcasconf[nbrejour]/(vtaillepop*1000000))*1000

  # chargement du nombre de cas guéris cumulés par jour 
  nomFicIn <- paste("covidCGU",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  #dim(xdatCSV)						# 253  71
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  #nbrejour						# 67
  selj <- ((xdatCSV[,2]==vpays)&(xdatCSV[,1]==vregion))
  xcasguer <- as.numeric(xdatCSV[selj,c(5:length(nomCol))])
  #length(xcasguer)					# 67
  
  # chargement du nombre de cas décédés cumulés par jour 
  nomFicIn <- paste("covidCMO",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  #dim(xdatCSV)						# 253  71
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  #nbrejour						# 67
  selj <- ((xdatCSV[,2]==vpays)&(xdatCSV[,1]==vregion))
  xcasdece <- as.numeric(xdatCSV[selj,c(5:length(nomCol))])
  #length(xcasdece)					# 67
  
  # évaluation de la proportion de la population "infectée"
  # rapport du nombre d'individus "infectés" sur la taille de la population (exprimée en million) en % pour 1000 habitants
  xtfd <- c(0.01,0.02,0.03)
  xZT2b <- (xcasdece[nbrejour] - (xtfd*(xcasconf[nbrejour])))/xtfd
  xNpinf <- ((xcasconf[nbrejour]+xZT2b)/(vtaillepop*1000000))*1000

  xcasacti <- xcasconf - xcasguer - xcasdece
  #range(xcasacti)					# 0 46638
  
  # Export graphique
  nomFicGra <- paste("GraphPDF-COVID19-",vpays,"-",vdatfin,"-ModEst.pdf",sep="")
  if ( vregion != c("") ) {
    nomFicGra <- paste("GraphPDF-COVID19-",vpays,"-",vregion,"-",vdatfin,"-ModEst.pdf",sep="")
    }
  pdf(file=nomFicGra,paper="a4r",height=7,width=10)
  
  nbrejour <- length(xcasconf)	
  x <- 1:nbrejour
  y <- xcasconf
  vtext <- paste("Données pour ",nom_pays," du ",vdatdeb," au ",vdatfin,
               "\n avec un maximum de ",max(xcasconf)," cas confirmés et ",max(xcasdece)," décés",sep="")
  if ( vregion != c("") ) {
  vtext <- paste("Données pour ",nom_pays," & ",vregion," du ",vdatdeb," au ",vdatfin,
               "\n avec un maximum de ",max(xcasconf)," cas confirmés et ",max(xcasdece)," décés",sep="")
    }
  plot(x,y,ylim=range(c(0,y)),type="n",xlab="jour",ylab="nombre de cas cumulés",
     main=vtext)
  lines(x,xcasconf,lty=1,col=1,lwd=2)
  lines(x,xcasdece,lty=1,col=2,lwd=2)
  lines(x,xcasguer,lty=1,col=3,lwd=2)
  lines(x,xcasacti,lty=1,col=4,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)
  legend("topleft",legend=vlegend,col=c(1,2,3,4),lty=rep(1,4),lwd=rep(2,4),cex=1.0,bg="white")
  
  # Proportions
  x <- 1:nbrejour
  selk <- (xcasconf!=0.0)
  nbrejour <- length(xcasconf)	
  xpcasdece <- rep(0,nbrejour)
  xpcasguer <- rep(0,nbrejour)
  xpcasacti <- rep(0,nbrejour)
  xpcasdece[selk] <- xcasdece[selk]/xcasconf[selk]
  xpcasguer[selk] <- xcasguer[selk]/xcasconf[selk]
  xpcasacti[selk] <- xcasacti[selk]/xcasconf[selk]
  y <- xpcasacti
  vtext <- paste("Données pour ",nom_pays," du ",vdatdeb," au ",vdatfin,
                "\n Proportion de cas actifs ou guéris ou décédés / cas confirmés",sep="")
  if ( vregion != c("") ) {
    vtext <- paste("Données pour ",nom_pays," & ",vregion," du ",vdatdeb," au ",vdatfin,
                 "\n Proportion de cas actifs ou guéris ou décédés / cas confirmés",sep="")
    }
  plot(x,y,ylim=range(c(0,1)),type="n",xlab="jour",ylab="proportion de cas / cas confirmés",
       main=vtext)
  abline(h=1,lty=2,col=1,lwd=1)
  lines(x,xpcasdece,lty=1,col=2,lwd=2)
  lines(x,xpcasguer,lty=1,col=3,lwd=2)
  lines(x,xpcasacti,lty=1,col=4,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)
  legend("left",legend=vlegend[-1],col=c(2,3,4),lty=rep(1,3),lwd=rep(2,3),cex=1.0,bg="white")
  legend(0,((xpcasdece[nbrejour])+0.1),legend=paste(round((xpcasdece[nbrejour])*1000)/1000,sep=""),col=2,cex=1.0,
         text.col=2, box.col=2, box.lwd=2,bg="white")
  
  ############################################
  # Fonction de calcul du modéle de SEDO
  fctModelSEDO <- function( px, xcode ) { 
    
    kd <- exp(px[1])	# pour les "cas décédés"
    kg <- exp(px[2])	# pour les "cas guéris"
    
    nbJ <- 500

    nbSEDO  <- 3
    my.atol <- rep(1e-9,nbSEDO)
  
    fctModelSEDO <- function(t, y, p ) {
      # parametres locaux
      kds <- p[1]	
      kgs <- p[2]
      # sedo 
      yd1 <- +kds*y[3]	
      yd2 <- +kgs*y[3]*(1-y[3])   
      yd3 <- -(kds*y[3]) - (kgs*y[3]*(1-y[3]))
      #
      res <- c(yd1,yd2,yd3)
      list(res)
      } 
    
    # valeurs initiales
    xt   <- 1:nbJ
    xpD <- c(0.0,rep(0.0,(nbJ-1)))
    xpG <- c(0.0,rep(0.0,(nbJ-1)))
    xpA <- c(1.0,rep(0.0,(nbJ-1)))
    pkp  <- xt[1]
    xppD <- xpD[1]
    xppG <- xpG[1]
    xppA <- xpA[1]
    
    for ( k in 2:nbJ ) {
      
      xstart  <- c(xppD, xppG, xppA) 
      times   <- c(pkp,xt[k])
      parms <- c(kd,kg)
      resSEDO <- rk4(xstart, times, fctModelSEDO, parms )
      xpD[k]   <- resSEDO[2,1+1]
      xpG[k]   <- resSEDO[2,1+2]
      xpA[k]   <- resSEDO[2,1+3]
      pkp <- xt[k]
      xppD <- xpD[k]
      xppG <- xpG[k]
      xppA <- xpA[k]
      }
    
    xpA[(xpA < 0.0)] <- 0.0
    xpG[(xpG < 0.0)] <- 0.0
    xpD[(xpD < 0.0)] <- 0.0
    xpA[(xpA > 1.0)] <- 1.0
    xpG[(xpG > 1.0)] <- 1.0
    xpD[(xpD > 1.0)] <- 1.0
    
    # valeurs estimées 
    if ( xcode == 0 ) {			# xcode == 0
      yres <- rbind(xpD,xpG,xpA)
    } else {				# xcode == 1
      yres <- rbind(xpD,xpG,xpA)
      }
    
    yres
    
    }  # FIN : fctModelPLW
  ############################################
  # Fonction pour le Crit?re des moindres carrés ...
  fctMC2M <- function(px, xDMW) {
    # matrice des données d'entrée :
    # * YJobs1  (les proportions "décédés")
    # * YJobs2  (les proportions "guéris")
    # * YJobs3  (les proportions "actifs")
    # * joursSEDO  (les indices des jours dans l'échelle du SEDO )  
    # * nkJ    (nombre total de jour pour l'estimation)

    YJobs <- xDMW[c(1:3),]
    XJSEDO <- xDMW[4,]
    
    nkJ <- length(XJSEDO)
    
    # fonction d'estimation du modele de SEDO pour tous les jours 
    MYJest <- fctModelSEDO(px,0)
    YJest <- MYJest[,XJSEDO]

    # calcul du critere des moindres carrés
    resMC <- 0.0
    for ( j in 1:nkJ ) {
      for ( i in 1:3 ) {
        y0 <- YJobs[i,j]
        y1 <- YJest[i,j]
        resMC <- resMC + ((y0-y1)*(y0-y1))
        }
    }
    
    # valeur du critere des MC
    resMC
    }  # FIN : fctMC2M

  ############################################
  # modele logistique avec modele de SEDO ...
  nbK <- vindJfin	# nbrejour
  x <- 1:nbrejour
  y <- xpcasacti
  plot(x,y,ylim=range(c(0,1)),type="n",xlim=c(0,nbK),
       xlab="jour",ylab="proportion de cas / cas confirmés",
       main=vtext)
  abline(h=1,lty=2,col=1,lwd=1)
  lines(x,xpcasdece,lty=1,col=2,lwd=2)
  lines(x,xpcasguer,lty=1,col=3,lwd=2)
  lines(x,xpcasacti,lty=1,col=4,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)
  
  nbrejour <- length(xcasconf)	
  #nbrejour		# 69
  x0 <- 1:(nbrejour)	# en jour  depuis j0
  #length(x0)		# 69

  # les jours pour lesquels on utilise les données pour l'estimation
  joursPourEstim <- c((nbrejour-vnjestim+1):nbrejour)
  joursPourSEDOEstim <- c((joursPourEstim[1]-vindJdebut+1):(joursPourEstim[vnjestim]-vindJdebut+1))
  
  kd0 <- 0.0020		# 0.0035	# 0.0001
  kg0 <- 0.0500		# 0.085	# 0.18
  
  xDMW <- rbind(xpcasdece[joursPourEstim],
                xpcasguer[joursPourEstim],
                xpcasacti[joursPourEstim],
                joursPourSEDOEstim)
  
  pinit <- c(log(kd0),log(kg0))
  
  ############################################
  # fonction de minimisation avec le SEDO
  resNLMsedo <- nlm( fctMC2M, pinit, xDMW, hessian = FALSE, iterlim = 400 ) 
  
  kdf <- exp(resNLMsedo$estimate[1])
  kgf <- exp(resNLMsedo$estimate[2])
  
  kdelta <- 0	# 12
  
  xte <- vindJdebut:(nbK-kdelta)
  indJ  <- (1:length(xte))
  
  MresY <- fctModelSEDO(resNLMsedo$estimate,1)  
  xpD <- MresY[1,indJ]
  xpG <- MresY[2,indJ]
  xpA <- MresY[3,indJ]
  
  xte <- vindJdebut:(nbK-kdelta)
  indJ  <- (1:length(xte))
  lines(xte,xpA[indJ],lty=2,col=4,lwd=2)
  lines(xte,xpG[indJ],lty=2,col=3,lwd=2)
  lines(xte,xpD[indJ],lty=2,col=2,lwd=2)
  
  # détermination de l'instant de croisement des courbes ...
  # date de début vague épidémie a vindJdebut
  dateDebutVague <- as.Date("2020-01-22") + xte[1]	
  VR0 <- list(vindJdebut,dateDebutVague)
  
  xtmp <- abs(xpA-xpG)
  selk <- (xtmp==min(xtmp))
  jourCroisement <- xte[selk]
  dateCroisement <- as.Date("2020-01-22") + jourCroisement
  VR1 <- list(jourCroisement,dateCroisement)
  
  selk <- (xpA < 0.01)
  indJA01est <- min((1:(nbK+1))[selk]) - kdelta + vindJdebut
  dateJA01est <- as.Date("2020-01-22") + indJA01est
  deltaJA01est <- (indJA01est - nbrejour) 
  VR2 <- list(indJA01est,dateJA01est,deltaJA01est)
  abline(v=indJA01est,lty=3,col=1,lwd=3)
  
  selk <- (xpA < 0.001)
  indJA02est <- min((1:(nbK+1))[selk]) - kdelta + vindJdebut
  dateJA02est <- as.Date("2020-01-22") + indJA02est
  deltaJA02est <- (indJA02est - nbrejour) 
  VR3 <- list(indJA02est,dateJA02est,deltaJA02est)
  abline(v=indJA02est,lty=2,col=1,lwd=3)
  
  prop1Deces <- xpD[indJA01est]
  VR4 <- list(xpcasdece[nbrejour],prop1Deces)
  
  vlegendP <- c(paste("Confirmés"," : ",xcasconf[nbrejour]," cas",sep=""),
              paste("Décédés"," : ",xcasdece[nbrejour]," cas",sep=""),
              paste("Guéris"," : ",xcasguer[nbrejour]," cas",sep=""),
              paste("Actifs"," : ",xcasacti[nbrejour]," cas",sep=""))
  
  legend("topright",legend=vlegendP,col=c(1,2,3,4),lty=rep(1,4),lwd=rep(2,4),cex=1.0,bg="white")
  legend("left",legend=paste(round((xpcasdece[nbrejour])*1000)/1000,sep=""),col=2,cex=1.0,
         text.col=2, box.col=2, box.lwd=2,bg="white")
  legend("right",legend=paste(round((prop1Deces)*1000)/1000,sep=""),col=2,cex=1.0,
         text.col=2, box.col=2, box.lwd=2,bg="white")
  
  VR5 <- list(kdf,kgf,xpropnbrepopulation)
  
  VR6 <- list(xNpinf)
  
  # Fermeture du fichier PDF
  dev.off()
  
  
  # résultats a conserver
  dataResSedo <- list(VR0,VR1,VR2,VR3,VR4,VR5,VR6)  
  
  dataResSedo
  
}
