#' Fonction necessaire a l'estimation des parametres du modele covid-19
#' Auteurs: Patrick Gasqui, Maude Jacquot
#' Cree le 10 Avril 2020
#' Derniere modification le 19 Avril 2020

# Estimation des parametres du modele et visualisation des resultats et des donnees pour un pays donne
FunctionRCovidModelEstim <- function(vxpays,vxregion,vxdatfin,vxindJdebut,vxindJfin,vxnjestim,vxtaillepop,vxdatFic,vxdelta,vxnomFicOutResnum) {

  # Initialisation
  vpays <- vxpays
  nom_pays <- vpays
  if (vpays=="Spain") {nom_pays="l'Espagne"}
  if (vpays=="Italy") {nom_pays="l'Italie"}
  if (vpays=="France") {nom_pays="la France"}
  if (vpays=="Austria") {nom_pays="l'Autriche"}
  if (vpays=="Germany") {nom_pays="l'Allemagne"}
  if (vpays=="Belgium") {nom_pays="la Belgique"}
  if (vpays=="Switzerland") {nom_pays="la Suisse"}
  if (vpays=="Denmark") {nom_pays="le Danemark"}
  if (vpays=="Sweden") {nom_pays="la Suede"}
  if (vpays=="US") {nom_pays="les Etats-Unis"}
  if (vpays=="Portugal") {nom_pays="le Portugal"}
  if (vpays=="Canada") {nom_pays="le Canada"}
  if (vpays=="Netherlands") {nom_pays="les Pays-Bas"}
  if (vpays=="United Kingdom") {nom_pays="le Royaume-Uni"}

  vregion <- vxregion
  vdatfin <- vxdatfin
  vindJdebut <- vxindJdebut
  vindJfin <- vxindJfin
  vnjestim <- vxnjestim
  vtaillepop <- vxtaillepop
  vdatFic <- vxdatFic 
  vnomFicOutResnum <- vxnomFicOutResnum
  vdelta <- vxdelta

  # date de debut de suivi des donnees
  vdatdeb <- c("22-01-2020")

  # chargement du nombre de cas confirmes cumules par jour 
  nomFicIn <- paste("covidCCO",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  if ( vpays == "Canada" ){
    selj <- (xdatCSV[,2]==vpays)
    mx <- as.matrix(xdatCSV[selj,c(5:length(nomCol))])
    xcasconf <- apply(mx,2,sum)
  } else {
    selj <- ((xdatCSV[,2]==vpays)&(xdatCSV[,1]==vregion))
    xcasconf <- as.numeric(xdatCSV[selj,c(5:length(nomCol))])
  }

  # rapport du nombre de "cas confirmes" sur la taille de la population (exprimee en million) en % pour 1000 habitants
  xpropnbrepopulation <- (xcasconf[nbrejour]/(vtaillepop*1000000))*1000

  # chargement du nombre de cas gueris cumules par jour 
  nomFicIn <- paste("covidCGU",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  #
  selj <- ((xdatCSV[,2]==vpays)&(xdatCSV[,1]==vregion))
  xcasguer <- as.numeric(xdatCSV[selj,c(5:length(nomCol))])

  # chargement du nombre de cas decedes cumules par jour 
  nomFicIn <- paste("covidCMO",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  if ( vpays == "Canada" ){
    selj <- (xdatCSV[,2]==vpays)
    mx <- as.matrix(xdatCSV[selj,c(5:length(nomCol))])
    xcasdece <- apply(mx,2,sum)
  } else {
    selj <- ((xdatCSV[,2]==vpays)&(xdatCSV[,1]==vregion))
    xcasdece <- as.numeric(xdatCSV[selj,c(5:length(nomCol))])
  }

  # evaluation de la proportion de la population "infectee"
  # rapport du nombre d'individus "infectes" sur la taille de la population (exprimee en million) en % pour 100 habitants
  xtfd <- c(0.01,0.02,0.03)
  xZT2b <- (xcasdece[nbrejour] - (xtfd*(xcasconf[nbrejour])))/xtfd
  xNpinf <- ((xcasconf[nbrejour]+xZT2b)/(vtaillepop*1000000))*100

  # Calcul du nombre de cas actifs 
  xcasacti <- xcasconf - xcasguer - xcasdece

  # Export graphique
  nomFicGra <- paste("GraphPDF-COVID19-",vpays,"-",vdatfin,"-ModEst.pdf",sep="")
  if ( vregion != c("") ) {
    nomFicGra <- paste("GraphPDF-COVID19-",vpays,"-",vregion,"-",vdatfin,"-ModEst.pdf",sep="")
    }
  pdf(file=nomFicGra,paper="a4r",height=7,width=10)

  # Graphique du nombre de cas cumules au cours du temps
  nbrejour <- length(xcasconf)	
  x <- 1:nbrejour
  y <- xcasconf
  vtext <- paste(" Données pour ",nom_pays," du ",vdatdeb," au ",vdatfin,
               "\n avec un maximum de ",max(xcasconf)," cas confirmés et ",max(xcasdece)," décés",sep="")
  if ( vregion != c("") ) {
  vtext <- paste(" Données pour ",nom_pays," & ",vregion," du ",vdatdeb," au ",vdatfin,
               "\n avec un maximum de ",max(xcasconf)," cas confirmés et ",max(xcasdece)," décés",sep="")
    }
  plot(x,y,ylim=range(c(0,y)),type="n",xlab="Date",ylab="Nombre de cas cumulés",
     main=vtext,axes=FALSE)
  vx1pos <- round(seq(from=1,to=nbrejour,length.out=6))	
  datex1 <- as.Date("2020-01-21") + vx1pos
  vx1lab <- rep(" ",length(vx1pos))
  for ( i in 1:length(vx1pos) ) { vx1lab[i] <- paste(substring(datex1[i],9,10),"/",substring(datex1[i],6,7),sep="") }
  axis(1,at=vx1pos,labels=vx1lab)
  axis(2)
  box()
  lines(x,xcasconf,lty=1,col=1,lwd=2)
  lines(x,xcasdece,lty=1,col=2,lwd=2)
  lines(x,xcasguer,lty=1,col=3,lwd=2)
  lines(x,xcasacti,lty=1,col=4,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)
  vlegend <- c("Confirmés","Décédés","Guéris","Actifs")
  legend("topleft",legend=vlegend,col=c(1,2,3,4),lty=rep(1,4),lwd=rep(2,4),cex=1.0,bg="white")

  # Graphique des nombres de cas par jour au cours du temps
  # cas confirmes-gueris-decedes par jour ...
  #
  x <- 1:nbrejour
  nbrejour <- length(xcasconf)	
  xjcasconf <- c(0,diff(xcasconf))
  xjcasdece <- c(0,diff(xcasdece))
  xjcasguer <- c(0,diff(xcasguer))
  y <- xjcasconf
  vtext <- paste(" Données pour ",nom_pays," du ",vdatdeb," au ",vdatfin,":",
                "\n nombre de cas confirmés ou décédés ou guéris par jour",sep="")
  if ( vregion != c("") ) {
    vtext <- paste(" Données pour ",nom_pays," & ",vregion," du ",vdatdeb," au ",vdatfin,":",
                 "\n nombre de cas confirmés ou décédés ou guéris par jour",sep="")
    }
  plot(x,y,ylim=range(c(0,y)),type="n",xlab="Date",ylab="Nombre de cas ",
       main=vtext,axes=FALSE)
  vx1pos <- round(seq(from=1,to=nbrejour,length.out=6))	
  datex1 <- as.Date("2020-01-21") + vx1pos
  vx1lab <- rep(" ",length(vx1pos))
  for ( i in 1:length(vx1pos) ) { vx1lab[i] <- paste(substring(datex1[i],9,10),"/",substring(datex1[i],6,7),sep="") }
  axis(1,at=vx1pos,labels=vx1lab)
  axis(2)
  box()
  lines(x,xjcasconf,lty=1,col=1,lwd=2)
  lines(x,xjcasdece,lty=1,col=2,lwd=2)
  lines(x,xjcasguer,lty=1,col=3,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)
  vlegend <- c(paste("Confirmés"," : ",xcasconf[nbrejour],sep=""),
              paste("Décédés"," : ",xcasdece[nbrejour],sep=""),
              paste("Guéris"," : ",xcasguer[nbrejour],sep=""))
  legend("topleft",legend=vlegend,col=c(1,2,3),lty=rep(1,3),lwd=rep(2,3),cex=1.0,bg="white")

  # Graphique des proportions des differents cas / cas confirmes au cours du temps
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
  vtext <- paste(" Données pour ",nom_pays," du ",vdatdeb," au ",vdatfin,":",
                "\n proportion de cas actifs ou guéris ou décédés / cas confirmés",sep="")
  if ( vregion != c("") ) {
    vtext <- paste(" Données pour ",nom_pays," & ",vregion," du ",vdatdeb," au ",vdatfin,":",
                 "\n proportion de cas actifs ou guéris ou décédés / cas confirmés",sep="")
    }
  plot(x,y,ylim=range(c(0,1)),type="n",xlab="Date",ylab="Proportion",
       main=vtext,axes=FALSE)
  vx1pos <- round(seq(from=1,to=nbrejour,length.out=6))	
  datex1 <- as.Date("2020-01-21") + vx1pos
  vx1lab <- rep(" ",length(vx1pos))
  for ( i in 1:length(vx1pos) ) { vx1lab[i] <- paste(substring(datex1[i],9,10),"/",substring(datex1[i],6,7),sep="") }
  axis(1,at=vx1pos,labels=vx1lab)
  axis(2)
  box()
  abline(h=1,lty=2,col=1,lwd=1)
  lines(x,xpcasdece,lty=1,col=2,lwd=2)
  lines(x,xpcasguer,lty=1,col=3,lwd=2)
  lines(x,xpcasacti,lty=1,col=4,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)
  vlegend <- c("Confirmés","Décédés","Guéris","Actifs")
  legend("left",legend=vlegend[-1],col=c(2,3,4),lty=rep(1,3),lwd=rep(2,3),cex=1.0,bg="white")
  legend(0,((xpcasdece[nbrejour])+0.1),legend=paste(round((xpcasdece[nbrejour])*1000)/1000,sep=""),col=2,cex=1.0,
         text.col=2, box.col=2, box.lwd=2,bg="white")

    ############################################	
    # Fonction de calcul du modele de SEDO 
    fctModelSEDO <- function( px, xcode ) { 

      kd <- exp(px[1])	# pour les "cas decedes"
      kg <- exp(px[2])	# pour les "cas gueris"

      nbJ <- 600

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

      # valeurs estimees 
      if ( xcode == 0 ) {		# xcode == 0
        yres <- rbind(xpD,xpG,xpA)
      } else {				# xcode == 1
        yres <- rbind(xpD,xpG,xpA)
        }

      yres

      }  # FIN : fctModelSEDO

    ############################################	
    # Fonction pour le Critere des moindres carres
    #
    fctMC2M <- function(px, xDMW) {
      # matrice des donnees d'entree :
      # * YJobs1  (les proportions "decedes")
      # * YJobs2  (les proportions "gueris")
      # * YJobs3  (les proportions "actifs")
      # * joursSEDO  (les indices des jours dans l'echelle du SEDO )  
      # * nkJ    (nombre total de jour pour l'estimation)

      YJobs <- xDMW[c(1:3),]
      XJSEDO <- xDMW[4,]

      nkJ <- length(XJSEDO)

      # fonction d'estimation du modele de SEDO pour tous les jours 
      MYJest <- fctModelSEDO(px,0)
      YJest <- MYJest[,XJSEDO]

      # calcul du critere des moindres carres
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
  # modele logistique avec modele de SEDO 
  # Graphique des proportions des differents cas / cas confirmes au cours du temps
  # avec les courbes estimees par le modele
  nbK <- vindJfin	# nbrejour
  x <- 1:nbrejour
  y <- xpcasacti
  plot(x,y,ylim=range(c(0,1)),type="n",xlim=c(0,nbK),
       xlab="Date",ylab="Proportion",
       main=vtext,axes=FALSE)
  vx1pos <- round(seq(from=1,to=nbK,length.out=6))	
  datex1 <- as.Date("2020-01-21") + vx1pos
  vx1lab <- rep(" ",length(vx1pos))
  for ( i in 1:length(vx1pos) ) { vx1lab[i] <- paste(substring(datex1[i],9,10),"/",substring(datex1[i],6,7),sep="") }
  axis(1,at=vx1pos,labels=vx1lab)
  axis(2)
  box()
  abline(h=1,lty=2,col=1,lwd=1)
  lines(x,xpcasdece,lty=1,col=2,lwd=2)
  lines(x,xpcasguer,lty=1,col=3,lwd=2)
  lines(x,xpcasacti,lty=1,col=4,lwd=2)
  abline(h=0,lty=2,col=1,lwd=1)

  nbrejour <- length(xcasconf)	
  x0 <- 1:(nbrejour)	# en jour  depuis j0

  # les jours pour lesquels on utilise les donnees pour l'estimation
  joursPourEstim <- c((nbrejour-vnjestim+1):nbrejour)
  joursPourSEDOEstim <- c((joursPourEstim[1]-vindJdebut+1):(joursPourEstim[vnjestim]-vindJdebut+1))

  # valeurs initiales des parametres
  kd0 <- 0.0020		
  kg0 <- 0.0500		

  xDMW <- rbind(xpcasdece[joursPourEstim],
                xpcasguer[joursPourEstim],
                xpcasacti[joursPourEstim],
                joursPourSEDOEstim)

  pinit <- c(log(kd0),log(kg0))

  ############################################
  # fonction de minimisation avec le SEDO ...
  resNLMsedo <- nlm( fctMC2M, pinit, xDMW, hessian = FALSE, iterlim = 400 ) 

  kdf <- exp(resNLMsedo$estimate[1])
  kgf <- exp(resNLMsedo$estimate[2])

  kdelta <- 0	

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

  # date de debut vague epidemie a vindJdebut
  dateDebutVague <- as.Date("2020-01-22") + xte[1]	
  VR0 <- list(vindJdebut,dateDebutVague)

  # determination de l'instant de croisement des courbes
  xtmp <- abs(xpA-xpG)
  selk <- (xtmp==min(xtmp))
  jourCroisement <- xte[selk]
  dateCroisement <- as.Date("2020-01-22") + jourCroisement
  VR1 <- list(jourCroisement,dateCroisement)

  # determination de l'instant de realisation du critere a 0.01 
  selk <- (xpA < 0.01)
  indJA01est <- min(xte[selk])
  dateJA01est <- as.Date("2020-01-22") + indJA01est
  deltaJA01est <- (indJA01est - vindJdebut + 1) 
  VR2 <- list(indJA01est,dateJA01est,deltaJA01est)
  abline(v=indJA01est,lty=3,col=1,lwd=3)

  # determination de l'instant de realisation du critere a 0.001 
  selk <- (xpA < 0.001)
  indJA02est <- min(xte[selk])
  dateJA02est <- as.Date("2020-01-22") + indJA02est
  deltaJA02est <- (indJA02est - vindJdebut + 1) 
  VR3 <- list(indJA02est,dateJA02est,deltaJA02est)
  abline(v=indJA02est,lty=2,col=1,lwd=3)

  # estimation de la proportion finale de deces
  prop1Deces <- xpD[max(indJ)]	
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

  # Export des resultats numeriques au format csv avec ";" comme separateur
  sink(vnomFicOutResnum,append=TRUE)
  cat(vpays)
  cat(";",VR0[[1]],";",paste(VR0[[2]],sep=""))
  cat(";",VR1[[1]],";",paste(VR1[[2]],sep=""))
  cat(";",VR2[[1]],";",paste(VR2[[2]],sep=""),";",VR2[[3]])
  cat(";",VR3[[1]],";",paste(VR3[[2]],sep=""),";",VR3[[3]])
  cat(";",paste(round(as.numeric(VR4[1])*1000)/1000,sep=""),";",paste(round(as.numeric(VR4[2])*1000)/1000,sep=""))
  cat(";",paste(round(as.numeric(VR5[1])*10000)/10000,sep=""),";",paste(round(as.numeric(VR5[2])*10000)/10000,sep=""),";",paste(round(as.numeric(VR5[3])*1000)/1000,sep=""))
  cat(";",paste(round(as.numeric(VR6[[1]][1])*10000)/10000,sep=""),";",paste(round(as.numeric(VR6[[1]][2])*10000)/10000,sep=""),";",paste(round(as.numeric(VR6[[1]][3])*1000)/1000,sep=""))
  cat("\n")
  sink()
  
  # resultats a conserver 
  dataResSedo <- list(VR0,VR1,VR2,VR3,VR4,VR5,VR6)  

  dataResSedo

  }
