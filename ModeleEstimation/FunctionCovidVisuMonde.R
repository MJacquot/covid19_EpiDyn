#' Fonction necessaire a l'estimation des parametres du modele covid-19
#' Auteurs: Patrick Gasqui, Maude Jacquot
#' Cree le 10 Avril 2020
#' Derniere modification le 24 Avril 2020

# Visualisation des donnees observees pour le monde
FctVisuCovidMonde <- function(vxpays,vxdatfin,vxtaillepop,vxdatFic) {

  # Initialisation
  vpays <- vxpays
  #vpays <- c("Monde")
  vregion <- ""
  vdatfin <- vxdatfin
  vtaillepop <- vxtaillepop
  vdatFic <- vxdatFic 

  # date de debut de suivi des donnees
  vdatdeb <- c("22-01-2020")

  # chargement du nombre de cas confirmes cumules par jour 
  nomFicIn <- paste("covidCCO",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  mx <- as.matrix(xdatCSV[,c(5:length(nomCol))])
  xcasconf <- apply(mx,2,sum)

  # chargement du nombre de cas gueris cumules par jour 
  nomFicIn <- paste("covidCGU",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  mx <- as.matrix(xdatCSV[,c(5:length(nomCol))])
  xcasguer <- apply(mx,2,sum)

  # chargement du nombre de cas decedes cumules par jour 
  nomFicIn <- paste("covidCMO",vdatFic,".txt",sep="")
  xdatCSV <- read.csv(file=nomFicIn, header = TRUE, sep=",", dec = ".")
  nomCol <- dimnames(xdatCSV)[[2]]
  nbrejour <- length(nomCol)-4	
  mx <- as.matrix(xdatCSV[,c(5:length(nomCol))])
  xcasdece <- apply(mx,2,sum)

  # Calcul du nombre de cas actifs 
  xcasacti <- xcasconf - xcasguer - xcasdece

  # Export graphique
  nomFicGra <- paste("GraphPDF-COVID19-",vpays,"-",vdatfin,"-Visu.pdf",sep="")
  if ( vregion != c("") ) {
    nomFicGra <- paste("GraphPDF-COVID19-",vpays,"-",vregion,"-",vdatfin,"-Visu.pdf",sep="")
    }
  pdf(file=nomFicGra,paper="a4r",height=7,width=10)

  # Graphique du nombre de cas cumultes au cours du temps
  nbrejour <- length(xcasconf)	
  x <- 1:nbrejour
  y <- xcasconf
  vtext <- paste(" Données ",vpays," du ",vdatdeb," au ",vdatfin,
               "\n avec un maximum de ",max(xcasconf)," cas confirmés et ",max(xcasdece)," décés",sep="")
  if ( vregion != c("") ) {
    vtext <- paste(" Données ",vpays," & ",vregion," du ",vdatdeb," au ",vdatfin,
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
  x <- 1:nbrejour
  nbrejour <- length(xcasconf)	
  xjcasconf <- c(0,diff(xcasconf))
  xjcasdece <- c(0,diff(xcasdece))
  xjcasguer <- c(0,diff(xcasguer))
  y <- xjcasconf
  maxy <- max(xjcasconf,xjcasdece,xjcasguer)
  vtext <- paste(" Données ",vpays," du ",vdatdeb," au ",vdatfin,
                "\n cas confirmés-décédés-guéris / jour",sep="")
  if ( vregion != c("") ) {
    vtext <- paste(" Données ",vpays," & ",vregion," du ",vdatdeb," au ",vdatfin,
                 "\n cas confirmés-décédés-guéris / jour",sep="")
    }
  plot(x,y,ylim=range(c(0,maxy)),type="n",xlab="Date",ylab="Nombre de cas ",
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
  tauxdeces <- (xcasdece[nbrejour])/(xcasconf[nbrejour])
  legend(0,(max(xjcasdece)),legend=paste(round(tauxdeces*1000)/1000,sep=""),col=2,cex=1.0,
       text.col=2, box.col=2, box.lwd=2,bg="white")

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
  vtext <- paste(" Données ",vpays," du ",vdatdeb," au ",vdatfin,
                "\n proportion de cas actifs ou guéris ou décédés / cas confirmés",sep="")
  if ( vregion != c("") ) {
    vtext <- paste(" Données ",vpays," & ",vregion," du ",vdatdeb," au ",vdatfin,
                 "\n proportion de cas actifs ou guéris ou décédés / cas confirmés",sep="")
    }
  plot(x,y,ylim=range(c(0,1)),type="n",xlab="Date",ylab="Proportion de cas / cas confirmés",
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
  abline(h=(xpcasdece[nbrejour]),lty=2,col=2,lwd=1)
  legend(0,((xpcasdece[nbrejour])+0.1),legend=paste(round((xpcasdece[nbrejour])*1000)/1000,sep=""),col=2,cex=1.0,
         text.col=2, box.col=2, box.lwd=2,bg="white")

  # Fermeture du fichier PDF
  dev.off()

  }
