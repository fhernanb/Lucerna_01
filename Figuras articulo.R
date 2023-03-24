# Fijando el lugar de las figuras
setwd("G:/Mi unidad/13_Mis_Articulos/11 Lucerna/Paper07/figuras")

# Lectura de base de datos ------------------------------------------------
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/vacas'
dat <- read.table(url, header=T, sep='\t')

dim(dat)
head(dat)

# Numero de animales con cierta cantidad de partos ------------------------
freq_anim <- with(dat, sort(table(Id), decreasing=TRUE))
tabla <- prop.table(table(freq_anim)) * 100

#pdf("animales_por_parto.pdf", width = 8, height = 6)
xx <- barplot(tabla, las=1, ylim=c(0, 35), col='white',
              xlab='NÃºmero de partos',
              ylab='Porcentaje de animales')
etiqueta <- paste0(round(tabla, 1), " %")
text(x=xx, y=tabla, pos=3, cex=0.8, col="black", label=etiqueta)
#dev.off()


# Numero de animales por cada parto ---------------------------------------
tabla <- prop.table(table(dat$parto)) * 100

#pdf("animales_por_parto.pdf", width = 8, height = 6)
xx <- barplot(tabla, las=1, ylim=c(0, 30), col='white',
              xlab='NÃºmero de partos',
              ylab='Porcentaje de animales')
etiqueta <- paste0(round(tabla, 1), " %")
text(x=xx, y=tabla, pos=3, cex=0.8, col="black", label=etiqueta)
#dev.off()



# Ppd vs cc ---------------------------------------------------------------

#pdf("plot1.pdf", width = 8, height = 8)
par(mfrow=c(2, 2))
for (i in 1:4) {
  subdat <- subset(dat, parto == i)
  subdat$cc <- jitter(subdat$cc, amount=0.1)
  subdat$ppd <- jitter(subdat$ppd, amount=0.8)
  with(subdat, plot(ppd ~ jitter(cc), xlab='CondiciÃ³n corporal', type='n',
                    main=paste('Parto', i), xaxt='n',
                    ylab='ProducciÃ³n promedio de leche por dÃ­a (lt)', 
                    las=1, ylim=c(0, max(dat$ppd)),
                    xlim=c(2, 4.5)))
  axis(side=1, at=seq(1, 5, by=0.5), labels=seq(1, 5, by=0.5))
  with(subset(subdat, cr=='CR4'), points(x=cc, y=ppd, col='blue'))
  with(subset(subdat, cr=='CR3'), points(x=cc, y=ppd, col='tomato1'))
  with(subset(subdat, cr=='CR2'), points(x=cc, y=ppd, col='seagreen3'))
  with(subset(subdat, cr=='CR1'), points(x=cc, y=ppd, col='black'))
  if (i==1) {
    legend('topleft', legend=paste('CR', 4:1, sep=''), pch=1,
           col=c('blue', 'tomato1', 'seagreen3', 'black'))
  }
}
#dev.off()

#pdf("plot2.pdf", width = 8, height = 8)
par(mfrow=c(2, 2))
for (i in 5:8) {
  subdat <- subset(dat, parto == i)
  subdat$cc <- jitter(subdat$cc, amount=0.1)
  subdat$ppd <- jitter(subdat$ppd, amount=0.8)
  with(subdat, plot(ppd ~ jitter(cc), xlab='CondiciÃ³n corporal', type='n',
                    main=paste('Parto', i), xaxt='n',
                    ylab='ProducciÃ³n promedio de leche por dÃ­a (lt)', las=1,
                    ylim=c(0, max(dat$ppd)),
                    xlim=c(2, 4.5)))
  axis(side=1, at=seq(1, 5, by=0.5), labels=seq(1, 5, by=0.5))
  with(subset(subdat, cr=='CR4'), points(x=cc, y=ppd, col='blue'))
  with(subset(subdat, cr=='CR3'), points(x=cc, y=ppd, col='tomato1'))
  with(subset(subdat, cr=='CR2'), points(x=cc, y=ppd, col='seagreen3'))
  with(subset(subdat, cr=='CR1'), points(x=cc, y=ppd, col='black'))
  if (i==5) {
    legend('topleft', legend=paste('CR', 4:1, sep=''), pch=1,
           col=c('blue', 'tomato1', 'seagreen3', 'black'))
  }
}
#dev.off()

# Diagrama de dispersion ppd vs edad --------------------------------------

#pdf("ppd_edad.pdf", width = 8, height = 5)
require(gplots)
paleta <- palette(rich.colors(8))
par(mfrow=c(1, 1))
plot(ppd ~ edad, data=subset(dat, parto %in% 1:8), 
     xlab='Edad (meses)',
     col=paleta[parto], 
     pch=20,
     ylab='ProducciÃ³n promedio de leche por dÃ­a (lt)', las=1,
     ylim=c(0, 20),
     xlim=c(25, 200))
legend('topright', legend=paste('Parto ', 1:8),
       pch=20, col=1:8)
#dev.off()

# Diagrama de dispersion ppd vs cc --------------------------------------

#pdf("ppd_cc.pdf", width = 8, height = 5)
require(gplots)
paleta <- palette(rich.colors(8))
par(mfrow=c(1, 1))
plot(jitter(ppd, factor=100) ~ jitter(cc, factor=100), 
     data=subset(dat, parto %in% 1:8), xlab='CondiciÃ³n corporal',
     col=paleta[parto], 
     pch=1,
     ylab='ProducciÃ³n promedio de leche por dÃ­a (lt)', las=1,
     ylim=c(0, 20),
     xlim=c(2, 5.5))
legend('topright', legend=paste('Parto ', 1:8),
       pch=20, col=1:8)
#dev.off()

# Diagrama de dispersiÃÂ³n ppd vs cc diferenciando por parto ----------------
require(lattice)

#pdf("ppd_edad.pdf", width = 8, height = 8)
xyplot(ppd ~ edad | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat, type=c('b'), auto.key=F,
       ylab = "ProducciÃ³n promedio de leche por dÃ­a (lt)", 
       xlab = "Edad (meses)")
#dev.off()

# Boxplot ppd vs parto ----------------------------------------------------

#pdf("plot4.pdf", width=8, height=5)
subdat <- subset(dat, parto <= 8)
var.edad <- sapply(with(subdat, split(edad, parto)), var)
with(subdat, boxplot(ppd~parto, width=var.edad,
                     xlab='Parto',
                     ylab='ProducciÃ³n promedio de leche por dÃ­a (lt)'))
#dev.off()

# Densidades ppd ----------------------------------------------------------

f <- lapply(split(dat$ppd, dat$parto), density)

require(gplots)
paleta <- palette(rich.colors(8))

#pdf("densidad_ppd.pdf", width=8, height=5)
plot(f[[1]], main='', las=1, lwd=3,
     xlim=c(4, 20), col=paleta[1],
     xlab='ProducciÃ³n promedio de leche por dÃ­a (lt)',
     ylab='Densidad')
for (i in 2:8) lines(f[[i]], lwd=3, col=paleta[i])

legend('topright', col=paleta,
       lwd=2, bty='n',
       legend=paste('Parto ', 1:8))
#dev.off()

# Acumuladas ppd ----------------------------------------------------------

F <- lapply(split(dat$ppd, dat$parto), ecdf)

#pdf("plot6.pdf", width=8, height=5)
plot(F[[1]], main='', las=1, lwd=3,
     xlim=c(4, 16), col=paleta[1], do.points=FALSE,
     xlab='ProducciÃÂ³n promedio por dÃÂ­a (lt)',
     ylab='Probabilidad acumulada')

for (i in 2:8) lines(F[[i]], lwd=3, do.points=FALSE,
                     col=paleta[i])

axis(4, las=1)

legend('topleft', col=paleta,
       lwd=2, bty='n',
       legend=paste('Parto ', 1:8))
#dev.off()


# Precoz y tardio ---------------------------------------------------------
dat1 <- subset(dat, parto==1)

require(gplots)
paleta <- palette(rich.colors(8))

k <- 1.5
precoz <- with(dat1, Id[edad < mean(edad) - k * sd(edad)])
tardio <- with(dat1, Id[edad > mean(edad) + k * sd(edad)])
extrem <- c(precoz, tardio)
precoz
tardio

#pdf("plot7.pdf", width=10, height=6)
par(mfrow=c(2, 2))
mypch <- 20
mycex <- 1.1
plot(f[[1]], las=1, lwd=1,
     xlim=c(1, 22),
     ylim=c(0, 0.26), main='Parto 1',
     xlab='ProducciÃÂ³n promedio por dÃÂ­a (lt)',
     ylab='Densidad')

ppd.precoz <- subset(dat, parto==1)$ppd[dat1$Id %in% precoz]
points(x=ppd.precoz, y=rep(0, length(ppd.precoz)),
       pch=mypch, cex=mycex, col="#0000CB")

ppd.tardio <- subset(dat, parto==1)$ppd[dat1$Id %in% tardio]
points(x=ppd.tardio, y=rep(0.007, length(ppd.tardio)),
       pch=mypch, cex=mycex, col="#FF3300")

legend('topright', legend=c('Precoz', 'TardÃÂ­a'),
       pch=20, col=c("#0000CB", "#FF3300"), bty="n")

for (i in 2:4) {
  plot(f[[i]], las=1, lwd=1,
       xlim=c(1, 22),
       ylim=c(0, 0.26), main=paste('Parto ', i),
       xlab='ProducciÃÂ³n promedio por dÃÂ­a (lt)',
       ylab='Densidad')
  
  ppd.precoz <- subset(dat, parto==i)$ppd[dat1$Id %in% precoz]
  points(x=ppd.precoz, y=rep(0, length(ppd.precoz)),
         pch=mypch, cex=mycex, col="#0000CB")
  
  ppd.tardio <- subset(dat, parto==i)$ppd[dat1$Id %in% tardio]
  points(x=ppd.tardio, y=rep(0.007, length(ppd.tardio)),
         pch=mypch, cex=mycex, col="#FF3300")
}
#dev.off()


# Boxplot ppd vs epoca ----------------------------------------------------

#pdf("plot4.pdf", width=9, height=5)
np <- 8
subdat <- subset(dat, parto <= np)
with(subdat, boxplot(ppd~epoca*parto, las=1,
                     xlab='ÃÂpoca y Parto', #col=paleta,
                     ylab='ProducciÃÂ³n promedio por dÃÂ­a (lt)'))
abline(v=seq(from=4.5, by=4, length.out=np - 1), lty='dotted', col=gray(0.8))
#dev.off()

# Edad vs parto para extremas ---------------------------------------------
dat1 <- subset(dat, parto==1)

k <- 1.5
precoz <- with(dat1, Id[edad < mean(edad) - k * sd(edad)])
tardio <- with(dat1, Id[edad > mean(edad) + k * sd(edad)])
extrem <- c(precoz, tardio)

npar <- 6
dat.ext <- subset(dat, Id %in% extrem & parto <= npar)
dat.ext$clas <- ifelse(dat.ext$Id %in% precoz, 'Precoz', 'TardÃÂ­a')

#pdf("plot8.pdf", width=12, height=5)
with(dat.ext, boxplot(edad ~ clas * parto, las=1,
                      ylab='Edad (meses)',
                      xlab='ClasificaciÃÂ³n y parto'))
abline(v=seq(from=2.5, length.out=npar-1, by=2),
       lty='dotted', col=gray(0.8))
#dev.off()


#pdf("plot9.pdf", width=12, height=5)
with(dat.ext, boxplot(ppd ~ clas * parto, las=1,
                      ylab='ProducciÃÂ³n promedio de leche (lt)',
                      xlab='ClasificaciÃÂ³n y parto'))
abline(v=seq(from=2.5, length.out=npar-1, by=2),
       lty='dotted', col=gray(0.8))
#dev.off()


################################
lapply(split(dat.ext[, c('ppd', 'clas')], dat.ext$parto), 
       function(x) summary(aov(x[, 1] ~ x[, 2])))

lapply(split(dat.ext[, c('ppd', 'clas')], dat.ext$parto), 
       function(x) summary( kruskal.test(x[, 1] ~ x[, 2])  )  )

lapply(split(dat.ext[, c('ppd', 'clas')], dat.ext$parto), 
       function(x) shapiro.test(x[, 1])) ????????????????????????????

mycol <- ifelse(dat.ext$clas == 'Precoz', 'blue', 'red')
with(dat.ext, coplot(ppd~edad|as.factor(parto), columns=5,
                     pch=19, col=mycol))




