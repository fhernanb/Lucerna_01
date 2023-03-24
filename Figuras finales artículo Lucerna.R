# Fijando el lugar de las figuras
setwd("G:/Mi unidad/13_Mis_Articulos/11 Lucerna/Paper07/figuras")

# Lectura de base de datos ------------------------------------------------
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/vacas'
dat <- read.table(url, header=T, sep='\t')

dim(dat)
head(dat)

# Funciones auxiliares ----------------------------------------------------
subconjunto <- function(nobs=5) {
  freq_anim <- with(dat, sort(table(Id), decreasing=T))
  animales <- freq_anim[freq_anim >= nobs]
  animales <- as.numeric(names(animales))
  subdat <- subset(dat, Id %in% animales)
  subdat$Id <- factor(subdat$Id)
  return(subdat)
}

# Numero de animales con cierta cantidad de partos ------------------------
freq_anim <- with(dat, sort(table(Id), decreasing=TRUE))
tabla <- prop.table(table(freq_anim)) * 100

#pdf("animales_por_parto.pdf", width = 8, height = 6)
xx <- barplot(tabla, las=1, ylim=c(0, 35), col='white',
              xlab='Número de partos',
              ylab='Porcentaje de animales')
etiqueta <- paste0(round(tabla, 1), " %")
text(x=xx, y=tabla, pos=3, cex=0.8, col="black", label=etiqueta)
#dev.off()

# Densidades ppd ----------------------------------------------------------

f <- lapply(split(dat$ppd, dat$parto), density)

require(gplots)
paleta <- palette(rich.colors(8))

#pdf("densidad_ppd.pdf", width=8, height=6)
plot(f[[1]], main='', las=1, lwd=3,
     xlim=c(4, 20), col=paleta[1],
     xlab='Producción promedio de leche por día (lt)',
     ylab='Densidad')
for (i in 2:8) lines(f[[i]], lwd=3, col=paleta[i])

legend('topright', col=paleta,
       lwd=2, bty='n',
       legend=paste('Parto ', 1:8))
#dev.off()

# Diagrama de dispersion ppd vs edad --------------------------------------

#pdf("ppd_edad.pdf", width = 8, height = 6)
require(gplots)
paleta <- palette(rich.colors(8))
par(mfrow=c(1, 1))
plot(ppd ~ edad, data=subset(dat, parto %in% 1:8), 
     xlab='Edad (meses)',
     col=paleta[parto], 
     pch=20,
     ylab='Producción promedio de leche por día (lt)', las=1,
     ylim=c(0, 20),
     xlim=c(25, 200))
legend('topright', legend=paste('Parto ', 1:8),
       pch=20, col=1:8)
#dev.off()

# Diagrama de dispersion ppd vs cc --------------------------------------

#pdf("ppd_cc.pdf", width = 8, height = 6)
require(gplots)
paleta <- palette(rich.colors(8))
par(mfrow=c(1, 1))
plot(jitter(ppd, factor=100) ~ jitter(cc, factor=100), 
     data=subset(dat, parto %in% 1:8), xlab='Condición corporal',
     col=paleta[parto], 
     pch=1,
     ylab='Producción promedio de leche por día (lt)', las=1,
     ylim=c(0, 20),
     xlim=c(2, 5.5))
legend('topright', legend=paste('Parto ', 1:8),
       pch=20, col=1:8)
#dev.off()


# Diagrama de dispersión ppd vs edad con animales de 8 o más obs ----------

# Animales con al menos 8 obs
dat36 <- subconjunto(nobs=8)

require(lattice)
#pdf("ppd_edad_36.pdf", width = 8, height = 8)
xyplot(ppd ~ edad | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat36, type=c('b'), auto.key=F,
       ylab = "Producción promedio de leche por día (lt)", 
       xlab = "Edad (meses)")
#dev.off()


# Diagrama de dispersión entre ppd y edad dado componente racial ----------

# Animales con al menos 5 obs
dat197 <- subconjunto(nobs=5)

#pdf("ppd_edad_cr.pdf", width = 10, height = 5.5)
require(gplots)
paleta <- palette(rich.colors(10))

library(lattice)
xyplot(ppd ~ edad | cr, col=paleta,
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat197, type=c('b'), auto.key=F,
       ylab = "Producción promedio de leche por día (lt)",
       xlab = "Edad (meses)")
#dev.off()


# Diagrama de dispersión ppd versus cc ------------------------------------

#pdf("ppd_cc_36.pdf", width = 8, height = 8)
xyplot(ppd ~ cc | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat36, type=c('p'), auto.key=F,
       ylab = "Producción promedio de leche por día (lt)",
       xlab = "Condición corporal")
#dev.off()








