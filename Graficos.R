# Lectura de base de datos ------------------------------------------------
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/vacas'
dat <- read.table(url, header=T, sep='\t')
dat$Id <- as.factor(dat$Id)
dat <- dat[with(dat, order(Id, edad)), ]

# Animales con al menos nobs obs
freq_anim <- with(dat, sort(table(Id), decreasing=T))
nobs <- 8
animales <- freq_anim[freq_anim >= nobs]
animales <- as.numeric(names(animales))
dat1 <- subset(dat, Id %in% animales)
dat1$Id <- factor(dat1$Id)
length(levels(dat1$Id))

# Correlaciones
with(dat1, cor(ppd, cc))
correl <- sapply(split(dat1[, c('ppd', 'cc')], dat1$Id), 
                 function (x) cor(x[, 1], x[, 2]))
sort(correl)

# Numero de animales por componente racial
table(sapply(split(dat1, dat1$Id), function(x) x[1, 4]))


# Creacion de graficos ----------------------------------------------------

require(lattice)

#pdf("ppd_edad.pdf", width = 8, height = 8)
xyplot(ppd ~ edad | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat1, type=c('b'), auto.key=F,
       ylab = "Producción promedio de leche por día (lt)", 
       xlab = "Edad (meses)")
#dev.off()

#pdf("ppd_cc.pdf", width = 8, height = 8)
xyplot(ppd ~ cc | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat1, type=c('p'), auto.key=F,
       ylab = "Producción promedio de leche por día (lt)",
       xlab = "Condición corporal")
#dev.off()

#pdf("ppd_edad_cr.pdf", width = 8, height = 4)
require(gplots)
paleta <- palette(rich.colors(10))
xyplot(ppd ~ edad | cr, col=paleta,
       strip = strip.custom(bg="white"),
       pch=1,
       groups=Id, data=dat1, type=c('b'), auto.key=F,
       ylab = "Producción promedio de leche por día (lt)",
       xlab = "Edad (meses)")
#dev.off()



