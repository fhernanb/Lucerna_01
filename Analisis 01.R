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

#x <- c(3088, 2377, 2618, 2899)
#dat1 <- dat1[dat1$Id %in% x, ]

#dat1$edad <- scale(dat1$edad)  # scaling the age
#dat1$cc <- scale(dat1$cc)  # scaling the cc

#cambio <- function(datos) {
#  x <- datos$ppd
#  n <- length(x)
#  c(NA, (x[-1] - x[-n]) / x[-n])
#}

#res <- lapply(split(dat1, dat1$Id, drop=TRUE), cambio)
#res <- Reduce(c, res)
#dat1$res <- res
#malos <- unique(dat1$Id[which(abs(dat1$res) > 0.5)])
#dat1 <- dat1[!dat1$Id %in% malos, ]

#length(unique(dat1$Id))


# lmm ---------------------------------------------------------------------
require(nlme)

# Creando un modelo solo con efectos fijos
lmm0 <- gls(ppd ~ cc * edad + I(edad^2) + cr, data=dat1, method='ML')
summary(lmm0)

# quitando cr
lmm1 <- update(lmm0, model=ppd ~ cc * edad + I(edad^2), method='ML')

# comparando ambos modelos para ver si es necesario incluir Componente Racial
anova(lmm0, lmm1)

# Creando un modelo con intercepto aleatorio
lmm2 <- lme(ppd ~ cc * edad + I(edad^2), random= ~ 1 | Id, 
            data=dat1, method='REML')

anova(lmm1, lmm2) # se debe incluir intercepto aleatorio

# comparing lmm1 and lmm2
D <- -2 * (logLik(lmm1) - logLik(lmm2))
0.5 * (1-pchisq(D, df=1)) + 0.5 * (1-pchisq(D, df=2))
# with lmm1 is enough

# Adding random slope
lmm3 <- lme(ppd ~ cc * edad + I(edad^2), random= ~ 1 + edad | Id, 
            data=dat1, method='REML')

# comparing lmm2 and lmm3
D <- -2 * (logLik(lmm2) - logLik(lmm3))
0.5 * (1-pchisq(D, df=1)) + 0.5 * (1-pchisq(D, df=2))
# with lmm1 is enough

# Residual analysis for lmm2 ----------------------------------------------

plot(lmm2, Id ~ resid(., type='p'), abline=0,
     xlim=c(-4.1, 4.1),
     xlab='Residuales', main='Standarized residuals for lmm2')

# Residuals versus fitted values given cr
plot(lmm2, resid(., type='p') ~ fitted(.) | cr)
plot(lmm2, resid(., type='p') ~ fitted(.))
plot(lmm2, resid(., type='p') ~ edad)
plot(lmm2, resid(., type='p') ~ cc)



resi <- residuals(lmm2, type='normalized')

pdf("resid.pdf", width = 10, height = 4)
par(mfrow=c(1, 3))
plot(x=fitted.values(lmm2), 
     y=resi, las=1,
     ylab='Residuales estandarizados',
     col='black', xlab='Valores ajustados')
qqnorm(resi, xlab='Cuantiles de N(0, 1)', main='',
       ylab='Residuales estandarizados', las=1)
qqline(resi)
qqnorm(ranef(lmm22)[[1]], xlab='Cuantiles de N(0, 1)', main='',
       ylab=expression(tilde(b)[0]), las=1)
qqline(ranef(lmm22)[[1]])
dev.off()






# Creando nuevos modelos a partir de lmm1 ---------------------------------

lmm2 <- update(lmm2, method='REML')

lmm21 <- update(lmm2, weights=varPower(form= ~ cc))
lmm22 <- update(lmm2, weights=varPower(form= ~ edad))
lmm23 <- update(lmm2, weights=varPower())
lmm24 <- update(lmm2, weights=varPower(fixed = 1))

# comparando los anidados
anova(lmm2, lmm21)
anova(lmm2, lmm22)
anova(lmm2, lmm23)
anova(lmm2, lmm24)

anova(lmm21, lmm24)
anova(lmm22, lmm24)
anova(lmm22, lmm24)
anova(lmm23, lmm24)


# loglik y GAIC -----------------------------------------------------------
ll <- c(logLik(lmm1), logLik(lmm2), logLik(lmm3),
        logLik(lmm21), logLik(lmm22), logLik(lmm23), logLik(lmm24))


aic <- AIC(lmm1, lmm2, lmm3, lmm21, lmm22, lmm23, lmm24)

names(ll) <- c('lmm1', 'lmm2', 'lmm3',
               'lmm21', 'lmm22', 'lmm23', 'lmm24')

llnames <- c('M1', 'M2', 'M3', 'M21', 'M22', 'M23', 'M24')

aic <- aic[names(ll), ]

#pdf("ll_aic.pdf", width = 9, height = 5)
par(mfrow=c(1, 2))
plot(ll, type='b', xaxt='n', ylab='Log-verosimilitud', 
     xlab='Modelo', las=1)
axis(side=1, at=1:length(ll), labels=llnames)
grid()
plot(aic[, 2], type='b', xaxt='n', ylab='AIC', 
     xlab='Modelo', las=1)
axis(side=1, at=1:length(ll), labels=llnames)
grid()
#dev.off()


# Exploring the autocorrelation -------------------------------------------

# El siguiente grafico no muestra evidencias de correlacion
plot(ACF(lmm22, maxLag=5), alpha=0.01,
     xlab='Rezago', ylab='Autocorrelaci?n')

# Including correlation ---------------------------------------------------
lmm3 <- update(lmm22, correlation=corCompSymm(form=~1|Id))
lmm4 <- update(lmm22, correlation=corAR1(form=~1|Id))
lmm5 <- update(lmm22, correlation=corARMA(value=c(0.0), p=0, q=1,  form=~1|Id))
lmm6 <- update(lmm22, correlation=corARMA(value=c(0.8, 0.4), p=1, q=1, form=~1|Id))

anova(lmm2, lmm22, lmm3, lmm4, lmm5, lmm6, test=F)


# Tabla de latex ----------------------------------------------------------

library(stargazer)
stargazer(lmm1, lmm2, lmm21, lmm22, lmm23, lmm24)


# Analisis de residuales para lmm12 ---------------------------------------

plot(lmm22, Id ~ resid(., type='p'), abline=0,
     xlab='Residuales', main='Standarized residuals for lmm22')
plot(lmm22, resid(., type='p') ~ fitted(.))
plot(lmm22, resid(., type='p') ~ edad)
plot(lmm22, resid(., type='p') ~ cc)

qqnorm(lmm22, ~ resid(.))
require(car)
qqPlot(residuals(lmm22))
qqnorm(lmm22, ~ ranef(.))

fixef(lmm22)
coef(lmm22)

# Correlations between y and y.fitted -------------------------------------
cor(dat1$ppd, fitted.values(lmm1))
cor(dat1$ppd, fitted.values(lmm2))
cor(dat1$ppd, fitted.values(lmm3))
cor(dat1$ppd, fitted.values(lmm21))
cor(dat1$ppd, fitted.values(lmm22))
cor(dat1$ppd, fitted.values(lmm23))
cor(dat1$ppd, fitted.values(lmm24))


# grafico 3d --------------------------------------------------------------

media <- function(cc, edad) {
  sum(c(1, cc, edad, edad^2, cc*edad) * fixef(lmm22))
}

media <- Vectorize(media)

cc <- seq(from=min(dat1$cc), to=max(dat1$cc), length.out=10)
edad <- seq(from=min(dat1$edad), to=max(dat1$edad), length.out=10)
z <- outer(cc, edad, media)

#pdf("surface.pdf", width = 9, height = 5)
par(mfrow=c(1, 2))
persp(x=cc, y=edad, z=z, theta=-45, phi=20, col='lightblue',
      xlab='Condici?n corporal', ylab='Edad (semanas)', 
      zlab='Ppd (litros)',
      main='a) Superficie de respuesta')

contour(x=cc, y=edad, z=z, las=1, nlevels=20, lwd=2,
        xlab='Condici?n corporal', ylab='Edad (semanas)',
        main='b) Gr?fico de contornos')
#dev.off()


# usando plotly
require(plotly)
p <- plot_ly(z = z, x=cc, y=edad, type = "surface") %>%
  layout(title = "3d",
         scene = list(
           xaxis = list(title = "Condici?n corporal"), 
           yaxis = list(title = "Edad"), 
           zaxis = list(title = "Producci?n")))
ggplotly(p)


# Figuras -----------------------------------------------------------------

require(lattice)

#pdf("evolucion.pdf", width = 8, height = 8)
xyplot(ppd ~ edad | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=16,
       groups=Id, data=dat1, type=c('b'), auto.key=F,
       ylab = "Producci?n de leche (litros)", xlab = "Edad (meses)")
#dev.off()

#pdf("evol_fitted.pdf", width = 8, height = 8)
xyplot(ppd ~ edad | Id, data = dat1, fit = lmm22,
       strip = strip.custom(bg="white"), 
       pch = 16,
       panel = function(x, y, ..., fit, subscripts) {
         panel.xyplot(x, y, ...)
         ypred <- fitted(fit)[subscripts]
         panel.lines(x, ypred, col = "black", lwd=2)
       },
       ylab = "Producci?n de leche (litros)", xlab = "Edad (meses)")
#dev.off()



# lmm ---------------------------------------------------------------------

require(lme4)
lmm1 <- lmer(ppd ~ cc + edad + cr + (1 | Id), data=dat1, REML=F)
lmm2 <- lmer(ppd ~ cc + edad + cr + (1 + edad| Id), data=dat1, REML=F)
lmm3 <- lmer(ppd ~ cc + edad + I(edad^2) + cr + (1 + edad| Id), data=dat1, REML=F)
lmm4 <- lmer(ppd ~ cc + edad + I(edad^2) + (1 + edad| Id), data=dat1, REML=F)
lmm5 <- lmer(ppd ~ cc + edad + I(edad^2) + (1 | Id), data=dat1, REML=F)
lmm6 <- lmer(ppd ~ cc * edad + I(edad^2) + (1 + edad | Id), data=dat1, REML=F)
lmm7 <- lmer(ppd ~ cc * edad + I(edad^2) + (1 + edad + I(edad^2)| Id), data=dat1, REML=F)

require(model)
summary_lmer(mod=lmm1)
summary_lmer(mod=lmm2)
summary_lmer(mod=lmm3)
summary_lmer(mod=lmm4)
summary_lmer(mod=lmm5)
summary_lmer(mod=lmm6)
summary_lmer(mod=lmm7)

# anova
anova(lmm1, lmm2, lmm3, lmm4, lmm5, lmm6, lmm7)

# AIC
AIC(lmm1, lmm2, lmm3, lmm4, lmm5, lmm6, lmm7, k=2)


# Residual analysis
par(mfrow=c(1, 2))
plot(residuals(lmm6) ~ dat1$Id, xlab='Animal', ylab='Residual')
plot(residuals(lmm4) ~ dat1$Id, xlab='Animal', ylab='Residual')

plot(fitted.values(lmm6), residuals(lmm6))
plot(fitted.values(lmm4), residuals(lmm4))

qqnorm(scale(residuals(lmm6)))
qqnorm(scale(residuals(lmm4)))

plot(y=dat1$ppd, x=fitted.values(lmm6))
abline(a=0, b=1, lty=2)

plot(y=dat1$ppd, x=fitted.values(lmm4))
abline(a=0, b=1, lty=2)

for (i in 1:7) 
  print(eval(parse(text=paste('cor(dat1$ppd, fitted.values(lmm', i, '))', sep=''))))

cor(dat1$ppd, fitted.values(lmm4))

par(mfrow=c(2, 2))
qqnorm(ranef(lmm6)[[1]][, 1])
qqnorm(ranef(lmm6)[[1]][, 2])
qqnorm(ranef(lmm4)[[1]][, 1])
qqnorm(ranef(lmm4)[[1]][, 2])


# glmm --------------------------------------------------------------------

require(glmmADMB)

lmm1 <- glmmadmb(ppd ~ cc + edad + cr + (1 | Id), data=dat1, family="gaussian")
lmm2 <- glmmadmb(ppd ~ cc + edad + cr + (1 + edad| Id), data=dat1, family="gaussian")
lmm3 <- glmmadmb(ppd ~ cc + edad + I(edad^2) + cr + (1 + edad| Id), data=dat1, family="gaussian")
lmm4 <- glmmadmb(ppd ~ cc + edad + I(edad^2) + (1 + edad| Id), data=dat1, family="gaussian")
lmm5 <- glmmadmb(ppd ~ cc + edad + I(edad^2) + (1 | Id), data=dat1, family="gaussian")
lmm6 <- glmmadmb(ppd ~ cc * edad + I(edad^2) + (1 + edad | Id), data=dat1, family="gaussian")
lmm7 <- glmmadmb(ppd ~ cc * edad + I(edad^2) + (1 + edad + I(edad^2)| Id), data=dat1, family="gaussian")



glmm1 <- glmmadmb(ppd ~ cc + edad + cr + (1 | Id), data=dat1, family="gamma")
glmm2 <- glmmadmb(ppd ~ cc + edad + cr + (1 + edad| Id), data=dat1, family="gamma")
glmm3 <- glmmadmb(ppd ~ cc + edad + I(edad^2) + cr + (1 + edad| Id), data=dat1, family="gamma")
glmm4 <- glmmadmb(ppd ~ cc + edad + I(edad^2) + (1 + edad| Id), data=dat1, family="gamma")
glmm5 <- glmmadmb(ppd ~ cc + edad + I(edad^2) + (1 | Id), data=dat1, family="gamma")



