# Lectura de base de datos ------------------------------------------------
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/vacas'
dat <- read.table(url, header=TRUE, sep='\t')
dat$Id <- as.factor(dat$Id)
dat <- dat[with(dat, order(Id, edad)), ]

# Explorando los datos
dim(dat)
head(dat)

# Explorando el numero de animales con cierta cantidad de observaciones
freq_anim <- with(dat, sort(table(Id), decreasing=TRUE))

nobs <- 5
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

# Funciones para generar figuras ------------------------------------------

# Funcion para comparar las estimaciones entre rta y mu_hat
plot_y_yhat <- function(modelo, main=NULL, digits=3) {
  if (is.null(main)) main <- deparse(substitute(modelo))
  the_cor <- cor(fitted.values(modelo), dat1$ppd)
  plot(y=fitted.values(modelo), x=dat1$ppd, las=1, pch=".",
       ylab="Media ajustada", xlab="Valor observado",
       main=main, xlim=range(dat1$ppd), ylim=range(dat1$ppd))
  mensaje <- paste0("r= ", round(the_cor, digits))
  legend("topleft", legend=mensaje, bty="n")
  abline(a=0, b=1, lty="dashed")
}

plot_residuals <- function(model) {
  par(mfrow=c(1, 3))
  resi <- residuals(model, type='normalized')
  plot(x=fitted.values(model), y=resi, las=1,
       ylab='Residuales estandarizados',
       col='black', xlab='Valores ajustados')
  qqnorm(resi, xlab='Cuantiles de N(0, 1)', main='',
         ylab='Residuales estandarizados', las=1)
  qqline(resi)
  qqnorm(ranef(model)[[1]], xlab='Cuantiles de N(0, 1)', main='',
         ylab=expression(tilde(b)[0]), las=1)
  qqline(ranef(model)[[1]])
}


# lmm ---------------------------------------------------------------------
require(nlme)

# Creando un modelo solo con efectos fijos
mod0 <- lm(ppd ~ cc * edad + I(edad^2) + I(cc^2) + cr, data=dat1)
summary(mod0)

# Para hacer selección de variables
library(MASS)
stepAIC(mod0)

# ajustandolo con gls y solo con los terminos 
mod1 <- gls(ppd ~ cc + edad + I(edad^2) + cc:edad, 
            data=dat1, method='REML')
summary(mod1)

# quitando cc:edad del modelo anterior
mod1 <- gls(ppd ~ cc + edad + I(edad^2), 
            data=dat1, method='REML')
summary(mod1) # ya todo es significativo al 5%

# Creando un modelo con intercepto aleatorio
mod2 <- lme(ppd ~ cc + edad + I(edad^2), random= ~ 1 | Id, 
            data=dat1, method='REML')

# Likelihood ratio test with anova
anova(mod1, mod2) # se debe incluir intercepto aleatorio

# Likelihood ratio test manually
D <- -2 * (logLik(mod1) - logLik(mod2))
D
0.5 * (1-pchisq(D, df=1)) + 0.5 * (1-pchisq(D, df=2))
# mod2 is better

# Adding random slope for edad
mod3 <- lme(ppd ~ cc + edad + I(edad^2), random= ~ 1 + edad | Id, 
            data=dat1, method='REML')

# Likelihood ratio test with anova
anova(mod2, mod3) # se debe incluir intercepto y pendiente aleatorio

# Likelihood ratio test manually
D <- -2 * (logLik(mod2) - logLik(mod3))
D
0.5 * (1-pchisq(D, df=1)) + 0.5 * (1-pchisq(D, df=2))
# mod3 is better

# Adding two random slopes for edad and cc
mod4 <- lme(ppd ~ cc + edad + I(edad^2), random= ~ 1 + edad + cc | Id, 
            data=dat1, method='REML')

# Likelihood ratio test with anova
anova(mod3, mod4)

# comparando los modelos hasta ahora
par(mfrow=c(2, 3))
plot_y_yhat(mod0)
plot_y_yhat(mod1)
plot_y_yhat(mod2)
plot_y_yhat(mod3)
plot_y_yhat(mod4)

# Creando nuevos modelos a partir del mejor modelo --------------------
mod5 <- update(mod3, method='ML')

mycontrol <- lmeControl(maxIter=50000,
                        msMaxIter=50000,
                        msMaxEval=50000)

mod5 <- update(mod3, weights=varPower(form= ~ cc), control=mycontrol)
mod6 <- update(mod3, weights=varPower(form= ~ edad), control=mycontrol)
mod7 <- update(mod3, weights=varPower(), control=mycontrol)
mod8 <- update(mod3, weights=varPower(fixed = 1), control=mycontrol)

# comparando los modelos anidados
anova(mod3, mod5)
anova(mod5, mod6)
anova(mod5, mod7)
anova(mod5, mod8)

anova(mod3, mod5, mod6, mod7, mod8)

par(mfrow=c(2, 3))
plot_y_yhat(mod3)
plot_y_yhat(mod5)
plot_y_yhat(mod6)
plot_y_yhat(mod7)
plot_y_yhat(mod8)

summary(mod5)

# Exploring the autocorrelation -------------------------------------------

# El siguiente grafico no muestra evidencias de correlacion
plot(ACF(mod5, maxLag=5), alpha=0.05,
     xlab='Rezago', ylab='Autocorrelacion')

# Including correlation ---------------------------------------------------
mod9  <- update(mod5, correlation=corCompSymm(form=~1|Id))
mod10 <- update(mod5, correlation=corARMA(value=c(0.0), p=0, q=1, form=~1|Id))
mod11 <- update(mod5, correlation=corARMA(value=c(0.0), p=1, q=0, form=~1|Id))

anova(mod5, mod9, mod10, mod11)

par(mfrow=c(2, 2))
plot_y_yhat(mod5)
plot_y_yhat(mod9)
plot_y_yhat(mod10)
plot_y_yhat(mod11)


# Tabla de latex ----------------------------------------------------------

library(stargazer)
stargazer(mod0, 
          mod1, mod2, mod3, mod4, mod5, mod6,
          mod7, mod8, mod9, mod10, mod11)

stargazer(mod1, mod5,
          title='Resultados',
          covariate.labels=c('cc', 'edad', 'edad2', 'Intercepto'))


# Analisis de residuales para mod5 ---------------------------------------

plot(mod5, Id ~ resid(., type='p'), abline=0,
     xlab='Residuales', main='Standarized residuals for mod22')
plot(mod5, resid(., type='p') ~ fitted(.))
plot(mod5, resid(., type='p') ~ edad)
plot(mod5, resid(., type='p') ~ cc)

qqnorm(mod5, ~ resid(.))
require(car)
qqPlot(residuals(mod5))
qqnorm(mod5, ~ ranef(.))

fixef(mod5)
coef(mod5)

# Correlations between y and y.fitted -------------------------------------
cor(dat1$ppd, fitted.values(mod1))
cor(dat1$ppd, fitted.values(mod2))
cor(dat1$ppd, fitted.values(mod3))
cor(dat1$ppd, fitted.values(mod4))
cor(dat1$ppd, fitted.values(mod5))
cor(dat1$ppd, fitted.values(mod6))
cor(dat1$ppd, fitted.values(mod7))
cor(dat1$ppd, fitted.values(mod8))
cor(dat1$ppd, fitted.values(mod9))
cor(dat1$ppd, fitted.values(mod10))
cor(dat1$ppd, fitted.values(mod11))

# grafico 3d --------------------------------------------------------------

media <- function(cc, edad) {
  sum(c(1, cc, edad, edad^2) * fixef(mod5))
}

media <- Vectorize(media)

cc <- seq(from=min(dat1$cc), to=max(dat1$cc), length.out=10)
edad <- seq(from=min(dat1$edad), to=max(dat1$edad), length.out=10)
z <- outer(cc, edad, media)

#pdf("surface.pdf", width = 9, height = 5)
par(mfrow=c(1, 2))
persp(x=cc, y=edad, z=z, theta=-45, phi=20, col='lightblue',
      xlab='Condición corporal', ylab='Edad (meses)', 
      zlab='Ppd (litros)',
      main='a) Superficie de respuesta')

contour(x=cc, y=edad, z=z, las=1, nlevels=20, lwd=2,
        xlab='Condición corporal', ylab='Edad (meses)',
        main='b) Gráfico de contornos')
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
library(lattice)

#pdf("evolucion.pdf", width = 8, height = 8)
xyplot(ppd ~ edad | Id, col='black',
       strip = strip.custom(bg="white"),
       pch=16,
       groups=Id, data=dat1, type=c('b'), auto.key=F,
       ylab = "Producci?n de leche (litros)", xlab = "Edad (meses)")
#dev.off()


# Explorando la cor y mse -------------------------------------------------
library(dplyr)

MSE <- function(x, y) mean((x-y)^2)

datis <- data.frame(Id=dat1$Id, ppd=dat1$ppd, y_hat=fitted(mod5))
best_fit <- datis %>% group_by(Id) %>% 
  summarise(correl=cor(ppd, y_hat), mse=MSE(ppd, y_hat)) %>% #arrange(correl)
  arrange(desc(correl))

best_fit
View(best_fit)

# Graficos de evolucion --------------------------------------------------

# USANDO LATTICE
library(lattice)

#pdf("evol_fitted_complete.pdf", width=15, height=20)
xyplot(ppd ~ edad | Id, data=dat1, fit=mod5,
       strip=strip.custom(bg="white"), 
       pch=16,
       panel = function(x, y, ..., fit, subscripts) {
         panel.xyplot(x, y, ...)
         ypred <- fitted(fit)[subscripts]
         panel.lines(x, ypred, col="black", lwd=2)
       },
       ylab="Producción promedio de leche por día (lt)", 
       xlab="Edad (meses)")
#dev.off()

# MANUALMENTE EL MIO

#pdf("evol_fitted.pdf", width = 8, height = 8)
animals <- best_fit$Id[1:16]
par(mfrow=c(4, 4))
for (i in animals) {
  dt <- subset(dat1, Id == i)
  with(dt, plot(x=edad, y=ppd, las=1, pch=19, col='#1568b9',
                main=paste0('Id ', i),
                ylim=range(dat1$ppd), xlim=range(dat1$edad),
                ylab="ppd", 
                xlab="Edad (meses)"))
  lines(x=dt$edad, y=fitted(mod5)[dat1$Id == i])
}
#dev.off()

# lmm ---------------------------------------------------------------------

require(lme4)
mod1 <- lmer(ppd ~ cc + edad + cr + (1 | Id), data=dat1, REML=F)
mod2 <- lmer(ppd ~ cc + edad + cr + (1 + edad| Id), data=dat1, REML=F)
mod3 <- lmer(ppd ~ cc + edad + I(edad^2) + cr + (1 + edad| Id), data=dat1, REML=F)
mod4 <- lmer(ppd ~ cc + edad + I(edad^2) + (1 + edad| Id), data=dat1, REML=F)
lmm5 <- lmer(ppd ~ cc + edad + I(edad^2) + (1 | Id), data=dat1, REML=F)
lmm6 <- lmer(ppd ~ cc * edad + I(edad^2) + (1 + edad | Id), data=dat1, REML=F)
lmm7 <- lmer(ppd ~ cc * edad + I(edad^2) + (1 + edad + I(edad^2)| Id), data=dat1, REML=F)

require(model)
summary_lmer(mod=mod1)
summary_lmer(mod=mod2)
summary_lmer(mod=mod3)
summary_lmer(mod=mod4)
summary_lmer(mod=lmm5)
summary_lmer(mod=lmm6)
summary_lmer(mod=lmm7)

# anova
anova(mod1, mod2, mod3, mod4, lmm5, lmm6, lmm7)

# AIC
AIC(mod1, mod2, mod3, mod4, lmm5, lmm6, lmm7, k=2)


# Residual analysis
par(mfrow=c(1, 2))
plot(residuals(lmm6) ~ dat1$Id, xlab='Animal', ylab='Residual')
plot(residuals(mod4) ~ dat1$Id, xlab='Animal', ylab='Residual')

plot(fitted.values(lmm6), residuals(lmm6))
plot(fitted.values(mod4), residuals(mod4))

qqnorm(scale(residuals(lmm6)))
qqnorm(scale(residuals(mod4)))

plot(y=dat1$ppd, x=fitted.values(lmm6))
abline(a=0, b=1, lty=2)

plot(y=dat1$ppd, x=fitted.values(mod4))
abline(a=0, b=1, lty=2)

for (i in 1:7) 
  print(eval(parse(text=paste('cor(dat1$ppd, fitted.values(lmm', i, '))', sep=''))))

cor(dat1$ppd, fitted.values(mod4))

par(mfrow=c(2, 2))
qqnorm(ranef(lmm6)[[1]][, 1])
qqnorm(ranef(lmm6)[[1]][, 2])
qqnorm(ranef(mod4)[[1]][, 1])
qqnorm(ranef(mod4)[[1]][, 2])


# Residual analysis ----------------------------------------------

plot(mod5, Id ~ resid(., type='p'), abline=0,
     xlim=c(-4.1, 4.1),
     xlab='Residuales', main='Standarized residuals for mod5')

# Residuals versus fitted values, edad and cc
plot(mod5, resid(., type='p') ~ fitted(.))
plot(mod5, resid(., type='p') ~ edad)

#pdf("resid.pdf", width = 10, height = 4)
plot_residuals(mod5)
#dev.off()


#pdf("resid.pdf", width = 10, height = 10)
model <- mod5
par(mfrow=c(2, 2), mar=c(4.5, 5, 2, 2))
resi <- residuals(model, type='normalized')
plot(x=fitted.values(model), y=resi, las=1,
     ylab='Residuales estandarizados',
     col='black', xlab='Valores ajustados')
qqnorm(resi, xlab='Cuantiles de N(0, 1)', main='',
       ylab='Residuales estandarizados', las=1)
qqline(resi)
qqnorm(ranef(model)[[1]], xlab='Cuantiles de N(0, 1)', main='',
       ylab=expression(tilde(b)[0]), las=1)
qqline(ranef(model)[[1]])
qqnorm(ranef(model)[[2]], xlab='Cuantiles de N(0, 1)', main='',
       ylab=expression(tilde(b)[1]), las=1)
qqline(ranef(model)[[2]])
#dev.off()


