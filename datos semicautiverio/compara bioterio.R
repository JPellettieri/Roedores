setwd("~/Documentos/Ire/growth model")
cautiv <- read.delim("datos semicautiverio/peso-edad.txt")
caut.l <- read.delim("datos semicautiverio/long-tabla7-1.txt")
bioter <- read.delim("datos bioterio/tabla paper.txt", dec = ",")

# longitud vs. edad (bioterio + cautiverio)
with(bioter, plot(x = edad * 365, y = largo, pch = 4, col = "black"))
with(caut.l, points(x = edad, y = media, pch = 5, col = crías))

# Suarez cita a Zuleta 1989
Edad <- function(CC) {return ((CC- 70.261)/4.476)}
cc <- seq(50,100)
lines(x = 30 * Edad(cc), y = cc, lty="dashed")

# peso vs. edad (bioterio + cautiverio)
with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", log="x"))
with(cautiv[cautiv$Season == "Early", ], 
     points(x = Age, y = Weight, pch = 0, col = "blue"))
with(cautiv[cautiv$Season == "Mid", ], 
     points(x = Age, y = Weight, pch = 1, col = "green"))
with(cautiv[cautiv$Season == "Late", ], 
     points(x = Age, y = Weight, pch = 2, col = "red"))

points(x = c(0,23), y = c(2.6,7.9), pch = 5, col = "cyan")

x <- seq(0,300)
lines(x, gompertz(list(A = 21, K = 0.035, W0 = 2), x) + 0.0 * x, col = "red")

# peso vs. longitud (bioterio + cautiverio)
with(bioter, plot(x = largo, y = peso, pch = 4, col = "black", log = "xy"))
points(x = caut.l$media, y = rep.int(c(2.6,7.9),5), pch = 5, col = caut.l$crías)

# ajuste con asíntota lineal
par.gomp <- c(21, 0.035, 2.0, 0.0)
err.fn <- function(p)
{
  x <- bioter$edad * 365
  y <- bioter$peso
  n <- bioter$n.peso
  yfit<- gompertz(list(A = p[1], K = p[2], W0 = p[3]), x) + p[4] * x
  return(sum((y - yfit)^2/n))
}
fit.gomp <- optim(par.gomp, err.fn)
x <- seq(0,300)
lines(x, with(fit.gomp, gompertz(list(A = par[1], K = par[2], W0 = par[3]), x) + par[4] * x), col = "blue")

# ajuste con modelo logsigmoid

with(bioter, plot(x = log(edad), y = peso, pch = 4, col = "black", log=""))
par.lgsg <- c(10, 0, 5)
err.fn <- function(p)
{
  x <- log(bioter$edad[-1])
  y <- bioter$peso[-1]
  n <- bioter$n.peso[-1]
  yfit<- logsigmoid(list(a = p[1], b = p[2], c = p[3]), x)
  return(sum((y - yfit)^2/n))
}
fit.lgsg <- optim(par.lgsg, err.fn)

with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", log="x", xlim = c(1,400), ylim = c(0,30)))
x <- seq(0,400)
lines(x, logsigmoid(list(a = fit.lgsg$par[1], b = 0*fit.lgsg$par[2], c = fit.lgsg$par[3]), log(x/365)), col = "red")

with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", log="", xlim = c(1,400), ylim = c(1,30)))
x <- seq(0,400)
lines(x, logsigmoid(list(a = fit.lgsg$par[1], b = fit.lgsg$par[2], c = fit.lgsg$par[3]), log(x/365)), col = "red")

fit.wg <- lm(peso ~ log(edad), data = bioter[-1,])
summary(fit.wg)

with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", 
                  log="x", xlim = c(1,400), ylim = c(0,30)))
x <- seq(0,400)
lines(x, fit.wg$coefficients[1] + fit.wg$coefficients[2] * log(x/365), col = "red")

fit.sz <- lm(largo ~ edad + log(edad), data = bioter[-1,])
summary(fit.sz)

with(bioter, plot(x = edad, y = largo, pch = 4, col = "black", 
                  log="x", xlim = c(1/400,1), ylim = c(30,110)))
x <- seq(0,400)
lines(bioter$edad[-1], predict(fit.sz), col = "red")



# ajuste con modelo linexp

with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", log="x"))
par.lnex <- c(10, 0, 5)
err.fn <- function(p)
{
  x <- log(bioter$edad)
  y <- bioter$peso
  n <- bioter$n.peso
  yfit<- logsigmoid(list(a = p[1], b = p[2], c = p[3]), x)
  return(sum((y - yfit)^2/n))
}
fit.lgsg <- optim(par.lgsg, err.fn)

with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", log="x", xlim = c(1,400), ylim = c(0,50)))
x <- seq(0,400)
lines(x, logsigmoid(list(a = fit.lgsg$par[1], b = fit.lgsg$par[2], c = fit.lgsg$par[3]), log(x/365)), col = "red")

with(bioter, plot(x = edad * 365, y = peso, pch = 4, col = "black", log="", xlim = c(1,400), ylim = c(1,50)))
x <- seq(0,400)
lines(x, logsigmoid(list(a = fit.lgsg$par[1], b = fit.lgsg$par[2], c = fit.lgsg$par[3]), log(x/365)), col = "red")



# ajuste con asíntota constante
par.gomp <- c(21, 0.035, 2.0)
err.fn <- function(p)
{
  x <- bioter$edad * 365
  y <- bioter$peso
  n <- bioter$n.peso
  yfit<- gompertz(list(A = p[1], K = p[2], W0 = p[3]), x)
  return(sum((y - yfit)^2/n))
}
fit.gomp0 <- optim(par.gomp, err.fn)

x <- seq(0,300)
lines(x, with(fit.gomp0, gompertz(list(A = par[1], K = par[2], W0 = par[3]), x)), col = "blue")


# modelos de crecimiento
par.gomp.2 <- c(85, 365*0.06, 40, 365*0.05,2.5,0.5)

# ajuste con asíntota lineal
err.fn.2 <- function(p)
{
  x <- bioter$edad
  y <- bioter$largo
  n <- bioter$n.largo
  z <- bioter$peso
  m <- bioter$n.peso
  yfit <- gompertz(list(A = p[1], K = p[2], W0 = p[3]), x) + p[4] * x
  zfit <- exp(p[6] + p[5] * log(yfit/p[3]))
  
  return(sum((y - yfit)^2/n) + sum((z - zfit)^2/m))
}
fit.gomp.2 <- optim(par.gomp.2, err.fn.2, hessian = T)
df <- 2*16 - 6
covmat <- 2 * fit.gomp.2$value / df * solve(fit.gomp.2$hessian)
stderr <- sqrt(diag(covmat))

t(rbind(fit.gomp.2$par, stderr)) / c(1,365,1,365 * fit.gomp.2$par[1],1,1)

gompertz.plus <- function(p,x)
{
  y <- gompertz(list(A = p[1], K = p[2], W0 = p[3]), x) + p[4] * x
  z <- exp(p[6] + p[5] * log(y/p[3]))
  return(list(sz = y, wg = z))
}
x <- seq(0,300)
gplus.fit <- gompertz.plus(fit.gomp.2$par, x)

lines(x, gplus.fit$sz, col = "red")

lines(x, gplus.fit$wg, col = "red")


# Akodon molinae
# peso

A.molin <- data.frame(x = c(0,4,8,12,24,32,48),
                      wg.f.min = c(3,11,30,39,41,50,48),
                      wg.f.max = c(4,23,40,44,44,56,53),
                      wg.m.min = c(3, 5,32,38,47,52,52),
                      wg.m.max = c(4,23,44,44,65,58,57))
with(A.molin,
     plot(x, wg.f.min, col = "red", pch = "+", ylim = c(0,65)))
with(A.molin,
     points(x, wg.f.max, col = "red", pch = "+"))
with(A.molin,
     points(x, wg.m.min, col = "blue", pch = "+"))
with(A.molin,
     points(x, wg.m.max, col = "blue", pch = "+"))
x <- seq(0,60)
lines(x, gompertz(list(A = 50, K = 0.25, W0 = 2.5), x) + 0.05 * x, col = "gray")
