# setwd("~/Documents/Ire/allometric growth/datos bioterio")
setwd("~/Documentos/Ire/growth model/datos bioterio")

tabla <- read.delim("tabla paper.txt", header = T, dec = ',') # Datos de Gomez de Villafañe

# Ajuste exploratorio peso vs. largo
with(tabla, plot(x = largo, y = peso))
allo.sz.wg <- lm(log(peso) ~ log(largo), data = tabla)
summary(allo.sz.wg)
x.plt = seq(30, 100, 1)
y.plt = exp(allo.sz.wg$coefficients[1] + allo.sz.wg$coefficients[2] * log(x.plt))
lines(x.plt,y.plt)

# modelo generico exp + lin
gr.model <- function(edad, par)
{
  gr.0 <- par[1]
  gr.1 <- par[2]
  r0 <- par[3]
  r1 <- par[4]
  lin.growth <- gr.0 + r0 * edad
  exp.growth <- gr.1 * (1 - exp(- r1 * edad))
  return(lin.growth + exp.growth)
}

# Ajuste exploratorio del largo(edad)
sz.chi2 <- function(par)
{
  return(sum(with(tabla, largo - gr.model(edad, par))^2))
}

sz.fit <- optim(c(35, 65, 0.0, 6.6), sz.chi2)

plt.t <- seq(0,1,1/121)
plt.s <- gr.model(plt.t, sz.fit$par)
plot(x = tabla$edad, y = tabla$largo)
lines(plt.t, plt.s, col = 'blue')

# Ajuste exploratorio del peso(edad)
wg.chi2 <- function(par)
{
  return(sum(with(tabla, peso - gr.model(edad, par))^2))
}
wg.fit <- optim(c(2, 20, 0.0, 6.6), wg.chi2)

plt.t <- seq(0,1,1/121)
plt.w <- gr.model(plt.t, wg.fit$par)
plot(x = tabla$edad, y = tabla$peso)
lines(plt.t, plt.w, col = 'blue')

sz.fit$par
# 35.09973 48.94434 17.06019 20.62321
wg.fit$par
# 1.730990 14.735766  7.671033 11.674711

# Ajuste exploratorio del largo y peso vs edad
szwg.chi2 <- function(par)
{
  largo.fit <- with(tabla, gr.model(edad, par[1:4]))
  peso.fit  <- exp(par[5] + par[6] * log(largo.fit))
  return(sum(with(tabla, (largo - largo.fit)^2 + (peso - peso.fit)^2)))
}

sz.fit2 <- optim(c(sz.fit$par, allo.sz.wg$coefficients), szwg.chi2, hessian = T)

plt.t <- seq(0,1,1/121)
plt.s <- gr.model(plt.t, sz.fit2$par[1:4])
plt.w <- exp(sz.fit2$par[5] + sz.fit2$par[6] * log(plt.s))
plot(x = tabla$edad, y = tabla$largo)
lines(plt.t, plt.s, col = 'blue')
plot(x = tabla$edad, y = tabla$peso)
lines(plt.t, plt.w, col = 'blue')

sz.fit2$par
# 35.309846   48.692025   17.388865   20.341996   -8.175008    2.463212
