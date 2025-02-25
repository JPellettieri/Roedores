tabla <- read.delim("tabla paper.txt", header = T, dec = ',') # Datos de Gomez de Villafañe

with(tabla, plot(x = edad, y = peso))
with(tabla, plot(x = edad, y = largo))
with(tabla, plot(x = largo, y = peso))

with(tabla, points(x = edad, y = peso, col='blue', pch = '+'))
with(tabla, points(x = edad, y = largo, col='blue', pch = '+'))

allo.sz.wg <- lm(log(peso) ~ log(largo), data=tabla)
summary(allo.sz.wg)
plot(allo.sz.wg)

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

# ajuste del largo(edad)

sz.chi2 <- function(par)
{
  return(sum(with(tabla, largo - gr.model(edad, par))^2))
}

sz.fit <- optim(c(35, 65, 0.0, 6.6), sz.chi2)

plt.t <- seq(0,1,1/121)
plt.s <- gr.model(plt.t, sz.fit$par)
plot(x = tabla$edad, y = tabla$largo)
lines(plt.t, plt.s, col = 'red')

# ajuste del peso(edad)

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

