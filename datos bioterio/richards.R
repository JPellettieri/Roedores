tabla <- read.delim("tabla paper.txt", header = T, dec = ',') # Datos de Gomez de Villafañe

richards <- function(t,A,W0,K,d){
    return(A * (1 + ((W0/A)^(1-d)-1) * exp(-K*t/d^(d/(1-d))) )^(1/(1-d)) )
}

plt.t <- seq(0,1,1/121)

# ajuste del largo(edad) ----

plot(x = tabla$edad, y = tabla$largo)
lines(plt.t, richards(plt.t, 90,40,5,1.5), col = 'blue')

sz.chi2 <- function(par)
{
    return(sum(with(tabla, largo - richards(edad, par[1], par[2], par[3], par[4]))^2))
}

sz.fit <- optim(c(90,40,5,1.5), sz.chi2)

plt.s <- gr.model(plt.t, sz.fit$par)
plot(x = tabla$edad, y = tabla$largo)
lines(plt.t, with(sz.fit,richards(plt.t, par[1], par[2], par[3], par[4])), col = 'blue')


with(tabla, plot(x = edad, y = largo))
with(tabla, plot(x = largo, y = peso))

tplot <- seq(0,1,by=0.01)

plot(tplot, richards(tplot, 10,1,1,1.5), type = "l")
lines(tplot, richards(tplot, 10,1,1,0.75), col = "red")
lines(tplot, richards(tplot, 10,1,1,2.25), col = "blue")

plot(tplot, richards(tplot, 10,1,5,1.5), type = "l")
lines(tplot, richards(tplot, 10,1,2,1.5), col = "red")
lines(tplot, richards(tplot, 10,1,0.5,1.5), col = "blue")
