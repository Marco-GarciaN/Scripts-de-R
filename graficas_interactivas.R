##### Graficas Interactivas #####

##### Ejemplo 1: Ajuste a la Distribución Exponencial ######

library(ggplot2)

# Datos Truncados
datosB <- c(27,82,115,126,155,161,243,294,340,384,457,
            680,855,877,974,1193,1340,1884,2558,3476)

x <- datosB[datosB>50]
x <- sort(x)
n <- length(x)
j <- c(1:n)
Fx_e <- j/n

theta <- sum(x-50)/n

Fexp <- function(x){ 1-exp(-(x-50)/theta) }

Fx_m <- Fexp(x)

Datos_Et <- data.frame(j,x,Fx_e, Fx_m)
colnames(Datos_Et) <- c("j","x","Fn(x)","F*(x)")

#### Grafica del Modelo Empirico y Teorico ####

ggplot(Datos_Et, aes(x = x)) +
  geom_step(aes(y = `Fn(x)`, colour = "Empírico"), lwd=1.1) + 
  geom_function(fun =~ Fexp(.x), aes(colour = "Teórico"), lwd=1.1) +  
  labs(title="Ajuste Exponencial", 
       subtitle="Comparación Grafica de Modelos", 
       caption = "Fuente: Loss models: from data to decisions (4.ª ed.), ed. Wiley.",
       x="x", y="F(x)") + 
  scale_colour_manual(values = c("black", "cyan4")) +
  theme(legend.title = element_blank())

library(plotly)

g <- ggplot(Datos_Et, aes(x = x)) + 
  geom_step(aes(y = `Fn(x)`, colour = "Empírico"), lwd=0.5) + 
  geom_line(aes(y = `F*(x)`, colour = "Teórico"), lwd=0.5, alpha=0.9) + 
  labs(title="Modelo Teórico y Empírico", 
       x="x", y="F(x)", color="Modelo:") + 
  scale_colour_manual(values = c("black", "cyan4"))

ggplotly(g)


library(gganimate)

gg <- ggplot(Datos_Et, aes(x = x)) +
  geom_step(aes(y = `Fn(x)`, colour = "Empírico"), lwd=1.1) + 
  geom_line(aes(y = `F*(x)`, colour = "Teórico"), lwd=1.1, alpha=0.8) +
  labs(title="Ajuste Exponencial", 
       subtitle="Comparación Grafica de Modelos", 
       caption = "Fuente: Loss models: from data to decisions (4.ª ed.), ed. Wiley.",
       x="x", y="F(x)") + 
  scale_colour_manual(values = c("black", "cyan4")) +
  theme(legend.title = element_blank())

gg + transition_reveal(x)


#### Diferencias ####

Datos_Et$`D(x)` <- Datos_Et$`Fn(x)`- Datos_Et$`F*(x)`

ggplot(Datos_Et, aes(x = x)) +
  geom_point(aes(y =`D(x)`), cex = 2, col="cyan4") +
  geom_line(aes(y =`D(x)`), lwd = 0.5, col="cyan4") +
  labs(title="Ajuste Exponencial", 
       subtitle="Grafica de D(x)", 
       caption = "Fuente: Loss models: from data to decisions (4.ª ed.), ed. Wiley.",
       x="x", y="D(x)")

#### P-P Plot ####

Datos_Et$`F(n+1)(x)` <- Datos_Et$j/(length(Datos_Et$x)+1)

ggplot(Datos_Et, aes(x = `F(n+1)(x)`, y =`F*(x)`)) + geom_point (col="black") +
  geom_abline(intercept = 0, slope = 1, col="cyan4", lwd=1) +
  labs(title="Ajuste Exponencial", 
       subtitle="p-p plot", 
       caption = "Fuente: Loss models: from data to decisions (4.ª ed.), ed. Wiley.",
       x="F(n+1)(x)", y="F*(x)")

#### Prueba Kolmogorov Smirnov #####

Datos_Et$`Fn(x-)` <- (Datos_Et$j-1)/length(Datos_Et$x)

Datos_Et$`|Fn(x)-F*(x)|` <- abs(Datos_Et$`Fn(x)` - Datos_Et$`F*(x)`)

Datos_Et$`|Fn(x-)-F*(x)|` <- abs(Datos_Et$`Fn(x-)` - Datos_Et$`F*(x)`)

for (i in 1:length(Datos_Et$x)) {
  Datos_Et$`Diferencia Maxima`[i] <- 
    max(Datos_Et$`|Fn(x)-F*(x)|`[i], Datos_Et$`|Fn(x-)-F*(x)|`[i])
}

D_Expt <- max(Datos_Et$`Diferencia Maxima`)
D_Expt

# Nivel de significancia de 5% = 0.05
VC_Expt <- 1.36/sqrt(length(Datos_Et$x))
VC_Expt


