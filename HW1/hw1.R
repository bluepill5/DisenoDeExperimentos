
## ----echo=FALSE, warning=FALSE, message=FALSE, results='hide'------------
# Librerías requeridas
library(ggplot2)
library(grid)
library(reshape2)
library(abd)
library(nortest)
# Cragamos los datos necesarios
data(KenyaFinches)
attach(KenyaFinches)
# Para personalizar las gráficas (NO incluye leyendas)
theme.b5.basic <- theme_grey(base_family = "mono") + # Font of the tittle
    theme(text = element_text(face = "bold", size = 13, colour = "Black")) + # Font of the axes
    theme(legend.key.size = unit(1, "cm")) +
    theme(legend.text = element_text(size = 10))
# Función para gráficar el boxplot
myBoxplot <- function(datos, factor, variable, title, xlab = "", ylab = "") {
    ggplot(datos, aes_string(x = factor, y = variable, fill = factor)) + 
        geom_boxplot(lwd = 1.5) + 
        stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) + 
        stat_boxplot(geom ="errorbar") +
        guides(fill = FALSE) + # colour, fill, shape
        labs(title = title, x = xlab, y = ylab) + 
        theme.b5.basic
}
# Función para gráficar un QQPlot
ggqnorm <- function(lm) {
    y <- quantile(lm$resid[!is.na(lm$resid)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y) / diff(x)
    int <- y[1L] - slope * x[1L]
    p <- ggplot(lm, aes(sample = .resid)) +
        stat_qq(alpha = 0.5) +
        geom_abline(slope = slope, 
                    intercept = int, 
                    color = "#CD2626") + 
        labs(title = "QQnorm-plot",
             x = "Teorícos",
             y = "Muestrales") + 
        theme.b5.basic
    p
}
# Función para crear un grid con varios gráficos
vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
}



## ------------------------------------------------------------------------
# Obtenemos la probailidad de que X > 1
pnorm(1, mean = 0.5, sd = sqrt(1), lower.tail = F)


## ----echo=FALSE, fig.width=7, fig.height=4, fig.align='center'-----------
p <- pnorm(1, mean = 0.5, sd = sqrt(1), lower.tail = F)

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
    stat_function(fun = function(x) dnorm(x, mean = 0.5, sd = 1), 
                  size = 1) + 
    geom_vline(aes(xintercept = p), color = "#8B0000", 
               linetype = "dashed", size = 1) + 
    labs(title ="Función de densidad ~N(0.5,1)", x = "x", y = "f(x)") +
    annotate("text", x = 1.3, y = 0.1, label = "P(x>1) = 0.308", 
             size = 5, colour = "#8B0000") + 
    theme.b5.basic


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
# Guardamos los volúmenes capturadas de las dos máquinas
vol <- c(16.03, 16.01, 16.04, 15.96, 16.05, 15.98, 16.05, 16.02, 16.02,
         15.99, 16.02, 16.03, 15.97, 16.04, 15.96, 16.02, 16.01, 16.01, 
         15.99, 16.00)
# Asignamos los valores a un data.frame
datos <- data.frame(trat = factor(rep(c("máquina 1","máquina 2"), 
                                      each = 10)), vol)
n1 <- 10
n2 <- 10
# Gráficamos un boxplot de los datos
myBoxplot(datos, "trat", "vol", "Boxplot por máquina")


## ----warning=FALSE-------------------------------------------------------
# Pruebe la hipótesis de que el promedio de llenado de las dos máquinas 
# es el mismo vs. es diferente
# Bajo el supuesto de varianzas iguales:
# Método largo
# Manipulamos los datos para los cálculos
datos <- data.frame(maquina1=subset(datos, trat == "máquina 1")[, "vol"],
                    maquina2=subset(datos, trat == "máquina 2")[, "vol"])
# Cálculo de las medias
colMeans(datos)
# Cálculo de la diferencia de medias
dif <- as.numeric(colMeans(datos)[1] - colMeans(datos)[2])
S2_1 <- var(datos[, 1])
S2_2 <- var(datos[, 2])
S2_p <- ((n1 - 1) * S2_1 + (n2 - 1) * S2_2) / (n1 + n2 - 2)

t_0 <- dif/(sqrt(S2_p*(1/n1 + 1/n2))); t_0

# Región de rechazo
alpha <- 0.05
gl <- n1 + n2 - 2
t <- qt(1 - (alpha/2), gl)
# P-value de 2 colas
pt(t_0, gl, lower.tail = FALSE) * 2

# Método corto
t.test(datos[, 1], datos[, 2])


## ----warning=FALSE-------------------------------------------------------
# Bajo el supuesto de varianzas diferentes:
# Método largo
# t welch
tw <- dif / (sqrt(S2_1/n1 + S2_2/n2)); tw
#región de rechazo
gl_tw <- (S2_1/n1 + S2_2/n2)**2 / (((S2_1/n1)**2/(n1 - 1)) + ((S2_2/n2)**2/(n2-1)))
t2 <- qt(1-(alpha/2), gl_tw); t2

# P-value
pt(tw, gl_tw, lower.tail = FALSE) * 2

# Método Corto
t.test(datos[, 1], datos[, 2], var.equal=F)


## ----warning=FALSE-------------------------------------------------------
var.test(datos[, 1], datos[, 2])


## ----warning=FALSE-------------------------------------------------------
# Bajo el supuesto de homocedasticidad
ic_vi <- cbind(dif - t * sqrt(S2_p*(1/n1 + 1/n2)),
               dif + t * sqrt(S2_p*(1/n1 + 1/n2)))
colnames(ic_vi) <- cbind("límite inferior","límite superior");ic_vi
# Amplitud del intervalo
diff(as.vector(ic_vi))
# Bajo el supuesto de no homocedasticidad
ic_vd <- cbind(dif - t2 * sqrt(S2_1/n1 + S2_2/n2),
               dif + t2 * sqrt(S2_1/n1 + S2_2/n2))
colnames(ic_vd) <- cbind("límite inferior","límite superior");ic_vd
# Amplitud del intervalo
diff(as.vector(ic_vd))


## ------------------------------------------------------------------------
alpha <- 0.05
qf(1 - alpha, df1= 3, df2 = 92)


## ------------------------------------------------------------------------
alpha <- 0.01
qf(1 - alpha, df1= 3, df2 = 92)


## ------------------------------------------------------------------------
# Significancia del 5%
F95 <- qf(0.95, df1 = 4, df2 = 20); F95
# Significancia del 1%
F99 <- qf(0.99, df1 = 4, df2 = 20); F99


## ----echo=FALSE, fig.width=7, fig.height=4, fig.align='center'-----------
# Gráficas para ubicar F0
ggplot(data.frame(x = c(0, 6)), aes(x)) + 
    stat_function(fun = function(x) df(x, df1 = 4, df2 = 20), size = 1) + 
    geom_vline(aes(xintercept = 3.5),
               color = "#548B54", linetype = "dashed", size = 1) + 
    annotate("text", x = 3.9, y = 0.05, label = paste("F == ", 3.5), 
             size = 5, col = "#548B54", parse = T) +
    geom_vline(aes(xintercept = F95),
               color = "#8B0000", linetype = "dashed", size = 1) +
    annotate("text", x = 3.1, y = 0.1, label = paste("alpha == ", 0.05), 
             size = 5, col = "#8B0000", parse = T) + 
    geom_vline(aes(xintercept = F99),
               color = "#00688B", linetype = "dashed", size = 1) +
    annotate("text", x = 5, y = 0.030, label = paste("alpha == ", 0.01), 
             size = 5, col = "#00688B", parse = T) + 
    labs(title ="Función de densidad ~ F(4,20)", x = "x", y = "f(x)") + 
    theme.b5.basic


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
hemo <- c(6.7, 7.8, 5.5, 8.4, 7.0, 7.8, 8.6, 7.4, 5.8, 7.0, 9.9, 8.4,10.4
          ,9.3, 10.7, 11.9, 7.1, 6.4, 8.6, 10.6, 10.4, 8.1, 10.6, 8.7, 
          10.7, 9.1, 8.8, 8.1, 7.8, 8.0, 9.9, 9.3, 7.2, 7.8, 9.3, 10.2, 
          8.7, 8.6, 9.3, 7.2)
trat <- c(rep(0, 10), rep(5, 10), rep(10, 10), rep(15,10))
trat <- as.factor(trat)
peces <- data.frame(trat,hemo)
# Gráficamos un boxplot de los datos
myBoxplot(peces, "trat", "hemo", "Boxplot: tratamientos~hemoglobina", 
          xlab = "Sulfametazina en comida (g/100 lb)",
          ylab = "Concentración de hemoglobina (g/100ml)")


## ------------------------------------------------------------------------
# Creamos un pequeña funciòn para calcular la tabla ANOVA
myANDEVA <- function(n, t, data, value, factor) {
    # Crea la tabla de análisis de varianza
    # n: Tamaño de la muestra
    # t: Número de tratamientos
    # data: Data.frame con los datos
    # value: Variable respuesta
    # factor: Variable que indica el factor
    
    # Calculamos la media general
    mu <- mean(data[, value])
    # Calculamos la suma de los errores del modelo reducido
    SSE_r <- sum((data[, value] - mu)**2)
    # Ahora calculamos la suma de los errores del modelo completo
    SSE_c <- sum(
        unlist(
            lapply(
                split(data, data[, factor]), 
                function(x) sum((x[, value] - mean(x[, value]))**2))))
    # Calculamos la suma de los errores por tratamiento
    SS_t <- SSE_r - SSE_c
    # Calculamos la suma de los errores totales
    SS_total <- SS_t + SSE_c
    # Calculamos la suma de los errores entre sus grados de libertad
    CM_t <- SS_t / (t - 1)
    CME <- SSE_c / (n - t)
    # Claculamos el estadístico de prueba F_0
    F_0 <- CM_t / CME
    # Ahora el p-value
    p_val <- pf(F_0, t - 1, n - t, lower.tail = FALSE)
    # Ahora creamos la tabla
    gl <- c(t - 1, n - t, n - 1)
    SS <- round(c(SS_t, SSE_c, SS_total), digits = 2)
    CM <- c(round(CM_t, digits = 2), round(CME, digits = 2), "")
    F_value <- c(round(F_0, digits = 2), "", "")
    p_value <- c(round(p_val, digits = 4), "", "")
    andeva <- data.frame(gl, SS, CM, F_value, p_value)
    # Nombramos a las columnas y renglones
    colnames(andeva) <- c("g.l.", "S.S.", "C.M.", "F", "p-value")
    rownames(andeva) <- c("Tratamientos", "Error", "Total")
    andeva
}
myANDEVA(n = 40, t = 4, data = peces, value = "hemo", factor = "trat")
# O ajustando el modelo
pecesMod <- lm(hemo ~ trat, data = peces)
anova(pecesMod)


## ------------------------------------------------------------------------
# Prueba de Tukey
tukey <- TukeyHSD(aov(pecesMod), conf.level = 0.95); tukey 


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
plot(tukey)


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
# QQPlot
ggqnorm(pecesMod)
# Gráfica de residuales
e <- pecesMod$residuals
ggplot(data = data.frame(x = peces$trat, y = e), aes(x = x, y = y)) + 
    geom_point(colour = "#00688B", size = 3) + 
    labs(title = "Residuales", x = "x", y = "residual") +
    theme.b5.basic


## ------------------------------------------------------------------------
#Prueba de normalidad Anderson-Darling
ad.test(pecesMod$res)
#Prueba de bartlett
bartlett.test(pecesMod$res, peces$trat)


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
peso <- c(9.632, 9.086, 8.67, 8.51, 9.084, 8.797, 9.188, 9.215, 9.099,
          9.851, 8.426, 8.532, 7.724, 8.467, 7.477, 7.503, 7.391, 8.379,
          7.638, 8.528)
perros <- data.frame(sexo = factor(rep(c("Macho","Hembra"),
                                       each = 10)), peso)
# Gráficamos un boxplot de los datos
myBoxplot(perros, "sexo", "peso", "Boxplot: peso~sexo",
          xlab = "Sexo",
          ylab = "Peso (kg)")


## ------------------------------------------------------------------------
myANDEVA(n = 20, t = 2, data = perros, value = "peso", factor = "sexo")
# O ajustando el modelo
perrosMod <- lm(peso ~ sexo, data = perros)
anova(perrosMod)


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
# QQPlot
ggqnorm(perrosMod)
# Gráfica de residuales
e <- perrosMod$residuals
ggplot(data = data.frame(x = perros$sexo, y = e), aes(x = x, y = y)) + 
    geom_point(colour = "#00688B", size = 3) + 
    labs(title = "Residuales", x = "x", y = "residual") +
    theme.b5.basic


## ------------------------------------------------------------------------
#Prueba de normalidad Anderson-Darling
ad.test(perrosMod$res)
#Prueba de bartlett
bartlett.test(perrosMod$res, perros$sexo)


## ------------------------------------------------------------------------
str(KenyaFinches)
# Veamos una mestra de la base
sample(KenyaFinches, 6)[, -4]
# Observamos el número de observaciones por pinzón
table(KenyaFinches$species)


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
# Gráficamos un boxplot de los datos
myBoxplot(KenyaFinches, "species", "beak.length", 
          "Boxplot: pico~especie",
          xlab = "Especie",
          ylab = "Longitud pico (mm)")


## ------------------------------------------------------------------------
myANDEVA(n = 45, t = 3, data = KenyaFinches, 
         value = "beak.length", factor = "species")
# O ajustando el modelo
pinzonMod <- lm(beak.length ~ species, data = KenyaFinches)
anova(pinzonMod)


## ------------------------------------------------------------------------
# Prueba de Tukey
tukey <- TukeyHSD(aov(pinzonMod), conf.level = 0.95); tukey


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
plot(tukey)


## ----echo=FALSE, fig.height=4, fig.width=5, fig.align='center'-----------
# QQPlot
ggqnorm(pinzonMod)
# Gráfica de residuales
e <- pinzonMod$residuals
ggplot(data = data.frame(x = KenyaFinches$species, y = e), 
       aes(x = x, y = y)) + 
    geom_point(colour = "#00688B", size = 3) + 
    labs(title = "Residuales", x = "x", y = "residual") +
    theme.b5.basic


## ------------------------------------------------------------------------
#Prueba de normalidad Anderson-Darling
ad.test(pinzonMod$res)
#Prueba de bartlett
bartlett.test(pinzonMod$res, KenyaFinches$species)


