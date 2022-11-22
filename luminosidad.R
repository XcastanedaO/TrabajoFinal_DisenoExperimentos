library(ggplot2)
library(car)
require(agricolae)
library(pwr)
# --------------- Intensidad lumínica (lux) -----------------------------------
# Registro de intensidad lumínica:
y_luz <- c(40, 10, 22, 26,
           28, 31, 44, 33,
           6, 6, 7, 14,
           41, 44, 51, 7,
           26, 31, 45, 11)

# Marca de la pila en orden de medición:
marca_luz <-c("Duracell", "Tronex", "Futura","Futura",
              "Varta","Varta", "Duracell", "Panasonic",
              "Tronex", "Tronex", "Futura", "Futura",
              "Varta", "Panasonic", "Panasonic", "Tronex",
              "Panasonic", "Duracell", "Duracell", "Varta")

# Linternas en orden de medición:
Linterna_luz <- c("3", "1", "2", "4",
                  "1", "4", "2", "3",
                  "2", "4", "1", "3", 
                  "2", "1", "4", "3", 
                  "2", "4", "1", "3")

# Dataframe con los registros de intensidad lumínica, marcas y linternas:
luz <- data.frame(y_luz, marca_luz,Linterna_luz)

## Análisis descriptivo para luminosidad:
# Medidas descriptivas de acuerdo a la linterna:
by(luz$y_luz, luz$Linterna_luz, summary)
by(luz$y_luz, luz$Linterna_luz, sd)

# Boxplot de luminosidad de acuerdo a la linterna:
ggplot(data = luz, aes(x = Linterna_luz, y = y_luz, fill = factor(Linterna_luz))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Luminosidad") + 
  scale_x_discrete(name = "Linterna") +
  ggtitle("Boxplot: Luminosidad de acuerdo a la linterna") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=y_luz, x=Linterna_luz),fun=mean, geom="point", shape=20,
               size=4, color="gold", position = position_dodge(0.75))+
  scale_fill_manual(values = c("maroon1", "chocolate","darkolivegreen3", "deepskyblue3"))+labs(fill = "Linterna")+
  theme_minimal()

## Medidas descriptivas de acuerdo a la marca:
by(luz$y_luz, luz$marca_luz, summary)
by(luz$y_luz, luz$marca_luz, sd)

# Boxplot de luminosidad de acuerdo a la marca:
ggplot(data = luz, aes(x = marca_luz, y = y_luz, fill = factor(marca_luz))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Luminosidad") + 
  scale_x_discrete(name = "Marca") +
  ggtitle("Boxplot: Luminosidad de acuerdo a la marca de pila") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=y_luz, x=marca_luz),fun=mean, geom="point", shape=20,
               size=4, color="gold", position = position_dodge(0.75))+
  scale_fill_manual(values = c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ))+labs(fill = "Marca")+
  theme_minimal()

# ANOVA para luminosidad con un diseño de un factor con un bloque:
modelo_luz <- aov(y_luz~marca_luz+Linterna_luz, data = luz)
summary(modelo_luz)

# ANOVA para luminosidad con diseño de un factor: marca.
modelo_luz2 <- aov(y_luz~marca_luz, data = luz)
summary(modelo_luz2)

### Validación de supuestos:

## Diagnóstico de normalidad:
# Q-Q plor
qqnorm(residuals(modelo_luz2), pch = 19)
qqline(residuals(modelo_luz2))
# Test de Shapiro:
shapiro.test(residuals(modelo_luz2))

## Diagnóstico de homogeneidad de varianza:
# Test de Bartlett:
bartlett.test(y_luz~marca_luz)
# Gráfico de residuales versus valores ajustados:
plot(fitted(modelo_luz2),residuals(modelo_luz2),pch=19, cex=1, 
     col=c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ), 
     xlab = "Valores ajustados", ylab = "Residuales", main = "Residuales vs. valores ajustados")
abline(h=0)

## Independencia de los errores:
# Prueba de independencia Durbin Watson
library(car)
durbinWatsonTest(modelo_luz2)
plot(residuals(modelo_luz2), pch =16, ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales",
     col=c("deepskyblue3", "chocolate","darkolivegreen3", "orange", "maroon1"),)
abline(h=0)

## Comparaciones múltiples: 
# Método de Tukey
tukey <- TukeyHSD(modelo_luz2, "marca_luz")
tukey

# Método de mínima diferencia:
mds <- LSD.test(modelo_luz2, "marca_luz")
mds$statistics$LSD
mds$groups

## Tamaño de muestra 

# Con mean(diff)
f1 = sqrt(17.33^2/(2*5*79.2))
pwr.anova.test(f = f1, k=5, power=0.9, sig.level=0.05)

## Potencia:
f1 = sqrt(17.33^2/(2*5*79.2))
pwr.anova.test(f = f1, k=5, power=0.4413751, sig.level=0.05)
