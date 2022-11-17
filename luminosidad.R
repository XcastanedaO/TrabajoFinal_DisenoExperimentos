
############################### Luminosidad ########################
######### Luz #########
y_luz <- c(40, 10, 22, 26,
           28, 31, 44, 33,
           6, 6, 7, 14,
           41, 44, 51, 7,
           26, 31, 45, 11)

marca_luz <-c("Duracell", "Tronex", "Futura","Futura",
              "Varta","Varta", "Duracell", "Panasonic",
              "Tronex", "Tronex", "Futura", "Futura",
              "Varta", "Panasonic", "Panasonic", "Tronex",
              "Panasonic", "Duracell", "Duracell", "Varta")

operador_luz <- c("Ximena", "Valentina", "Veronica", "Yojan",
                  "Valentina", "Yojan", "Veronica", "Ximena",
                  "Veronica", "Yojan", "Valentina", "Ximena", 
                  "Veronica", "Valentina", "Yojan", "Ximena", 
                  "Veronica", "Yojan", "Valentina", "Ximena")

luz <- data.frame(y_luz, marca_luz,operador_luz)

##### ---------------------------- Descriptivos luminosidad:
# Por operador:
by(luz$y_luz, luz$operador_luz, summary)
by(luz$y_luz, luz$operador_luz, sd)

ggplot(data = luz, aes(x = operador_luz, y = y_luz, fill = factor(operador_luz))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red") +
  scale_y_continuous(name = "Luminosidad") + 
  scale_x_discrete(name = "Operador") +
  ggtitle("Boxplot: Luminosidad de acuerdo al operador") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=y_luz, x=operador_luz),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("blue", "red","green", "pink"))+labs(fill = "Operador")

### --------------- Descriptivos por marca
by(luz$y_luz, luz$marca_luz, summary)
by(luz$y_luz, luz$marca_luz, sd)

ggplot(data = luz, aes(x = marca_luz, y = y_luz, fill = factor(marca_luz))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red") +
  scale_y_continuous(name = "Luminosidad") + 
  scale_x_discrete(name = "Marca") +
  ggtitle("Boxplot: Luminosidad de acuerdo al operador") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=y_luz, x=marca_luz),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("blue", "red","green", "pink", "cyan"))+labs(fill = "Marca")

### ------------- Por operador y marca
# No tiene mucho sentido pues solo hay una observación por tratamiento pero ayuda a mirar variabilidad:
ggplot(luz,aes(operador_luz,y_luz, fill = operador_luz))+geom_boxplot()+facet_wrap(~marca_luz)+
  stat_summary(aes(y=y_luz, x=operador_luz),fun=mean, geom="point", shape=20,size=4, color="yellow", position = position_dodge(0.75))+
  labs(fill = "Operador", x = "Operador", y = "Luminosidad")


ggplot(luz,aes(marca_luz,y_luz, fill = marca_luz))+geom_boxplot()+facet_wrap(~operador_luz)+
  stat_summary(aes(y=y_luz, x=marca_luz),fun=mean, geom="point", shape=20,size=4, color="yellow", position = position_dodge(0.75))+
  labs(fill = "Marca", x = "Marca", y = "Voltaje")


#Modelo 
modelo_luz <- aov(y_luz~marca_luz+operador_luz, data = luz)
summary(modelo_luz)

#Modelo sin bloque 

modelo_luz2 <- aov(y_luz~marca_luz, data = luz)
summary(modelo_luz2)

#Normalidad
qqnorm(residuals(modelo_luz2))
qqline(residuals(modelo_luz2))

shapiro.test(residuals(modelo_luz2))

# Homogeneidad
bartlett.test(y_luz~marca_luz)
plot(fitted(modelo_luz2),residuals(modelo_luz2))
abline(h=0)

#Gráfico de residuales vs orden de recolección
plot(residuals(modelo_luz2), pch =16, ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales")
abline(h=0)


### Orden 

y_luz_orden <- c(40, 10, 22, 26,
                 28, 31, 44, 33,
                 6, 6, 7, 14,
                 41, 44, 51, 7,
                 26, 31, 45, 11)
luz_marca_orden <- c("Duracell", "Tronex", "Futura", "Futura",
                   "Varta", "Varta", "Duracell", "Panasonic", 
                   "Tronex", "Tronex", "Futura", "Futura",
                   "Varta", "Panasonic", "Panasonic", "Tronex",
                   "Panasonic", "Duracell", "Duracell", "Varta")

luz_orden <- data.frame(y_luz_orden, luz_marca_orden)

mod_luz_orden <- aov(y_luz_orden~luz_marca_orden, data = luz_orden)
summary(mod_luz_orden)
# Independencia
#Prueba de independencia Durbin Watson
library(car)
durbinWatsonTest(mod_luz_orden)

#Comparaciones 

tukey <- TukeyHSD(modelo_luz2, "marca_luz")
tukey

require(agricolae)
duncann <- duncan.test(modelo_luz2, "marca_luz")
duncann

mds <- LSD.test(modelo_luz2, "marca_luz")
mds

# Tamaño de muestra 
library(pwr)
f1 = sqrt(13.40866^2/(2*5*79.2))
pwr.anova.test(f = f1, k=5, power=0.9, sig.level=0.05)
