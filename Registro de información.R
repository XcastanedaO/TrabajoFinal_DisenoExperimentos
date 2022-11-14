library(ggplot2)

############### Verónica ########
vero_panasonic_in <- (1.6 + (1.5 + 1.6)/2 + 1.6)/3
vero_panasonic_fin <- (0.9 + 0.9 + 0.9 )/3
vero_panasonic_dif <- vero_panasonic_in - vero_panasonic_fin
  
vero_futura_in <- (1.6 + (1.5 + 1.6)/2 + (1.5 + 1.6)/2 )/3
vero_futura_fin <- (0.9 + 0.9 + 0.9 )/3
vero_futura_dif <- vero_futura_in - vero_futura_fin
  
vero_tronex_in <- (1.6 + (1.5+1.6)/2+ (1.6+1.7)/2)/3
vero_tronex_fin <- (0.9 + 0.9 + (0.8 + 0.9)/2)/3
vero_tronex_dif <- vero_tronex_in - vero_tronex_fin

vero_varta_in <- ((1.5+1.6)/2 + (1.4+1.5+1.6)/3 + (1.4+1.5+1.6)/3)/3
vero_varta_fin <- ((0.9 + 1.0)/2 + (0.9 + 1.0)/2 + (0.9 + 1.0)/2)/3
vero_varta_dif <- vero_varta_in - vero_varta_fin

vero_duracell_in <- ((1.5 + 1.6)/2 + 1.6 + 1.5)/3
vero_duracell_fin <- (0.9 + (0.8 + 0.9)/2 + (0.8 + 0.9)/2)/3
vero_duracell_dif <- vero_duracell_in - vero_duracell_fin

############### Valentina ########
vale_panasonic_in <- (1.6 + 1.6 + 1.6)/3
vale_panasonic_fin <- (0.9 + (0.8 + 0.9)/2 + (1.0 + 0.9)/2 )/3
vale_panasonic_dif <- vale_panasonic_in - vale_panasonic_fin

vale_futura_in <- (1.6  + 1.5 + 1.6)/3
vale_futura_fin <- (0.8 + (0.8+0.9)/2 + 1.0)/3
vale_futura_dif <- vale_futura_in - vale_futura_fin
  
vale_tronex_in <- ((1.6 + 1.7)/2+ (1.6 + 1.7)/2+ 1.6)/3
vale_tronex_fin <- (1.0 + 1.0 + 0.7)/3
vale_tronex_dif <- vale_tronex_in - vale_tronex_fin

vale_varta_in <- (1.6+(1.5 + 1.6)/2+ 1.6)/3
vale_varta_fin <- ((0.8+0.9)/2 +0.9+0.9)/3
vale_varta_dif <- vale_varta_in - vale_varta_fin

vale_duracell_in <- (1.5 + 1.5 + 1.6)/3
vale_duracell_fin <- (1.0 + 1.0 +1.0)/3
vale_duracell_dif <- vale_duracell_in - vale_duracell_fin

############### Yojan ########
Yojan_panasonic_in <- (1.6 + 1.6 + 1.6)/3
Yojan_panasonic_fin <- (0.9 + (0.8 + 0.9)/2 + 0.9)/3
Yojan_panasonic_dif <- Yojan_panasonic_in - Yojan_panasonic_fin
  
Yojan_futura_in <- ((1.5 + 1.6)/2+(1.5 + 1.6)/2+ 1.6)/3
Yojan_futura_fin <- (0.9+0.9+(1.0 + 0.9)/2)/3
Yojan_futura_dif <- Yojan_futura_in - Yojan_futura_fin
    
Yojan_tronex_in <- ((1.5+1.6)/2 + 1.6 + 1.6)/3
Yojan_tronex_fin <- (0.9 + (1.0+1.1)/2 + 0.9)/3
Yojan_tronex_dif <- Yojan_tronex_in - Yojan_tronex_fin

Yojan_varta_in <- (1.6+1.6+1.6)/3
Yojan_varta_fin <- (0.9 + 0.9 + 0.9)/3
Yojan_varta_dif <- Yojan_varta_in - Yojan_varta_fin

Yojan_duracell_in <- (1.6 + (1.5 +1.6)/2 + (1.5 +1.6)/2)/3
Yojan_duracell_fin <- (0.9 + 0.9 + 0.9 )/3
Yojan_duracell_dif <- Yojan_duracell_in - Yojan_duracell_fin

############### Ximena ########
Xim_panasonic_in <- (1.6 + 1.6 + (1.5 + 1.6)/2)/3
Xim_panasonic_fin <- (1 + 0.9 + 1)/3
Xim_panasonic_dif <- Xim_panasonic_in - Xim_panasonic_fin

Xim_futura_in <- (1.6 + 1.6 + (1.5 + 1.6)/2)/3
Xim_futura_fin <- (1.1 + 1.0 + 1.0)/3
Xim_futura_dif <- Xim_futura_in - Xim_futura_fin
  
Xim_tronex_in <- (1.7 + 1.6 + 1.6)/3
Xim_tronex_fin <- (0.9 + 0.9 + 0.9)/3
Xim_tronex_dif <- Xim_tronex_in - Xim_tronex_fin
  
Xim_varta_in <- (1.6+(1.5 + 1.6)/2+1.6)/3
Xim_varta_fin <- (0.9 + 0.9 + 0.9 )/3
Xim_varta_dif <-  Xim_varta_in - Xim_varta_fin

Xim_duracell_in <- ((1.5 + 1.6)/2 + (1.4 + 1.5 + 1.6)/3 + 1.6)/3
Xim_duracell_fin <- (0.9 + 0.9 + 0.9)/3
Xim_duracell_dif <- Xim_duracell_in - Xim_duracell_fin

############# Registro para modelo voltaje ###########
y_vol <- c(vero_panasonic_dif,vale_panasonic_dif,Yojan_panasonic_dif,Xim_panasonic_dif,
       vero_futura_dif,vale_futura_dif,Yojan_futura_dif,Xim_futura_dif,
       vero_tronex_dif,vale_tronex_dif,Yojan_tronex_dif,Xim_tronex_dif,
       vero_varta_dif,vale_varta_dif,Yojan_varta_dif,Xim_varta_dif,
       vero_duracell_dif,vale_duracell_dif,Yojan_duracell_dif,Xim_duracell_dif
)

marca <- as.factor(c(rep("Panasonic",4),
                     rep("Futura",4),
                     rep("Tronex",4),
                     rep("Varta",4),
                     rep("Duracell",4)))

operador <- as.factor(c(rep( c("Veronica","Valentina", "Yojan", "Ximena"), 5)))

voltaje <- data.frame(y_vol,marca,operador)

marca_ord <- as.factor(c("Tronex","Duracell","Futura","Futura", "Varta", "Panasonic", "Duracell", "Varta",
                         "Futura","Futura","Tronex","Tronex","Varta","Panasonic","Panasonic","Tronex",
                         "Panasonic","Duracell","Duracell","Varta"
))

operador_ord <- as.factor(c("Valentina","Ximena","Veronica","Yojan",
                            "Valentina","Ximena","Veronica","Yojan",
                            "Ximena", "Valentina", "Veronica","Yojan",
                            "Veronica","Yojan", "Valentina","Ximena",
                            "Veronica","Yojan", "Valentina","Ximena"))

orden_vol <- c(vale_tronex_dif, Xim_duracell_dif, vero_futura_dif, Yojan_futura_dif,
               vale_varta_dif, Xim_panasonic_dif, vero_duracell_dif, Yojan_varta_dif,
               Xim_futura_dif, vale_futura_dif, vero_tronex_dif, Yojan_tronex_dif,
               vero_varta_dif, Yojan_panasonic_dif,vale_panasonic_dif, Xim_tronex_dif,
               vero_panasonic_dif,Yojan_duracell_dif,vale_duracell_dif,Xim_varta_dif
)

voltaje_ord <- data.frame(orden_vol,marca_ord,operador_ord)

#### -------------------- Descriptivos para voltaje --------------------------------------
# Por operador:
by(voltaje_ord$orden_vol, voltaje_ord$operador_ord, summary)
by(voltaje_ord$orden_vol, voltaje_ord$operador_ord, sd)

ggplot(data = voltaje_ord, aes(x = operador_ord, y = orden_vol, fill = factor(operador_ord))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red") +
  scale_y_continuous(name = "Voltaje") + 
  scale_x_discrete(name = "Operador") +
  ggtitle("Boxplot: Voltaje de acuerdo al operador") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=orden_vol, x=operador_ord),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("blue", "red","green", "pink"))+labs(fill = "Operador")

### --------------- Descriptivos por marca
by(voltaje_ord$orden_vol, voltaje_ord$marca_ord, summary)
by(voltaje_ord$orden_vol, voltaje_ord$marca_ord, sd)

ggplot(data = voltaje_ord, aes(x = marca_ord, y = orden_vol, fill = factor(marca_ord))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red") +
  scale_y_continuous(name = "Voltaje") + 
  scale_x_discrete(name = "Marca") +
  ggtitle("Boxplot: Voltaje de acuerdo a la marca") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=orden_vol, x=marca_ord),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("blue", "red","green", "pink", "magenta"))+labs(fill = "Marca")

### ------------- Por operador y marca
# No tiene mucho sentido pues solo hay una observación por tratamiento pero ayuda a mirar variabilidad:
ggplot(voltaje_ord,aes(operador_ord,orden_vol, fill = operador_ord))+geom_boxplot()+facet_wrap(~marca_ord)+
  stat_summary(aes(y=orden_vol, x=operador_ord),fun=mean, geom="point", shape=20,size=4, color="yellow", position = position_dodge(0.75))+
  labs(fill = "Operador", x = "Operador", y = "Voltaje")


ggplot(voltaje_ord,aes(marca_ord,orden_vol, fill = marca_ord))+geom_boxplot()+facet_wrap(~operador_ord)+
  stat_summary(aes(y=orden_vol, x=marca_ord),fun=mean, geom="point", shape=20,size=4, color="yellow", position = position_dodge(0.75))+
  labs(fill = "Marca", x = "Marca", y = "Voltaje")


modelo <- aov(y_vol ~ marca + operador, data = voltaje)
#summary(modelo)

modelo_ord <- aov(orden_vol~marca_ord+operador_ord, data = voltaje_ord)
summary(modelo_ord)

#Homogeneidad
bartlett.test(orden_vol~marca_ord)
bartlett.test(orden_vol~operador_ord)


#Prueba de independencia
library(car)
#durbinWatsonTest(modelo)
durbinWatsonTest(modelo_ord)

#Gráfico de residuales vs orden de recolección
plot(residuals(modelo_ord), pch =16, ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales")
abline(h=0)

qqnorm(residuals(modelo_ord))
qqline(residuals(modelo_ord))

shapiro.test(residuals(modelo_ord))

library(asbio)
tukey.add.test(orden_vol, marca_ord, operador_ord)

#Modelo de un factor
mol_marca <- aov(orden_vol~marca_ord)
summary(mol_marca)

#Tranformación 
modelo_trans <- aov(log(orden_vol)~marca_ord+operador_ord, data = voltaje_ord)
summary(modelo_trans)

#Homogeneidad
#Homogeneidad
bartlett.test(log(orden_vol)~marca_ord)
bartlett.test(log(orden_vol)~operador_ord)

#Prueba de independencia

#durbinWatsonTest(modelo)
durbinWatsonTest(modelo_trans)

#Gráfico de residuales vs orden de recolección
plot(residuals(modelo_trans), pch =16, ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales")
abline(h=0)

qqnorm(residuals(modelo_trans))
qqline(residuals(modelo_trans))

shapiro.test(residuals(modelo_trans))

ggplot(voltaje_ord, aes(orden_vol)) +
  geom_density(aes(fill=factor(operador_ord)), alpha=0.4)+
  scale_fill_manual(values = c("blue", "red","green", "yellow")) +
  labs(x = "Tiempo de reacción", y = "Densidad", fill = "Bloque")+
  theme_bw()+scale_x_continuous(limits=c(0,1))

ggplot(voltaje_ord, aes(orden_vol)) +
  geom_density(aes(fill=factor(marca_ord)), alpha=0.4)+
  scale_fill_manual(values = c("blue", "red","green", "yellow", "magenta")) +
  labs(x = "Tiempo de reacción", y = "Densidad", fill = "Bloque")+
  theme_bw()+scale_x_continuous(limits=c(0,1))



############################### Luminosidad ########################
######### Luz #########
y_luz <- c(40, 10, 54, 96,
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






modelo_luz <- aov(y_luz~marca_luz+operador_luz, data = luz)
summary(modelo_luz)

#Normalidad
qqnorm(residuals(modelo_luz))
qqline(residuals(modelo_luz))

shapiro.test(residuals(modelo_luz))

# Homogeneidad
bartlett.test(y_luz~marca_luz)
plot(fitted(modelo_luz),residuals(modelo_luz))
abline(h=0)

# Independencia
#Prueba de independencia Durbin Watson
library(car)
durbinWatsonTest(modelo_luz)

#Gráfico de residuales vs orden de recolección
plot(residuals(modelo_luz), pch =16, ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales")
abline(h=0)

library(asbio)
tukey.add.test(y_luz, marca_luz, operador_luz)

#Modelo interacción 
mod <- aov(y_luz ~ marca_luz*operador_luz, data = luz)
summary(mod)



