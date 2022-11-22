#Librerías usadas
library(ggplot2)
library(car)
require(agricolae)
library(asbio)
library(pwr)

#Aleatorización de las pilas dentro de cada bloque:
#Operador 1
Operador_1<- c("Duracell", "Varta", "Tronex", "Panasonic", "Futura")
set.seed(123)
sample(Operador_1)
#Operador 2
Operador_2 <- pilas <- c("Duracell", "Varta", "Tronex", "Panasonic", "Futura")
set.seed(456)
sample(Operador_2 )
#Operador 3
Operador_3  <- c("Duracell","Varta","Tronex","Panasonic","Futura")
set.seed(651)
sample(Operador_3)
#Operador 4
Operador_4 <- c("Duracell", "Varta", "Tronex","Panasonic","Futura")
set.seed(889)
sample(Operador_4)

#Registro de datos

## Operador 1
# Pilas panasonic
op1_panasonic_in <- (1.6 + 1.6 + 1.6)/3
op1_panasonic_fin <- (0.9 + (0.8 + 0.9)/2 + (1.0 + 0.9)/2 )/3
op1_panasonic_dif <- op1_panasonic_in - op1_panasonic_fin #Medida de interés
# Pilas futura
op1_futura_in <- (1.6  + 1.5 + 1.6)/3
op1_futura_fin <- (0.8 + (0.8+0.9)/2 + 1.0)/3
op1_futura_dif <- op1_futura_in - op1_futura_fin #Medida de interés
# Pilas tronex  
op1_tronex_in <- ((1.6 + 1.7)/2+ (1.6 + 1.7)/2+ 1.6)/3
op1_tronex_fin <- (1.0 + 1.0 + 0.7)/3
op1_tronex_dif <- op1_tronex_in - op1_tronex_fin #Medida de interés
# Pilas varta
op1_varta_in <- (1.6+(1.5 + 1.6)/2+ 1.6)/3
op1_varta_fin <- ((0.8+0.9)/2 +0.9+0.9)/3
op1_varta_dif <- op1_varta_in - op1_varta_fin #Medida de interés
# Pilas duracell
op1_duracell_in <- (1.5 + 1.5 + 1.6)/3
op1_duracell_fin <- (1.0 + 1.0 +1.0)/3
op1_duracell_dif <- op1_duracell_in - op1_duracell_fin #Medida de interés

## Operador 2  
# Pilas panasonic
op2_panasonic_in <- (1.6 + (1.5 + 1.6)/2 + 1.6)/3
op2_panasonic_fin <- (0.9 + 0.9 + 0.9 )/3
op2_panasonic_dif <- op2_panasonic_in - op2_panasonic_fin #Medida de interés
# Pilas futura  
op2_futura_in <- (1.6 + (1.5 + 1.6)/2 + (1.5 + 1.6)/2 )/3
op2_futura_fin <- (0.9 + 0.9 + 0.9 )/3
op2_futura_dif <- op2_futura_in - op2_futura_fin #Medida de interés
# Pilas tronex  
op2_tronex_in <- (1.6 + (1.5+1.6)/2+ (1.6+1.7)/2)/3
op2_tronex_fin <- (0.9 + 0.9 + (0.8 + 0.9)/2)/3
op2_tronex_dif <- op2_tronex_in - op2_tronex_fin #Medida de interés
# Pilas varta
op2_varta_in <- ((1.5+1.6)/2 + (1.4+1.5+1.6)/3 + (1.4+1.5+1.6)/3)/3
op2_varta_fin <- ((0.9 + 1.0)/2 + (0.9 + 1.0)/2 + (0.9 + 1.0)/2)/3
op2_varta_dif <- op2_varta_in - op2_varta_fin #Medida de interés
# Pilas duracell 
op2_duracell_in <- ((1.5 + 1.6)/2 + 1.6 + 1.5)/3
op2_duracell_fin <- (0.9 + (0.8 + 0.9)/2 + (0.8 + 0.9)/2)/3
op2_duracell_dif <- op2_duracell_in - op2_duracell_fin #Medida de interés

## Operador 3
# Pilas panasonic 
op3_panasonic_in <- (1.6 + 1.6 + (1.5 + 1.6)/2)/3
op3_panasonic_fin <- (1 + 0.9 + 1)/3
op3_panasonic_dif <- op3_panasonic_in - op3_panasonic_fin #Medida de interés
# Pilas futura
op3_futura_in <- (1.6 + 1.6 + (1.5 + 1.6)/2)/3
op3_futura_fin <- (1.1 + 1.0 + 1.0)/3
op3_futura_dif <- op3_futura_in - op3_futura_fin #Medida de interés
# Pilas tronex  
op3_tronex_in <- (1.7 + 1.6 + 1.6)/3
op3_tronex_fin <- (0.9 + 0.9 + 0.9)/3
op3_tronex_dif <- op3_tronex_in - op3_tronex_fin #Medida de interés
# Pilas varta  
op3_varta_in <- (1.6+(1.5 + 1.6)/2+1.6)/3
op3_varta_fin <- (0.9 + 0.9 + 0.9 )/3
op3_varta_dif <-  op3_varta_in - op3_varta_fin #Medida de interés
# Pilas duracell
op3_duracell_in <- ((1.5 + 1.6)/2 + (1.4 + 1.5 + 1.6)/3 + 1.6)/3
op3_duracell_fin <- (0.9 + 0.9 + 0.9)/3
op3_duracell_dif <- op3_duracell_in - op3_duracell_fin #Medida de interés

## Operador 4
op4_panasonic_in <- (1.6 + 1.6 + 1.6)/3
op4_panasonic_fin <- (0.9 + (0.8 + 0.9)/2 + 0.9)/3
op4_panasonic_dif <- op4_panasonic_in - op4_panasonic_fin #Medida de interés
# Pilas futura 
op4_futura_in <- ((1.5 + 1.6)/2+(1.5 + 1.6)/2+ 1.6)/3
op4_futura_fin <- (0.9+0.9+(1.0 + 0.9)/2)/3
op4_futura_dif <- op4_futura_in - op4_futura_fin #Medida de interés
# Pilas tronex    
op4_tronex_in <- ((1.5+1.6)/2 + 1.6 + 1.6)/3
op4_tronex_fin <- (0.9 + (1.0+1.1)/2 + 0.9)/3
op4_tronex_dif <- op4_tronex_in - op4_tronex_fin #Medida de interés
# Pilas varta
op4_varta_in <- (1.6+1.6+1.6)/3
op4_varta_fin <- (0.9 + 0.9 + 0.9)/3
op4_varta_dif <- op4_varta_in - op4_varta_fin #Medida de interés
# Pilas duracell
op4_duracell_in <- (1.6 + (1.5 +1.6)/2 + (1.5 +1.6)/2)/3
op4_duracell_fin <- (0.9 + 0.9 + 0.9 )/3
op4_duracell_dif <- op4_duracell_in - op4_duracell_fin #Medida de interés

## Base de datos con la información recolectada
# El orden en el que se disponen los datos corresponden al de medición previamente aleatorizado
marca <- as.factor(c("Tronex","Duracell","Futura","Futura", "Varta", "Panasonic", "Duracell", "Varta",
                      "Futura","Futura","Tronex","Tronex","Varta","Panasonic","Panasonic","Tronex",
                      "Panasonic","Duracell","Duracell","Varta"
))


operador <- as.factor(c("1","3","2","4",
                        "1","3","2","4",
                        "3", "1", "2","4",
                        "2","4", "1","3",
                        "2","4", "1","3"))

vol <- c(op1_tronex_dif, op3_duracell_dif, op2_futura_dif, op4_futura_dif,
               op1_varta_dif, op3_panasonic_dif, op2_duracell_dif, op4_varta_dif,
               op3_futura_dif, op1_futura_dif, op2_tronex_dif, op4_tronex_dif,
               op2_varta_dif, op4_panasonic_dif,op1_panasonic_dif, op3_tronex_dif,
               op2_panasonic_dif,op4_duracell_dif,op1_duracell_dif,op3_varta_dif
)

voltaje <- data.frame(vol,marca,operador)

## Análisis descriptivo para el voltaje:
# Medidas descriptivas de acuerdo al operador:
by(voltaje$vol, voltaje$operador, summary)
by(voltaje$vol, voltaje$operador, sd)

# Boxplot del voltaje de acuerdo al operador:
ggplot(data = voltaje, aes(x = operador, y = vol, fill = factor(operador))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Voltaje") + 
  scale_x_discrete(name = "Operador") +
  ggtitle("Boxplot: Voltaje de acuerdo al operador") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=vol, x=operador),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("maroon1", "chocolate","darkolivegreen3", "deepskyblue3"))+labs(fill = "Operador")+
  theme_minimal()

# Medidas descriptivas de acuerdo a la marca:
by(voltaje$vol, voltaje$marca, summary)
by(voltaje$vol, voltaje$marca, sd)

# Boxplot de voltaje de acuerdo a la marca:
ggplot(data = voltaje, aes(x = marca, y = vol, fill = factor(marca))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Voltaje") + 
  scale_x_discrete(name = "Marca") +
  ggtitle("Boxplot: Voltaje de acuerdo a la marca") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=vol, x=marca),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ))+labs(fill = "Marca")+
  theme_minimal()

## ANOVA para el voltaje con un diseño de un factor con un bloque:
mod_vol <- aov(vol~marca+operador, data = voltaje)
summary(mod_vol)

## Validación de supuestos:

## Diagnóstico de normalidad:
# Q-Q plot
qqnorm(residuals(mod_vol),pch=19, cex=1)
qqline(residuals(mod_vol))
# Test de Shapiro
shapiro.test(residuals(mod_vol))

## Diagnóstico de homogeneidad de varianza
# Test de Bartlet
bartlett.test(vol~marca) #por marca
bartlett.test(vol~operador) #por operador
# Gráfico de valores ajustados vs residuales
plot(fitted(mod_vol),residuals(mod_vol),pch=19, cex=1, col=c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ), xlab = "Valores ajustados", ylab = "Residuales", main = "Residuales vs. valores ajustados")
abline(h=0)

## Diagnóstico de independencia
# Prueba de Durbin Watson
durbinWatsonTest(mod_vol)

#Gráfico de residuales vs orden de recolección
plot(residuals(mod_vol), pch =16,col = c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ), ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales")
abline(h=0)

## Prueba de aditividad de Tukey
tukey.add.test(vol, marca, operador)

## Altenativas para mejorar significancia del factor y el bloque 

# Tranformación logaritmo
modelo_trans <- aov(log(vol)~marca+operador, data = voltaje)
summary(modelo_trans)

## Test no paramétrico para significancia del factor 
friedman.test(vol, marca, operador) # incluye el efecto del bloque 
kruskal.test(vol~marca,data=voltaje) 

#### Eliminando el valor mínimo de voltaje de cada marca de pila

## Datos
vol_2 <- c(op1_tronex_dif, op3_duracell_dif, op2_futura_dif, op4_futura_dif,
             op1_varta_dif,op2_duracell_dif, op4_varta_dif,
             op1_futura_dif, op2_tronex_dif, 
             op4_panasonic_dif,op1_panasonic_dif, op3_tronex_dif,
             op2_panasonic_dif,op4_duracell_dif,op3_varta_dif)

marca_2 <- as.factor(c("Tronex","Duracell","Futura","Futura", 
                         "Varta", "Duracell", "Varta",
                         "Futura","Tronex","Panasonic","Panasonic","Tronex",
                         "Panasonic","Duracell","Varta"
))


voltaje_2 <- data.frame(vol_2,marca_2)

## Análisis descriptivo para voltaje:
# Medidas descriptivas de acuerdo a la marca
by(voltaje_2$vol_2, voltaje_2$marca_2, summary)
by(voltaje_2$vol_2, voltaje_2$marca_2, sd)

# Boxplot de voltaje de acuerdo al operador:
ggplot(data = voltaje_2, aes(x = marca_2, y = vol_2, fill = factor(marca_2))) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot(alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Voltaje") + 
  scale_x_discrete(name = "Marca") +
  ggtitle("Boxplot: Voltaje de acuerdo a la marca") +     
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))+
  stat_summary(aes(y=vol_2, x=marca_2),fun=mean, geom="point", shape=20,
               size=4, color="yellow", position = position_dodge(0.75))+
  scale_fill_manual(values = c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ))+labs(fill = "Marca")+
  theme_minimal()

### ANOVA para voltaje de un diseño con un factor 
mod_marca <- aov(vol_2~marca_2, data = voltaje_2)
summary(mod_marca)

## Validación de supuestos
## Diagnóstico de normalidad 
# Q-Q plot
qqnorm(residuals(mod_marca),pch=19, cex=1)
qqline(residuals(mod_marca))
# Test de Shapiro
shapiro.test(residuals(mod_marca))

## Diagnóstico de homogeneidad 
# Prueba de Bartlet
bartlett.test(vol_2~marca_2)
# Gráfico de valores ajustados vs residuales
plot(fitted(mod_marca),residuals(mod_marca),pch=19, cex=1, col=c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ), xlab = "Valores ajustados", ylab = "Residuales", main = "Residuales vs. valores ajustados")
abline(h=0)

## Diagnóstico de independencia
# Prueba de Durbin Watson
durbinWatsonTest(mod_marca)
#Gráfico de residuales vs orden de recolección
plot(residuals(mod_marca), pch =16, ylab="Residuales", xlab="Orden",
     main="Gráfico de Orden vs Residuales", col = c("maroon1", "chocolate","darkolivegreen3", "orange","deepskyblue3" ))
abline(h=0)

## Comparaciones múltiples
# Método de Tukey
tukey <- TukeyHSD(mod_marca, "marca_2")
tukey

# Método de mínima diferencia:
mds <- LSD.test(mod_marca, "marca_2")
mds

# Tamaño de muestra
f1 = sqrt(0.031^2/(2*5*0.0002037))
pwr.anova.test(f = f1, k=5, power=0.9, sig.level=0.05)

# Potencia del diseño
pwr.anova.test(f = f1, k=5, power=0.53, sig.level=0.05)

