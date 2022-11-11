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

vero_duracell_in <- (1.1 + 1.1 + 1.1)/3
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

Xim_duracell_in <- ((1.5 + 1.6)/2 + (1.4 + 1.5 + 1.6)/2 + 1.6)/3
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

modelo <- aov(y_vol ~ marca + operador, data = voltaje)
summary(modelo)

orden_vol <- c(vale_tronex_dif, Xim_duracell_dif, vero_futura_dif, Yojan_futura_dif,
              vale_varta_dif, Xim_panasonic_dif, vero_duracell_dif, Yojan_varta_dif,
              Xim_futura_dif, vale_futura_dif, vero_tronex_dif, Yojan_tronex_dif,
              vero_varta_dif, Yojan_panasonic_dif,vale_panasonic_dif, Xim_tronex_dif,
              vero_panasonic_dif,Yojan_duracell_dif,vale_duracell_dif,Xim_varta_dif
)

marca_ord <- as.factor(c("Tronex","Duracell","Futura","Futura", "Varta", "Panasonic", "Duracell", "Varta",
                         "Futura","Futura","Tronex","Tronex","Varta","Panasonic","Panasonic","Tronex",
                         "Panasonic","Duracell","Duracell","Varta"
))

operador_ord <- as.factor(c("Valentina","Ximena","Veronica","Yojan",
                            "Valentina","Ximena","Veronica","Yojan",
                            "Ximena", "Valentina", "Veronica","Yojan",
                            "Veronica","Yojan", "Valentina","Ximena",
                            "Veronica","Yojan", "Valentina","Ximena"))
voltaje_ord <- data.frame(orden_vol,marca_ord,operador_ord)

modelo_ord <- aov(orden_vol~marca_ord+operador_ord, data = voltaje_ord)
summary(modelo_ord)

#Homogeneidad
bartlett.test(y_vol~marca)

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

#Modelo de un factor
mol_marca <- aov(orden_vol~marca_ord)
summary(mol_marca)

library(asbio)
tukey.add.test(orden_vol, marca_ord, operador_ord)

#Modelo de interacción
mol_int <- aov(orden_vol~marca_ord*operador_ord, data = voltaje_ord)
summary(mol_int)
# mol_int <- aov(y_vol~marca*operador, data = voltaje)
# summary(mol_int)


############################### Luminosidad ########################
######### Luz #########
y_luz <- c(28, 10, 54, 96,
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
