library(aplpack)
library(TeachingDemos)
library(bpca)
library(car)


Base<-read.csv("C:/Users/Ricardo/Multivariado/Tarea Componentes Principales/Ejercicio 10/ConsumoAgua.csv", sep=",", header=T)
Base
names(Base)

Base1<-Base[,-1] ##Quitamos la variable cualitativa "nombres de los estados"
names(Base1)
attach(Base1)

Base1

faces2(Base1,labels=Base[,1]) ##grafica de caras 
faces(Base1,labels=Base[,1])  ## grafica de caras 2
stars(Base1,col.stars=rainbow(32),main='Tipo de Consumo de Agua') ##grafica de estrellas



p.comp<-princomp(Base1,cor=TRUE) ##Realizamos los componentes principales
p.comp


p.comp$loadings ##Loadings de nuestros componentes principales (analisis en el readme!!!)


summary(p.comp) ## Resumen de los componentes principales (analisis en el readme!!)
 


##################################################################################################################### 
###################################          EXTRA!!              ###################################################
###################################     (ANALISIS GRAFICO)        ###################################################
###################################             |                 ###################################################
###################################             V                 ###################################################
#####################################################################################################################



p.comp$sdev
p.comp$center 
p.comp$scale
p.comp$n.obs
p.comp$scores



plot(p.comp,col="lightpink",border="black")

plot(p.comp$sdev,type="o",pch=17,main="Gráfica de codo",ylab="eigenvalores",xlab="Indice",col="red")


plot(p.comp$scores[,1],p.comp$scores[,2],main="Componentes Principales\nConsumo de Agua por estado",pch=15)


biplot(p.comp)

###Podemos observar que los valores 2, 15, 9 y 25 pueden ser outliers.
###vemos que comercial y domestica explican mayor varianza, ya que es de mayor medida sus vectores
###al igual que servicios publicos, dist por pipas, se queda atras en este ambito


plot(bpca(Base1),var.factor=1,var.cex=1,var.col=c("blue","red","green","violet","brown"),main="Relación variables vs Individuos")


##Seguimos viendo lo observado, el sujeto 2, esta siendo influyente, esta muy lejano
##de la masa, y parece tener gran peso en la variable servicios publicos
## por otro lado, el sujeto 9, se observa que tiene un peso considerable en la variable domestica.

##Verificacion de lo anterior

##para el sujeto 2

Base1$Servicios.públicos[2]
min(Servicios.públicos)
max(Servicios.públicos)

## Verificamos que es el maximo en Servicios publicos este sujeto!

## para el sujeto 9

Base1$Doméstica[9]
min(Doméstica)
max(Doméstica)

## el valor se aproxima al maximo en demasia.


##para el sujeto 15, el cual parece estar entre Distribucion por pipas y Domestico


Base1[15,]
max(Doméstica)
min(Doméstica)
max(Distribución.por.pipas)
min(Distribución.por.pipas)

##Se observa que toma los valores maximos en las dos variables, asi que es un sujeto
##influyente en ambas variables
