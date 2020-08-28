library(ca)
library(ade4)
library(anacor)
library(FactoMineR)

PRO<-read.csv("C:/Users/Punisher/Desktop/punisher/UNAM/Análisis Multivariado/Proyecto final/BASEP.csv",sep=",",header=T)
attach(PRO)
PRO

pro<-PRO[c(1:6),c(2:5)]
pro

rownames(pro)<-c("A","B","CC","CT","F","M")

total1.row<-apply(pro,1,sum)

total1.col<-apply(pro,2,sum)

pro1<-cbind(pro,total1.row) 
pro2<-rbind(pro1,c(total1.col,sum(pro)))
rownames(pro2)<-c(rownames(pro),"total1.col")
colnames(pro2)<-c(colnames(pro),"total1.row")

pro2

pro<-as.matrix(pro)

table.p<-prop.table(pro)

table.p1<-prop.table(pro,1)

table.p2<-prop.table(pro,2)

### Análisis de Perfiles Renglón vía método gráfico ###

win.graph()

carrera<-rownames(pro)
matplot(t(table.p1),type="l",ylab="Proporciones",lty=1:8,col=rainbow(8),xaxt="n")
axis(1,at=1:length(colnames(pro)),labels=colnames(pro))
title("Perfiles renglón: Carrera")
legend("topright",paste(" ",carrera),lty=1:8,col=rainbow(8))

##En la gráfica de correspondencias de perfiles renglón podemos observar similitud entre
# los perfiles de Biología y Ciencias de la Tierra.
##Nótese que mediante esta gráfica podemos ver que la incidencia de Actuarios que provienen
# de Preparatoria UNAM es mucho mayor que para las otras carreras, mientras que la cantidad
# de alumnos que provienen de CCH es muy parecida para las distintas carreras.
# En cuanto a las escuelas prticulares, los matemáticos son los mas y los actuarios son los
# menos.

##Biología y Ciencias de la Tierra

win.graph()
perfiles.B.CT<-(cbind(table.p1[2,],table.p1[4,]))
colnames(perfiles.B.CT)<-c("Biología","C. de la Tierra")
matplot(perfiles.B.CT,type="l",ylab="proporciones",lty=1:2,col=c("yellow","green"),xaxt="n")
axis(1,at=1:length(colnames(pro)),labels=colnames(pro))
legend("topright",c("Biología","C. de la Tierra"),lty=1:2,col=c("yellow","green"))

##Los perfiles son muy similares, aunque difieran considerablemente en la cantidad de alumnos
# provenientes de CCH. Esta similitud puede deberse a que ambas carreras comparten un origen 
# parecido, es decir, las dos carreras provienen de las ciencias naturales, a diferencia de las 
# otras carreras de la facultad que tienen sus bases principalmente en las matemáticas.

win.graph()
perfiles<-(cbind(table.p1[2,],table.p1[4,]))
colnames(perfiles.B.CT)<-c("Biología","C. de la Tierra")
barplot(t(perfiles.B.CT), beside = TRUE,col=c("yellow","green"),legend=colnames(perfiles.B.CT))

##El barplot muestra mucha similitud entre las dos carreras en términos de la escuela 
# de procedencia.

### Análisis de Perfiles Columna vía método gráfico ###

win.graph()

Procedencia<-colnames(pro)
matplot(table.p2,type="l",ylab="Proporciones",lty=1:4,col=rainbow(8),xaxt="n")
axis(1,at=1:length(rownames(pro)),labels=rownames(pro))
title("Perfiles columna: Escuela de procedencia")
legend("topright",paste(" ",Procedencia),lty=1:8,col=rainbow(8))

##Respecto a los perfiles columna, los mas parecidos son los perfiles que representan Preparatoria
# y CCH.

##Preparatoria y CCH

win.graph()
perfiles<-(cbind(table.p2[,1],table.p2[,2]))
colnames(perfiles)<-c("Preparatoria","CCH")
matplot(perfiles,type="l",ylab="proporciones",lty=1:2,col=c("red","orange"),xaxt="n")
axis(1,at=1:length(rownames(pro)),labels=rownames(pro))
legend("topright",c("Preparatoria","CCH"),lty=1:2,col=c("red","orange"))

##Se puede observar que los perfiles difieren poco, excepto en Actuaría donde parece existir 
# una gran discrepancia entre alumnos provenientes de Preparatoria y de CCH, en efecto, hay una
# mayor proporcion de sujetos provenientes de Preparatoria.

win.graph()
perfiles<-(cbind(table.p2[,1],table.p2[,2]))
colnames(perfiles)<-c("Preparatoria","CCH")
barplot(t(perfiles), beside = TRUE,col=c("red","orange"),legend=colnames(perfiles))

##Sigue siendo muy visual la gran diferencia que existe entre los alumnos de Actuaría 
# que provienen de Preparatoria y de CCH.

win.graph()

table.cont(pro,csize=1.5,row.labels=rownames(pro),col.labels=colnames(pro))
mosaicplot(pro,color=TRUE, shade = T,main = "Mosaico")

##En el gráfico de Mosaico podemos observar que la asociación entre la carrera de actuaría
# y Preparatoria es muy fuerte, de hecho es una asociación positiva (color azúl y tamaño del
# rectángulo). Esto podría significar que los estudiantes de la carrera de actuaría se distinguen 
# por provenir de Preparatoria UNAM.
# Por otro lado, las carreras de Física y Matemáticas presentan una considerable asociación
# con Otros, es decir, los sujetos provenientes de "otras" escuelas se distinguen por estudiar
# estas carreras. 
# Las otras carreras (Biología, Ciencias de la Computación y Ciencias de la Tierra) no muestran
# asociación alguna con las escuelas de las que provienen.
 
##Veamos que ocurre con los residuos

###################################################################################################
tabla.completa<- function(X) { 
        result <- chisq.test(X) 
        print(result)
        res<-result$residuals 
        obs <- result$observed 
        exp <- result$expected
        res.crudos<-obs-exp
        chi.table<-((obs-exp)^2)/exp 
        row.sum <- apply(obs,1,sum) 
        col.sum <- apply(obs,2,sum) 
        N <- sum(obs) 
 
        tablacompleta<- cbind(obs,row.sum) 
        tablacompleta<- rbind(tablacompleta,c(col.sum,N)) 
        rownames(tablacompleta) <- c(rownames(obs), "Total") 
        colnames(tablacompleta) <- c(colnames(obs), "Total")
        I<-length(X[,1])
        J<-length(X[1,])
        resnorm<-matrix(0,I,J)
        
         for(i in 1:I){
              for(j in 1:J){
               resnorm[i,j]<-(obs[i,j]-exp[i,j])/sqrt(exp[i,j]*(1-(row.sum[i]/N))*(1-(col.sum[j]/N)))

}
rownames(resnorm)<-rownames(obs)
colnames(resnorm)<-colnames(obs)
}

return(list(Tabla.completa=tablacompleta,Expected=exp,Chi=chi.table,r.crudos=res.crudos,residuos=res,ajustados=resnorm))
        } 
#####################################################################################################################################
tabla.completa(pro)

##De acuerdo a la prueba Ji-cuadrada tenemos lo siguiente: 
# Un estadístico de 39.7522, con 15 grados de libertad y p-value=0.0004942
# Recordemos que para esta prueba tenemos el siguiente contraste:
#       Ho:Independencia entre renglones y columnas 
#                             vs
# Ha:Al menos existe asociación entre algún renglón y alguna columna.
#
# Ho también se puede interpretar como la inexistencia de asociación entre renglones 
# y columnas. Como nuestro p-value es muy cercano a cero, entonces nos encontramos en
# área de rechazo, es decir, rechazamos la hipótesis de no asociación, entonces 
# existe asociación entre al menos un renglón y una columna. 
# Notemos lo siguiente:
# Al comparar los valores de la tabla con los valores esperados, podemos observar 
# que difieren en gran medida los siguientes variables:
# -Los valores de Actuaría
# -Los valores de Matemáticas
# -Los valores de Física

# Esta diferencia evidencía el grado de no independencia entre las variables.

# En la matriz de distancias Ji-Cuadrada, encontramos valores altos para la variable "Actuaría"
# con respecto a las distintas variables de escuela de procedencia, excepto con la variable "CCH".
# También la variable "Física" presenta un valor muy alto con respecto a la variable "Otros".
# Finalmente la variable "Matemáticas presenta grandes valores con respecto a las variables 
# "Preparatoria", "Particular" y "Otros".
# Recordemos que la distancia Ji-cuadrada representa la "lejania" entre los perfiles renglón o columna de su perfil medio,
# entonces, mientras mas grandes sean los valores de las variables renglón con respecto a las variables de columna, 
# la asociación es mayor, ya que la Ji-cuadrada es una medida del nivel de asociación global de las variables en estudio.
##Analicemos los residuos.
##Recordemos que entre mas grande sea el residuo mas fuerte es la asociación entre las variables.
# La variable Actuaría cuenta con residuos crudos altos con respecto a las variables Preparatoria, Particular 
# y Otros, con residuos negativos para las ultimas dos variables. Lo anterior se podría interpretar como que
# los estudiantes de la carrera de actuaría se distinguen por provenir de Preparatoria UNAM (como ya habíamos
# mencionado anteriormente) y no provienen de escuelas Particulares ni de Otras. 
# Por otro lado, Ciencias de la Computación cuenta con residuo crudo significativo y positivo respecto a la variable
# CCH, es decir, los estudiantes de ésta carrera se distinguen por provenir de CCH.
# En cuanto a las variables de Física y Matemáticas cuentan con residuos significativamente negativos 
# respecto a la variable Preparatoria y residuos significativamente positivas con respecto a la variable
# Otros. De aqui que los estudiantes de éstas carreras se identifican por provenir de otras escuelas y no de
# provenir de Preparatorias UNAM. 
# Ciencias de la Tierra y Biología no cuentan con residuos significantes para ninguna variable.
##Ahora bien, respecto a los residuos ajustados, las variables que cuentan con valores elevados son Actuaría con
# Preparatoria (nuevamente) ,y Matemáticas y Física con la variable Otros. Ésto confirma lo anteriormente dicho,
# la asociación entre estas variables es fuerte pues sus residuos, tanto crudos como ajustados son significativamente
# grandes.
##Es importante aclarar que al calcular estas tablas aparece un mensaje el cual advierte la posible existencia 
# de error al momento de aproximar el valor de la Ji-Cuadrada, esto se puede deber a la cantidad de información 
# utilizada.

ca(pro)

##Con la función ca() obtenemos la masa, la distancia Ji-cuadrada, la inercia y las coordenadas 
# de los puntos que representan a las varaibles en dos dimensiones.
# De las variables de Carrera podemos notar que la mayor cantidad de masa la obtuvo Biología.
# Las distancias Ji-cuadrada mas elevadas las proporcionan las variables Actuaría, Física y Matemáticas.
# Esta distancia representa la "lejania" entre los perfiles renglón o columna de su perfil medio,
# mas aún, si los perfiles difieren poco de sus perfiles medios, entonces el valor de su inercia sería
# bajo y eso implicaría una pobre asociación entre las variables. 
##Ahora bien, las variables de escuela de procedencia obtuvieron los siguientes resultados:
# La variable en obtener más masa fue Preparatoria, mientras que la distancia Ji-cuadrada más elevada
# fue la de la variable Otros con 0.9690. Estó indica que hay una fuerte asociación entre esta variable 
# y alguna variable de carrera.

summary(ca(pro))

##Al observar la proporción de inercia acumulada nos dimos cuenta de que la representación en dos dimensiones es
# bastante buena, ya que explica el 96.2% de la inercia (que es como la varianza acumulada). Recordemos
# que el criterio empírico contempla como inercia acumulada suficiente el 75% de la inercia explicada. 

##Renglones 
# Las variables Actuaría, Biología, Física y Matemáticas cuentan con una excelente calidad de representación en 
# dos dimensiones.
# Respecto a la primera dimensión, las variables con mayor calidad y contribución fueron Actuaría y Matemáticas, 
# mientras que en la segunda dimensión la variable con mejor calidad y mayor contribución es Biología. Las otras
# variables cuentan con mala calidad de representación. 

##Columnas
# Las variables Preparatoria, Particular y Otros cuentan con calidad de representación muy buena. La razón por la que 
# la variable CCH no tiene buena calidad de representación es porque para todas las carreras mostraba
# tener similar número de sujetos provenientes de esa escuela.
# Las variables Preparatoria y Otros se encuentran muy bien representadas en la primer dimensión, mas aún, la contribución
# mas alta en esta dimensión proviene de la variable Otros.
# La segunda dimensión no cuenta con variables de escolaridad bien representadas ni que contribuyan en gran medida.

win.graph()

##Gráfica perfiles renglón y perfiles columna juntos.
acs.1<-CA(pro)

par(mfrow=c(1,2))
##Gráfica perfiles renglón.
lab<-rownames(pro)
plot(acs.1$row$coord[,1:2],pch=17,col="red",cex=2)
text(acs.1$row$coord[,1:2],labels=lab,cex=0.8)
abline(h=0,lty=3)
abline(v=0,lty=3)

##Gráfica perfiles columna.
lab1<-colnames(pro)
plot(acs.1$col$coord[,1:2],pch=18,col="blue",cex=2)
text(acs.1$col$coord[,1:2],labels=lab1,cex=0.8)
abline(h=0,lty=3)
abline(v=0,lty=3)

##En éstas gráficas podemos observar las asociaciones perfiles columna y perfiles renglón en dos 
# dimensiones, por ejemplo Biología y Ciencias de la Tierra se encuentran muy cercanos, ésto ya lo habíamos notado
# con las gráficas iniciales, y habíamos dicho que se puede deber a que son carreras con bases en ciencias naturales.
# Con esta gráfica podemos visualizar lo que deciamos anteriormente sobre la contribución de las variables
# a cada dimensión. En el análisis del summary(ca(pro)) vimos que Actuaría, Biología, Física y Matemáticas cuentan con 
# una excelente calidad de representación en dos dimensiones, mas aún, Matemáticas cuenta con una calidad muy fuerte 
# en la primera dimensión (de hecho la calidad mas elevada, 993), esto se ve reflejado en la gráfica de biplot,
# ya que es la variable que se encuentra mas cercana al eje x que representa a la primera dimensión.
# Un dato que es importante denotar es que, variables como Ciencias de la Tierra que no se encontraban bien representadas en dos 
# dimensiones, es posible que al momento de ver el biplot en 2D el punto que represente a ésta variable
# se encuentre a poca distancia de otro, pero ésto no significa que sean similares, ya que por su baja
# calidad de representación puede pasar que en 3 dimensiones ese punto se proyecte a un sitio completamente lejano
# de la masa de puntos. 

acs.1$eig[,1]
barplot(acs.1$eig[,1],col=rainbow(6),legend=c("e1","e2","e3","e4"))
##La gráfica anterior muestra la inercia explicada.

acs.1$eig[,2:3]
##Para la tercera dimensión, ya se explica el 100% de la varianza como ya habíamos visto anteriormente.

acs.1$row$contrib[,1:2]

##La tabla enterior muestra la contribución de cada variabe de renglón en cada dimensión. 

acs.1$col$contrib[,1:2]

##La tabla enterior muestra la contribución de cada variabe de columna cada dimensión. 

acs.1$row$cos2[,1:2]
acs.1$col$cos2[,1:2]

## Función anacor()

acs.2<-anacor(pro,scaling=c("standard","centroid"))
summary(acs.2)

### Gráfica de correspondencias por renglones

plot(acs.2,plot.type="rowplot",col="blue")

plot(acs.2,plot.type="colplot",col="red")

### Grafica conjunta renglones y columnas

plot(acs.2,plot.type="jointplot",xlim = c(-2,1.5), ylim = c(-2, 1.5), asp = 1)

plot(acs.2,plot.type="graphplot",xlim = c(-3.1,1.5), ylim = c(-3.1, 1.5), wlines = 5, asp=1) 

### Graficas Biplot

plot(ca(pro),main="Asociación gráfica Licenciatura vs Escuela de procedencia")
legend("topright",c("Escuela","Carrera"),pch=c(17,19),col=c("red","blue"))

plot(ca(pro), mass = TRUE,arrows = c(FALSE, TRUE), main="Asociación gráfica Licenciatura vs Escuela de procedencia")
legend("topright",c("Escuela","Carrera"),pch=c(17,19),col=c("red","blue"))


plot(ca(pro), mass = TRUE,arrows = c(TRUE, FALSE), main="Asociación gráfica Licenciatura vs Escuela de procedencia")
legend("topright",c("Escuela","Carrera"),pch=c(17,19),col=c("red","blue"))


plot(ca(pro), mass = TRUE,arrows = c(TRUE, TRUE), main="Asociación gráfica Licenciatura vs Escuela de procedencia")
legend("topright",c("Escuela","Carrera"),pch=c(17,19),col=c("red","blue"))

##En la gráfica biplot anterior se pude notar la cercanía entre las carreras Ciencias de la Tierra y Biología, y 
# simultáneamente la cercanía de estas carreras con la escuela de procedencia CCH, sin embargo, recordemos que la 
# variable CCH no ayudaba a distinguir las variables de carrera, entonces ¿Por qué gráficamente parece que estas 
# variables se encuentran ampliamente relacionadas? Esta pregunta es fácil de responder ya que la calidad de 
# representación de la variable CCH en la primera y segunda dimensión es muy mala, al igual que sus respectivas 
# contribuciones. Esto nos lleva a pensar que esta variable es colapsada en dos dimensiones de tal suerte que parece
# estar asociada con estas variables, entonces si vemos la representación en tres dimensiones es posible que esta variable 
# se separe bastante de la masa de puntos.

res.1<-anacor(pro,ndim=2,scaling=c("Benzecri","Benzecri"))
res.2<-anacor(pro,ndim=3,scaling=c("Benzecri", "Benzecri"))

### Graficas para la evaluacion de las calidades de representacion de los puntos

plot(res.1,plot.type="benzplot",main="Distancia Benzecri(2D)")

plot(res.2,plot.type="benzplot",main="Distancia Benzecri(3D)")

##Las anteriores gráficas de Benzecri muestran que son pocas las similitudes entre 
# perfiles tanto de renglón como de columna que se encuentran mal representadas en dos dimensiones,
# por ejemplo, la asociación entre Ciencias de la Computación y Ciencias de la Tierra cuentan con 
# una regular representación en la segunda dimensión, ya que el punto que representa esta asociación
# se encuentra alejado de la recta de Benzecri. Otro punto que se encuentra fuera de la recta de 45° 
# es el que representa la asociación entre las variables Matemáticas y Ciencias de la Computación, 
# sin embargo, tanto esta diferencia como la de la asociación CC-A son poco considerables ya que no 
# se encuentran extremadamente alejados de la recta. Respecto a los perfiles columna, todos los puntos 
# se encuentran muy cercanos (o sobre) la recta de Benzecri, lo cual habla de una buena representación 
# de estas asociaciones en la segunda dimensión. 
##En la tercera dimensión todas las asociaciones son perfectas, lo cual tiene mucho sentido ya que 
# habíamos visto que en la tercera dimensión ya se acumulaba el 100% de la inercia explicada.

### Otro biplot

win.graph()

plot(ca(pro), mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE))

### Gráfica 3D.

win.graph()

plot3d.ca(ca(pro,nd=3))
