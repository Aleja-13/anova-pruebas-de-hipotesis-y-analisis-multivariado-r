# Se quiere establecer si existe relacion entre el genero y la preferencia hacia una marca especifica de gaseosa.

#Datos
Preferencia <- c("Baja","Media","Media","Alta","Media","Media","Media","Media","Alta","Media","Media","Media","Media","Alta","Alta","Baja","Media","Baja","Media","Baja","Media","Media","Media","Media","Media","Alta","Media","Baja","Media","Alta")

Genero <- c("F","M","M","F","F","M","M","M","F","F","M","M","F","F","F","M","M","F","F","F","F","M","F","M","F","M","M","M","F","M")

datos <- data.frame(Preferencia, Genero)
datos
X<-data.matrix(datos)

G<-matrix(data=0,nrow=1,ncol=ncol(X))
C<-matrix(data=0,nrow=nrow(X),ncol=ncol(X))
Z<-matrix(data=0,nrow=nrow(X),ncol=ncol(X))
M<-matrix(data=0,nrow=ncol(X),ncol=ncol(X))
N<-matrix(data=0,nrow=nrow(X),ncol=nrow(X))
restaX<-matrix(data=0,nrow=1,ncol=ncol(X))
restaC<-matrix(data=0,nrow=1,ncol=ncol(X))
restaZ<-matrix(data=0,nrow=1,ncol=ncol(X))
T<-matrix(data=0,nrow=ncol(X),ncol=1)
FI<-matrix(data=0,nrow=nrow(X),ncol=ncol(X))
V<-matrix(data=0,nrow=ncol(X),ncol=ncol(X))
COR<-matrix(data=0,nrow=ncol(X),ncol=ncol(X))

# Analisis de correspondencia

res.ca <- CA(X, graph = FALSE)

# Para los nombres de las variables de las filas y columnas

get_ca_row(res.ca)
get_ca_col(res.ca)

#Se visualizan las contribuciones en el eje 1:
#Filas
fviz_contrib(res.ca, choice = "row", axes = 1)
#Columnas
fviz_contrib(res.ca, choice = "col", axes = 1)

#Graficamos los resultados
#Filas
fviz_ca_row(res.ca, repel = TRUE)
#Columnas
fviz_ca_col(res.ca)

#conjunta
fviz_ca_biplot(res.ca, repel = TRUE)
