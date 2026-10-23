###LECTURA DE DATOS###
X<-read.table(file.choose())
X<-data.matrix(X)
X

###DEFINICIÓN DE ALGUNOS ARREGLOS###
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
###

##CALCULANDO EL CENTROIDE##
for (j in 1:ncol(X)){
G[1,j]<-mean(X[,j])
}
G

##creando matriz con DATOS CENTRADOS##
for (j in 1:4){
C[,j]<-X[,j]-G[1,j]
}
C

###AHORA ESTANDARICEMOS LA MATRIZ X###
Z<-scale(X)
Z

###VEAMOS LA MATRIZ DE CORRELACIONES DE X,C y Z###
cor(X)
cor(C)
cor(Z)
#######################################################################
########################################################
####  CONSTRUCCIÓN DE LAS MATRICES DE LA MÉTRICA  ####
M
N
for(i in 1:ncol(X)){M[i,i]<-1/var(X[,i])}
M
for(i in 1:nrow(X)){N[i,i]<-1/nrow(X)}
N

###############################################################################

###CÁLCULO DE SUMA DE TODAS LAS DISTANCIAS AL CUADRADO SIN MÉTRICA###

###################################

##CALCULANDO LA SUMA DE TODAS LAS DISTANCIAS CUADRADAS USANDO LA MATRIX X##
## CALCULANDO UNA A UNA LAS DISTANCIAS (n*(n-1) distancias activas)##
sumX1<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
for (j in 1:ncol(X)){
sumX1<- sumX1+(X[i,j]-X[k,j])^2
}
}
}
sumX1
##
##CALCULANDO LAS DISTANCIAS COMO PRODUCTO ESCALAR DE LA RESTA DE DOS FILAS##
sumX2<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
sumX2<- sumX2+t(X[i,]-X[k,])%*%(X[i,]-X[k,])
}
}
sumX2
###################################
##CALCULANDO LA SUMA DE TODAS LAS DISTANCIAS CUADRADAS USANDO LA MATRIX C##
sumC<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
sumC<- sumC+t(C[i,]-C[k,])%*%(C[i,]-C[k,])
}
}
#Distancia con datos centrados
sumC

##CALCULANDO LA SUMA DE TODAS LAS DISTANCIAS CUADRADAS USANDO LA MATRIX Z##
sumZ<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
sumZ<- sumZ+t(Z[i,]-Z[k,])%*%(Z[i,]-Z[k,])
}
}
#Distancia con datos centrados
sumZ

###############################################################################
##CÁLCULO DE SUMA DE TODAS LAS DISTANCIAS CUADRADAS AL CENTROIDE##
sum2<-0
for(i in 1:nrow(X)){
for (j in 1:ncol(X)){
sum2<- sum2+(X[i,j]-G[1,j])^2
}
}
sum2
###############################################################################
##VERIFICANDO RELACIÓN ENTRE DISTANCIAS sum=2*n*sum2   (sin métrica)##
sum
2*10*sum2
##

###############################################################################
##CALCULEMOS LA SUMA DE TODAS LAS DISTANCIAS ENTRE INDIVIDUOS BAJO LA MÉTRICA ##
#############################USANDO LA MATRIZ X

Xsum1<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
w<-N[i,i]
Xsum1<- Xsum1+w*t(X[i,]-X[k,])%*%M%*%(X[i,]-X[k,])
}
}
Xsum1

##CALCULEMOS LA SUMA DE TODAS LAS DISTANCIAS ENTRE INDIVIDUOS BAJO LA MÉTRICA ##
##############################USANDO LA MATRIZ C

Csum1<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
w<-N[i,i]
Csum1<- Csum1+w*t(C[i,]-C[k,])%*%M%*%(C[i,]-C[k,])
}
}
Csum1

##CALCULEMOS LA SUMA DE TODAS LAS DISTANCIAS ENTRE INDIVIDUOS BAJO LA MÉTRICA ##
#################################USANDO LA MATRIZ Z

Zsum1<-0
for(i in 1:nrow(X)){
for(k in 1:nrow(X)){
w<-N[i,i]
Zsum1<- Zsum1+w*t(Z[i,]-Z[k,])%*%(Z[i,]-Z[k,])
}
}
Zsum1


################################################################################
#####################           CÁLCULO DE LA INERCIA          #################

#PRIMERO CON DATOS BRUTOS, LUEGO CON DATOS CENTRADOS Y FINALMENTE ESTANDARIZADOS
#I=Sumatoria(N(i,i)*(distancia al cuadrado en la métrica M, entre xi y G)###

######
###     CALCULEMOS LA INERCIA DE LA MATRIZ X     ###
IX<-0
for(i in 1:nrow(X)){
restaX[1,]<-(X[i,]-G[1,])
T<-t(restaX)
IX<-IX+N[i,i]*(restaX%*%M%*%T)
restaX[1,1:4]<-0
}
IX
##HAGAMOS EL CÁLCULO ANTERIOR (LA INERCIA) TOMANDO CADA UNO DE LOS ELEMENTOS DE X##
I2<-0
for(i in 1:nrow(X)){
for(j in 1:ncol(X)){
I2<-I2+N[i,i]*(((X[i,j]-G[1,j])^2)*M[j,j])
}
}
I2
######
###     CALCULEMOS LA INERCIA DE LA MATRIZ C     ###
IC<-0
for(i in 1:nrow(X)){
restaC[1,]<-(C[i,])
TC<-t(restaC)
IC<-IC+N[i,i]*(restaC%*%M%*%TC)
restaC[1,1:4]<-0
}
IC
######
###     CALCULEMOS LA INERCIA DE LA MATRIZ Z     ###
IZ<-0
for(i in 1:nrow(X)){
restaZ[1,]<-(Z[i,])
TZ<-t(restaZ)
IZ<-IZ+N[i,i]*(restaZ%*%TZ)
restaZ[1,1:4]<-0
}
IZ

#####################FIN DE LOS PUNTOS 1 y 2##################################

##################INICIAN LOS PUNTOS 3 y 4###############################
####ENCONTREMOS LA MATRIZ DE VARIANZAS Y COVARIANZAS DE X,C y Z#####
VX<-var(X)
VC<-var(C)
VZ<-var(Z)
VX
VC
VZ
####ENCONTREMOS LA MATRIZ DE CORRELACIONES DE X,C y Z#####
CORX<-cor(X)
CORC<-cor(C)
CORZ<-cor(Z)
CORX
CORC
CORZ
###AHORA, COMO EJERCICIO,CONSTRUYAMOS LA MATRICES DE COVARIANZAS Y CORRELACIONES
###MATRIZ DE VARIANZAS Y COVARIANZAS##
for (i in 1:ncol(X)){
for (j in 1:ncol(X)){
V[i,j]<-t(C[,i])%*%C[,j]
}
}
V<-V*(1/(nrow(X)-1))
V
##MATRIZ DE CORRELACIONES##
for (i in 1:ncol(X)){
for (j in 1:ncol(X)){
COR[i,j]<-(V[i,j])/(sqrt(V[i,i])*sqrt(V[j,j]))
}
}
COR
##########################fin de los puntos 3 Y 4###########################

#############################INICIA EL PUNTO 5##############################
#######CÁLCULO DE VALORES Y VECTORES PROPIOS DE V Y COR#####################
descompV<-eigen(V)
descompV$values
descompV$vectors

descompCOR<-eigen(COR)
descompCOR$values
descompCOR$vectors
############################################################################
#######CÁLCULO DE VALORES Y VECTORES PROPIOS DE (X'X), (XX'), (Z'Z), (ZZ')##
#######[(X'NXM)'(X'NXM)] Y [(X'NXM)(X'NXM)']
descompX1X<-eigen(t(X)%*%X)
descompX1X$values
descompX1X$vectors

descompXX1<-eigen(X%*%t(X))
descompXX1$values
descompXX1$vectors

descompZ1Z<-eigen(t(Z)%*%Z)
descompZ1Z$values
descompZ1Z$vectors

descompZ1Z2<-eigen((1/(nrow(X)-1))*(t(Z)%*%Z))
descompZ1Z2$values
descompZ1Z2$vectors

descompZZ1<-eigen(Z%*%t(Z))
descompZZ1$values
descompZZ1$vectors

descompZZ12<-eigen((1/(nrow(X)-1))*(Z%*%t(Z)))
descompZZ12$values
descompZZ12$vectors

descompXNXM<-eigen(t(t(X)%*%N%*%X%*%M)%*%(t(X)%*%N%*%X%*%M))
descompXNXM$values
descompXNXM$vectors

descompTXNXM<-eigen((t(X)%*%N%*%X%*%M)%*%(t(t(X)%*%N%*%X%*%M)))
descompTXNXM$values
descompTXNXM$vectors

############################################################################

#VERIFICANDO QUE LA SUMA DE LOS VALORES PROPIOS DE (X'X) ES IGUAL A LA TRAZA(X'X)
descompXX<-eigen(t(X)%*%X)
descompXX$values
descompXX$vectors
sum(descompXX$values)
sum(diag(t(X)%*%X))

##VERIFIQUEMOS LA RELACIÓN CON MÉTRICA (X'NXM)'(X'NXM)##
descompXNXM<-eigen(t(t(X)%*%N%*%X%*%M)%*%(t(X)%*%N%*%X%*%M))
descompXNXM$values
descompXNXM$vectors
sum(descompXNXM$values)
sum(diag(t(t(X)%*%N%*%X%*%M)%*%(t(X)%*%N%*%X%*%M)))

##HAGAMOSLO CON Z LOS DATOS ESTANDARIZADOS
descompZZ<-eigen((1/(nrow(X)-1))*(t(Z)%*%Z))
descompZZ$values
descompZZ$vectors
sum(descompZZ$values)
sum((1/(nrow(X)-1))*diag(t(Z)%*%Z))
###########################################################################
corZ<-cor(Z)
corZ
(1/(nrow(X)-1))*(t(Z)%*%Z)

#############################fin del punto 5##################################












descompZZ<-eigen((t(Z)%*%Z))
descompZZ$values
descompZZ$vectors
X
descompZZ$vectors[,1]

t(descompZZ$vectors[,1])%*%descompZZ$vectors[,1]
Fi1<-Z%*%descompZZ$vectors[,1]
Fi1

Fi2<-Z%*%descompZZ$vectors[,2]
Fi2

Fi3<-Z%*%descompZZ$vectors[,3]
Fi3

Fi4<-Z%*%descompZZ$vectors[,4]
Fi4

FI[,1]<-Fi1
FI[,2]<-Fi2
FI[,3]<-Fi3
FI[,4]<-Fi4
FI
X
2.689739*descompZZ$vectors[,1]+0.6650977*descompZZ$vectors[,2]-0.09023207*descompZZ$vectors[,3]+0.14616755*descompZZ$vectors[,4]
Z
t(FI[,1])%*%FI[,1]
#######################################################################
###ENCONTREMOS LOS VALORES Y VECTORES PROPIOS EN LA NUVE DE VARIABLES##
descompZZp<-eigen(Z%*%(t(Z)))
descompZZp$values
descompZZp$vectors
(1/sqrt(descompZZ$values[1]))*FI[,1]
descompZZp$values[,1]
XI1<-t(Z)%*%descompZZp$vectors[,1]
XI1
(1/sqrt(descompZZ$values[1]))*XI1
descompZZ$vectors

###########################################################################



FI[,1]<-X%*%descompX1X$vectors[,1]

FI[,1]

sum(FI[,1]^2)
(1/(sqrt(descompX1X$values[1])))%*%FI[,1]






























