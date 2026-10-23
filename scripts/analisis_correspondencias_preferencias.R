#Analizar el grado de preferencia de una hamburguesa nueva entre distintos grupos de edades.
# Cargar datos

datos<-matrix(c(21,21,14,13,8,8,9,6,8,2,2,3,4,10,10),ncol=5, nrow = 3, byrow=TRUE, dimnames = list(c("Bajo","Medio","alto"), c("16-22","22-30","26-35","35-65","65-X")))
datos

# Calcular porcentajes 
ncol<-5 
nfila<-3 
n<-sum(datos) 
n
filaTotal<-apply(datos,1,sum)
filaTotal
colTotal<-apply(datos,2,sum)
colTotal
xfilaTotal<-cbind(filaTotal,filaTotal,filaTotal,filaTotal,filaTotal)
xfilaTotal
xcolTotal<-rbind(colTotal,colTotal,colTotal)
xcolTotal
xfilaTotal<-datos/xfilaTotal 
xcolTotal<-datos/xcolTotal 
rdot<-filaTotal/n 
rdot
cdot<-colTotal/n 
cdot

# Calcular las matrices de distancias entre columnas 
dcols<-matrix(0,ncol,ncol) 
for(i in 1:ncol){ 
  for(j in 1:ncol){d<-0 
  for(k in 1:nfila) d<-d+(xcolTotal[k,i]-xcolTotal[k,j])^2/rdot[k] 
  dcols[i,j]<-sqrt(d)}} 
dcols

# Calcular las matrices de distancias entre filas
dfilas<-matrix(0,nfila,nfila) 
for(i in 1:nfila){ 
  for(j in 1:nfila){d<-0 
  for(k in 1:ncol) d<-d+(xfilaTotal[i,k]-xfilaTotal[j,k])^2/cdot[k] 
  dfilas[i,j]<-sqrt(d)}} 
dfilas

# Ejecutar el analisis de correspondencias
r1<-cmdscale(dcols,eig=TRUE) 
r1$points 
r1$eig 
c1<-cmdscale(dfilas,eig=TRUE) 
c1$points 
c1$eig 

# Dibujar las coordenadas en un dos dimensiones 
par(pty="s") 
plot(r1$points,xlim=range(r1$points[,1],c1$points[,1]),ylim=range(r1$points[,1],c1$points[,1]),type="n", xlab="Coordenada 1",ylab="Coordenada 2",lwd=2) 
text(r1$points,labels=c("16-22","22-30","26-35","35-65","65-X"),lwd=2) 
text(c1$points,labels=c("Bajo","Medio","alto"),lwd=4) 
abline(h=0,lty=2) 
abline(v=0,lty=2)
