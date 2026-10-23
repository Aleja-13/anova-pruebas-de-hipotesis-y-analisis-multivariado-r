#Arreglamos los datos para la muestra A y B
A<-c(1,2,3,5,9,16,4,10,11,6,17,18,20,19,21,26,27,28,29,30,31,32,41,40,42,43,44,45)
B<-c(7,8,12,13,14,15,22,23,24,25,33,34,35,36,37,38,46,47,39,48,49,50)

#1 para identificar la hipótesis 

#Prueba de hipotesis para la diferencia de medias
#### Hipotesis
# Ho: miuA - miuB=0
# H1: miuA - miuB=/0  Diferente

#H0 : No hay diferencia entre las medias, media control???media tratamiento=0

#Ha: Sí hay diferencia entre las medias, media control???media tratamiento???0

#2 EStableemos el intervalo de confianza
alfa<-0.05

#3 hallar el estadístico de prueba 

Xa<- mean(A)
Xa
Xb<-mean(B)
Xb
est<-Xa-Xb

SA<-sd(A)
SA
SB<-sd(B)
SB
nA<-length(A)
nB<-length(B)

T<-((Xa-Xb)/sqrt((SA^2/nA)+(SB^2/nB)))
T

#4 hallar el P valor 
GL <-(nA-1 + nB-1)
GL

pt(T,GL, lower.tail = F) 

#5 hallar la condición

tcritico<-qt(p = (1-alfa) + alfa/2, df = GL, lower.tail = TRUE)
tcritico


#6 la conclusión


t.test(
  x           = A,
  y           = B,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = TRUE,
  conf.level  = 0.95
)


#Como el p-value es > 0.05 podemos afirmar que las muestras no difieren en su media, es decir, los dos variables no son diferentes