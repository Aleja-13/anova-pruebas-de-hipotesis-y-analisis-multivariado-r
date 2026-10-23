library(FactoMineR)
library(ggplot2)
library(factoextra)

###LECTURA DE DATOS###

data("housetasks")

# tabla
dt <- as.table(as.matrix(housetasks))
dt

#Grafico

ggballoonplot(housetasks, main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# la prueba de chi-cuadrado para evaluar si existe una dependencia significativa entre las categorías de filas y columnas
chisq <- chisq.test(housetasks)
chisq

####En nuestro ejemplo, las variables de fila y columna están asociadas de manera estadísticamente significativa

# análisis de correspondencia
res.ca <- CA(housetasks, graph = FALSE)
print(res.ca)

#Significancia estadística
# Chi-square statistics
chi2 <- 1944.456
# Degree of freedom
df <- (nrow(housetasks) - 1) * (ncol(housetasks) - 1)
# P-value
pval <- pchisq(chi2, df = df, lower.tail = FALSE)
pval

#Autovalores / Varianzas

eig.val <- get_eigenvalue(res.ca)
eig.val

fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
fviz_screeplot(res.ca) +
  geom_hline(yintercept=33.33, linetype=2, color="red")
fviz_ca_biplot(res.ca, repel = TRUE)

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


fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

##### Usando datos precargados en R
install.packages("ca")
data("author")
ca(author)
plot(ca(author))

# table method
haireye <- margin.table(HairEyeColor, 1:2)
haireye.ca <- ca(haireye)
haireye.ca
plot(haireye.ca)
# some plot options
plot(haireye.ca, lines=TRUE)
plot(haireye.ca, arrows=c(TRUE, FALSE))