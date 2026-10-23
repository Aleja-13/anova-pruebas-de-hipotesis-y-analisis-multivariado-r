NUM_HIJ=as.factor(c("287","295","303","311", "312"))
LACT=(ID=c("LACT.1", "LACT.2","LACT.3"))

NUM_HIJ<- as.factor(c("287","295","303","311", "312"))
LACT <- rep(c("LACT.1", "LACT.2","LACT.3"))

tabla <- cbind(c("4603","5229","4935","5793","4930"), 
               c("5653","5456","6726","6927","5307"), 
               c("5672","3352","5874","0","0"))

rownames(tabla) <- c("287","295","303","311","312")
colnames(tabla) <- c("LACT.1", "LACT.2","LACT.3")
print(tabla)

summary(tabla)
datos<-as.data.frame(tabla)
datos

anova <- aov(tabla)
summary(anova)
anova(datos)
aov(datos)
