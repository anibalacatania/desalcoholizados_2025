library(SensoMineR)
library(readxl)
library(missMDA)
library(lmerTest)
library(predictmeans)
library(tidyverse)
library(ggrepel)
library(readxl)
library(readr)
library(agricolae)
data <- read_excel("data2.xlsx")


data$producto<-as.factor(data$producto)
data$sesion<-as.factor(data$sesion)
data$panelista<-as.factor(data$panelista)
data<-as.data.frame(data)
str(data)
plot<- panellipse(data,col.p=1,col.j=3,firstvar=4, 
                  level.search.desc =0.05,
                  graph.type = "ggplot")

table(data$sesion,data$panelista)

# 700 * 418
plot$graph$plotIndEll+
  theme(aspect.ratio = 1) +labs(title="")+
  theme(axis.title.y = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(hjust = 0.5))+
  
  stat_ellipse(type = "t")

plot$graph$plotVarVariab+ theme(legend.position = "none")+
  labs(title="")+
  theme(axis.title.y = element_text(hjust = 0.5))+
  theme(axis.title.x = element_text(hjust = 0.5))

#######################3
data <- na.omit(data)
res <- panellipse.session(data, col.p = 1, col.j = 3, col.s = 2,
                          firstvar = 4)



######################

#grafico de interacciones
text <- function(...) {}   # redefino text() vacío

graphinter(data,col.p=1,col.j=3,
           firstvar=4,
           lastvar=30,
           numr=1,
           numc=1
           )
rm(text) 

#########################


# imputation
#data<-na.omit(data)
da.imp=imputeFAMD(data, ncp = 2)
data<-da.imp$completeObs

res.panelperf <- panelperf(data,firstvar=4,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion",random=F)
coltable(res.panelperf$p.value[order(res.panelperf$p.value[,1]),],col.lower="gray", level.lower = 0.05,cex=0.8)


#####

#poder de Discrimination
res.paneliperf <- paneliperf(data,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=3,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$prob.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=0.05,level.upper=0.06, col.lower="gainsboro",col.upper="gray",cex = 0.5, nbcol=16)
write.table(round(res.magicsort,3),"poderdiscri.csv",row.names = TRUE)
#Acuerdo de los panelistas
res.paneliperf <- paneliperf(data,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=3,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$agree.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=-0.001,level.upper=0.80, col.lower="gainsboro",col.upper="gray",cex = 0.5,nbcol=16)


#Repetibilidad
res.paneliperf <- paneliperf(data,formul="~producto+panelista+ sesion+producto:panelista+producto:sesion+panelista:sesion", formul.j="~producto+sesion",col.j=3,firstvar=4,synthesis=TRUE)
names(res.paneliperf)
res.magicsort <- magicsort(res.paneliperf$res.ind,method="median")
round(res.magicsort,3)
coltable(round(res.magicsort,2),level.lower=0.001,level.upper=1.96, col.lower="gainsboro",col.upper="gray",cex = 0.5,nbcol=16)

###############

# Calcular promedio por producto
avg_intensidad <- data %>%
  group_by(producto) %>%
  summarise(intensidad_prom = mean(`Compota`, na.rm = TRUE))

# Paleta tomada de seaborn por defecto (deep)
colores <- c("#4C72B0", "#55A868", "#C44E52", "#8172B3", 
             "#CCB974", "#64B5CD", "#937860", "#DA8BC3")

ggplot(avg_intensidad, aes(x = producto, y = intensidad_prom, fill = producto)) +
  geom_col() +
  scale_fill_manual(values = colores) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"   # 👈 Esto elimina la leyenda
  ) +
  labs(
    title = "Compota promedio por producto",
    x = "Producto",
    y = "Compota (promedio)"
  )
####

data2 <- data %>%
  rename(Producto = producto,
         Valuador = panelista,
         Rep=sesion)
reproducibilidad<-as.data.frame(calcular_indice_reproducibilidad(data2))
write.csv(reproducibilidad,"reproducibilidad.csv")
estabilidad<-calcular_indice(data2)
write.csv(estabilidad,"estabilidad.csv")
