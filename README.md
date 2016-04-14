

library("rgeos")
library("sp")
library("rgdal")
#definir a pasta "dist?ncias" como diret?rio de trabalho e recarregar as planilhas


petal <- read.csv2("petal.csv", dec=",")


LB <- data.frame(especie= c(1:30),X=label$X[label$categoria=="base"], Y=label$Y[label$categoria=="base"]) 
LT <- data.frame(X=label$X[label$categoria=="tip"], Y=label$Y[label$categoria=="tip"])  


bla <- data.frame(especie= c(1:60),X=label$X, Y=label$Y)


amostra<- sample(bla[,1], size=30, replace=FALSE)

amostra.a.xy <- bla[bla[,1][(amostra)], c(2,3)]
amostra.b.xy <- bla[bla[,1][-(amostra)], c(2,3)]


#poligono 1
PTch <- chull(amostra.a.xy)
coordsPT <- amostra.a.xy[c(PTch, PTch[1]), ]

plot(amostra.a.xy, pch=19)
lines(coordsPT, col="red")

PT_poly <- SpatialPolygons(list(Polygons(list(Polygon(coordsPT)), ID="PB")))


#poligono 2
PBch <- chull(amostra.a.xy)
coordsPT <- amostra.a.xy[c(PBch, PTch[1]), ]

plot(amostra.a.xy, pch=19)
lines(coordsBT, col="red")

PB_poly <- SpatialPolygons(list(Polygons(list(Polygon(coordsPB)), ID="PB")))


#area de interseccao

int<-gIntersection(PB_poly, PT_poly)
