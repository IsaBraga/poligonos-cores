

library("rgeos")
library("sp")
library("rgdal")
#definir a pasta "dist?ncias" como diret?rio de trabalho e recarregar as planilhas


petal <- read.csv2("petal.csv", dec=",")


LB <- data.frame(especie= c(1:30),X=label$X[label$categoria=="base"], Y=label$Y[label$categoria=="base"]) 
LT <- data.frame(X=label$X[label$categoria=="tip"], Y=label$Y[label$categoria=="tip"])  


bla <- data.frame(especie= c(1:60),X=label$X, Y=label$Y)




resulta<-rep(NA,1000)
for(i in 1:1000){
  
amostra<- sample(bla[,1], size=30, replace=FALSE)

amostra.a.xy <- bla[bla[,1][(amostra)], c(2,3)]
amostra.b.xy <- bla[bla[,1][-(amostra)], c(2,3)]


#poligono 1
PTch <- chull(amostra.a.xy)
coordsPT <- amostra.a.xy[c(PTch, PTch[1]), ]

PT_poly <- SpatialPolygons(list(Polygons(list(Polygon(coordsPT)), ID="PB")))


#poligono 2
PBch <- chull(amostra.b.xy)
coordsPB <- amostra.b.xy[c(PBch, PBch[1]), ]


PB_poly <- SpatialPolygons(list(Polygons(list(Polygon(coordsPB)), ID="PB")))


#area de interseccao
int<-gIntersection(PB_poly, PT_poly)

int@polygons[[1]]@area/PT_poly@polygons[[1]]@area #divide a area da inerseccao pela area de um dos poligonos



ifelse (is.null(int),
        resulta[i]<-0 ,
        resulta[i]<-int@polygons[[1]]@area/PT_poly@polygons[[1]]@area)
}


hist(resulta,ylab="Frequencia",xlab="porcentagens de sobreposição dos poligonos",
     nclass=50)
hist(resulta,ylab="Frequencia",xlab="porcentagens de sobreposição dos poligonos",
     nclass=50, xlim=c(0,1))



##original
bla_apice<-bla[1:30,2:3]
apice_ch <- chull(bla_apice)
coords_apice <- bla_apice[c(apice_ch, apice_ch[1]), ]
apice_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords_apice)), ID="PB")))


bla_base<-bla[31:60,2:3]
base_ch <- chull(bla_base)
coords_base <- bla_base[c(base_ch, base_ch[1]), ]
base_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords_base)), ID="PB")))


int_original<-gIntersection(base_poly, apice_poly)


valor_original<-NA


ifelse (is.null(int_original),
       valor_original <-0 ,
       valor_original<-int_original@polygons[[1]]@area/apice_poly@polygons[[1]]@area)


abline(v=valor_original, col="red")


conf<-quantile(resulta, prob=c(0.025, 0.975))
abline(v=conf, col="green")
