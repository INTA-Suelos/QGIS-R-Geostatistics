##[R-Geostatistics]=group
##showplots
##layer=vector
##field=field layer
##model=selection Exp;Sph;Gau;Mat

library('sp')
library('gstat')
print(model)
Models<-c("Exp","Sph","Gau","Mat")
select_model<-Models[model+1]
# adjust variogram
layer$field <- as.numeric(as.character(layer$field))
str(layer)
layer <- remove.duplicates(layer)
layer <- layer[!is.na(layer$field),]

g <- gstat(id = field, formula = field~1, data = layer)
vg <- variogram(g)
vgm <- vgm(nugget=0, range=sqrt(diff(layer@bbox[1,])^2 + diff(layer@bbox[2,])^2)/4, psill=var(layer$field), model="Exp")
vgm = fit.variogram(vg, vgm)

>vgm
>paste("SSErr:", attr(vgm, "SSErr"))
plot(vg, vgm, main = title , plot.numbers = TRUE)
