##[R-Geostatistics]=group
##showplots
##layer=vector
##field=field layer
##nugget=number 0
##model=selection Exp;Sph;Gau;Mat
##range=number 0
##psill=number 0

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
if(range==0){range=NA} # is user doesnt change default values for psill and/or range
if(psill==0){psill=NA} # this script will use NA and then, gstat will estimate psill
		       # and range from variogram ..
vgm <- vgm(nugget=nugget, range=range, psill=psill, model="Exp")
vgm = fit.variogram(vg, vgm)

>vgm
>paste("SSErr:", attr(vgm, "SSErr"))
plot(vg, vgm, main = title , plot.numbers = TRUE)
