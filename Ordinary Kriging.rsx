##[R-Geostatistics]=group
##showplots
##layer=vector
##field=field layer
##nugget=number 0
##model=selection Exp;Sph;Gau;Mat
##range=number 0
##psill=number 0
##kriging_prediction= output raster
##kriging_variance= output raster

library('gstat')
library('sp')
library('raster')
Models<-c("Exp","Sph","Gau","Mat")
model2<-Models[model+1]

create_new_data <- function (obj)
{
convex_hull = chull(coordinates(obj)[, 1], coordinates(obj)[,
2])
convex_hull = c(convex_hull, convex_hull[1])
d = Polygon(obj[convex_hull, ])
new_data = spsample(d, 5000, type = "regular")
gridded(new_data) = TRUE
attr(new_data, "proj4string") <-obj@proj4string
return(new_data)
}
mask<-create_new_data(layer)
names(layer)[names(layer)==field]="field"

layer$field <- as.numeric(as.character(layer$field))
str(layer)
layer <- remove.duplicates(layer)
layer <- layer[!is.na(layer$field),]

g = gstat(id = field, formula = field~1, data = layer)
vg = variogram(g)
vgm = vgm(nugget=nugget, psill=psill, range=range, model=model2)
vgm = fit.variogram(vg, vgm)

>vgm
>attr(vgm, "SSErr")
plot(vg, vgm, plot.numbers = TRUE)
prediction = krige(field~1, layer, newdata = mask, vgm)
kriging_prediction = raster(prediction)
kriging_variance = raster(prediction["var1.var"])
