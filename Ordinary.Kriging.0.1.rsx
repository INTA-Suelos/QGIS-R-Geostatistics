##[Prueba]=group
##showplots
##layer=vector
##field=field layer
##nugget=number 20000
##psill=number 80000
##range=number 2E5
##Sph_or_Exp=string Exp
##title=string Semivariogram@QGIS
##out= output raster

library('gstat')
library('sp')
library('raster')
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
g = gstat(id = field, formula = field~1, data = layer)
vg = variogram(g)
vgm = vgm(nugget=nugget, psill=psill, range=range, model=Sph_or_Exp)
vgm = fit.variogram(vg, vgm)

>vgm
>attr(vgm, "SSErr")
plot(vg, vgm, main = title , plot.numbers = TRUE)
prediction = krige(field~1, layer, newdata = mask, vgm)
out = raster(prediction)
