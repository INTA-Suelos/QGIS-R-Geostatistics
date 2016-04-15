##[R-Geostatistics]=group
##showplots
##layer=vector
##field=field layer
##Estimate_range_and_psill_initial_values_from_sample_variogram=boolean True
##nugget=number 0
##model=selection Exp;Sph;Gau;Mat
##range=number 0
##psill=number 0
##Local_kriging=boolean False
##Number_of_nearest_observations=number 25
##Show_Sum_of_Square_Errors=boolean False
##kriging_variance= output raster
##kriging_prediction= output raster


library('gstat')
library('sp')
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

if(Estimate_range_and_psill_initial_values_from_sample_variogram & range==0){range=NA} 
if(Estimate_range_and_psill_initial_values_from_sample_variogram & psill==0){psill=NA}

vgm = vgm(nugget=nugget, psill=psill, range=range, model=model2)
vgm = fit.variogram(vg, vgm)
>vgm
plot(vg, vgm, plot.numbers = TRUE)
if(Local_kriging==FALSE){prediction = krige(field~1, layer, newdata = mask, vgm)}
if(Local_kriging==TRUE){prediction = krige(field~1, layer, newdata = mask, vgm, nmax=Number_of_nearest_observations)}
>if(Show_Sum_of_Square_Errors==TRUE){paste("SSE:", attr(vgm, "SSErr"))}
kriging_prediction = raster(prediction)
kriging_variance = raster(prediction["var1.var"])
