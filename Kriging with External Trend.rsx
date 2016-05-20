##[R-Geostatistics]=group
##showplots
##layer=vector
##field=field layer
##covariate=raster
##Estimate_range_and_psill_initial_values_from_sample_variogram=boolean True
##nugget=number 0
##model=selection Exp;Sph;Gau;Mat
##range=number 0
##psill=number 0
##Local_kriging=boolean False
##Number_of_nearest_observations=number 25
##Show_Sum_of_Square_Errors=boolean False
##Extent=selection Convex Hull; Layer Extent
##Resolution=number 0
##kriging_variance= output raster
##kriging_prediction= output raster


library('gstat')
library('sp')
Models<-c("Exp","Sph","Gau","Mat")
model2<-Models[model+1]

# layer  <- read.csv2("Proyectos/Eventuales/AguasCluster/Agua puntos Todos.csv")
# coordinates(layer) <- ~ Long + Lat
# plot(layer)

mask <- as(covariate, "SpatialGridDataFrame")

field <- make.names(field)
names(layer)[names(layer)==field]="field"

layer$field <- as.numeric(as.character(layer$field))
str(layer)
layer <- remove.duplicates(layer)
layer <- layer[!is.na(layer$field),]

names(covariate) <- "covariate"

layer@data <- cbind(layer@data, covariate=extract(covariate, layer))
summary(layer@data)
str(layer)
layer <- spTransform(layer, covariate@crs)
model <- lm(field ~ covariate, data=layer@data)
#>summary(model)

trend <- as.formula(model$call)

dependend_var.v <- variogram(trend, layer)
plot(dependend_var.v)

if(Estimate_range_and_psill_initial_values_from_sample_variogram){range=NA}
if(Estimate_range_and_psill_initial_values_from_sample_variogram){psill=NA}

vgm = vgm(nugget=nugget, psill=psill, range=range, model=model2)
vgm = fit.variogram(dependend_var.v, vgm)
>vgm
plot(dependend_var.v, vgm, plot.numbers = TRUE)

mask@data[is.na(mask@data)] <- 0
names(mask) <- "covariate"
if(Local_kriging==FALSE){prediction = krige(trend, layer, newdata = mask, vgm)}
if(Local_kriging==TRUE){prediction = krige(trend, layer, newdata = mask, vgm, nmax=Number_of_nearest_observations)}
>if(Show_Sum_of_Square_Errors==TRUE){paste("SSE:", attr(vgm, "SSErr"))}
>if(!is.projected(layer)){warning(paste0("'layer' isn't projected.\n", "Resolution was not used. Interpolation was done over 5000 cells"))}
>if(is.projected(layer) & Resolution == 0){warning("Resolution was set to 0. Final resolution estimated from data")}
>print(paste("R2=", round(summary(model)$r.squared,4)))
values(covariate)[!is.na(values(covariate))]  = 1
kriging_prediction = raster(prediction)*covariate
kriging_variance = raster(prediction["var1.var"])*covariate
