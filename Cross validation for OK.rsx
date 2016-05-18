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
##Cross_validation= output table


library('gstat')
library('sp')
Models<-c("Exp","Sph","Gau","Mat")
model2<-Models[model+1]

field <- make.names(field)
names(layer)[names(layer)==field]="field"

layer$field <- as.numeric(as.character(layer$field))
str(layer)
layer <- remove.duplicates(layer)
layer <- layer[!is.na(layer$field),]

g = gstat(id = field, formula = field~1, data = layer)
vg = variogram(g)

if(Estimate_range_and_psill_initial_values_from_sample_variogram){range=NA}
if(Estimate_range_and_psill_initial_values_from_sample_variogram){psill=NA}

vgm = vgm(nugget=nugget, psill=psill, range=range, model=model2)
vgm = fit.variogram(vg, vgm)
>vgm
plot(vg, vgm, plot.numbers = TRUE)
if(Local_kriging==FALSE){prediction = krige.cv(field~1, layer, vgm)}
if(Local_kriging==TRUE){prediction = krige.cv(field~1, layer, vgm, nmax=Number_of_nearest_observations)}

Cross_validation <- as.data.frame(prediction@data)

RMSE <- sqrt(mean(prediction$residual^2, na.rm=TRUE))
>print(RMSE)


