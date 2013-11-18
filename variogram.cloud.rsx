##[Prueba]=group
##showplots
##layer=vector
##field=field layer

library('gstat')
library('sp')
library('gstat')
library('rgeos')
library('maptools')
names(layer)[names(layer)==field]="field"
data <- as.data.frame(layer)

plot(variogram(field~1,locations=~coords.x1+coords.x2, data=data, cloud=TRUE))
