
#### par mess! quite RESOLVED

par(mfrow = c(2,2)) ## some random par change
par("mfrow")
par("col.lab")

## last resource xD
par(mfrow = c(1,1))


## A GOOD attempt --> BETTER copy the current settings

## take a look on ADVANCED PLOTS on Quick-R
## http://www.statmethods.net/advgraphs/parameters.html

par()              # view current settings
opar <- par()      # make a copy of current settings
par(col.lab="red") # red x and y labels 
hist(mtcars$mpg)   # create a plot with these new settings 
par(opar)          # restore original settings

