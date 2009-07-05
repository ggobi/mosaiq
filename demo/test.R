
library(mosaiq)

mydata <- iris
margin.vars <- list(species = expression(Species))
panel.vars <- list(x = expression(Petal.Length))

toplevel <- qvBasicWidget(minheight=500, minwidth=500)
toplevel

foo <- 
    mosaiq(data = mydata, enclos = .GlobalEnv,
           margin.vars = margin.vars,
           panel.vars = panel.vars,
           layout = c(2, 2))

toplevel[1, 1] <- foo

data(gvhd10, package = "latticeExtra")

foo <- 
    mosaiq(data = gvhd10, enclos = .GlobalEnv,
           margin.vars = list(expression(Days)),
           panel.vars = list(x = expression(asinh(FL3.H))),
           relation = list(x = "same", y = "same"),
           main = "density plot", sub = "gvhd10 data",
           xlab = "asinh(FL3.H)", ylab = "density")

foo <- 
    mosaiq(data = gvhd10, enclos = .GlobalEnv,
           margin.vars = list(expression(Days)),
           panel.vars = list(x = expression(asinh(FL3.H))),
           prepanel = prepanel.mosaiq.histogram,
           panel = panel.mosaiq.histogram, fill = "cyan",
           relation = list(x = "same", y = "same"),
           main = "density plot", sub = "gvhd10 data",
           xlab = "asinh(FL3.H)", ylab = "density")


1

## mypackets <- compute.packets(margin.vars, data = mydata)

## mylayout <-
##     compute.layout(c(0, prod(dim(mypackets))),
##                    dim(mypackets))

## mylimits <-
##     compute.limits(mypackets, panel.vars,
##                    prepanel = prepanel.mosaiq.densityplot,
##                    data = mydata)

## mylimits <- combine.limits(mylimits)

## mypanels <-
##     create.panels(packets = mypackets,
##                   limits = mylimits,
##                   panel.vars = panel.vars,
##                   panel = panel.mosaiq.densityplot,
##                   data = mydata)



