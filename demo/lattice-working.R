

library(mosaiq)

## export.mosaiq("fig/mosaiq_%03g.png")

mydata <-
    data.frame(x = 1:1000, y = rnorm(1000),
               g = gl(3, 1, 1000, labels = month.name[1:3]),
               a = gl(1, 1000))

mosaiq.xyplot(y ~ x, data = mydata,
              ## margin = ~a,
              groups = g,
              grid = TRUE)

myplot <-
    mosaiq.xyplot(y ~ x, data = mydata, groups = g,
                  margin = ~g, layout = c(2, 2))


mosaiq.xyplot(y ~ x, data = mydata, main = "foo", xlab = "x", ylab = "y")

mosaiq.densityplot(~ y, data = mydata, groups = g, 
                   margin = ~a)

mosaiq.densityplot(~ y, data = mydata, margin = ~g)

mosaiq.histogram(~ y, data = mydata, margin = ~g + a)

mosaiq.qqmath(y, data = mydata, groups = g, margin = ~a)


library("latticeExtra")
data(gvhd10)

mosaiq.xyplot(SSC.H ~ FSC.H, data = gvhd10, margin = ~Days,
              panel.groups = mosaiq.smoothScatter)

mosaiq.xyplot(SSC.H ~ FSC.H, data = gvhd10, margin = ~Days,
              panel = list(panel.mosaiq.xyplot, mosaiq.grid),
              panel.groups = mosaiq.smoothScatter)





data(Chem97, package = "mlmRev")
xtabs( ~ score, data = Chem97)

mosaiq.histogram(gcsescore, data = Chem97, margin = ~factor(score))

mosaiq.densityplot(gcsescore, data = Chem97, margin = ~factor(score))

mosaiq.densityplot(gcsescore, data = Chem97, groups = score,
                   legend.args = list(columns = 3))

data(Oats, package = "nlme")

mosaiq.xyplot(yield ~ nitro, data = Oats,
              margin = ~ Block + Variety, type = 'o')

mosaiq.xyplot(yield ~ nitro, data = Oats,
              margin = ~ Block + Variety, type = 'o',
              layout = c(3, 2),
              main = "Yield of Oats")


data(barley, package = "lattice")

mosaiq.dotplot(yield, variety, data = barley, groups = year, margin = ~ site,
               layout = c(1, 6), aspect = c(0.7), grid = FALSE)

mosaiq.xyplot(yield, variety, data = barley, jitter.y = TRUE)

mosaiq.densityplot(eruptions, data = faithful)

mosaiq.densityplot(x = eruptions, data = faithful, kernel = "rect", bw = 0.2)

library("latticeExtra")
data(gvhd10)


mosaiq.xyplot(SSC.H ~ FSC.H, data = gvhd10, margin = ~Days,
              panel.groups = mosaiq.smoothScatter)




mosaiq.densityplot(log(FSC.H), data = gvhd10, margin = ~Days, layout = c(2, 4))

mosaiq.histogram(~log2(FSC.H), data = gvhd10, margin = ~Days,
                 xlab = "log Forward Scatter",
                 type = "density", nint = 50,
                 layout = c(2, 4))

mosaiq.qqmath(x = gcsescore, data = Chem97,
              margin = ~factor(score),
              f.value = ppoints(100))

## qqmath(~ gcsescore | gender, Chem97, groups = score, aspect = "xy", 
##        f.value = ppoints(100), auto.key = list(space = "right"),
##        xlab = "Standard Normal Quantiles", 
##        ylab = "Average GCSE Score")

Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)

mosaiq.bwplot(factor(score) ~ gcsescore, data = Chem97,
              margin = ~ gender,
              xlab = "Average GCSE Score")

mosaiq.bwplot(gcsescore^2.34 ~ gender, data = Chem97,
              margin = ~ factor(score), as.table = FALSE,
              varwidth = TRUE, layout = c(6, 1),
              main = "Transformed GCSE score")

