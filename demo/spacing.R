
## Observations about spacing

tmp <- rlnorm(100)
df <- data.frame(x = runif(200),
                 y = c(tmp, 2 * tmp), 
                 g = gl(2, 100))


## This is OK

mosaiq.xyplot(x, y, margin = ~g, data = df, layout = c(1, 2))

## This has one row taller, apparently because the axis labeling is
## absent.  stretch factors are cumulative?

## No axis on bottom row (OK without the 'alternating' argument) 
library(mosaiq)
mosaiq.xyplot(x, y, margin = ~g, data = df, layout = c(1, 2),
              alternating = list(y = c(1, 0)))

