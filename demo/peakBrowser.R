
library(mosaiq)
library(chipseq)

data(cstest)
cstest



xx <- cstest$ctcf$chr10



ext <- extendReads(xx, seqLen = 150)
head(ext)

cov <- coverage(ext)
cov


islands <- slice(cov, lower = 1)

idf <-
    data.frame(start = start(islands),
               end = end(islands),
               depth = viewMaxs(islands))

idf <- idf[rev(order(idf$depth)), ]


pointsAndCoverage <- function(x, y, seqLen = 150, ..., fill, col, panel.env)
{
    if (is.null(panel.env$panel.specific))
    {
        ## str(list(x, y, ...))
        xx <- split(x, y)
        names(xx) <- c("-", "+")
        ext <- extendReads(xx, seqLen = seqLen)
        panel.env$panel.specific <-
            coverage(ext,
                     shift = -min(start(ext)))
        panel.env$y.jittered <- y + 0.3 * (runif(length(y)) - 0.5)
        panel.env$shift <- min(start(ext))
    }
    cov <- panel.env$panel.specific
    rl <- runLength(cov)
    rv <- runValue(cov)
    cov.col <-
        lattice::level.colors(rv,
                              at = do.breaks(c(0, max(rv)), 50),
                              col.regions = terrain.colors,
                              colors = TRUE)
    mosaiq.rect(xleft = panel.env$shift + cumsum(c(0, head(rl, -1))),
                xright = panel.env$shift + cumsum(rl),
                ybottom = 1.4, ytop = 1.6,
                fill = cov.col, col = "transparent", ...)
    mosaiq.points(x, panel.env$y.jittered, fill = fill, col = col, ...)
}



plotIsland <- function(i)
{
    subdata <- 
        subset(do.call(make.groups, xx),
               data > idf$start[i] & data < idf$end[i])
    print(mosaiq.xyplot(which ~ data, data = subdata,
                        alternating = list(y = 3),
                        panel.groups = pointsAndCoverage,
                        grid = list(h = 0, v = -5)))
    plot(densityplot( ~ data, data = subdata, groups = which))
}

peakview <- qdataview(idf)
qsetExpanding(peakview, horizontal = FALSE)

qconnect(peakview, "cellActivated",
         handler = function(x) {
             plotIsland(qcurrentRow(x))
         }, user.data = peakview)
         

qaddWidgetToLayout(.MosaicEnv$toplayout, peakview,
                   1, 2, 2, 1)


rscene <- qsceneDevice(9, 4)
rview <- qgraphicsView(rscene)
qsetAntialias(rview, TRUE)
qsetExpanding(rview, horizontal = FALSE, vertical = FALSE)

qaddWidgetToLayout(.MosaicEnv$toplayout, rview,
                   2, 1)


ii <- 1L

mytimer <-
    qtimer(delay = 1000,
           handler = function() {
               ii <<- (ii + 1)
               if (ii > nrow(idf)) ii <<- 1
               plotIsland(ii)
           })
           



