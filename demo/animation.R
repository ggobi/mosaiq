
## Animation of a pendulum swinging on two perpendicular axes with different frequencies

library(qtbase)
library(qtpaint)
library(mosaiq)


pendulum <- function(col = "red", bg = "black", incr = 5,
                   n = 100, opengl = TRUE, verbose = getOption("verbose"))
{
    rgb.col <- col2rgb(col, TRUE)
    edata <- new.env(parent = emptyenv())

    restart <- function()
    {
        edata$t <- 1
        edata$a <- 0.5 + runif(1)
        edata$b <- 0.5 + runif(1)
    }
    restart() ## initialize
    
    step <- function(...) {
        edata$t <- edata$t + incr
        if (verbose) print(edata$t)
        qupdate(points)
    }

    render_data<- function(item, painter, exposed) {
        qantialias(painter) <- FALSE
        tseq <- seq(0, edata$t, by = 0.5) / (2 * base::pi)
        if (length(tseq) > n) 
        {
            hseq <- head(tseq, -(n-1))
            mosaiq.points(x = cos(edata$a * hseq), y = cos(edata$b * hseq),
                          type = "l", col = bg, painter = painter)
        }
        tseq <- tail(tseq, n)
        mosaiq.points(x = cos(edata$a * tseq), y = cos(edata$b * tseq),
                      type = "l", col = col, painter = painter)
        tseq <- tail(tseq, 1)
        mosaiq.points(x = cos(edata$a * tseq), y = cos(edata$b * tseq),
                      type = "p", col = col, fill = col,
                      painter = painter)
        
    }
    
    render_axes  <- function(item, painter, exposed) {
        mosaiq.grid(item = item, painter = painter, exposed = exposed)
    }

    ## Create canvas for displaying tour
    scene <- qgraphicsScene()
    root <- qlayer(scene)

    axes <- qlayer(root, render_axes)
    points <- qlayer(root, render_data)
    qcacheMode(points) <- "none"
    qcacheMode(axes) <- "none"
    qlimits(points) <- qrect(c(-1, 1), c(-1, 1))
    qlimits(axes) <- qrect(c(-1, 1), c(-1, 1))
    
    view <- qplotView(scene = scene, opengl = opengl)
    qsetContextMenuPolicy(view, "actions")
    qsetDeleteOnClose(view, TRUE)

    ## start/stop animation
    timer <- qtimer(100, step)
    pauseAct <- qaction(desc = "Pause", shortcut = "Ctrl+P", checkable = TRUE)
    qconnect(pauseAct, signal = "triggered", 
             handler = function() {
                 if (pauseAct$checked) timer$stop() else timer$start()
             })
    qaddAction(view, pauseAct)

    ## random restart
    timer <- qtimer(30, step)
    restartAct <- qaction(desc = "Restart", shortcut = "Ctrl+R")
    qconnect(restartAct, signal = "triggered", 
             handler = restart)
    qaddAction(view, restartAct)

    ## stop timer on close
    qconnect(view, signal = "destroyed", 
             handler = function() {
                 timer$stop()
                 message("destroyed!")
             })

    timer$start()
    message("use context menu to control animation")
    view
}

## bug? closing window does not stop timer

## view <- pendulum(col = "red", bg = "orange", incr = 2)


