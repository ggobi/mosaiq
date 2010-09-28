
## Animation of a pendulum swinging on two perpendicular axes with different frequencies

library(qtbase)
library(qtpaint)
library(mosaiq)


pendulum <-
    function(col = "red", bg = "black", incr = 5,
             n = 100, opengl = FALSE,
             verbose = getOption("verbose"))
{
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
        print("drawing grid")
        mosaiq.grid(item = item, painter = painter, exposed = exposed)
    }

    ## Create canvas
    scene <- qscene()
    root <- qlayer(scene, cache = TRUE)

    axes <- qlayer(root, render_axes, cache = TRUE, limits = qrect(c(-1, 1), c(-1, 1)))
    points <- qlayer(root, render_data, cache = TRUE, limits = qrect(c(-1, 1), c(-1, 1)))
    view <- qplotView(scene = scene, opengl = opengl)
    view$setContextMenuPolicy(Qt$Qt$ActionsContextMenu)
    view$setAttribute(Qt$Qt$WA_DeleteOnClose, TRUE)

    ## start/stop animation
    timer <- qtimer(100, step)
    pauseAct <- Qt$QAction("Pause", view)
    pauseAct$setCheckable(TRUE)
    pauseAct$setShortcut(Qt$QKeySequence("Ctrl+P"))
    qconnect(pauseAct, signal = "triggered", 
             handler = function() {
                 if (pauseAct$isChecked()) timer$stop() else timer$start()
             })
    view$addAction(pauseAct)

    ## random restart
    ## timer <- qtimer(30, step)
    restartAct <- Qt$QAction("Restart", view)
    restartAct$setShortcut(Qt$QKeySequence("Ctrl+R"))
    qconnect(restartAct, signal = "triggered", 
             handler = restart)
    view$addAction(restartAct)

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


