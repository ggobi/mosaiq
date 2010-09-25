
## a basic demo of mouse event callbacks

library(qtutils)
library(qtbase)
library(qtpaint)
library(mosaiq)



tsplot <- function(y, x = NULL, type = "o", opengl = FALSE, ...)
{
    if (missing(x))
        x <-
            if (is(x, "ts")) times(y)
            else seq_along(y)
    render_data<- function(item, painter, exposed) {
        qantialias(painter) <- TRUE
        mosaiq.points(x = x, y = jitter(y, amount = 2),
                      type = type, painter = painter, ...)
    }
    handle_wheel <- function(layer, event)
    {
        delta <- event$delta() / 120
        ## Normal: zoom. +Shift: fast zoom. +Ctrl: pan
        shiftPressed <- (event$modifiers() == Qt$Qt$ShiftModifier)
        controlPressed <- (event$modifiers() == Qt$Qt$ControlModifier)
        lims <- getLimits(layer$limits())
        if (controlPressed) 
        {
            layer$setLimits(qrect(lims$xlim + 0.08 * delta * diff(lims$xlim),
                                  lims$ylim))
        }
        else 
        {
            f <- -(if (shiftPressed) 0.1 else 0.01) * delta
            layer$setLimits(qrect(extendrange(lims$xlim, f = f), lims$ylim))
        }
        ## qupdate(view)
    }
    handle_move <- function(layer, event) 
    {
        ## print(as.numeric(event$pos()))
        ## print(as.numeric(event$buttonDownPos(Qt$Qt$LeftButton)))
        ## delta <- as.numeric(event$pos()) - as.numeric(event$buttonDownPos(Qt$Qt$LeftButton))
        delta <- as.numeric(event$pos()) - as.numeric(event$lastPos())
        lims <- getLimits(layer$limits())
        layer$setLimits(qrect(lims$xlim - delta[1], lims$ylim)) # - delta[2]))
        ## qupdate(view)
    }
    handle_default <- function(s = "default")
    {
        function(layer, event)
        {
            cat(s, ": ")
            print(event)        
        }
        NULL
    }
    scene <- qscene()
    root <- qlayer(scene)
    points <-
        qlayer(root,
               paintFun = render_data,
               wheelFun = handle_wheel,
               mouseMoveFun = handle_move,

               ## keyPressFun        = handle_default("keyPressFun"),
               ## keyReleaseFun      = handle_default("keyReleaseFun"),
               ## mouseDoubleClickFun= handle_default("mouseDoubleClickFun"),
               ## mousePressFun      = handle_default("mousePressFun"),
               ## mouseReleaseFun    = handle_default("mouseReleaseFun"),
               ## ## hoverMoveFun       = handle_default("hoverMoveFun"),
               ## ## hoverEnterFun      = handle_default("hoverEnterFun"),
               ## ## hoverLeaveFun      = handle_default("hoverLeaveFun"),
               ## contextMenuFun     = handle_default("contextMenuFun"),
               ## dragEnterFun       = handle_default("dragEnterFun"),
               ## dragLeaveFun       = handle_default("dragLeaveFun"),
               ## dragMoveFun        = handle_default("dragMoveFun"),
               ## dropFun            = handle_default("dropFun"),
               ## ## focusInFun         = handle_default("focusInFun"),
               ## ## focusOutFun        = handle_default("focusOutFun"),

               limits = qrect(range(x), range(y)),
               cache = TRUE)
    ## points$setCacheMode(Qt$QtGraphicsItem$ItemCoordinateCache, qsize(200L, 200L))
    view <- qplotView(scene = scene, opengl = opengl)
    ## view$setContextMenuPolicy(Qt$Qt$ActionsContextMenu)
    view$setAttribute(Qt$Qt$WA_DeleteOnClose, TRUE)
    view
}

