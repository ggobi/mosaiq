
varlist <- function(...)
{
    mcall <- match.call()
    lapply(as.list(mcall)[-1], as.expression)
}



## helper function to convert formula to expression-list

margin.terms <- function(x, sep = as.symbol("+"))
{
    if (!inherits(x, "formula")) stop("'x' must be a formula")
    if (length(x) != 2) stop("'x' must have length 2")
    x <- x[[2]]
    ans <- list()
    while ((length(x) == 3) && (x[[1]] == sep))
    {
        ans[[ length(ans)+1L ]] <- x[[3]]
        x <- x[[2]]
    }
    ans[[ length(ans)+1L ]] <- x
    lapply(rev(ans), as.expression)
}

## 'x' is a list of values returned by substitute.  We want to create
## an expression list from it, but allow the first term to be a
## formula (only when specified as such, as otherwise substitute won't
## know (we don't want to evaluate anything here))

panel.terms <- function(x, tilde = as.symbol("~"))
{
    ans <- lapply(x, as.expression)
    x <- x[[1]] ## overrides 'x' and possibly 'y' if this is a formula (has a ~)
    formula.terms <- list()
    if ((length(x) > 1) && (x[[1]] == tilde))
    {
        if (length(x) == 2)
            formula.terms$x <- as.expression(x[[2]])
        else
        {
            formula.terms$x <- as.expression(x[[3]])
            formula.terms$y <- as.expression(x[[2]])
        }
    }
    ans[names(formula.terms)] <- formula.terms
    ans
}


## constructors

mosaiq.relation <- function(x = "same", y = "same", z = "same",
                     margin = NULL)
{
    list(x = x, y = y, z = z, margin = margin)
}

mosaiq.alternating <- function(x = TRUE, y = TRUE)
{
    if (is.logical(x)) x <- c(1, 2)
    if (is.logical(y)) y <- c(1, 2)
    list(x = x, y = y)
}





## the high-level function

mosaiq <-
    function(data = environment(), enclos = .GlobalEnv,
             margin.vars = list(),
             layout = NULL, skip = FALSE,

             ## perhaps useful for predefined packets; multiple data sources
             packets = compute.packets(margin.vars,
                                       data = data,
                                       enclos = enclos),
             
             panel.vars, panel, prepanel,

             ## specify how limits are combined
             relation = list(),

             ## specify how to alternate labels
             alternating = list(),

             xlim = NULL, ylim = NULL,
             xlab = NULL, ylab = NULL,
             main = NULL, sub = NULL,

             theme = mosaiq.theme(),
             legend = NULL,

             outer.padding = 10,
             ...)
{

    ## an environment to store shared information, notably axis
    ## limits.  All panel, axis functions, etc. have access to this.
    ## Panel functions (but not prepanel functions) also have access
    ## to a panel-specific environment 'panel.env'
    shared.env <- new.env(parent = emptyenv())
    shared.env$layer.envs <- list()

    relation <- do.call(mosaiq.relation, relation)
    alternating <- do.call(mosaiq.alternating, alternating)
    if (inherits(margin.vars, "formula"))
        margin.vars <- margin.terms(margin.vars)
    if (is.null(layout))
    {
        layout <-
            if (length(dim(packets)) == 1)
                c(0, dim(packets))
            else dim(packets)[1:2]
    }
    panel.layout <- compute.layout(layout, dim(packets), skip = skip)
    packet.limits <-
        compute.limits(packets = packets,
                       panel.vars = panel.vars,
                       prepanel = prepanel,
                       data = data,
                       enclos = enclos, ...)
    shared.env$limits <- combine.limits(packet.limits, relation = relation, xlim = xlim, ylim = ylim)

    ## create component widgets
    panelWidgets <-
        create.panels(layout = panel.layout, packets = packets, #limits = limits,
                      panel.vars = panel.vars, panel = panel, data = data, enclos = enclos,
                      shared.env = shared.env, ...)
    stripTopWidgets <- create.strip.top(layout = panel.layout, packets = packets, shared.env = shared.env, ...)
    xaxisBottomWidgets <- create.xaxis.bottom(layout = panel.layout, shared.env = shared.env, ...)
    xaxisTopWidgets <- create.xaxis.top(layout = panel.layout, shared.env = shared.env, ...)
    yaxisLeftWidgets <- create.yaxis.left(layout = panel.layout, shared.env = shared.env, ...)
    yaxisRightWidgets <- create.yaxis.right(layout = panel.layout, shared.env = shared.env, ...)

    ## create a 'figure region' widget for each page
    pageWidgets <-
        lapply(seq_len(dim(panel.layout)[3]), create.page, 
               panel.widgets = panelWidgets, strip.top.widgets = stripTopWidgets,
               xaxis.bottom.widgets = xaxisBottomWidgets, xaxis.top.widgets = xaxisTopWidgets,
               yaxis.left.widgets = yaxisLeftWidgets, yaxis.right.widgets = yaxisRightWidgets,
               relation = relation, alternating = alternating)

    ## create 'full' widget with page and associated labels.
    ans <- Qt$QWidget()
    lans <- Qt$QGridLayout(NULL)
    ##lans$margin <- 0
    ##lans$spacing <- 0
    ans$setLayout(lans)
    lans$margin <- outer.padding

    ## ans <- qvBasicWidget(canvas = theme$background$col, margin = ) # imagine a 9x9 layout

    if (length(pageWidgets) > 1) 
    {
        tabw <- Qt$QTabWidget()
        for (i in seq_len(length(pageWidgets)))
        {
            if (is(pageWidgets[[i]], "QWidget"))
                tabw$addTab(pageWidgets[[i]], label = sprintf("Page %g", i))
        }
        lans$addWidget(tabw, 5, 5)
    }
    else if (is(pageWidgets[[1]], "QWidget"))
        lans$addWidget(pageWidgets[[1]], 5, 5)

    if (!is.null(xlab))
        lans$addWidget(labelWidget(xlab, horizontal = TRUE), 6, 5)
    if (!is.null(ylab))
        lans$addWidget(labelWidget(ylab, horizontal = FALSE), 5, 4)
    if (!is.null(main)) 
        lans$addWidget(labelWidget(main, horizontal = TRUE), 1, 1, 1, 9)
    if (!is.null(sub)) 
        lans$qaddWidget(labelWidget(sub, horizontal = TRUE), 9, 1, 1, 9)
##     if (!is.null(legend)) 
##     {
##         for (space in names(legend))
##         {
##             switch(space,
##                    top = { ans[3, 5] <- legend[[space]] },
##                    bottom = { ans[7, 5] <- legend[[space]] },
##                    left = { ans[5, 3] <- legend[[space]] },
##                    left = { ans[5, 7] <- legend[[space]] })
##         }
##     }
    shared.env$widget <- ans
    class(ans) <- c("mosaiq", class(ans))
    attr(ans, "shared.env") <- shared.env
    ans
}

.MosaicEnv <- new.env(parent = emptyenv())

print.mosaiq <- function(x, row = 1, col = 1, ...)
{
    widget.index <- sprintf("item:%g,%g", row, col)
    old.widget <- .MosaicEnv[[widget.index]]
    .MosaicEnv[[widget.index]] <- x
    if (is.null(.MosaicEnv$toplevel)) 
    {
        .MosaicEnv$toplevel <- Qt$QWidget()
        .MosaicEnv$toplayout <- Qt$QGridLayout()
        .MosaicEnv$toplevel$setLayout(.MosaicEnv$toplayout)
        .MosaicEnv$toplevel$styleSheet <- " QWidget { background: white }"
        .MosaicEnv$toplevel$resize(800, 600)
        .MosaicEnv$toplevel$show()
        ## Actions
        
        ## Activate context menu with actions
        .MosaicEnv$toplevel$setContextMenuPolicy(Qt$Qt$ActionsContextMenu)

        ## Helper function to print
        printHandler <- function()
        {
            printer <- Qt$QPrinter(Qt$QPrinter$HighResolution)
            rpaper <- getOption("papersize")
            if (is.null(rpaper)) rpaper <- "A4"
            qtpaper <- names(Qt$QPrinter)
            usepaper <- qtpaper[ match(tolower(rpaper), tolower(qtpaper)) ]
            if (is.na(usepaper)) usepaper <- "A4"
            printer$setPageSize(Qt$QPrinter[[usepaper]])
            pd <- Qt$QPrintDialog(printer)
            acceptPrint <- pd$exec()
            if (acceptPrint)
            {
                painter <- Qt$QPainter()
                painter$begin(printer)
                .MosaicEnv$toplevel$render(painter)
                painter$end()
            }
        }
        
        ## Actions to print
        printAct <- Qt$QAction("Print", .MosaicEnv$toplevel)
        printAct$setShortcut(Qt$QKeySequence("Ctrl+P"))
        qconnect(printAct,
                 signal = "triggered",
                 handler = function(checked) {
                     printHandler()
                 })
        .MosaicEnv$toplevel$addAction(printAct)
    }
    if (is(x, "QWidget"))
        .MosaicEnv$toplayout$addWidget(x, row, col)
    .MosaicEnv$toplevel$update()
    export.mosaiq()
    if (!is.null(old.widget)) old.widget$close()
    invisible(x)
}


export.mosaiq <- function(file = .MosaicEnv$file)
{
    if (!missing(file)) {
        .MosaicEnv$file <- file
        .MosaicEnv$index <- 1L
    }
    else if (!is.null(file))
    {
        Sys.sleep(0.2)
        outfile <- sprintf(file, .MosaicEnv$index)
        .MosaicEnv$index <- .MosaicEnv$index + 1L
        qexport(.MosaicEnv$toplevel, file = outfile)
    }
    ## gc()
}





## high-level convenience functions

mosaiq.xyplot <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("points", panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.xyplot, prepanel = prepanel.mosaiq.xyplot,
             ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}

mosaiq.densityplot <- 
    function(x, data, enclos, weights = NULL, groups = NULL, 
             legend = auto.legend("lines", panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.densityplot, prepanel = prepanel.mosaiq.densityplot, ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), weights = substitute(weights), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}

mosaiq.qqmath <- 
    function(x, data, enclos, groups = NULL, 
             legend = auto.legend("points", panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.qqmath, prepanel = prepanel.mosaiq.qqmath, ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), weights = substitute(weights), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}

mosaiq.histogram <- 
    function(x, data, enclos, 
             legend = auto.legend("rectangles", panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.histogram, prepanel = prepanel.mosaiq.histogram, ...)
{
    panel.vars <- panel.terms(list(x = substitute(x)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}

mosaiq.dotplot <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("points", panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.dotplot, prepanel = prepanel.mosaiq.dotplot,
             ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}


mosaiq.bwplot <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend(c("points", "lines"), panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.bwplot, prepanel = prepanel.mosaiq.bwplot, ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}

mosaiq.barchart <- 
    function(x, y = NULL, data, enclos, groups = NULL, 
             legend = auto.legend("rectangles", panel.vars, data = data, enclos = enclos, legend.args = legend.args),
             legend.args = list(),
             panel = panel.mosaiq.barchart, prepanel = prepanel.mosaiq.barchart, ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y), groups = substitute(groups)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, legend = legend, ...)
}

## maps


mosaiq.mapplot <- 
    function(x, y = NULL, data, enclos, 
             panel = panel.mosaiq.mapplot, prepanel = prepanel.mosaiq.mapplot, ...)
{
    panel.vars <- panel.terms(list(x = substitute(x), y = substitute(y)))
    if (missing(data)) data <- parent.frame()
    if (missing(enclos)) enclos <- parent.frame()
    mosaiq(data = data, enclos = enclos, panel.vars = panel.vars, panel = panel, prepanel = prepanel, ...)
}
