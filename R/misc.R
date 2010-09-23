
evaluate <- function(e, data, subset = TRUE, enclos = .GlobalEnv)
{
    ## for data.frame, maybe subset first
    eval(e, data, enclos)[subset]
}


compute.position <-
    function(column, row,
             what = c("panel", "strip.top", "strip.left",
                      "xaxis.bottom", "xaxis.top",
                      "yaxis.left", "yaxis.right", "legend"),
             space = "top")
{
    what <- match.arg(what)

    ## counting rows from top to bottom
    positions <- list(panel = c(3, 3),
                      strip.top = c(3, 2),
                      strip.left = c(2, 3),
                      yaxis.left = c(1, 3),
                      yaxis.right = c(4, 3),
                      xaxis.bottom = c(3, 4),
                      xaxis.top = c(3, 1))
    
    k <- 10L
    c(column = (column-1L) * k, row = (row-1L) * k) + positions[[what]]
}


getLimits <- function(r)
{
    e <- as.matrix(r)
    list(xlim = e[, 1], ylim = e[, 2])
}



