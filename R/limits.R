
compute.limits <-
    function(packets, panel.vars, prepanel,
             data = .GlobalEnv, enclos = .GlobalEnv, ...)
{
    ans <- packets
    for (i in seq_len(length(ans)))
    {
        ans[[i]] <-
            prepanel(panel.vars = panel.vars,
                     which.packet = i,
                     packets = packets,
                     data = data,
                     enclos = enclos, ...)
    }
    ans
}


combine.limits <-
    function(limits,
             relation = relation(),
             tick.number = 5,
             xlim = NULL, ylim = NULL)
{
    ## needs thought. 'margin' is intended to control combination
    ## across margins; e.g., may want a common scale for rows only.

    ## For now, just combine xlim and ylim.

    ## for factor annotation, [xy]at and [xy]labels should also be
    ## combined.

    xat <- unlist(lapply(limits, "[[", "xat"))
    if (is.null(xat))
    {
        if (is.null(xlim)) xlim <- extendrange(unlist(lapply(limits, "[[", "xlim")))
        xat <- pretty(xlim, tick.number)
        xlabels <- format(xat)
    }
    else # factor? (not necessarily; e.g., date-time, explicit specification)
    {
        if (is.null(xlim)) xlim <- range(unlist(lapply(limits, "[[", "xlim"))) + c(-0.6, 0.6)
        xlabels <- unlist(lapply(limits, "[[", "xlabels"))
        xid <- !duplicated(xat)
        xat <- xat[xid]
        xlabels <- xlabels[xid] ## should check that xlabels[!xid] are all duplicated
    }
    yat <- unlist(lapply(limits, "[[", "yat"))
    if (is.null(yat))
    {
        if (is.null(ylim)) ylim <- extendrange(unlist(lapply(limits, "[[", "ylim")))
        yat <- pretty(ylim, tick.number)
        ylabels <- format(yat)
    }
    else 
    {
        if (is.null(ylim)) ylim <- range(unlist(lapply(limits, "[[", "ylim"))) + c(-0.6, 0.6)
        ylabels <- unlist(lapply(limits, "[[", "ylabels"))
        yid <- !duplicated(yat)
        yat <- yat[yid]
        ylabels <- ylabels[yid]
    }
    for (i in seq_len(length(limits)))
    {
        if (relation$x == "same") 
        {
            limits[[i]][["xlim"]] <- xlim
            ## if (!is.null(xat))
            ## {
            limits[[i]][["xat"]] <- xat
            limits[[i]][["xlabels"]] <- xlabels
            ## }
        }
        else 
        {
            if (is.null(limits[[i]][["xat"]]))
            {
                limits[[i]][["xlim"]] <- extendrange(limits[[i]][["xlim"]])
                limits[[i]][["xat"]] <- pretty(limits[[i]][["xlim"]], tick.number)
                limits[[i]][["xlabels"]] <- format(limits[[i]][["xat"]])
            }
            else 
            {
                limits[[i]][["xlim"]] <- range(limits[[i]][["xlim"]]) + c(-0.6, 0.6)
            }
        }
    }
    for (i in seq_len(length(limits)))
    {
        if (relation$y == "same") 
        {
            limits[[i]][["ylim"]] <- ylim
            ## if (!is.null(yat))
            ## {
            limits[[i]][["yat"]] <- yat
            limits[[i]][["ylabels"]] <- ylabels
            ## }
        }
        else 
        {
            if (is.null(limits[[i]][["yat"]]))
            {
                limits[[i]][["ylim"]] <- extendrange(limits[[i]][["ylim"]])
                limits[[i]][["yat"]] <- pretty(limits[[i]][["ylim"]], tick.number)
                limits[[i]][["ylabels"]] <- format(limits[[i]][["yat"]])
            }
            else 
            {
                limits[[i]][["ylim"]] <- range(limits[[i]][["ylim"]]) + c(-0.6, 0.6)
            }
        }
    }
    limits
}

