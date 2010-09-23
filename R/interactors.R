
## some useful interaction functions

mosaiq.zoom <- function(which.packet,
                        packets,
                        ...,
                        zoom.relation = "same", 
                        shared.env,
                        event)
{
    .GlobalEnv$last.event <- event
    ## print(event$delta)
    zoomx <- event$modifiers["shift"]
    zoomy <- event$modifiers["control"]
    if (!zoomx && !zoomy) ## neither
    {
        zoomx <- zoomy <- TRUE ## do both
    }
    f <- -0.005 * event$delta / 120
    do.packets <-
        if (zoom.relation == "same") seq_along(shared.env$limits)
        else which.packet
    for (i in do.packets)
    {
        if (zoomx)
            shared.env$limits[[i]]$xlim <-
                extendrange(shared.env$limits[[i]]$xlim, f = f)
        if (zoomy)
            shared.env$limits[[i]]$ylim <-
                extendrange(shared.env$limits[[i]]$ylim, f = f)
    }
    if (is(shared.env$widget, "QWidget"))
    {
        updateLayerLimits(shared.env)
        ## qupdate(shared.env$widget)
    }
}

registerLayerEnv <- function(env, layerenv)
{
    env$layer.envs[[ length(env$layer.envs) + 1L ]] <- layerenv
}


updateLayerLimits <- function(env)
{
    ## take all registered layer environments in env and update their
    ## limits

    limits <- env$limits
    lapply(env$layer.envs,
           function(x) {
               if (!is.null(x$panel.layer))
                   x$panel.layer$setLimits(qrect(limits[[x$i]]$xlim,
                                                 limits[[x$i]]$ylim))
               if (!is.null(x$axis.layer))
               {
                   cl <- x$axis.layer$limits()
                   x$axis.layer$setLimits(switch(x$side,
                                                 top = ,
                                                 bottom =
                                                 qrect(limits[[x$i]]$xlim,
                                                       cl[, 2]),
                                                 left = ,
                                                 right = qrect(cl[, 1],
                                                   limits[[x$i]]$ylim)))
               }
           })
}
