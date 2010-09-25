
## some useful interaction functions

mosaiq.zoom <-
    function(which.packet, packets,
             ...,
             zoom.relation = "same", 
             shared.env,
             factor, xonly = TRUE, yonly = TRUE)
{
    if (!xonly && !yonly) ## neither
        xonly <- yonly <- TRUE ## do both
    do.packets <-
        if (zoom.relation == "same")
            seq_along(shared.env$limits)
        else which.packet
    for (i in do.packets)
    {
        if (xonly)
            shared.env$limits[[i]]$xlim <-
                extendrange(shared.env$limits[[i]]$xlim, f = factor)
        if (yonly)
            shared.env$limits[[i]]$ylim <-
                extendrange(shared.env$limits[[i]]$ylim, f = factor)
    }
    if (is(shared.env$widget, "QWidget"))
    {
        updateLayerLimits(shared.env)
    }
}

mosaiq.pan <-
    function(which.packet, packets,
             ...,
             zoom.relation = "same", 
             shared.env,
             delta, xonly = TRUE, yonly = TRUE)
{
    if (!xonly && !yonly) ## neither
        xonly <- yonly <- TRUE ## do both
    do.packets <-
        if (zoom.relation == "same")
            seq_along(shared.env$limits)
        else which.packet
    for (i in do.packets)
    {
        if (xonly)
            shared.env$limits[[i]]$xlim <-
                shared.env$limits[[i]]$xlim - delta[1]
        if (yonly)
            shared.env$limits[[i]]$ylim <-
                shared.env$limits[[i]]$ylim - delta[2]
    }
    if (is(shared.env$widget, "QWidget"))
    {
        updateLayerLimits(shared.env)
    }
}


registerLayerEnv <- function(env, layerenv)
{
    env$layer.envs[[ length(env$layer.envs) + 1L ]] <- layerenv
}


updateLayerLimits <- function(env)
{
    ## take all registered layer environments in env and update their
    ## limits.  This uses special knowledge of layer names used in
    ## createComponents.R

    limits <- env$limits
    lapply(env$layer.envs,
           function(renv) {
               if (!is.null(renv$panel.layer))
                   renv$panel.layer$setLimits(qrect(limits[[renv$i]]$xlim,
                                                    limits[[renv$i]]$ylim))
               if (!is.null(renv$interaction.layer))
                   renv$interaction.layer$setLimits(qrect(limits[[renv$i]]$xlim,
                                                          limits[[renv$i]]$ylim))
               if (!is.null(renv$axis.layer))
               {
                   cl <- getLimits(renv$axis.layer$limits())
                   renv$axis.layer$setLimits(switch(renv$side,
                                                    top = ,
                                                    bottom =
                                                    qrect(limits[[renv$i]]$xlim,
                                                          cl$ylim),
                                                    left = ,
                                                    right = qrect(cl$xlim,
                                                                  limits[[renv$i]]$ylim)))
               }
           })
}
