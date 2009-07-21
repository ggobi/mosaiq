
library(mosaiq)
library(maps)

state.map <- map("state", plot = FALSE, fill = TRUE)

state.info <-
    data.frame(name = state.name,
               long = state.center$x,
               lat = state.center$y,
               area = state.x77[, "Area"],
               population = 1000 * state.x77[, "Population"])
state.info$density <- with(state.info, population / area)

mosaiq.mapplot(x = density,
               y = name,
               data = state.info,
               map = state.map)




data(USCancerRates, package = "latticeExtra")

tmpdata <-
    with(USCancerRates,
         data.frame(rate = c(rate.male, rate.female),
                    gender = gl(2, length(rate.female), labels = c("Male", "Female")),
                    county = rownames(USCancerRates)))

mosaiq.mapplot(x = log(rate),
               y = county,
               margin = ~gender,
               data = tmpdata,
               colramp = terrain.colors,
               map = map("county", plot = FALSE, fill = TRUE,
                         projection = "mercator"))

mosaiq.mapplot(x = log(rate),
               y = county,
               margin = ~gender,
               data = tmpdata,
               colramp = terrain.colors,
               layout = c(1, 1),
               map = map("county", plot = FALSE, fill = TRUE))


