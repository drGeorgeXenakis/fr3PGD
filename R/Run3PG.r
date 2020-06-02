Run3PG <-
function (stand.init, weather, site, parms, general.info = parms.general, 
    presc = presc, cod.pred = "3PG", cod.clim = "Month") 
{
    state.init <- InitializeState(stand = stand.init, site = site, 
        parms = parms)
    weather <- PredictWeatherVariables(weather = weather)
    proj.results <- list()
    state <- PredictVariablesInterest.3PG(state = state.init, 
        parms = parms, cod.pred = cod.pred, month = state.init[["initial.month"]])
    t.proj <- 0
    proj.results[[1]] <- c(t.proj = t.proj, state)
    if (!missing(presc)) {
        fma.c <- presc[presc$cycle == state[["cycle"]], ]
        t.nsprouts <- unique(fma.c[, "t.nsprouts"])
        fst.row.fma.app <- min(c(which(fma.c$t > state[["t"]]), 
            nrow(fma.c)))
        fma.app <- fma.c[fst.row.fma.app:nrow(fma.c), ]
        fma.app$t[which(fma.app$t < state[["t"]])] <- state[["t"]]
    }
    if (cod.clim == "Average") {
        N = stand.init[["nyears"]] * 12
    }
    else if (cod.clim == "Month") {
        N = nrow(weather)
    }
    j = 1
    weather.i <- as.numeric()
    for (i in 1:N) {
        if (cod.clim == "Average") {
            if (j > 12) 
                j = 1
            weather.i <- weather[j, ]
        }
        else if (cod.clim == "Month") {
            weather.i <- weather[i, ]
        }
        state.apar <- EstimateAPAR(state = state, weather = weather.i, 
            parms = parms, general.info = general.info)
        state.mods <- CalculateModifiers(state = state.apar, 
            weather = weather.i, site = site, parms = parms, 
            general.info = general.info)
        state.npp <- EstimateNPP(state = state.mods, parms = parms)
        state.asw <- UpdateASW(state = state.npp, weather = weather.i, 
            site = site, parms = parms, general.info = general.info)
        state.walloc <- AllocateBiomass(state = state.asw, site = site, 
            parms = parms, weather = weather.i)
        state.mort <- EstimateMortality(state = state.walloc, 
            parms = parms)
        state.mort[["t"]] <- state.mort[["t"]] + 1/12
        state.end <- PredictVariablesInterest.3PG(state = state.mort, 
            parms = parms, cod.pred = cod.pred, month = weather.i[["Month"]])
        if (!missing(presc) && nrow(fma.app) > 0 && ((abs(fma.app$t[1] - 
            state.end[["t"]]) < 1/24) | (fma.app$t[1] < state.end[["t"]]))) {
            state.end <- DoThinning(state = state.end, parms = parms, 
                fma = fma.app, presc = presc)
            fma.app <- fma.app[-1, ]
            state.end <- PredictVariablesInterest.3PG(state = state.end, 
                parms = parms, cod.pred = cod.pred, month = weather.i[["Month"]])
        }
        if (!missing(presc) && state.end[["N"]] == 0) {
            if (state.end[["cycle"]] > max(presc$cycle)) {
                t.proj <- t.proj + 1/12
                proj.results[[i + 1]] <- c(t.proj = t.proj, state.end)
                break
            }
            fma.c <- presc[which(presc$cycle == state.end[["cycle"]]), 
                ]
            t.nsprouts <- unique(fma.c[, "t.nsprouts"])
            fma.app <- fma.c
            it.newsprouts <- 1
            if (state.end[["rotation"]] == 1) {
                state.end <- CreateNewPlantation(state = state.end, 
                  parms = parms, fma = fma.app)
                state.end <- PredictVariablesInterest.3PG(state = state.end, 
                  parms = parms, cod.pred = cod.pred, month = weather.i[["Month"]])
            }
        }
        if (!missing(presc) && state.end[["rotation"]] > 1 && 
            state.end[["t"]] < t.nsprouts) {
            state.end <- CreateNewSprouts(state = state.end, 
                parms = parms, newsprouts = it.newsprouts)
            state.end <- PredictVariablesInterest.3PG(state = state.end, 
                parms = parms, cod.pred = cod.pred, month = weather.i[["Month"]])
            it.newsprouts <- it.newsprouts + 1
        }
        if (!missing(presc) && state.end[["rotation"]] > 1 && 
            state.end[["t"]] > (t.nsprouts - 1/24) && state.end[["rm.sprouts"]] == 
            0) {
            state.end <- RemoveSprouts(state = state.end, parms = parms, 
                fma = fma.app)
        }
        t.proj <- t.proj + 1/12
        proj.results[[i + 1]] <- c(t.proj = t.proj, state.end)
        state <- state.end
        j = j + 1
    }
    proj.df <- data.frame(do.call(rbind, proj.results))
    return(proj.df)
}
