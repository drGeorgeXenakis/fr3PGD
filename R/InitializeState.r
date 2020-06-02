InitializeState <-
function (stand, site, parms) 
{
    names.state <- c("Year", "Month", "t", "hdom", "N", "G", 
        "dg", "Vu", "LAI", "Wl", "Wr", "Wsbr", "rotation", "cycle", 
        "Nharv", "rm.sprouts", "Ww", "Wb", "Wbr", "Wa", "W", 
        "wsbrg", "ASW", "Wlitt", "RAD.day", "RAD", "CanCover", 
        "lightIntcptn", "phi.p", "phi.pa", "fT", "fF", "fCalpha", 
        "fN", "fAge", "fVPD", "fSW", "PhysMod", "alphaC", "GPP", 
        "NPP", "RainIntcptn", "netRad", "fCg", "CanCond", "Etransp", 
        "CanTransp", "Transp", "EvapTransp", "excessSW", "scaleSW", 
        "pR", "pFS", "pS", "pF", "difWl", "difWr", "difWsbr", 
        "Littfall", "difLitter", "difRoots", "TotalLitter", "Ndead", 
        "Wdl", "Wds", "Wdr")
    .length.state.vec <- length(names.state)
    state.vector <- rep(NA, .length.state.vec)
    names(state.vector) <- names.state
    state.init <- state.vector
    state.init[names(stand)] <- stand
    state.init["ASW"] <- site[["ASW"]]
    if (state.init[["t"]] == 0) {
        state.init <- CreateNewPlantation(state = state.init, 
            parms = parms)
    }
    return(state.init)
}
