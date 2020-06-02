CalculateModifiers <-
function (state, weather, site, parms, general.info) 
{
    Tmin <- parms[["Tmin"]]
    Tmax <- parms[["Tmax"]]
    Topt <- parms[["Topt"]]
    Tav <- weather[["Tmean"]]
    if (Tav < Tmin | Tav > Tmax) {
        fT <- 0
    }
    else {
        fT <- ((Tav - Tmin)/(Topt - Tmin)) * ((Tmax - Tav)/(Tmax - 
            Topt))^((Tmax - Topt)/(Topt - Tmin))
    }
    kF <- parms[["kF"]]
    FrostDays <- weather[["FrostDays"]]
    fF <- 1 - kF * (FrostDays/30)
    CO2 <- site[["CO2"]]
    fCalphax <- parms[["fCalphax"]]
    fCalpha <- fCalphax * CO2/(350 * (fCalphax - 1) + CO2)
    fN0 <- parms[["fN0"]]
    FR <- site[["FR"]]
    fN <- fN0 + (1 - fN0) * FR
    t <- state[["t"]]
    RelAge <- t/parms[["MaxAge"]]
    rAge <- parms[["rAge"]]
    nAge <- parms[["nAge"]]
    fAge <- 1/(1 + (RelAge/rAge)^nAge)
    CoeffCond <- parms[["CoeffCond"]]
    VPD <- weather[["VPD"]]
    fVPD <- exp(-CoeffCond * VPD)
    MaxASW <- site[["MaxASW"]]
    parms.soil <- general.info$parms.soil
    parms.sw.site <- parms.soil[which(parms.soil$soilclass == 
        site[["soilclass"]]), ]
    SWconst <- parms.sw.site[["SWconst"]]
    SWpower <- parms.sw.site[["SWpower"]]
    ASW <- state[["ASW"]]
    MoistRatio <- ASW/MaxASW
    fSW <- 1/(1 + ((1 - MoistRatio)/SWconst)^SWpower)
    PhysMod <- fAge * min(fVPD, fSW)
    state[c("fT", "fF", "fCalpha", "fN", "fAge", "fVPD", "fSW", 
        "PhysMod")] <- c(fT, fF, fCalpha, fN, fAge, fVPD, fSW, 
        PhysMod)
    return(state)
}
