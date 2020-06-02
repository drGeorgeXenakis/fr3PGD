EstimateAPAR <-
function (state, weather, parms, general.info) 
{
    molPAR_MJ <- parms[["molPAR_MJ"]]
    fullCanAge <- parms[["fullCanAge"]]
    k <- parms[["k"]]
    RAD.day <- weather[["SolarRad"]]
    year <- weather[["Year"]]
    month <- weather[["Month"]]
    t <- state[["t"]]
    LAI <- state[["LAI"]]
    RAD <- RAD.day * general.info$daysinmonth[month]
    phi.p <- molPAR_MJ * RAD
    CanCover <- if (fullCanAge > 0 & t < fullCanAge) 
        (t + 0.01)/fullCanAge
    else 1
    lightIntcptn <- 1 - exp(-k * LAI/CanCover)
    phi.pa <- phi.p * lightIntcptn * CanCover
    state[c("Year", "Month", "RAD.day", "RAD", "CanCover", "lightIntcptn", 
        "phi.p", "phi.pa")] <- c(year, month, RAD.day, RAD, CanCover, 
        lightIntcptn, phi.p, phi.pa)
    return(state)
}
