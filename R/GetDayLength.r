GetDayLength <-
function (Lat, month, dayofyear = c(Jan = 16, Feb = 44, Mar = 75, 
    Apr = 105, May = 136, Jun = 166, Jul = 197, Aug = 228, Sep = 258, 
    Oct = 289, Nov = 319, Dec = 350)) 
{
    SLAt <- sin(pi * Lat/180)
    cLat <- cos(pi * Lat/180)
    sinDec <- 0.4 * sin(0.0172 * (dayofyear[month] - 80))
    cosH0 <- -sinDec * SLAt/(cLat * sqrt(1 - sinDec^2))
    if (cosH0 > 1) {
        fDayLength <- 0
    }
    else if (cosH0 < -1) {
        fDayLength <- 1
    }
    else {
        fDayLength <- acos(cosH0)/pi
    }
    daylength <- fDayLength * 86400
    return(daylength)
}
