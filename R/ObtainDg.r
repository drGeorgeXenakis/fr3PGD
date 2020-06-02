ObtainDg <-
function (N, G) 
{
    if (N <= 0 | G <= 0) {
        dg <- 0
    }
    else {
        dg <- sqrt((G * 4)/(pi * N)) * 100
    }
    return(dg)
}
