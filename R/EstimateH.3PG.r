EstimateH.3PG <-
function (N, Wa) 
{
    k <- 4.8965 - 0.3972 * N/1000
    if (N >= 12000) {
        k <- 4.8965 - 0.3972 * 12000/1000
    }
    a <- 0.3482 + 0.0014 * N/1000
    hdom <- k * Wa^a
    return(hdom)
}
