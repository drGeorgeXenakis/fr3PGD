isDormantSeason <-
function (current.month, leafgrow, leaffall) 
{
    is.dormant <- as.logical()
    if (any(c(leafgrow, leaffall) == 0)) {
        is.dormant <- FALSE
    }
    else if (leafgrow > leaffall) {
        if (current.month >= leaffall && current.month < leafgrow) {
            is.dormant <- TRUE
        }
        else is.dormant <- FALSE
    }
    else if (leafgrow < leaffall) {
        if (current.month < leafgrow || current.month >= leaffall) {
            is.dormant <- TRUE
        }
        else is.dormant <- FALSE
    }
    return(is.dormant)
}
