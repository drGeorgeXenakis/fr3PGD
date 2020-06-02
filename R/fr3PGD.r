fr3PGD <-
function (weather, presc, t = 0, N = 4430, Wl = 0.01, WlDormant = 0, 
    Wr = 0.05, Wsbr = 0.1, Wlitt = 0, rotation = 1, cycle = 1, 
    rm.sprouts = F, nyears = 35, initial.month = 1, latitude = 39, 
    FR = 0.1, soilclass = 0, ASW = 300, MaxASW = 431.245961281345, 
    MinASW = 211.143388917576, CO2 = 400, pFS2 = 0.033, pFS20 = 0.052, 
    pfsPower = -0.522878745280338, pfsConst = 0.43104582317421, 
    aS = 0.322, nS = 2.2, pRx = 0.331, pRn = 0.048, Tmin = -6.834, 
    Topt = 14.64, Tmax = 33.96, kF = 1, SWconst0 = 0.7, SWpower0 = 9, 
    m0 = 0.018, fN0 = 0.296, MaxAge = 265.6, nAge = 3.545, rAge = 0.796, 
    gammaFx = 0.019, gammaF0 = 0.002, tgammaF = 80.79, Rttover = 0, 
    MaxCond = 0.028, LAIgcx = 3.092, BLcond = 0.013, wSx1000 = 394.2, 
    thinPower = 1.5, mF = 0.207, mR = 0.127, mS = 0.128, SLA0 = 20.1, 
    SLA1 = 22.88, tSLA = 16.33, k = 0.504, fullCanAge = 22.32, 
    MaxIntcptn = 0.382, LAImaxIntcptn = 6.302, alpha = 0.047, 
    Y = 0.473, poolFractn = 0, e20 = 2.2, rhoAir = 1.2, lambda = 2460000, 
    VPDconv = 0.000622, fracBB0 = 0.93, fracBB1 = 0.087, tBB = 32.48, 
    rhoMin = 0.342, rhoMax = 0.576, tRho = 128.1, Qa = -90, Qb = 0.8, 
    gDM_mol = 24, molPAR_MJ = 2.3, CoeffCond = 0.007, fCalpha700 = 1.433, 
    fCg700 = 0.451, fCalphax = 2.33333333333333, fCg0 = 1.75, 
    MinCond = 0.015, Wl.s = 0.526, Wsbr.s = 0.2035, Wr.s = 0.22775, 
    pWl.sprouts = 0.5, pWsbr.sprouts = 0.9, cod.pred = "3PG", 
    cod.clim = "Month", leaf.grow = 0, leaf.fall = 0) 
{
    parms <- c(pFS2, pFS20, pfsPower, pfsConst, aS, nS, pRx, 
        pRn, Tmin, Topt, Tmax, kF, SWconst0, SWpower0, m0, fN0, 
        MaxAge, nAge, rAge, gammaFx, gammaF0, tgammaF, Rttover, 
        MaxCond, LAIgcx, BLcond, wSx1000, thinPower, mF, mR, 
        mS, SLA0, SLA1, tSLA, k, fullCanAge, MaxIntcptn, LAImaxIntcptn, 
        alpha, Y, poolFractn, e20, rhoAir, lambda, VPDconv, fracBB0, 
        fracBB1, tBB, rhoMin, rhoMax, tRho, Qa, Qb, gDM_mol, 
        molPAR_MJ, CoeffCond, fCalpha700, fCg700, fCalphax, fCg0, 
        MinCond, Wl.s, Wsbr.s, Wr.s, pWl.sprouts, pWsbr.sprouts, 
        leaf.grow, leaf.fall)
    names(parms) <- c("pFS2", "pFS20", "pfsPower", "pfsConst", 
        "aS", "nS", "pRx", "pRn", "Tmin", "Topt", "Tmax", "kF", 
        "SWconst0", "SWpower0", "m0", "fN0", "MaxAge", "nAge", 
        "rAge", "gammaFx", "gammaF0", "tgammaF", "Rttover", "MaxCond", 
        "LAIgcx", "BLcond", "wSx1000", "thinPower", "mF", "mR", 
        "mS", "SLA0", "SLA1", "tSLA", "k", "fullCanAge", "MaxIntcptn", 
        "LAImaxIntcptn", "alpha", "Y", "poolFractn", "e20", "rhoAir", 
        "lambda", "VPDconv", "fracBB0", "fracBB1", "tBB", "rhoMin", 
        "rhoMax", "tRho", "Qa", "Qb", "gDM_mol", "molPAR_MJ", 
        "CoeffCond", "fCalpha700", "fCg700", "fCalphax", "fCg0", 
        "MinCond", "Wl.s", "Wsbr.s", "Wr.s", "pWl.sprouts", "pWsbr.sprouts", 
        "leaf.grow", "leaf.fall")
    vars.ini <- c(t, N, Wl, WlDormant, Wr, Wsbr, Wlitt, rotation, 
        cycle, rm.sprouts, nyears, initial.month)
    names(vars.ini) <- c("t", "N", "Wl", "WlDormant", "Wr", "Wsbr", 
        "Wlitt", "rotation", "cycle", "rm.sprouts", "nyears", 
        "initial.month")
    site.info <- c(latitude, FR, soilclass, ASW, MaxASW, MinASW, 
        CO2)
    names(site.info) <- c("latitude", "FR", "soilclass", "ASW", 
        "MaxASW", "MinASW", "CO2")
    parms.general <- list(daysinmonth = c(Jan = 31, Feb = 28, 
        Mar = 31, Apr = 30, May = 31, Jun = 30, Jul = 31, Aug = 31, 
        Sep = 30, Oct = 31, Nov = 30, Dec = 31), parms.soil = data.frame(soilclass.name = c("Sandy", 
        "Sandy loam", "Clay loam", "Clay", "Non standard", "No effect of ASW"), 
        soilclass = c(1, 2, 3, 4, NA, 0), SWconst = c(0.7, 0.6, 
            0.5, 0.4, parms[["SWconst0"]], 1), SWpower = c(9, 
            7, 5, 3, parms[["SWpower0"]], 1)))
    proj <- Run3PG(stand.init = vars.ini, weather = weather, 
        site = site.info, general.info = parms.general, presc = presc, 
        parms = parms, cod.pred = cod.pred, cod.clim = cod.clim)
    return(as.data.frame(proj))
}
