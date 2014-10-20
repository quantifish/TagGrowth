summarise.simulations <- function(directory = ".")
{
    directory <- "../simulation/sims"
    pdHess <- rep(NA, 100)
    for (Isim in 1:47)
    {
        fname <- paste(directory, "/sim", Isim, ".RData", sep = "")
        load(fname)
        if (!is.null(sim$Report$pdHess)) pdHess[Isim] <- sim$Report$pdHess
    }
    print("pdHess?")
    print(pdHess)
}
