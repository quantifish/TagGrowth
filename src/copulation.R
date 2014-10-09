require(copula)
load("../data/ATR_mod.RData")
source("time-step.R")
source("normalise.R")

ATR_mod <- time.step(ATR_mod, units = "weeks")
gumbel.cop <- gumbelCopula(3, dim = 2)
set.seed(7) # for reproducibility
x1 <- cbind(ATR_mod$Age1, ATR_mod$Age2)
u1 <- x1
u1[,1] <- normalise(x1[,1], a = 0.01, b = 0.99)               ## pseudo-observations
u1[,2] <- normalise(x1[,2], a = 0.01, b = 0.99)               ## pseudo-observations
fg <- fitCopula(gumbel.cop, u1, method = "ml")
fg
gumbel.cop <- gumbelCopula(fg@estimate, dim = 2)
u2 <- rCopula(315, gumbel.cop)## "true" observations
x2 <- x1
x2[,1] <- normalise(u2[,1], a = min(x1[,1]), b = max(x1[,1]))               ## pseudo-observations
x2[,2] <- normalise(u2[,2], a = min(x1[,2]), b = max(x1[,2]))               ## pseudo-observations
par(mfrow = c(2,2))
plot(x1[,1], x1[,2], xlab = "Age1", ylab = "Age2", main = "Original data")
plot(u1[,1], u1[,2], xlab = "Age1", ylab = "Age2", main = "Original data on unit space")
plot(u2[,1], u2[,2], xlab = "Age1", ylab = "Age2", main = "Simulated data on unit space")
plot(x2[,1], x2[,2], xlab = "Age1", ylab = "Age2", main = "Simulated data")


par(mfrow = c(2,2))
plot(density(ATR_mod$Age1), type = "l")
lines(density(x2[,1]), col = 2)
        #for (j in 1:100) lines(density(Age1[j,]), col = 2)
        #lines(density(ATR_mod$Age1))
plot(density(ATR_mod$Age2), type = "l")
lines(density(x2[,2]), col = 2)
        #for (j in 1:100) lines(density(Age2[j,]), col = 2)
        #lines(density(ATR_mod$Age2))
plot(density(ATR_mod$iLiberty), type = "l")
lines(density(x2[,2] - x2[,1]), col = 2)
        #for (j in 1:100) lines(density(Liberty[j,]), col = 2)
        #lines(density(ATR_mod$Liberty))
        #plot(ATR_mod$Age1, ATR_mod$Age2)


# DID NOT WORK REALLY - TRY AGAIN BUT USING Age1 AND Liberty AS THESE TWO THING
# ARE OBSERVED
gumbel.cop <- gumbelCopula(3, dim = 2)
x1 <- cbind(ATR_mod$Age2, ATR_mod$Liberty)
u1 <- x1
u1[,1] <- normalise(x1[,1], a = 0.1, b = 0.9)               ## pseudo-observations
u1[,2] <- normalise(x1[,2], a = 0.1, b = 0.9)               ## pseudo-observations
fg <- fitCopula(gumbel.cop, u1, method = "ml")
fg
gumbel.cop <- gumbelCopula(fg@estimate, dim = 2)
u2 <- rCopula(315, gumbel.cop)## "true" observations
x2 <- x1
x2[,1] <- normalise(u2[,1], a = min(x1[,1]), b = max(x1[,1]))               ## pseudo-observations
x2[,2] <- normalise(u2[,2], a = min(x1[,2]), b = max(x1[,2]))               ## pseudo-observations
par(mfrow = c(2,2))
plot(x1[,1], x1[,2], xlab = "Age2", ylab = "Liberty", main = "Original data")
plot(u1[,1], u1[,2], xlab = "Age1", ylab = "Age2", main = "Original data on unit space")
plot(u2[,1], u2[,2], xlab = "Age1", ylab = "Age2", main = "Simulated data on unit space")
plot(x2[,1], x2[,2], xlab = "Age1", ylab = "Age2", main = "Simulated data")





Nindiv <- 315

chooser <- function()
{
    ss <- TRUE
    while (ss)
    {
        abit <- function() 0
        #abit <- function() rnorm(1, 0, 100)
        #abit <- function() runif(1, -200, 200)
            Age1 <- sample(ATR_mod$Age1 + abit(), size = 1) #+ runif(1, 0, 200)#rnorm(1, 0, 100)
            Age2 <- sample(ATR_mod$Age2 + abit(), size = 1) #+ runif(1, 0, 200)#rnorm(1, 0, 100)
            Liberty <- sample(ATR_mod$Liberty + abit(), size = 1) #+ runif(1, 0, 100)#rnorm(1, 0, 50)
            rnd <- sample(1:3, size = 1)
            if (rnd == 1) Age1 <- Age2 - Liberty
            if (rnd == 2) Age2 <- Age1 + Liberty
            if (rnd == 3) Liberty <- Age2 - Age1
        #Age2 <- Age1 + Liberty
            if (Liberty > 0 & Age1 > 0 & Age2 > Age1) ss <- FALSE
    }
        #cat(Age1, "|", Age2, "|", Liberty, "\n")
        if (Liberty < 0) stop("Error: a negative time at liberty was simulated")
        if (Age1 < 0 | Age2 < 0) stop("Error: a fish younger than zero was simulated")
        return(c(Age1, Age2, Liberty))
}

        Age1 <- matrix(NA, 100, Nindiv)
        Age2 <- matrix(NA, 100, Nindiv)
        Liberty <- matrix(NA, 100, Nindiv)
        for (j in 1:100)
        {
            for (i in 1:Nindiv)
            {
                tmp <- chooser()
                Age1[j,i] <- tmp[1]
                Age2[j,i] <- tmp[2]
                Liberty[j,i] <- tmp[3]
            }
        }

png("Sim-age-liberty-2.png")
        par(mfrow = c(2,2))
        plot(density(ATR_mod$Age1), type = "l")
        for (j in 1:100) lines(density(Age1[j,]), col = 2)
        lines(density(ATR_mod$Age1))
        plot(density(ATR_mod$Age2), type = "l")
        for (j in 1:100) lines(density(Age2[j,]), col = 2)
        lines(density(ATR_mod$Age2))
        plot(density(ATR_mod$Liberty), type = "l")
        for (j in 1:100) lines(density(Liberty[j,]), col = 2)
        lines(density(ATR_mod$Liberty))
        plot(ATR_mod$Age1, ATR_mod$Age2)
        points(Age1, Age2, col = 2, pch = 3)
        points(ATR_mod$Age1, ATR_mod$Age2)
dev.off()
