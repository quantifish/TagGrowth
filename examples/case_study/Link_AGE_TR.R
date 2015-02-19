# Clean up R
rm(list=ls())

# Load data
load("../../data/AGE.RData")
load("../../data/TR.RData")

# Identify which aged fish had a tag
a <- AGE[AGE$tagged == TRUE,]
cat("Number of aged fish with a tag:", dim(a)[1], "\n")

# Identify the tag numbers by extracting from comments field
tags <- list()
for (j in 1:length(a$tag.ref))
{
    tags[[j]] <- as.numeric(na.omit(as.numeric(strsplit(a$tag.ref, "[^0-9]+")[[j]])))
}

# Identify the potential maximum number of tags referred to in comments (cc) and
# the number of unique tags referred to in comments
cc <- 1
utag <- NULL
for (j in 1:length(a$tag.ref))
{
    xx <- length(tags[[j]])
    cc <- max(cc, xx)
    utag <- c(utag, tags[[j]])
}
print(cc)
length(unique(utag[utag > 1000]))

# Create an index to link the AGE and TR objects for each tag in the comments
# field to each tag in the TR object (i.e. no1, no2, no3)
out <- array(NA, dim = c(length(tags), 3, cc))
for (j in 1:cc)
{
    for (i in 1:length(tags))
    {
        if (is.na(tags[[i]][j]))
        {
            x1 <- NA; x2 <- NA; x3 <- NA
        } else {
            x1 <- which(TR$no1 %in% tags[[i]][j])
            x2 <- which(TR$no2 %in% tags[[i]][j])
            x3 <- which(TR$no3 %in% tags[[i]][j])
        }
        if (length(x1)==0) x1 <- NA
        if (length(x2)==0) x2 <- NA
        if (length(x3)==0) x3 <- NA
        out[i,1,j] <- x1; out[i,2,j] <- x2; out[i,3,j] <- x3
    }
}
head(out[,,1])

# Link the AGE and TR objects and put into a new object called ATR
ATR <- matrix(NA, nrow = 1, ncol = (dim(a)[2] + dim(TR)[2]))
ATR <- data.frame(ATR)
names(ATR) <- c(names(a), names(TR))
for (j in 1:cc)
{
    xx <- matrix(NA, nrow=max(dim(a)[1], dim(TR)[1]), ncol=(dim(a)[2]+dim(TR)[2]))
    xx <- data.frame(xx)
    names(xx) <- c(names(a), names(TR))
    for (i in 1:length(tags))
    {
        xx[i,] <- cbind(a[i,], TR[out[i,1,j],])
    }
    ATR <- rbind(xx, ATR)
    ATR <- ATR[which(!is.na(ATR$growth)),]
}
head(ATR)
dim(ATR) #310, 8, 0, 0 = 318

# Check for duplicates
i <- anyDuplicated(ATR)
ATR <- ATR[-i,]

# Do some additional grooming to these data and create some useful columns for
# the analysis
ATR_mod <- ATR[!ATR$sex %in% "U",] # remove fish with undefined sex (x2)
ATR_mod <- cbind( ATR_mod, "Age2" = ATR_mod$age )
ATR_mod <- cbind( ATR_mod, "Age1" = ATR_mod[,'Age2'] - ATR_mod$liberty )
ATR_mod <- cbind( ATR_mod, "Liberty" = ATR_mod[,'liberty'] )
ATR_mod <- cbind( ATR_mod, "Length2" = ATR_mod$length )
ATR_mod <- cbind( ATR_mod, "Length1" = ATR_mod$release.length )
ATR_mod <- cbind( ATR_mod, "Sex" = as.numeric(as.factor(ATR_mod$sex)) )
ATR_mod <- cbind( ATR_mod, "Area1" = as.numeric(as.factor(ATR_mod[,'SSRU'])) )
ATR_mod <- cbind( ATR_mod, "Date2" = strptime(ATR_mod$date, format = "%Y%m%d") )
ATR_mod <- cbind( ATR_mod, "Date1" = strptime(ATR_mod$release.date, format = "%Y%m%d") )
Date0 <- strptime(ATR_mod$release.date, format = "%Y%m%d")
Date0$year <- Date0$year - ATR_mod[,'Age2']
ATR_mod <- cbind( ATR_mod, "Date0" = Date0 )

#ord <- c("Age1","Age2","Liberty","Length1","Length2","Sex","Area1","Date0","Date1","Date2","Year0","Year1","Year2")
#ATR_mod <- ATR_mod[,ord]

# Select some of the columns
cols <- c("Age1","Age2","Liberty","Length1","Length2","Sex","Area1",
          "Date0","Date1","Date2","release.weight","weight","distance")
ATR_mod <- ATR_mod[,cols]

head(ATR_mod)
write.csv( ATR_mod, file = "../../data/ATR_mod.csv" )
save( ATR_mod, file = "../../data/ATR_mod.RData" )

#Year1 <- as.numeric(as.factor(strftime(Date1, format="%Y")))
#Nyears <- length(unique(Year1))

# END
