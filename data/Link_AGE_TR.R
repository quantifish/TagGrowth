# Clean up R
rm(list=ls())

# Load data
load("AGE.RData")
load("TR.RData")

# Identify which aged fish had a tag
a <- AGE[AGE$tagged == TRUE,]
cat("Number of aged fish with a tag:", dim(a)[1], "\n")

# Identify the tag numbers by extracting from comments field
tags <- list()
for (j in 1:length(a$tag.ref))
{
    tags[[j]] <- as.numeric(na.omit(as.numeric(strsplit(a$tag.ref, "[^0-9]+")[[j]])))
}

# Identify the potential maximum number of tags referred to in comments (cc)
# and the number of unique tags referred to in comments
cc <- 1
utag <- NULL
for (j in 1:length(a$tag.ref))
{
  xx <- length(tags[[j]])
  cc <- max(cc, xx)
  utag <- c(utag, tags[[j]])
}
print(cc)
length(unique(utag[utag>1000]))

# Create an index to link the AGE and TR objects for each tag in the comments
# field to each tag in the TR object (i.e. no1, no2, no3)
out <- array(NA, dim=c(length(tags),3,cc))
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
ATR <- matrix(NA, nrow=1, ncol=(dim(a)[2]+dim(TR)[2]))
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

# Save
save(ATR, file = "ATR.RData")

# END
