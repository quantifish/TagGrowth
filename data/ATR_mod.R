
load("ATR.RData")
head(ATR)

# Data
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
head(ATR_mod)
write.csv( ATR_mod, file = "ATR_mod.csv" )
save( ATR_mod, file = "ATR_mod.RData" )

#Year1 <- as.numeric(as.factor(strftime(Date1, format="%Y")))
#Nyears <- length(unique(Year1))

# END
