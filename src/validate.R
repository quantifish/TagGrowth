# Validate that the counter in the model will index the correct years (daily)
for (II in 1:nrow(ATR_mod))
{
    time0 = ATR_mod$Time0[II]
    year1 = ATR_mod$Year0[II]
    for (i in 0:(ATR_mod$iAge1[II]-1))
    {
        time1 = time0 + i
        if ( time1 %% 365 == 0. ) { year1 = year1 + 1; }
    }
    cat("Time1:", time1, ATR_mod[II,]$Time1, "| Year1:", year1, ATR_mod[II,]$Year1, "|", year1 == ATR_mod[II,]$Year1, "\n")
}

# Validate that the counter in the model will index the correct years (weeks)
for (II in 1:nrow(ATR_mod))
{
    time0 = ATR_mod$Time0[II]
    year1 = ATR_mod$Year0[II]
    for (i in 0:(ATR_mod$iAge1[II]-1))
    {
        time1 = time0 + i
        if ( time1 %% 52 == 0. ) { year1 = year1 + 1; }
    }
    cat("Time1:", time1, ATR_mod[II,]$Time1, "| Year1:", year1, ATR_mod[II,]$Year1, "|", year1 == ATR_mod[II,]$Year1, "\n")    
    time1 = ATR_mod$Time1[II]
    year2 = ATR_mod$Year1[II]
    for (i in 0:(ATR_mod$iLiberty[II]-1))
    {
        time2 = time0 + i
        if ( time2 %% 52 == 0. ) { year2 = year2 + 1; }
    }
    cat("Time2:", time2, ATR_mod[II,]$Time2, "| Year2:", year2, ATR_mod[II,]$Year2, "|", year2 == ATR_mod[II,]$Year2, "\n")
}
