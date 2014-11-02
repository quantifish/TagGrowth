#===============================================================
# UPDATE DESCRIPTION FILE
#===============================================================

VERSION <- "1.0"
DATE    <- Sys.Date()

DESCRIPTION <- readLines("tagGrowth/DESCRIPTION")
DESCRIPTION[3] <- paste("Version:", VERSION)
DESCRIPTION[4] <- paste("Date:", DATE)
writeLines(DESCRIPTION, "tagGrowth/DESCRIPTION")

# Write lobview.version()
filename <- "tagGrowth/R/tagGrowth.version.R"
cat("#' Function to return version number\n", file = filename)
cat("#'\n", file = filename, append = TRUE)
cat("#' @export\n",file = filename, append = TRUE)
cat("#'\n", file = filename, append = TRUE)
cat("tagGrowth.version <- function()\n", file = filename, append = TRUE)
cat("{\n", file = filename, append = TRUE)
cat(paste("    return(\"Version: ", VERSION, "\\n", "Compile date: ", DATE, "\\n\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)

#===============================================================
