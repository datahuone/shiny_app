# Description:
# Usage:
# Output:       dr_colours.Rds in the cache folder containing a named vector
#               of the Data Room colours
# Author:       Meeri Seppä, Annakaisa Ritala
# Date:         2024-08-27
#
# Dependencies ----
# Local attributes ----
# Run ----

dr_colours <- c(dark_green = "#234721",
                light_green = "#AED136",
                dark_blue = "#393594",
                light_blue = "#8482BD",
                dark_red = "#721D41",
                light_red = "#CC8EA0",
                yellow = "#FBE802",
                orange = "#F16C13",
                light_orange = "#FFF1E0")

saveRDS(dr_colours, file = file.path("cache", "dr_colours.Rds"))

# Clean-up ----
rm(list = ls())
gc()
