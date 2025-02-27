# load libraries
library(tidyverse)
library(eia)


# set API Key
eia_set_key(Sys.getenv("EIA_API_KEY"))


# getting International Energy Outlook Data from DOE/EIA
# documentation found here: https://docs.ropensci.org/eia/articles/eia.html#eia-data
# max rows returned is 5000

# setting region to total world, and under high economic development scenario
df_1 <- eia_data(dir= "ieo/2023",
                 facets = list(regionId = "6-0",
                               scenario = "HighMacro"))

# setting region to total world, and under low oil price scenario
df_2 <- eia_data(dir= "ieo/2023",
                 facets = list(regionId = "6-0",
                               scenario = "LowOilPrice"))

# setting region to Americas, setting history to projected outcomes, 
# and scenario as high economic development 
df_3 <- eia_data(dir= "ieo/2023",
                 facets = list(regionId = "6-1",
                               history = "PROJECTION",
                               scenario = "HighMacro"))

### other scrap code below

# To add more geographic information of these projects, FIPS county codes can be 
# used. FIPS county codes are five-digit numbers that identify counties in the 
# United States. The first two digits of a FIPS county code represent the state, 
# and the last three digits represent the county. 

fips_data <- read.socrata("https://data.ny.gov/resource/juva-r6g2.json")

ny_fips <- fips_data |> 
  mutate(County=toupper(`county`)) |> 
  select(-county)

knitr::kable(head(ny_fips, format ="markdown")) 

