# convert data from PROFOUND database to rsofun
# p-model and lm3ppa common format

# will use the half hourly flux data as a basis rather than daily data

# Check which variables are actually needed (Beni, Laura?)

# load libraries
library(ProfoundData)
library(ggplot2)
library(tidyverse)

db <- "data/profound/ProfoundData.sqlite"
setDB(db)
getDB()

# RH es function
es <- function(temp){
  0.6108*exp((17.27 * temp)/(temp + 237.3))
}

# get all sites
sites <- browseData() %>%
  filter(
    METEOROLOGICAL == 1
  )

# cycle over all sites and grab
# compile data
met_data <- sites %>%
  group_by(site) %>%
  do({

  # get meteorological data
  met <- getData(
    dataset ="METEOROLOGICAL",
    site = .$site,
    collapse = TRUE
    ) %>%
    as_tibble()

  met <- met %>%
    select(
      date,
      year,
      taFMDS_degC,
      swInFMDS_Wm2,
      #lwInFMDS_Wm2,
      vpdFMDS_hPa,
      paF_kPa,
      pF_mm,
      wsF_ms1
    ) %>%
    rename(
      'vpd' = 'vpdFMDS_hPa',
      'prec'  = 'pF_mm',
      'wind' = 'wsF_ms1',
      'temp' = 'taFMDS_degC',
      'ppfd' = 'swInFMDS_Wm2',
      # get CO2 data (ISIMIP)
      'patm' = 'paF_kPa',
      'vpd'  = 'vpdFMDS_hPa'
    ) %>% # back fill the data
    mutate(
      date = as.POSIXct(date, "%Y-%m-%d %H:%M:%S"),
      year = year,
      doy = as.numeric(format(date, "%j")),
      hour = as.numeric(format(date, "%H")),
      temp = temp,
      prec = prec,
      snow = NA,
      vpd = vpd,
      # temperature in KELVIN
      rh = (1 - vpd / es(273.15 + temp) ) * 100,
      par = NA,
      patm = patm / 1000,
      wind = wind,
      ccov_int = NA,
      ccov = NA
    )

  # get soil data
  soil <- getData(
    dataset ="SOILTS",
    site = .$site,
    collapse = TRUE
    ) %>%
    as_tibble()

  soil <- soil %>%
    select(
      date,
      starts_with("swc1"),
      starts_with("tsFMDS1_deg")
    ) %>%
    rename(
      'swc' = starts_with('swc1'),
      'temp_soil' = 'tsFMDS1_degC'
    ) %>%
    mutate(
      date = as.POSIXct(date, "%Y-%m-%d %H:%M:%S")
    )

  if("swc" %in% names(soil)){
    print("swc")
  } else {
    soil <- soil %>%
      mutate(
        swc = NA
      )
  }

  met <- left_join(soil, met)

  # co2 data
  co2 <- climate::co2_demo %>%
    rename(
      'year' = 'yy',
      'month' = 'mm',
      'co2' = 'co2_interp'
    ) %>%
    select(
      'year',
      'month',
      'co2'
    )

  met <- met %>%
    mutate(
      month = as.numeric(format(date, "%m")),
    ) %>%
    left_join(co2)

  # sort variables
  met <- met %>%
    # sort the variables
    select(
      date,
      year,
      doy,
      hour,
      temp,
      temp_soil,
      prec,
      snow,
      vpd,
      rh,
      ppfd,
      par,
      patm,
      wind,
      ccov_int,
      ccov,
      co2,
      swc
    )

  met

  })

#---- gap filling ----

# Only retain variables that have 80% coverage throughout
# the day (for calculation of hourly / daily means)


#---- aggregation to hourly and daily level ----



#---- plotting ----

p <- ggplot(met_data) +
  geom_point(
    aes(
      date,
      vpd
    )
  ) +
  facet_wrap(~site)
p



