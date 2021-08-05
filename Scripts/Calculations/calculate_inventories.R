# Andres Piñango. Last change: 01/08/2021
# "calculate_inventories" function--------------------------------------------------------
# Calculate the inventories of anthropogenic carbon in the water column from 200 to 2000
# using the Cant estimates calculated by the "calculate_changes" function. This
# methodology is an adaptation of the approach taken by Tanhua & Keeling (2012) doi:
# 10.5194/bg-9-4819-2012

calculate_inventories <- function(data_list, elapsed_years, save_results, section) {
  # Auxiliary functions ------------------------------------------------------------------
  # cero_carbon()
  # Make 0 all the negative Cant values
  cero_carbon <- function(dataframe) {
    corrected <- ifelse(dataframe$Ca < 0, 0, dataframe$Ca)
    dataframe$Ca <- corrected
    dataframe
  }

  # density_conversion()
  # Calculate the in-situs density and change the units of Cant from μmol/l to mol/m³
  density_conversion <- function(dataframe) {
    density <- swRho(dataframe$Sal,
      temperature = dataframe$Temp,
      pressure = dataframe$Press,
      longitude = dataframe$Longitude,
      latitude = dataframe$Latitude
    )
    dataframe$Ca <- dataframe$Ca * density * 1 / 1000000
    dataframe
  }

  # divide_station()
  # Split the data in stations
  divide_station <- function(dataframe) {
    stations <- distinct(dataframe, Latitude, Longitude)
    divided_data <- vector(mode = "list", length = nrow(stations))
    for (n in 1:nrow(stations)) {
      divided_data[[n]] <- filter(
        dataframe,
        Longitude == stations[n, 1] & Latitude == stations[n, 2]
      )
    }
    divided_data
  }

  # interpolation()
  # Interpolate the Cant values from 200 to 2000 using a piecewise hermite interpolating
  # polynomial routine only if exist data outside this interval. This step is responsible
  # for the stations without data.
  interpolation <- function(dataframe) {
    inter_depth <- c(200, 250, 300, 400, 500, 800, 1100, 1400, 1700, 2000)
    if (min(dataframe$Depth) <= 300 & max(dataframe$Depth) >= 2000) {
      result <- pchip(dataframe$Depth, dataframe$Ca, inter_depth)
    }
  }

  # integration()
  integration <- function(list) {
    inter_depth <- c(200, 250, 300, 400, 500, 800, 1100, 1400, 1700, 2000)
    if (length(list) > 0) {
      result <- integrate.xy(inter_depth, list)
    } else {
      result <- 0
    }
  }

  # Calculations -------------------------------------------------------------------------
  output <- vector(mode = "list", length = length(data_list))
  for (n in 1:length(data_list)) {
    dataframe <- data_list[[n]] %>% drop_na(Ca)
    stations <- distinct(dataframe, Latitude, Longitude)
    resultado <- dataframe %>%
      cero_carbon() %>%
      density_conversion() %>%
      distinct(., Latitude, Longitude, Depth, .keep_all = T) %>%
      divide_station() %>%
      lapply(., arrange, Depth) %>%
      lapply(., interpolation) %>%
      lapply(., integration) %>%
      unlist(.)
    resultado <- resultado / elapsed_years[[n]]
    output[[n]] <- data.frame(stations, "Inventory" = resultado) %>% 
      filter(., Inventory != 0)
  }
  names(output) <- names(data_list)
  # Export the results -------------------------------------------------------------------
  if (save_results == TRUE) {
    for (n in 1:length(output)) {
      write.csv(
        output[[n]],
        file = paste0(
          "./Results/",
          section,
          "/Inventory Change ",
          names(output)[[n]],
          ".csv"
        ),
        row.names = F
      )
    }
  }
  return(output)
}
