# Andres Piñango. Last change: 05/08/2021
# "calculate_pH" function--------------------------------------------------------
# Calculate the changes in pH and the carbonate system parameters as result of the uptake
# of anthropogenic carbon (Cant) and natural changes in DIC. Uses the estimates calculated
# by the "calculate_changes" function.

calculate_pH <- function(data_list, elapsed_years, save_results, section) {
  # Auxiliary functions ------------------------------------------------------------------
  # carbsys()
  # carbonate system
  carbsys <- function(data, DIC) {
    result <- carbfull(15,
      (data[["TA"]] / 1000000),
      (data[[DIC]] / 1000000),
      S = data[["Sal"]],
      T = data[["Temp"]],
      P = (data[["Press"]] * 0.1),
      Pt = (data[["Phos"]] / 1000000),
      Sit = (data[["Si"]] / 1000000),
      k1k2 = "m06",
      kf = "pf",
      ks = "d",
      b = "u74"
    )
  }

  # carberr()
  # errors in the carbonate system variables
  carberr <- function(data, DIC, errorDIC) {
    result <- errors(15,
      (data[["TA"]] / 1000000),
      (data[[DIC]] / 1000000),
      S = data[["Sal"]],
      T = data[["Temp"]],
      P = (data[["Press"]] * 0.1),
      Pt = (data[["Phos"]] / 1000000),
      Sit = (data[["Si"]] / 1000000),
      evar1 = 2 / 1000000,
      evar2 = errorDIC,
      eS = 0.002,
      eT = 0.002,
      ePt = 0.05 / 1000000,
      eSit = 0.5 / 1000000,
      epK = c(0.002, 0.0075, 0.015, 0.01, 0.01, 0.02, 0.02),
      eBt = 0.02,
      method = "ga",
      k1k2 = "m06",
      kf = "pf",
      ks = "d",
      b = "u74",
      warn = "n",
      eos = "teos10",
      long = data[["Longitude"]],
      lat = data[["Latitude"]]
    )
  }

  # carbbuf
  # Buffer capacities of the seawater carbonate system
  carbbuf <- function(data, DIC) {
    result <- buffesm(15,
      (data[["TA"]] / 1000000),
      (data[[DIC]] / 1000000),
      S = data[["Sal"]],
      T = data[["Temp"]],
      P = (data[["Press"]] * 0.1),
      Pt = (data[["Phos"]] / 1000000),
      Sit = (data[["Si"]] / 1000000),
      k1k2 = "m06",
      kf = "pf",
      ks = "d",
      b = "u74",
      warn = "n",
      eos = "teos10",
      long = data[["Longitude"]],
      lat = data[["Latitude"]]
    )
  }

  # Calculations -------------------------------------------------------------------------
  output <- vector(mode = "list", length = length(data_list))
  names(output) <- names(data_list)
  
  for (n in 1:length(data_list)) {
    data <- data_list[[n]] %>%
      drop_na(Temp, Sal, TA, DIC, Ca) %>%
      mutate(DICwca = DIC - Ca, DICwaou = DIC - C_AOU)

    original <- data.frame(
      carbsys(data, "DIC")[, c(6, 18, 19, 37)],
      carberr(data, "DIC", 2 / 1000000)[, c(2, 8, 9, 1)],
      carbbuf(data, "DIC")
    )
    without_ca <- data.frame(
      carbsys(data, "DICwca")[, c(6, 18, 19, 37)],
      carberr(data, "DICwca", sqrt((2^2) + (3^2)) / 1000000)[, c(2, 8, 9, 1)],
      carbbuf(data, "DICwca")
    )
    without_aou <- data.frame(
      carbsys(data, "DICwaou")[, c(6, 18, 19, 37)],
      carberr(data, "DICwaou", sqrt((2^2) + (3^2)) / 1000000)[, c(2, 8, 9, 1)],
      carbbuf(data, "DICwaou")
    )

    delta_ca <- bind_cols(
      data[, c(1:5, 20)],
      (original - without_ca)[, c(1:4, 9:15)] / elapsed_years[[n]],
      sqrt(original[, 5:8]^2 + without_ca[, 5:8]^2) / elapsed_years[[n]]
    )
    delta_aou <- bind_cols(
      data[, c(1:5, 20)],
      (original - without_aou)[, c(1:4, 9:15)] / elapsed_years[[n]],
      sqrt(original[, 5:8]^2 + without_aou[, 5:8]^2) / elapsed_years[[n]]
    )
    names(delta_ca)[7:10] <- c("dpH", "dOA", "dOC", "dH")
    names(delta_ca)[18:21] <- c("pH error", "OA error", "OC error", "H error")
    names(delta_aou)[7:10] <- c("dpH", "dOA", "dOC", "dH")
    names(delta_aou)[18:21] <- c("pH error", "OA error", "OC error", "H error")
    delta_ca$dH <- delta_ca$dH * 10^9
    delta_ca$`H error` <- delta_ca$`H error` * 10^9
    delta_aou$dH <- delta_aou$dH * 10^9
    delta_aou$`H error` <- delta_aou$`H error` * 10^9
    output[[n]][[1]] <- delta_ca
    output[[n]][[2]] <- delta_aou
    names(output[[n]]) <- c("Cant", "AOU")
  }
  
  # Export the results -------------------------------------------------------------------
  if (save_results == TRUE) {
    for (n in 1:length(output)) {
      write.csv(
        output[[n]][[1]],
        file = paste0("./Results/", section, "/pH Change ", names(output[[n]])[[1]], " ",
          names(output)[[n]], ".csv"),
        row.names = F
      )
      write.csv(
        output[[n]][[2]],
        file = paste0("./Results/", section, "/pH Change ", names(output[[n]])[[2]], " ",
          names(output)[[n]], ".csv"),
        row.names = F
      )
    }
  }
  return(output)
}
