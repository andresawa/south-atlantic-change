# Andres Piñango. Last change: 01/08/2021
# "calculate_changes" function------------------------------------------------------------
# eMLR method for the determination of anthropogenic carbon (Cant) and AOU changes. This
# function allows the determination of anthropogenic and natural changes in DIC. For this,
# the user needs to supply a directory containing the oceanographic data of the section of
# interest (minimum two years/occupations). The save_result variable is used to specify if
# the results should be saved as files in the "Result" folder. The expected results are:
# 1) A file with the coefficients of the Cant regression for each occupations pars.
# 2) A file with the coefficients of the AOU regression for each occupations pars.
# 3) A file with the data of the most recent cruise along with Cant y AOU changes for each 
# occupation par.

calculate_changes_modified <- function(input, save_results, section) {
  # Auxiliary functions ------------------------------------------------------------------
  # read_data()
  read_data <- function(x) {
    data <- read.csv(x, na.strings = c("", " ", NA)) %>%
      .[, 5:24] %>%
      drop_na(DIC, TA, Ni, AOU) %>%
      mutate(DICabio = DIC - (117 / 170 * ifelse(AOU < 0, 0, AOU))) %>%
      filter(Latitude <= -10, Latitude >= -40)
    data
  }

  # calculate_DICabio()
  # DIC without remineralization influence. Based in Carter et al. (2017)
  calculate_DICabio <- function(x) {
    x %>% mutate(DICabio = DIC - (117 / 170 * ifelse(AOU < 0, 0, AOU)))
    x
  }

  # split_isopycnal()
  # Isopycnals with less that 6 observations are eliminated because they do not converge
  # during the robust multiple linear regression step. Based in Sarmiento & Gruber (2018).
  split_isopycnal <- function(x) {
    iso01 <- subset(x, ndens <= 26)
    iso02 <- subset(x, ndens >= 26 & ndens <= 26.50)
    iso03 <- subset(x, ndens >= 26.50 & ndens <= 26.75)
    iso04 <- subset(x, ndens >= 26.75 & ndens <= 27.00)
    iso05 <- subset(x, ndens >= 27.00 & ndens <= 27.25)
    iso06 <- subset(x, ndens >= 27.25 & ndens <= 27.50)
    iso07 <- subset(x, ndens >= 27.50 & ndens <= 27.75)
    iso08 <- subset(x, ndens >= 27.75 & ndens <= 27.85)
    iso09 <- subset(x, ndens >= 27.85 & ndens <= 27.95)
    iso10 <- subset(x, ndens >= 27.95 & ndens <= 28.05)
    iso11 <- subset(x, ndens >= 28.05 & ndens <= 28.10)
    iso12 <- subset(x, ndens >= 28.10 & ndens <= 28.15)
    iso13 <- subset(x, ndens >= 28.15)
    lista <- list(
      iso01, iso02, iso03, iso04, iso05, iso06,
      iso07, iso08, iso09, iso10, iso11, iso12, iso13
    )
    names(lista) = c("iso01", "iso02", "iso03", "iso04", "iso05", "iso06", 
                     "iso07", "iso08", "iso09", "iso10", "iso11", "iso12", "iso13")
    remove <- NULL
    for (n in 1:13) {
      if (nrow(lista[[n]]) <= 6) {
        remove <- c(remove, n * -1)
      }
    }

    if (length(remove) > 0) {
      lista <- lista[remove]
    }
    lista
  }

  # coe_extraction()
  coe_extraction <- function(x) {
    result <- data.frame(t(x[["coefficients"]]), summary(x)[["sigma"]])
  }

  # coe_merge()
  coe_merge <- function(x) {
    result <- bind_rows(x, .id = "Regresion")
    colnames(result) <- c(
      "Regresion", "Intercept", "Temp", "Sal", "AOU", "RMSE"
    )
    result
  }

  # model_AOU()
  model_AOU <- function(x) {
    result <- rlm(AOU ~ ptemp + Sal, data = x, psi = psi.bisquare, maxit = 200)
    result
  }

  # emlr_AOU()
  emlr_AOU <- function(x) {
    coeficientes_finales <- vector(mode = "list", ncol(combinaciones))
    for (n in 1:ncol(combinaciones)) {
      tiempo2 <- subset(
        x[[as.character(combinaciones[2, n])]],
        Isopycnal %in% x[[as.character(combinaciones[1, n])]][["Isopycnal"]]
      )
      tiempo1 <- subset(
        x[[as.character(combinaciones[1, n])]],
        Isopycnal %in% x[[as.character(combinaciones[2, n])]][["Isopycnal"]]
      )
      coeficientes_finales[[n]] <- data.frame(
        tiempo2[, 2:4] - tiempo1[, 2:4],
        RMSE2 = tiempo2[, 5],
        RMSE1 = tiempo1[, 5],
        tiempo2[, 1],
        tiempo1[, 1]
      )
    }
    coeficientes_finales
  }

  # Caou()
  Caou <- function(x) {
    result = vector(mode = "list", length = ncol(combinaciones))
    for (i in 1:ncol(combinaciones)){
      df = subset(
        data[[as.character(combinaciones[2,i])]], 
        names(data[[as.character(combinaciones[2,i])]]) %in% x[[i]][,6])
      for (j in 1:nrow(x[[i]])) {
        result[[i]][[j]] = (
          x[[i]][j,1] + 
            (df[[j]][,13] * x[[i]][j,2]) + 
            (df[[j]][,6] * x[[i]][j,3])
        ) * 117/170
      }
    }
    result
  }
  # model_DICabio()
  # combinations range from minimum 3 to maximum 6 variables
  model_DICabio <- function(x) {
    regression = rlm(DICabio ~ ptemp + Sal + AOU, data = x, psi = psi.bisquare, maxit=200)
    regression
  }

  # emlr_DICabio()
  emlr_DICabio <- function(x) {
    coeficientes_finales <- vector(mode = "list", ncol(combinaciones))
    for (n in 1:ncol(combinaciones)) {
      tiempo2 <- subset(
        x[[as.character(combinaciones[2, n])]],
        Isopycnal %in% x[[as.character(combinaciones[1, n])]][["Isopycnal"]]
      )
      tiempo1 <- subset(
        x[[as.character(combinaciones[1, n])]],
        Isopycnal %in% x[[as.character(combinaciones[2, n])]][["Isopycnal"]]
      )
      coeficientes_finales[[n]] <- data.frame(tiempo2[, 1:2],
        tiempo2[, 3:6] - tiempo1[, 3:6],
        RMSE2 = tiempo2[, 7],
        RMSE1 = tiempo1[, 7],
        tRMSE = tiempo2[, 7] + tiempo1[, 7]
      )
    }
    coeficientes_finales
  }

  # Cant()
  # "x" is a integer ranging from 1 to 10 and represent the best regressions. "y" are the
  # coeficients generated by the emlr_DICabio function.
  Cant <- function(x, y) {
    result <- vector(mode = "list", length = ncol(combinaciones))
    for (i in 1:ncol(combinaciones)) {
      df <- subset(
        data[[as.character(combinaciones[2, i])]],
        names(data[[as.character(combinaciones[2, i])]]) %in% y[[i]][["Isopycnal"]]
      )
      for (j in 1:length(df)) {
        coe <- as.data.frame(
          subset(
            y[[i]],
            Isopycnal == names(df)[[j]]
          )
        )
        coe[is.na(coe)] <- 0
        Temp <- df[[j]][, 13]
        Sal <- df[[j]][, 6]
        AOU <- df[[j]][, 20]
        result[[i]][[j]] <- coe[x, 3] +
          (Temp * coe[x, 4]) +
          (Sal * coe[x, 5]) +
          (AOU * coe[x, 6])
      }
    }
    result
  }

  # consolidate()
  consolidate <- function(x, y) {
    result <- vector(mode = "list", length = ncol(combinaciones))
    for (i in 1:ncol(combinaciones)) {
      original_data <- bind_rows(
        subset(
          data[[as.character(combinaciones[2, i])]],
          names(data[[as.character(combinaciones[2, i])]]) %in% y[[i]][["Isopycnal"]]
        ),
        .id = "Isopycnal"
      )
      antropogenic_carbon <- data.frame(
        unlist(x[[1]][[i]])
      )
      result[[i]] <- data.frame(
        original_data,
        antropogenic_carbon
      )
    }
    result
  }

  # Calculations -------------------------------------------------------------------------
  # 1. Load the Data and determine the number of combinations from available occupations
  file.direction <- list.files(path = input, full.names = T)
  file.list <- list.files(path = input)
  data <- lapply(file.direction, read_data) %>%
    lapply(., filter, Depth > 150) %>%
    lapply(., split_isopycnal)
  names(data) <- substr(file.list, 1, 8)

  combinaciones <- combn(substr(file.list, 1, 8), 2)
  coefficient_names <- vector(mode = "list", ncol(combinaciones))
  for (n in 1:ncol(combinaciones)) {
    coefficient_names[[n]] <- paste(combinaciones[2, n], combinaciones[1, n], sep = " - ")
  }
  coefficient_names <- unlist(coefficient_names)

  # 2. Evaluate the DIC changes associated with natural process (AOU changes)
  coefficients_AOU <- lapply(data, lapply, model_AOU) %>%
    lapply(., lapply, coe_extraction) %>%
    lapply(., bind_rows, .id = "Isopycnal") %>%
    emlr_AOU(.)
  names(coefficients_AOU) <- coefficient_names

  carbono_AOU = Caou(coefficients_AOU) %>% 
    lapply(., unlist) %>% 
    lapply(., as.data.frame)

  # 3. Evaluate the DIC changes associated with anthropogenic uptake (Cant changes)
  coefficients_Cant <- lapply(data, lapply, model_DICabio) %>%
    lapply(., lapply, coe_extraction) %>%
    lapply(., lapply, coe_merge) %>%
    lapply(., bind_rows, .id = "Isopycnal") %>%
    emlr_DICabio(.)
  names(coefficients_Cant) <- coefficient_names

  carbono_Cant <- lapply(1, Cant, coefficients_Cant) %>%
    consolidate(., coefficients_Cant)

  # 4. Merge the Cant and AOU changes in a unique data frame
  resultados_totales <- vector(mode = "list", length = length(carbono_Cant))
  for (n in 1:length(carbono_Cant)) {
    carbono_antro <- data.frame(carbono_Cant[[n]][, 23])
    names(carbono_antro) <- c("Ca")
    C_AOU <- carbono_AOU[[n]]
    names(C_AOU) <- "C_AOU"
    resultados_totales[[n]] <- data.frame(carbono_Cant[[n]][, 1:22], C_AOU, carbono_antro)
  }

  nombres <- vector(mode = "list", length = length(resultados_totales))
  for (n in 1:length(resultados_totales)) {
    nombres[[n]] <- paste(combinaciones[2, n], combinaciones[1, n], sep = " - ")
  }
  nombres <- unlist(nombres)
  names(resultados_totales) <- nombres

  rm(carbono_Cant, nombres, n, combinaciones, C_AOU, carbono_antro, carbono_AOU)

  # Export the results -------------------------------------------------------------------
  if (save_results == TRUE) {
    # 1. Export Coefficients_AOU
    for (n in 1:length(coefficients_AOU)) {
      write.csv(
        coefficients_AOU[[n]],
        file = paste0(
          "./Results/",
          section,
          "/Coefficients AOU eMLR ",
          names(resultados_totales)[[n]],
          ".csv"
        ),
        row.names = F
      )
    }
    # 2. Export Coefficients_Cant
    for (n in 1:length(coefficients_Cant)) {
      write.csv(
        coefficients_Cant[[n]],
        file = paste0(
          "./Results/",
          section,
          "/Coefficients Cant eMLR ",
          names(resultados_totales)[[n]],
          ".csv"
        ),
        row.names = F
      )
    }
    # 3. Export the most recent cruise with the changes
    for (n in 1:length(resultados_totales)) {
      write.csv(resultados_totales[[n]],
        file = paste0(
          "./Results/",
          section,
          "/Changes ",
          names(resultados_totales)[[n]],
          ".csv"
        ),
        row.names = F
      )
    }
  }
  resultados_totales
}
