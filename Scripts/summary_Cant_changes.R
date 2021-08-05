# Andres Piñango. Last change: 01/08/2021
# Description ----------------------------------------------------------------------------
# Generates a summary table with the mean changes in anthropogenic carbon (Cant) and the
# carbonates system parameters in each section. The standard deviations (sd) are also
# included.

# Merge the changes ----------------------------------------------------------------------
A95.data <- changes_DIC_A95[[1]][, c(23, 24)] / 9
A95.pH <- pH_A95[[1]][[1]]
t1 <- data.frame(A95.pH, A95.data) %>% mutate(Type = "A9.5 (2009-2018)")

A10.data <- changes_DIC_A10[[3]][, c(23, 24)] / 8
A10.pH <- pH_A10[[3]][[1]]
t2 <- data.frame(A10.pH, A10.data) %>% mutate(Type = "A10 (2003-2011)")

A10_2.data <- changes_DIC_A10[[1]] %>%
  drop_na(Temp, Sal, TA, DIC, Ca) %>%
  .[, c(23, 24)] / 11
A10_2.pH <- pH_A10[[1]][[1]]
t3 <- data.frame(A10_2.pH, A10_2.data) %>% mutate(Type = "A10 (1992-2003)")

A17.data <- changes_DIC_A17[[3]][, c(23, 24)] / 6
A17.pH <- pH_A17[[3]][[1]]
t4 <- data.frame(A17.pH, A17.data) %>% mutate(Type = "A17 (2013-2019)")

A17_2.data <- changes_DIC_A17[[1]][, c(23, 24)] / 19
A17_2.pH <- pH_A17[[1]][[1]]
t5 <- data.frame(A17_2.pH, A17_2.data) %>% mutate(Type = "A17 (1994-2013)")

A16.data <- changes_DIC_A16[[3]][, c(23, 24)] / 8
A16.pH <- pH_A16[[3]][[1]]
t6 <- data.frame(A16.pH, A16.data) %>% mutate(Type = "A16 (2005-2013)")

A16_2.data <- changes_DIC_A16[[1]][, c(23, 24)] / 16
A16_2.pH <- pH_A16[[1]][[1]]
t7 <- data.frame(A16_2.pH, A16_2.data) %>% mutate(Type = "A16 (1989-2005)")

A13.data <- changes_DIC_A13[[1]] %>%
  drop_na(Temp, Sal, TA, DIC, Ca) %>%
  .[, c(23, 24)] / 15
A13.pH <- pH_A13[[1]][[1]]
t8 <- data.frame(A13.pH, A13.data) %>% mutate(Type = "A13 (1995-2010)")

A105.data <- changes_DIC_A105[[1]] %>%
  drop_na(Temp, Sal, TA, DIC, Ca) %>%
  .[, c(23, 24)] / 6
A105.pH <- pH_A105[[1]][[1]]
t9 <- data.frame(A105.pH, A105.data) %>% mutate(Type = "A10.5 (2017-2011)")

centrales <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8, t9) %>%
  filter(ndens <= 27.1)

intermediarias <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8, t9) %>%
  filter(ndens >= 27.1 & ndens <= 27.65)

rm(
  t1, t2, t3, t4, t5, t6, t7, t8, t9, A95.data, A95.pH, A10.data, A10.pH, A10_2.data,
  A10_2.pH, A17.data, A17.pH, A17_2.data, A17_2.pH, A16.data, A16.pH, A16_2.data,
  A16_2.pH, A13.data, A13.pH, A105.data, A105.pH
)

# Calculations ---------------------------------------------------------------------------
# Central waters
number_centrales <- centrales %>%
  count(Type) %>%
  bind_rows(., data.frame("Type" = "All", count(centrales)))

all_centrales <- centrales %>%
  filter(ndens >= 26.2 & Type != "A10.5 (2017-2011)") %>%
  summarise(
    mean(dpH), sd(dpH),
    mean(dH), sd(dH),
    mean(dOA), sd(dOA),
    mean(R), sd(R),
    mean(Ca), sd(Ca),
    mean(C_AOU), sd(C_AOU)
  ) %>%
  mutate(Type = "All")

cw_summary <- centrales %>%
  filter(ndens >= 26.2) %>%
  group_by(Type) %>%
  summarise(
    mean(dpH), sd(dpH),
    mean(dH), sd(dH),
    mean(dOA), sd(dOA),
    mean(R), sd(R),
    mean(Ca), sd(Ca),
    mean(C_AOU), sd(C_AOU)
  ) %>%
  bind_rows(., all_centrales) %>%
  bind_cols(., "n" = number_centrales$n)

# Intermediate Waters
number_intermediarias <- intermediarias %>%
  count(Type) %>%
  bind_rows(., data.frame("Type" = "All", count(intermediarias)))

all_intermediarias <- intermediarias %>%
  filter(Type != "A10.5 (2017-2011)") %>%
  summarise(
    mean(dpH), sd(dpH),
    mean(dH), sd(dH),
    mean(dOA), sd(dOA),
    mean(R), sd(R),
    mean(Ca), sd(Ca),
    mean(C_AOU), sd(C_AOU)
  ) %>%
  mutate(Type = "All")

iw_summary <- intermediarias %>%
  group_by(Type) %>%
  summarise(
    mean(dpH), sd(dpH),
    mean(dH), sd(dH),
    mean(dOA), sd(dOA),
    mean(R), sd(R),
    mean(Ca), sd(Ca),
    mean(C_AOU), sd(C_AOU)
  ) %>%
  bind_rows(., all_intermediarias) %>%
  bind_cols(., "n" = number_intermediarias$n)

rm(
  centrales, number_centrales, all_centrales, intermediarias, number_intermediarias,
  all_intermediarias
)
# Export the results ---------------------------------------------------------------------
if (save_results == TRUE) {
  write.csv(cw_summary, 
            file = paste0("./Results/", "Cant changes Central Waters Summary.csv"),
            row.names = F)
  write.csv(iw_summary, 
            file = paste0("./Results/", "Cant changes Intermediate Waters Summary.csv"), 
            row.names = F)
}
