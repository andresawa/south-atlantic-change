# Andres Piñango. Last change: 01/08/2021
# Description ----------------------------------------------------------------------------
# Generates a summary table with the mean changes the Cant inventory for each section.

# Merge the inventories ------------------------------------------------------------------
A9 = inventories_A95[[1]] %>% mutate(Type = "A9.5 (2009-2018)")
A10a = inventories_A10[[1]] %>% mutate(Type = "A10 (1992-2003)")
A10b = inventories_A10[[3]] %>% mutate(Type = "A10 (2003-2011)")
A105 = inventories_A105[[1]] %>% mutate(Type = "A10.5 (2017-2011)")
A17a = inventories_A17[[1]] %>% mutate(Type = "A17 (1994-2013)")
A17b = inventories_A17[[3]] %>% mutate(Type = "A17 (2013-2019)")
A16a = inventories_A16[[1]] %>% mutate(Type = "A16 (1989-2005)")
A16b = inventories_A16[[3]] %>% mutate(Type = "A16 (2005-2013)")
A13 = inventories_A13[[1]] %>% mutate(Type = "A13 (1995-2010)")

merged = bind_rows(A9, A10a, A10b, A105, A17a, A17b, A16a, A16b, A13)
rm(A9, A10a, A10b, A105, A17a, A17b, A16a, A16b, A13)

# Calculations ---------------------------------------------------------------------------
number_stations <- merged %>%
  count(Type) %>%
  bind_rows(., data.frame("Type" = "All", count(merged)))

all_sections <- merged %>%
  filter(Type != "A10.5 (2017-2011)") %>%
  summarise(
    mean(Inventory), sd(Inventory),
  ) %>%
  mutate(Type = "All")

inventory_summary <- merged %>%
  group_by(Type) %>%
  summarise(
    mean(Inventory), sd(Inventory),
  ) %>%
  bind_rows(., all_sections) %>%
  bind_cols(., "stations" = number_stations$n)

rm(
  all_sections, merged, number_stations
)
# Export the results ---------------------------------------------------------------------
if (save_results == TRUE) {
  write.csv(inventory_summary, 
            file = paste0("./Results/", "Cant changes Inventory Summary.csv"),
            row.names = F)
}