# Andres Piñango. Last change: 01/08/2021
# Description ----------------------------------------------------------------------------
# Figure 5. Vertical profile showing the effect of the anthropogenic carbon (Cant)
# invasion (in red) and changes in the carbon content due to apparent oxygen utilization
# (AOU) variations (in blue) over the annual acidifications rates of the water column at
# the (a) east and (b) west of 15°W in the South Atlantic Ocean. Data were binned in 12
# intervals of neutral density and the boxplots show a summary of the data distribution.
# Colored lines represent the median value of each interval of neutral density.

# Auxiliary functions --------------------------------------------------------------------
# isopycnal()
isopycnal <- function(x) {
  iso1 <- subset(x, ndens <= 26)
  iso2 <- subset(x, ndens >= 26 & ndens <= 26.50)
  iso3 <- subset(x, ndens >= 26.50 & ndens <= 26.75)
  iso4 <- subset(x, ndens >= 26.75 & ndens <= 27.00)
  iso5 <- subset(x, ndens >= 27.00 & ndens <= 27.25)
  iso6 <- subset(x, ndens >= 27.25 & ndens <= 27.50)
  iso7 <- subset(x, ndens >= 27.50 & ndens <= 27.75)
  iso8 <- subset(x, ndens >= 27.75 & ndens <= 27.85)
  iso9 <- subset(x, ndens >= 27.85 & ndens <= 27.95)
  iso10 <- subset(x, ndens >= 27.95 & ndens <= 28.05)
  iso11 <- subset(x, ndens >= 28.05 & ndens <= 28.10)
  iso12 <- subset(x, ndens >= 28.10 & ndens <= 28.15)
  iso13 <- subset(x, ndens >= 28.15)
  lista <- list(iso1, iso2, iso3, iso4, iso5, iso6, iso7, 
                iso8, iso9, iso10, iso11, iso12, iso13)
  names(lista) <- c("iso01", "iso02", "iso03", "iso04", "iso05", "iso06", "iso07", 
                    "iso08", "iso09", "iso10", "iso11", "iso12", "iso13")
  
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

# carbsys()
carbsys <- function(x) {
  result <- carb(15, 
                 (x[["TA"]] / 1000000), 
                 (x[["DIC"]] / 1000000),
                 S = x[["Sal"]], 
                 T = x[["Temp"]], 
                 P = (x[["Press"]] * 0.1),
                 Pt = (x[["Phos"]] / 1000000), 
                 Sit = (x[["Si"]] / 1000000), 
                 k1k2 = "m06", 
                 kf = "pf", 
                 ks = "d", 
                 b = "u74",
                 warn = "y", 
                 eos = "teos10", 
                 long = x[["Longitude"]], 
                 lat = x[["Latitude"]]
                 )
}

# substraction()
substraction <- function(x, y, z) {
  tiempo1 <- subset(x, names(x) %in% names(y))
  tiempo2 <- subset(y, names(y) %in% names(x))
  resta <- vector(mode = "list", length = length(tiempo1))
  for (n in 1:length(tiempo1)) {
    resta[[n]] <- data.frame(
      "Isopycnal" = names(tiempo1)[[n]],
      "deltapH" = (
        mean(tiempo2[[n]][["pH"]], na.rm = T) - mean(tiempo1[[n]][["pH"]], na.rm = T)
        ) / z
    )
  }
  final <- bind_rows(resta) %>% filter(Isopycnal != "iso01")
}

# Calculate the total pH change ----------------------------------------------------------
# A9.5
y1w <- read.csv("./Data/A9.5/A95-2009.txt") %>%
  filter(Depth > 150, Longitude <= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y1e <- read.csv("./Data/A9.5/A95-2009.txt") %>%
  filter(Depth > 150, Longitude >= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2w <- read.csv("./Data/A9.5/A95-2018.txt") %>%
  filter(Depth > 150, Longitude <= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2e <- read.csv("./Data/A9.5/A95-2018.txt") %>%
  filter(Depth > 150, Longitude >= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A9w <- substraction(y1w, y2w, (2018 - 2009)) %>% mutate("Type" = "Observed Trend")
A9e <- substraction(y1e, y2e, (2018 - 2009)) %>% mutate("Type" = "Observed Trend")
names(A9w) <- c("Isopycnal", "dpH", "Type")
names(A9e) <- c("Isopycnal", "dpH", "Type")
rm(y1w, y1e, y2w, y2e)

# A10 1993-2003
y1w <- read.csv("./Data/A10/A10-1992.txt") %>%
  filter(Depth > 150, Longitude <= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y1e <- read.csv("./Data/A10/A10-1992.txt") %>%
  filter(Depth > 150, Longitude >= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2w <- read.csv("./Data/A10/A10-2003.txt") %>%
  filter(Depth > 150, Longitude <= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2e <- read.csv("./Data/A10/A10-2003.txt") %>%
  filter(Depth > 150, Longitude >= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A10aw <- substraction(y1w, y2w, (2003 - 1992)) %>% mutate("Type" = "Observed Trend")
A10ae <- substraction(y1e, y2e, (2003 - 1992)) %>% mutate("Type" = "Observed Trend")
names(A10aw) <- c("Isopycnal", "dpH", "Type")
names(A10ae) <- c("Isopycnal", "dpH", "Type")
rm(y1w, y1e, y2w, y2e)

# A10 2003-2013
y1w <- read.csv("./Data/A10/A10-2003.txt") %>%
  filter(Depth > 150, Longitude <= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y1e <- read.csv("./Data/A10/A10-2003.txt") %>%
  filter(Depth > 150, Longitude >= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2w <- read.csv("./Data/A10/A10-2011.txt") %>%
  filter(Depth > 150, Longitude <= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2e <- read.csv("./Data/A10/A10-2011.txt") %>%
  filter(Depth > 150, Longitude >= -15) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A10bw <- substraction(y1w, y2w, (2011 - 2003)) %>% mutate("Type" = "Observed Trend")
A10be <- substraction(y1e, y2e, (2011 - 2003)) %>% mutate("Type" = "Observed Trend")
names(A10bw) <- c("Isopycnal", "dpH", "Type")
names(A10be) <- c("Isopycnal", "dpH", "Type")
rm(y1w, y1e, y2w, y2e)

# A17 2013-1994
y1 <- read.csv("./Data/A17/A17-1994.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2 <- read.csv("./Data/A17/A17-2013.txt") %>%
  filter(Depth > 150) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A17a <- substraction(y1, y2, (2013 - 1994)) %>% mutate("Type" = "Observed Trend")
names(A17a) <- c("Isopycnal", "dpH", "Type")
rm(y1, y2)

# A17 2019-2013
y1 <- read.csv("./Data/A17/A17-2013.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2 <- read.csv("./Data/A17/A17-2019.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A17b <- substraction(y1, y2, (2019 - 2013)) %>% mutate("Type" = "Observed Trend")
names(A17b) <- c("Isopycnal", "dpH", "Type")
rm(y1, y2)

# A16 1989-2005
y1 <- read.csv("./Data/A16/A16-1989.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2 <- read.csv("./Data/A16/A16-2005.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A16a <- substraction(y1, y2, (2005 - 1989)) %>% mutate("Type" = "Observed Trend")
names(A16a) <- c("Isopycnal", "dpH", "Type")
rm(y1, y2)

# A16 2005-2013
y1 <- read.csv("./Data/A16/A16-2005.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2 <- read.csv("./Data/A16/A16-2013.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A16b <- substraction(y1, y2, (2013 - 2005)) %>% mutate("Type" = "Observed Trend")
names(A16b) <- c("Isopycnal", "dpH", "Type")
rm(y1, y2)

# A13 2010-1995
y1 <- read.csv("./Data/A13/A13-1995.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

y2 <- read.csv("./Data/A13/A13-2010.txt") %>%
  filter(Depth > 150, Latitude >= -40, Latitude <= -10) %>%
  drop_na(TA, DIC, Sal, Temp) %>%
  mutate(carbsys(.)[6]) %>%
  isopycnal()

A13 <- substraction(y1, y2, (2010 - 1995)) %>% mutate("Type" = "Observed Trend")
names(A13) <- c("Isopycnal", "dpH", "Type")
rm(y1, y2)
west <- bind_rows(A9w, A10aw, A10bw, A17a, A17b, A16a, A16b)
east <- bind_rows(A9e, A10ae, A10be, A13)
rm(A9w, A10aw, A10bw, A17a, A17b, A16a, A16b, A9e, A10ae, A10be, A13)

# Merge the changes by anthropogenic uptake and AOU change -------------------------------
A17.pH <- pH_A17[[3]][[1]] %>% mutate(Type = "Cant change")
A17.AOU <- pH_A17[[3]][[2]] %>% mutate(Type = "AOU change")

A17_2.pH <- pH_A17[[1]][[1]] %>% mutate(Type = "Cant change")
A17_2.AOU <- pH_A17[[1]][[2]] %>% mutate(Type = "AOU change")

A16.pH <- pH_A16[[3]][[1]] %>% mutate(Type = "Cant change")
A16.AOU <- pH_A16[[3]][[2]] %>% mutate(Type = "AOU change")

A16_2.pH <- pH_A16[[1]][[1]] %>% mutate(Type = "Cant change")
A16_2.AOU <- pH_A16[[1]][[2]] %>% mutate(Type = "AOU change")

A13.pH <- pH_A13[[1]][[1]] %>% mutate(Type = "Cant change")
A13.AOU <- pH_A13[[1]][[2]] %>% mutate(Type = "AOU change")

A95.pH <- pH_A95[[1]][[1]] %>% mutate(Type = "Cant change")
A95.AOU <- pH_A95[[1]][[2]] %>% mutate(Type = "AOU change")

A10.pH <- pH_A10[[3]][[1]] %>% mutate(Type = "Cant change")
A10.AOU <- pH_A10[[3]][[2]] %>% mutate(Type = "AOU change")

A10_2.pH <- pH_A10[[1]][[1]] %>% mutate(Type = "Cant change")
A10_2.AOU <- pH_A10[[1]][[2]] %>% mutate(Type = "AOU change")

final <- bind_rows(
  A17.pH, A17.AOU, A17_2.pH, A17_2.AOU, A16.pH, A16.AOU, A16_2.pH,
  A16_2.AOU, A13.pH, A13.AOU, A95.pH, A95.AOU, A10.pH, A10.AOU, A10_2.pH,
  A10_2.AOU
)

rm(
  A17.pH, A17.AOU, A17_2.pH, A17_2.AOU, A16.pH, A16.AOU, A16_2.pH, A16_2.AOU, A13.pH,
  A13.AOU, A95.pH, A95.AOU, A10.pH, A10.AOU, A10_2.pH, A10_2.AOU
)

# Make the individual figures ------------------------------------------------------------
east_plot <- 
  ggplot(
    filter(final, Longitude >= -15, Isopycnal != "iso01"),
    aes(x = dpH, y = reorder(Isopycnal, desc(Isopycnal)), color = Type, fill = Type)
  ) +
  geom_boxplot(color = "#00000020", alpha = 0.2, outlier.shape = NA) +
  geom_hline(
    aes(yintercept = "iso05"), 
    alpha = 0.3, 
    color = "black", 
    linetype = "dotted", 
    size = 0.5
  ) +
  geom_hline(
    aes(yintercept = "iso07"), 
    alpha = 0.3, 
    color = "black", 
    linetype = "dotted", 
    size = 0.5
  ) +
  geom_vline(
    aes(xintercept = 0), 
    alpha = 0.3, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  stat_summary(data = east, fun = mean, geom = "line", aes(group = Type), size = 1.5) +
  stat_summary(fun = mean, geom = "line", aes(group = Type), size = 1.5) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(), 
    axis.text = element_text(colour = "black", size = rel(0.85), face = "bold")
  ) +
  coord_cartesian(xlim = c(-0.004, 0.004)) +
  labs(
    x = "Annual change in pH", 
    y = expression(Neutral ~ density ~ (kg ~ m^{-3})), 
    fill = "Driver", 
    color = "Driver"
  ) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#00000090")) +
  scale_color_manual(values = c("#2b83ba", "#d7191c", "#00000090"))

west_plot <- 
  ggplot(
    filter(final, Longitude <= -15, Isopycnal != "iso01"),
    aes(x = dpH, y = reorder(Isopycnal, desc(Isopycnal)), color = Type, fill = Type)
  ) +
  geom_boxplot(color = "#00000020", alpha = 0.2, outlier.shape = NA) +
  geom_hline(
    aes(yintercept = "iso05"), 
    alpha = 0.3, 
    color = "black", 
    linetype = "dotted", 
    size = 0.5
  ) +
  geom_hline(
    aes(yintercept = "iso07"), 
    alpha = 0.3, 
    color = "black", 
    linetype = "dotted", 
    size = 0.5
  ) +
  geom_vline(
    aes(xintercept = 0), 
    alpha = 0.3, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  stat_summary(data = west, fun = mean, geom = "line", aes(group = Type), size = 1.5) +
  stat_summary(fun = mean, geom = "line", aes(group = Type), size = 1.5) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(), 
    axis.text = element_text(colour = "black", size = rel(0.85), face = "bold")) +
  coord_cartesian(xlim = c(-0.004, 0.004)) +
  labs(
    x = "Annual change in pH", 
    y = expression(Neutral ~ density ~ (kg ~ m^{-3})), 
    fill = "Driver", 
    color = "Driver"
  ) +
  scale_fill_manual(values = c("#2b83ba", "#d7191c", "#00000090")) +
  scale_color_manual(values = c("#2b83ba", "#d7191c", "#00000090"))

# Ensamble the figures -------------------------------------------------------------------
figure5 <- ggarrange(west_plot, east_plot,
  ncol = 2, nrow = 1, align = "v", common.legend = T, legend = "bottom")

rm(
  carbsys, isopycnal, substraction, west, west_plot, east, east_plot, final
)

# Export the results ---------------------------------------------------------------------
if (save_results == TRUE) {
  ggsave(
    "Figure 5.pdf",
    plot = figure5,
    device = "pdf",
    path = "./Results/",
    width = 6,
    height = 7,
    units = "in",
  )
}

