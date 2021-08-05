# Andres Piñango. Last change: 05/08/2021
# Description ----------------------------------------------------------------------------
# Figure 6. Annual acidification rates and annual changes in the calcite saturation state,
# the hydrogen ion concentration and the Revelle factor as a consequence of the absorption
# of anthropogenic carbon for the central waters of the zonal (left) and meridional
# (right) sections evaluated in this study. The dots represent the in-situ data, while the
# lines were obtained by local polynomial regression fitting.

# Merge Data Zonal sections --------------------------------------------------------------
A95.pH <- pH_A95[[1]][[1]] %>% mutate(Type = "A9.5 (2009-2018)")
A10.pH <- pH_A10[[3]][[1]] %>% mutate(Type = "A10 (2003-2011)")
A10_2.pH <- pH_A10[[1]][[1]] %>% mutate(Type = "A10 (1992-2003)")
A105.pH <- pH_A105[[1]][[1]] %>% mutate(Type = "A10.5 (2011-2017)")

centrales <- bind_rows(A95.pH, A10.pH, A10_2.pH, A105.pH) %>%
  filter(ndens >= 26.2 & ndens <= 27.1)
rm(A95.pH, A10.pH, A10_2.pH, A105.pH)

# Merge Data Meridional sections ---------------------------------------------------------
A17.pH <- pH_A17[[3]][[1]] %>% mutate(Type = "A17 (2013-2019)")
A17_2.pH <- pH_A17[[1]][[1]] %>% mutate(Type = "A17 (1994-2013)")
A16.pH <- pH_A16[[3]][[1]] %>% mutate(Type = "A16 (2005-2013)")
A16_2.pH <- pH_A16[[1]][[1]] %>% mutate(Type = "A16 (1989-2005)")
A13.pH <- pH_A13[[1]][[1]] %>% mutate(Type = "A13 (1995-2010)")

centrales_2 <- bind_rows(A17.pH, A17_2.pH, A16.pH, A16_2.pH, A13.pH) %>%
  filter(ndens >= 26.2 & ndens <= 27.1)
rm(A17.pH, A17_2.pH, A16.pH, A16_2.pH, A13.pH)

# Make the individual figures ------------------------------------------------------------
pH_z <- ggplot(centrales, 
               aes(Longitude, dpH, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(-0.005, 0)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(Annual ~ acidification ~ rate),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

calcita_z <- ggplot(centrales, 
                    aes(Longitude, dOC, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(-0.03, 0)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(Annual ~ Ω[Ca] ~ change),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

aragonito_z <- ggplot(centrales, 
                      aes(Longitude, dOA, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(-0.025, 0)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(Annual ~ Ω[Ar] ~ change),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

revelle_z <- ggplot(centrales, 
                    aes(Longitude, R, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(0, 0.07)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(Annual ~ Revelle ~ factor ~ change),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

H_z <- ggplot(centrales, 
              aes(Longitude, dH, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(0, 0.15)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression("[" ~ H^{"+"} ~ "]" ~ change ~ (nmol ~ kg^{-1} ~ yr^{-1})),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

pH_m <- ggplot(centrales_2, 
               aes(Latitude, dpH, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(-0.005, 0)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(Annual ~ acidification ~ rate),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

calcita_m <- ggplot(centrales_2, 
                    aes(Latitude, dOC, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(-0.03, 0)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(Annual ~ Ω[Ca] ~ change),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

aragonito_m <- ggplot(centrales_2, 
                      aes(Latitude, dOA, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(-0.025, 0)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(Annual ~ Ω[Ar] ~ change),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

revelle_m <- ggplot(centrales_2, 
                    aes(Latitude, R, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(0, 0.07)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(Annual ~ Revelle ~ factor ~ change),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

H_m <- ggplot(centrales_2, 
              aes(Latitude, dH, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(0, 0.15)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression("[" ~ H^{"+"} ~ "]" ~ change ~ (nmol ~ kg^{-1} ~ yr^{-1})),
    fill = "Section",
    color = "Section",
    shape = "Section"
  ) +
  theme(axis.text = element_text(colour = "black", 
                                 size = rel(0.85), 
                                 face = "bold")
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, 
    color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )

# Ensamble the figures -------------------------------------------------------------------
zonales <- ggarrange(pH_z, 
                     aragonito_z, 
                     H_z, 
                     revelle_z,
                     ncol = 1, 
                     nrow = 4, 
                     align = "v", 
                     common.legend = TRUE, 
                     legend = "bottom"
)
meridionales <- ggarrange(pH_m, 
                          aragonito_m, 
                          H_m, 
                          revelle_m,
                          ncol = 1, 
                          nrow = 4, 
                          align = "v", 
                          common.legend = TRUE, 
                          legend = "bottom"
)
figure6 <- ggarrange(zonales, 
                     meridionales,
                     ncol = 2, 
                     nrow = 1, 
                     align = "v", 
                     common.legend = FALSE, 
                     legend = "bottom"
)

rm(
  meridionales, zonales, H_m, calcita_m, aragonito_m, revelle_m, pH_m, H_z, calcita_z,
  aragonito_z, revelle_z, pH_z, centrales, centrales_2
)
# Export the results ---------------------------------------------------------------------
if (save_results == TRUE) {
  ggsave(
    "Figure 6.pdf",
    plot = figure6,
    device = "pdf",
    path = "./Results/",
    width = 8.5,
    height = 10,
    units = "in",
  )
}
