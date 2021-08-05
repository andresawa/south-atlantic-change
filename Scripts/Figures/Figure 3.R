# Andres Piñango. Last change: 05/08/2021
# Description ----------------------------------------------------------------------------
# Figure 3. Annual anthropogenic carbon (Cant) accumulation rates and column inventory
# changes in the zonal (left panels) and meridional (right panels) sections evaluated in
# this study. The dots represent the in-situ data, while the lines show each section
# results, and the colored shadows, their uncertainties.

# Merge Data Zonal sections --------------------------------------------------------------
# Anthropogenic Rates
A95.data <- changes_DIC_A95[[1]]
t1 <- data.frame(A95.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A9.5 (2009-2018)")
t1[, 7] <- t1[, 7] / 9

A10.data <- changes_DIC_A10[[3]]
t2 <- data.frame(A10.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A10 (2003-2011)")
t2[, 7] <- t2[, 7] / 8

A10_2.data <- changes_DIC_A10[[1]]
t3 <- data.frame(A10_2.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A10 (1992-2003)")
t3[, 7] <- t3[, 7] / 11

A105.data <- changes_DIC_A105[[1]]
t4 <- data.frame(A105.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A10.5 (2011-2017)")
t4[, 7] <- t4[, 7] / 6

zonal.centrales <- bind_rows(t1, t2, t3, t4) %>%
  filter(ndens >= 26.2 & ndens <= 27.1 & Ca > 0)

zonal.intermediarias <- bind_rows(t1, t2, t3, t4) %>%
  filter(ndens >= 27.1 & ndens <= 27.65)

rm(t1, t2, t3, t4, A95.data, A10.data, A10_2.data, A105.data)

# Anthropogenic Inventories
A9 <- inventories_A95[[1]] %>% mutate(Type = "A9.5 (2009-2018)")
A10a <- inventories_A10[[1]] %>% mutate(Type = "A10 (1992-2003)")
A10b <- inventories_A10[[3]] %>% mutate(Type = "A10 (2003-2011)")
A105 <- inventories_A105[[1]] %>% mutate(Type = "A10.5 (2011-2017)")
zonal.inventory <- bind_rows(A9, A10a, A10b, A105)
rm(A9, A10a, A10b, A105)

# Merge Data Meridional sections ---------------------------------------------------------
# Anthropogenic Rates
A17.data <- changes_DIC_A17[[3]]
t1 <- data.frame(A17.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A17 (2013-2019)")
t1[, 7] <- t1[, 7] / 6

A17_2.data <- changes_DIC_A17[[1]]
t2 <- data.frame(A17_2.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A17 (1994-2013)")
t2[, 7] <- t2[, 7] / 19

A16.data <- changes_DIC_A16[[3]]
t3 <- data.frame(A16.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A16 (2005-2013)")
t3[, 7] <- t3[, 7] / 8

A16_2.data <- changes_DIC_A16[[1]]
t4 <- data.frame(A16_2.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A16 (1989-2005)--")
t4[, 7] <- t4[, 7] / 16

A13.data <- changes_DIC_A13[[1]]
t5 <- data.frame(A13.data[, c(1:5, 20, 24)]) %>% mutate(Type = "A13 (1995-2010)")
t5[, 7] <- t5[, 7] / 15

meridional.centrales <- bind_rows(t1, t2, t3, t4, t5) %>%
  filter(ndens >= 26.2 & ndens <= 27.1)

meridional.intermediarias <- bind_rows(t1, t2, t3, t4, t5) %>%
  filter(ndens >= 27.1 & ndens <= 27.65)

rm(t1, t2, t3, t4, t5, A17.data, A17_2.data, A16.data, A16_2.data, A13.data)

# Anthropogenic Inventories
A17a <- inventories_A17[[1]] %>% mutate(Type = "A17 (1994-2013)")
A17b <- inventories_A17[[3]] %>% mutate(Type = "A17 (2013-2019)")
A16a <- inventories_A16[[1]] %>% mutate(Type = "A16 (1989-2005)--")
A16b <- inventories_A16[[3]] %>% mutate(Type = "A16 (2005-2013)")
A13 <- inventories_A13[[1]] %>% mutate(Type = "A13 (1995-2010)")
meridional.inventory <- bind_rows(A17a, A17b, A16a, A16b, A13)
rm(A17a, A17b, A16a, A16b, A13)

# Make the individual figures ------------------------------------------------------------
# Accumulation rate Zonal Centrales
zonal.central <- ggplot(zonal.centrales, 
                        aes(Longitude, Ca, colour = Type, fill = Type, shape = Type)
                        ) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(0, 2)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(C[ant] ~ accumulation ~ rate ~ (μmol ~ kg^{-1} ~ yr^{-1})),
    fill = "Zonal Section",
    color = "Zonal Section",
    shape = "Zonal Section"
  ) +
  theme(
    axis.text = element_text(colour = "black", 
                             size = rel(0.85), 
                             face = "bold" 
                             ),
    legend.direction = "vertical", legend.title.align = 0.5
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

# Accumulation rate Zonal Intermediarias
zonal.inter <- ggplot(zonal.intermediarias, 
                      aes(Longitude, Ca, colour = Type, fill = Type, shape = Type)
                      ) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(-0.5, 1.5)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(C[ant] ~ accumulation ~ rate ~ (μmol ~ kg^{-1} ~ yr^{-1})),
    fill = "Zonal Section",
    color = "Zonal Section",
    shape = "Zonal Section"
  ) +
  theme(
    axis.text = element_text(colour = "black", 
                             size = rel(0.85), 
                             face = "bold"
                             ),
    legend.direction = "vertical", legend.title.align = 0.5
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

# Inventory Total Zonal
f.zonal <- ggplot(zonal.inventory, 
                  aes(Longitude, Inventory, colour = Type, fill = Type, shape = Type)) +
  geom_ribbon(
    aes(ymin = Inventory - (Inventory * 0.20), 
        ymax = Inventory + (Inventory * 0.20)), 
    alpha = 0.20, linetype = "blank"
  ) +
  geom_line(size = 1.25) +
  theme_bw() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-50, 16), ylim = c(0, 2.5)) +
  scale_x_continuous(breaks = seq(-50, 10, by = 10)) +
  labs(
    x = "Longitude (°)",
    y = expression(Column ~ Inventory ~ Change ~ (mol ~ m^{-2} ~ yr^{-1})),
    fill = "Zonal Section",
    color = "Zonal Section",
    shape = "Zonal Section"
  ) +
  theme(
    axis.text = element_text(colour = "black", 
                             size = rel(0.85), 
                             face = "bold" 
                             ),
    legend.direction = "vertical", legend.title.align = 0.5
  ) +
  geom_hline(
    aes(yintercept = 0), 
    alpha = 0.5, color = "black", 
    linetype = "longdash", 
    size = 0.5
  ) +
  guides(
    fill = guide_legend(nrow = 2), 
    color = guide_legend(nrow = 2), 
    shape = guide_legend(nrow = 2)
  )


# Accumulation rate Meridional Centrales
meridional.central <- ggplot(meridional.centrales, 
                             aes(Latitude, Ca, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(0, 2)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(C[ant] ~ accumulation ~ rate ~ (μmol ~ kg^{-1} ~ yr^{-1})),
    fill = "Meridional Section",
    color = "Meridional Section",
    shape = "Meridional Section"
  ) +
  theme(
    axis.text = element_text(colour = "black", 
                             size = rel(0.85), 
                             face = "bold" 
                             ),
    legend.direction = "vertical", legend.title.align = 0.5
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

# Accumulation rate Meridional Intermediarias
meridional.inter <- ggplot(meridional.intermediarias, 
                           aes(Latitude, Ca, colour = Type, fill = Type, shape = Type)) +
  geom_point(alpha = 0.10, size = 1) +
  geom_smooth(method = "loess", alpha = 0.20) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(-0.5, 1.5)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(C[ant] ~ accumulation ~ rate ~ (μmol ~ kg^{-1} ~ yr^{-1})),
    fill = "Meridional Section",
    color = "Meridional Section",
    shape = "Meridional Section"
  ) +
  theme(
    axis.text = element_text(colour = "black", 
                             size = rel(0.85), 
                             face = "bold" 
                             ),
    legend.direction = "vertical", legend.title.align = 0.5
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

# Inventory Total Meridional
f.meridional <- ggplot(meridional.inventory, 
                       aes(Latitude, Inventory, colour = Type, fill = Type, shape = Type)) +
  geom_ribbon(
    aes(ymin = Inventory - (Inventory * 0.20), 
        ymax = Inventory + (Inventory * 0.20)), 
    alpha = 0.20, linetype = "blank"
  ) +
  geom_line(size = 1.25) +
  theme_bw() +
  scale_color_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_fill_manual(values = c("#78c679", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) +
  scale_shape_manual(values = c(21:25)) +
  coord_cartesian(xlim = c(-40, -10), ylim = c(0, 2)) +
  scale_x_continuous(breaks = c(-40, -35, -30, -25, -20, -15, -10)) +
  labs(
    x = "Latitude (°)",
    y = expression(Column ~ Inventory ~ Change ~ (mol ~ m^{-2} ~ yr^{-1})),
    fill = "Meridional Section",
    color = "Meridional Section",
    shape = "Meridional Section"
  ) +
  theme(
    axis.text = element_text(colour = "black", 
                             size = rel(0.85), 
                             face = "bold" 
                             ),
    legend.direction = "vertical", legend.title.align = 0.5
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
col1 <- ggarrange(zonal.central, 
                  zonal.inter,
                  f.zonal,
                  ncol = 1, 
                  nrow = 3, 
                  align = "v", 
                  common.legend = TRUE, 
                  legend = "bottom"
)
col2 <- ggarrange(meridional.central, 
                  meridional.inter, 
                  f.meridional,
                  ncol = 1, 
                  nrow = 3, 
                  align = "v", 
                  common.legend = TRUE, 
                  legend = "bottom"
)

figure3 <- ggarrange(col1, 
                     col2,
                     ncol = 2, 
                     nrow = 1, 
                     align = "v", 
                     common.legend = FALSE, 
                     legend = "bottom"
)

rm(
  col1, col2, f.meridional, meridional.inter, meridional.central, f.zonal, zonal.inter,
   zonal.central, meridional.inventory, meridional.intermediarias, meridional.centrales,
   zonal.inventory, zonal.intermediarias, zonal.centrales
  )

# Export the results ---------------------------------------------------------------------
if (save_results == TRUE) {
  ggsave(
    "Figure 3.pdf",
    plot = figure3,
    device = "pdf",
    path = "./Results/",
    width = 8.5,
    height = 10,
    units = "in",
  )
}
