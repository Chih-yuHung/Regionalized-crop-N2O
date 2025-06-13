library(scatterpie)   # make sure the package is loaded
library(scales)       # for comma()

# five break values and their radii
legend_factor <- 1
breaks_ief  <- c(0.005, 0.010, 0.020, 0.050, 0.100)
break_radii <- sqrt(breaks_ief / pi) * scale_factor *legend_factor   # scale_factor = 100
labeller <- function(r){
  # convert radius back to the numeric IEF it represents
  val <- (r / (scale_factor * legend_factor))^2 * pi
  scales::comma(val, accuracy = 0.001)
}

p <- ggplot() +
  geom_scatterpie(
    data   = pie_data,
    aes(x = x, y = y, r = radius),
    cols   = c("Direct_FSN", "Direct_Irrigation"),
    colour = NA
  ) +
  scale_fill_manual(
    name   = "Pathway",
    values = c("Direct_FSN" = "#E34A33",
               "Direct_Irrigation" = "#3182BD")
  ) +
  geom_text_repel(
    data  = pie_data,
    aes(x = x, y = y, label = Region),
    size  = 2
  ) +

  ## ----- custom size legend (no boxes) -----------------
geom_scatterpie_legend(
  radius   = break_radii,
  x        = 1250,      # anchor
  y        =   700,
  colour   = "grey20",
  n        = length(break_radii),
  labeller = labeller
) +
  annotate(
    "text",
    x      = 1250,
    y      = 680 ,
    label = "atop(Total~IEF, (kg~N[2]*O-N/kg~N))",
    parse = TRUE, hjust = 0, vjust = 0, size = 3
    ) +
  coord_equal() +
  labs(
    x = "Weighted mean evapotranspiration (mm)",
    y = "Weighted mean precipitation (mm)",
    title    = expression(Direct~N[2]*O~implied~emission~factors~by~Region),
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position   = c(0.97, 0.97),      # colour legend in top-right
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.8), colour = NA)
  )

ggsave("Figures/Fig2_IEF_piechart.pdf",
       plot   = p,
       width  = 7.09, height = 8.27, units = "in",
       device = cairo_pdf)
