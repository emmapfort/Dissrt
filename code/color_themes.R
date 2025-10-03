#Define Colors for All Plots

#Stimulus colors
# stim_col <- c(
#   "TUN" = "#5A874A",
#   "THA" = "#CAF9BA",
#   "DOX" = "#3465A4",
#   "NUTL" = "#AFCDF9",
#   "DMSO" = "#BBBBBB",
#   "LPS" = "#B04C8C",
#   "TNFa" = "#FACDEA",
#   "H2O" = "#888888",
#   "BPA" = "#6C63A6",
#   "PFOA" = "#D7D3F4",
#   "EtOH" = "#666666"
# )

stim_col <- c(
  "TUN" = "#5A874A",
  "THA" = "#C4E1AE",
  "DOX" = "#3B4DA3",
  "NUTL" = "#A0CFF8",
  "DMSO" = "#999999",
  "LPS" = "#C0329A",
  "TNFa" = "#F09DD6",
  "H2O" = "#777777",
  "BPA" = "#7359A6",
  "PFOA" = "#D7BDF3",
  "EtOH" = "#555555"
)

# saveRDS(stim_col, "data/theme/stimulus_color_palette_all.RDS")

#Response colors
resp_col <- c(
  "UPR" = "#94C47D",
  "DDR" = "#6BA3F3",
  "IMR" = "#F386CB",
  "MMR" = "#A79EE0",
  "VEH" = "#BBBBBD"
)

# saveRDS(resp_col, "data/theme/response_category_color_palette.RDS")

ind_col <- c(
  Ind1 = "#264653",
  Ind2 = "#2A9D8F",
  Ind3 = "#06D6A0",
  Ind4 = "#68DD94",
  Ind5 = "#22C75E",
  Ind6 = "#2C5530",
  Ind7 = "#FFB347",
  Ind8 = "#F97316",
  Ind9 = "#F25401",
  Ind10 = "#F44E53",
  Ind11 = "#C6134F",
  Ind12 = "#F03A6E",
  Ind13 = "#5D0E70",
  Ind14 = "#A069E0"
)

# saveRDS(ind_col, "data/theme/individual_category_color_palette.RDS")

spec_col <- c(
  "C" = "#A6AA59",
  "H" = "#740949"
)

# saveRDS(spec_col, "data/theme/species_category_color_palette.RDS")

time_col <- c(
  "2" = "#D9AE61",
  "24" = "#585829"
)

 # saveRDS(time_col, "data/theme/time_category_color_palette.RDS")



#test all of these in swatches
make_df <- function(pal, name) {
  data.frame(
    key = names(pal),
    color = unname(pal),
    palette = name,
    stringsAsFactors = FALSE
  )
}

all_palettes <- bind_rows(
  make_df(ind_col, "Individuals"),
  make_df(spec_col, "Species"),
  make_df(stim_col, "Stimuli"),
  make_df(resp_col, "Response"),
  make_df(time_col, "Time")
)

# ---- Plot ----
ggplot(all_palettes, aes(x = key, y = 1, fill = color, label = key)) +
  geom_tile(color = "white", height = 1) +      # thick swatches
  geom_text(size = 3, color = "black") +          # labels on swatches
  scale_fill_identity() +                         # use hex colors directly
  facet_wrap(~palette, scales = "free_x", ncol = 1) +  # one row per palette
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(title = "Color Palettes: Individuals, Species, Stimuli, Response, Time")
