#flow cytometry results boxplots Stressor Proj

####cell lines####
# ##H21792
# H21792_flow <- c(80.9)
# ##C3649
# C3649_flow <- c(99.0)
# ##H20682
# H20682_flow <- c(93.9)
# ##H28126
# H28126_flow <- c(98.7)
# ##C3651
# C3651_flow <- c(84.7)
# ##C4955
# C4955_flow <- c(99.1)
# ##C8861
# C8861_flow <- c(98.9)
# ##C40210
# C40210_flow <- c(99.0)
# ##H22422
# H22422_flow <- c(98.7)
# ##H24280
# H24280_flow <- c(98.9)
# ##84-1
# H84_1_flow <- c(98.3)
# ##C3647
# C3647_flow <- c(94.1)
# ##C40280
# C40280_flow <- c(99.4)

#library(readr)
flow_stress <- read_csv("C:/Users/emmap/OneDrive/Desktop/Ward Lab/Experiments/Stressor Project/Flow Cytometry/StressorProj_FlowPurities_EMP_250226.csv")
View(flow_stress)

# saveRDS(flow_stress, "data/flow_purity_data.RDS")

flow <- readRDS("data/flow_purity_data.RDS")

# flow <- flow_stress

####now that I've loaded in the lenient gating number for each, let's make a boxplot

flow %>% ggplot(aes(x=Line, y=Purity, color = Ind)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  labs(title = "Percent cTnT Positive iPSC-CMs") +
  ylim(0,105) +
  scale_color_manual(values = ind_col)


##make subsets based on species
H_flow <- flow_stress %>% filter(Species=="Human")
C_flow <- flow_stress %>% filter(Species=="Chimp")

flow_stress %>% ggplot(aes(x=Species, y=Purity, color = Line)) +
  geom_boxplot() +
  theme_bw(base_size = 15) +
  labs(title = "Percent cTnT Positive iPSC-CMs") +
  ylim(0,105) +
  scale_color_manual(values = flow_col_ind)

ggplot(H_flow, aes(x = Species, y = Purity))+
  geom_boxplot(aes(fill = Species), show.legend = FALSE)+
  geom_point(aes(color = Line, size = 5, alpha = 0.9), show.legend = leg_show) +
  ylim(0,105) +
  labs(title = "Percent cTnT Positive iPSC-CMs")+
  theme_bw(base_size = 16)+
  scale_color_manual(values = flow_col_h)+
  scale_fill_manual(values = c("lightyellow"))

ggplot(C_flow, aes(x = Species, y = Purity))+
  geom_boxplot(aes(fill = Species), show.legend = FALSE)+
  geom_point(aes(color = Line, size = 5, alpha = 0.9), show.legend = leg_show) +
  ylim(0,105) +
  labs(title = "Percent cTnT Positive iPSC-CMs")+
  theme_bw(base_size = 16)+
  scale_color_manual(values = flow_col_c)+
  scale_fill_manual(values = c("lightblue"))


ggplot(flow_stress, aes(x = Species, y = Purity))+
  geom_boxplot(aes(fill = Species, shape = Species), show.legend = FALSE)+
  geom_point(aes(color = Line, size = 5, alpha = 0.9), show.legend = leg_show) +
  ylim(0,105) +
  labs(title = "Percent cTnT Positive iPSC-CMs")+
  theme_bw(base_size = 20)+
  scale_color_manual(values = flow_col_ind)+
  scale_fill_manual(values = c("lightblue", "lightyellow"))

leg_show <- c(size = FALSE, color = TRUE, alpha = FALSE)

flow_col_ind <- c(H21792 = "#4DD091", H20682 = "#FF5C77", H28126 = "#FFA23A", H24280 = "#C05780", H22422 = "#F4D283",'H84-1' = "#ABD33D",'H78-1'= "#FFF000",
                  C3647 = "#00B0BA", C3649 = "#FFADD9", C8861 = "#FC6238", C3651 = "#FF60AB", C4955 = "#FF0BE4", C40210 = "#00CDAC", C40280 = "#BAF499")

flow_col_h <- c(H21792 = "#4DD091", H20682 = "#FF5C77", H28126 = "#FFA23A", H24280 = "#C05780", H22422 = "#F4D283",'H84-1' = "#ABD33D",'H78-1'= "#FFF000")

flow_col_c <- c(C3647 = "#00B0BA", C3649 = "#FFADD9", C8861 = "#FC6238", C3651 = "#FF60AB", C4955 = "#FF0BE4", C40210 = "#00CDAC", C40280 = "#BAF499")


#also make a barplot for this
#change the individual colors

ind_col <- readRDS("data/theme/individual_category_color_palette.RDS")

# ind_fullnames <- list(
#   Ind1 = "C3647",
#   Ind2 = "H24280",
#   Ind3 = "C8861",
#   Ind4 = "H28126",
#   Ind5 = "C40210",
#   Ind6 = "H84-1",
#   Ind7 = "C3649",
#   Ind8 = "H21792",
#   Ind9 = "C3651",
#   Ind10 = "H20682",
#   Ind11 = "H224222",
#   Ind12 = "C40280",
#   Ind13 = "H78-1",
#   Ind14 = "C4955"
# )
#
# ind_lines <- unlist(ind_fullnames, use.names = TRUE)

# saveRDS(ind_lines, "data/theme/cell_lines_individual_vector.RDS")


flow <- flow %>%
  dplyr::select(-Median)

flow_plot <- flow %>%
  mutate(
    Ind_num = as.numeric(gsub("Ind", "", Ind)),   # extract number
    species = factor(species, levels = c("Chimp", "Human")),  # order species
    Ind = factor(Ind, levels = flow %>%
                   arrange(species, Ind_num) %>%
                   pull(Ind))
  )

flow <- flow %>%
  mutate(species = factor(species, levels = c("Chimp", "Human")))

library(dplyr)
library(ggplot2)
library(tidyr)

# ---- 1. Ensure species is a factor with correct order ----
flow <- flow %>%
  mutate(
    species = as.character(Species),
    species = factor(species, levels = c("Human", "Chimp")),
    Ind = factor(Ind, levels = paste0("Ind", 1:14))  # make sure Ind is ordered
  )

flow <- flow %>%
  arrange(species, Ind)


flow_barplot <- ggplot(flow, aes(x = Ind, y = Purity, fill = Ind)) +
  geom_col(aes(group = species), position = position_dodge(width = 0.3), width = 0.9) +
  facet_wrap(~species, scales = "free_x", nrow = 1) +  # Chimps and Humans separated
  scale_fill_manual(values = ind_col) +                # your named vector of colors
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # bottom at 0
  labs(x = "Individual", y = "TNNT2 expression (%)", fill = "Individual") +
  theme_custom() +
  theme(legend.position = "none")

flow <- flow %>%
  mutate(
    species = as.character(Species),
    species = factor(species, levels = c("Human", "Chimp")),
    Ind = factor(Ind, levels = paste0("Ind", 1:14))  # ensure proper order
  ) %>%
  arrange(species, Ind)

flow_barplot <- ggplot(flow, aes(x = Ind, y = Purity, fill = Ind)) +
  geom_col(aes(group = species), position = position_dodge(width = 0.4), width = 0.9) +
  facet_wrap(~species, scales = "free_x", nrow = 1) +  # Humans left, Chimps right
  scale_fill_manual(values = ind_col) +                # your named vector of colors
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # bottom at 0
  labs(x = "Individual", y = "TNNT2 expression (%)") +
  theme_custom() +
  theme(legend.position = "none")



save_plot(
  plot = flow_barplot,
  filename = "Flow_Barplot_Individuals_EMP",
  folder = output_folder,
  height = 4, width = 9
)


human_inds <- paste0("Human_Ind", c(2,4,6,8,10,11,13))
chimp_inds <- paste0("Chimp_Ind", c(1,3,5,7,9,12,14))

# Combine into a single factor with desired order
x_levels <- c(human_inds, chimp_inds)

# Create x_axis with correct factor levels
flow <- flow %>%
  mutate(
    # Add a dummy factor level to create a gap
    flow <- flow %>%
      mutate(
        x_axis = factor(Ind, levels = Ind),  # keep original order
        species = factor(Species, levels = c("Human", "Chimp"))
      ))

    # Plot with a small gap between species
    flow_barplot <- ggplot(flow, aes(x = x_axis, y = Purity, fill = Ind)) +
      geom_col(width = 0.9) +
      scale_fill_manual(values = ind_col) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # bottom at 0
      labs(x = "Individual", y = "TNNT2 expression (%)") +
      theme_custom() +
      theme(
        legend.position = "none"
      ) +
      # add separation by introducing a small space
      scale_x_discrete(expand = expansion(add = c(0.5, 0.5)))

    flow_barplot


    # Ensure species is a factor with desired order
    flow <- flow %>%
      mutate(
        species = factor(Species, levels = c("Human", "Chimp")),
        Ind = factor(Ind, levels = paste0("Ind", 1:14))  # keep number order
      )

    # Faceted barplot
    flow_barplot <- ggplot(flow, aes(x = Ind, y = Purity, fill = Ind)) +
      geom_boxplot(aes(fill = species))
      geom_col(width = 0.9) +
      scale_fill_manual(values = ind_col) +                # named colors
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # bottom at 0
      labs(x = "Individual", y = "TNNT2 expression (%)") +
      theme_custom() +
      theme(legend.position = "none") +
      facet_wrap(~species, scales = "free_x", nrow = 1)   # Humans left, Chimps right

    flow_barplot




  #now make a boxplot for each species like this, faceted by species
    # Prepare factors
    chimp_inds <- paste0("Ind", c(1,3,5,7,9,12,14))
    human_inds <- paste0("Ind", c(2,4,6,8,10,11,13))

    # Combine in the order you want
    x_levels <- c(chimp_inds, human_inds)

    # Factor Ind according to desired order
    flow <- flow %>%
      mutate(
        Ind = factor(Ind, levels = x_levels),
        Species = factor(Species, levels = c("Human", "Chimp")),
        species_short = ifelse(Species == "Human", "H",
                               ifelse(Species == "Chimp", "C", NA))
      )

    p_tnnt2 <- ggplot(flow, aes(x = Species, y = Purity, fill = species_short)) +
      # Boxplots colored by species
      geom_boxplot(width = 0.6, alpha = 0.9, color = "black", outlier.shape = NA) +
      # Points colored by individual using Ind
      geom_point(aes(color = Ind),
                 position = position_identity(),
                 size = 3, alpha = 0.8, stroke = 0) +
      # Manual fills
      scale_fill_manual(values = species_col) +
      scale_color_manual(values = ind_col) +
      # Wilcoxon test line above boxes
      stat_compare_means(
        method = "wilcox.test",
        aes(label = ..p.signif..),
        label.y = 102,       # slightly above 100
        tip.length = 0.03    # optional small line tips
      ) +
      # y-axis limits 0-100
      scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
      labs(x = "", y = "TNNT2 Expression (%)") +
      theme_custom() +
      guides(
        fill = guide_legend(override.aes = list(color = NA)),
        color = guide_legend(override.aes = list(size = 3))
      ) +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_blank(),          # no title
        panel.spacing.x = unit(0.32, "in")       # small gap between species
      )



    #print
    print(p_tnnt2)


    purity_human <- flow$Purity[flow$Species == "Human"]
    purity_chimp <- flow$Purity[flow$Species == "Chimp"]

    # Wilcoxon rank-sum test (unpaired)
    wilcox_res <- wilcox.test(purity_human, purity_chimp)

    # Show the result
    wilcox_res


    #save using custom function
    save_plot(
      plot = p_tnnt2,
      filename = "TNNT2_Purity_Boxplot_Human_vs_Chimp",
      folder = output_folder,
      width = 9.94,
      height = 4.05
    )




    # --- Species legend (H first) ---
    p_species_legend <- ggplot(flow, aes(x = Species, y = Purity, fill = species_short)) +
      geom_boxplot() +
      scale_fill_manual(
        values = species_col,
        breaks = c("H", "C"),
        labels = c("Human", "Chimp")
      ) +
      guides(
        fill = guide_legend(
          title = "Species",
          override.aes = list(color = NA)
        )
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "plain"),
        legend.text = element_text(face = "plain")
      )

    legend_species <- get_legend(p_species_legend)

    # --- Individual legend (Ind1–Ind14 numeric order) ---
    p_ind_legend <- ggplot(flow, aes(x = Species, y = Purity, color = Ind)) +
      geom_point() +
      scale_color_manual(
        values = ind_col,
        breaks = paste0("Ind", 1:14)
      ) +
      guides(
        color = guide_legend(
          title = "Individual",
          override.aes = list(size = 3)
        )
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "plain"),
        legend.text = element_text(face = "plain")
      )

    legend_ind <- get_legend(p_ind_legend)

    # --- Combine legends in one horizontal row ---
    legends_combined <- plot_grid(
      legend_species,
      legend_ind,
      ncol = 1,
      align = "v",
      rel_heights = c(0.2, 0.8)  # adjust relative space balance
    )

    # --- Print just the legends (no plot) ---
    print(legends_combined)

    # --- Save legends separately for cropping ---
    save_plot(
      plot = legends_combined,
      filename = "TNNT2_legends_combined",
      folder = output_folder,
      width = 3.5,
      height = 9.94
    )

    # --- Optionally add them to your main plot on the right ---
    final_plot_with_legends <- plot_grid(
      p_tnnt2 + theme(legend.position = "none"),
      legends_combined,
      ncol = 2,
      rel_widths = c(1, 0.3)
    )

    print(final_plot_with_legends)

    save_plot(
      plot = final_plot_with_legends,
      filename = "TNNT2_Purity_Boxplot_for_legends",
      folder = output_folder,
      width = 9.94,
      height = 4.05
    )
