library(here)
library(ggplot2)
library(tidyverse)
library(glue)

save_folder <- "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/Conferences/ISCB 2024"

# Plot 1: 
sim_name <- "240124-pred-new-data"
results_folder <- here::here("Results", "Database-extracts", sim_name)


var_u <- readRDS(file = here(results_folder, "var_u.RDS"))

myR2 <- 0.4  
plot_data <- var_u |> 
    filter(predict_method == "new0", 
           R2 == myR2, 
           ICC == 0.05,
           study_sample_size_train == 200,
           n_studies <20) |> 
  mutate(model = case_when(model == "Random intercept - REML" ~ "REML",
                           model == "Random intercept - ML" ~ "Maximum Liklihood"))

  plot <- plot_data |> 
    box_plot_error_var_iscb()  # ProfacSims:::
  
  var_u_plot <- plot + 
    labs(
      x = "Number of studies",
      y = "Estiamte",
      title = "Random intercept variance",
      subtitle = glue("R\u00b2 = {myR2}, ICC = 0.05, study size = 200"),
      caption = "Dashed line shows true random intercept variance",
      color = ""
      ) +
    theme_minimal(base_size = 18) +
    theme(legend.position = "top") + 
    theme(plot.title = element_text(hjust = 0.5))
  var_u_plot

ggsave(plot = var_u_plot, filename =  file.path(save_folder, "var_u_plot.png"), width = 14, height = 14, units = "cm")


# 2 Calibration in the large

calib_itl_stacked <- readRDS(file = here(results_folder, "calib_itl_stacked.RDS"))


plot_data <- calib_itl_stacked |> 
  filter(
    predict_method != "new_dynamic", 
    n_studies ==8, 
    study_sample_size_train ==200,
    ICC == 0.05, R2 == 0.4) 

plot <- plot_data |> 
  ProfacSims:::box_plot_by_pred_ss(what = "tau") #

plot_calib_itl <- plot + 
  labs(
    title = "Calibration in the large",
    subtitle = "Training data: 8 studies of size 200",
    color = ""
  ) +
  ylab("Between-study hetrogeneity (Tau)") +
  theme_minimal(base_size = 18 ) +
  theme(legend.position = "top",
        legend.text=element_text(size=12)) +
  facet_grid()
plot_calib_itl

ggsave(plot = plot_calib_itl, filename =  file.path(save_folder, "calib_itl_plot.png"), width = 14, height = 14, units = "cm")

# 3 Calibration slope: Sample size

# Loading files:
results_folder_many_pred <- here("Results", "Database-extracts", "240211-many-pred")

data_many_pred <- readRDS(file = here(results_folder_many_pred, "calib_slope.RDS")) |> 
 
  
plot_data <-  data_many_pred |> 
  filter(n_predictors == 30, 
         R2 == 0.30,
         n_studies == 16,
         ICC == 0.05,
         study_sample_size_train ==50) 

plot_caib_slope <- plot_data |>
  ProfacSims:::box_plot_by_pred_ss(what = "est") +
  labs(
    title = "No benifit from parsimony",
    subtitle = "16 studies of size 50, R\u00b2 = 0.3, ICC = 0.05",
    color = "",
    y = "Calibration slope"
  ) +
  ylab("Calibration slope") +  
  theme_minimal(base_size = 17 ) +
  theme(legend.position = "top",
        legend.text=element_text(size=11)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_grid()


print(plot_caib_slope)
ggsave(plot = plot_caib_slope, filename =  file.path(save_folder, "plot_calib_slope.png"), width = 14, height = 14, units = "cm")

# 4: Level 2 endogeneity

sim_name <- "240206-lvl2-endo"
results_folder <- here("Results", "Database-extracts", sim_name)
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#CC79A7")
calib_slope <- readRDS(file = here(results_folder, "calib_slope.RDS")) |> 
  filter(model != "Random intercept - ML", model != "Hausman - centred") |> 
  ProfacSims:::correct_model_spelling()


my_n_studies <- 8
my_icc <- 0.05


calib_slope$plot_value = calib_slope$est
plot_data <- calib_slope |> filter(n_studies ==my_n_studies,
                                   study_sample_size_train ==50,
                                   int_pred_corr == 0.5) 
plot_endo <- plot_data |> 
  ProfacSims:::box_plot_lvl2_endo_main() + #  
  ylab(glue("Calibration Slope")) +
  labs(
    title = "Impact of Level 2 endogeneity",
    subtitle = glue("{my_n_studies} studies of size 50, ICC = 0.05 \n Int-Pred corr: 0.5"),
    color = ""
  ) +  
  scale_y_continuous(limits = c(0.5, 1.5)) +
  facet_grid() + 
  theme_minimal(base_size = 18 ) +
  theme(legend.position = "top",
        legend.text=element_text(size=11))
  




print(plot_endo)


ggsave(plot = plot_endo, filename =  file.path(save_folder, "plot_endo.png"), width = 14, height = 14, units = "cm")
