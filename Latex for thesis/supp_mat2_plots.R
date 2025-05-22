library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)
library(here)
library(glue)

sim_name <- "240211_cluster-x"
results_folder <- here("Results", "Database-extracts", sim_name)



# Loading files:
sim_name <- "240211_cluster-x"
results_folder <- here("Results", "Database-extracts", sim_name)
supp_mat_name <- "supp_mat_pt2"
plots_folder <- here("Latex for thesis", "supp_plots", supp_mat_name)
latex_text <- ""
plot_height <- 24

studies_vector <- c(4, 8, 16, 32)
icc_vector <- c(0.05, 0.2)

add_figure_to_latex <- function(latex_text, plot_name_ext, caption, supp_mat_name) {
  latex_text <- glue::glue("
  {latex_text}
  \\begin{{figure}}[H]
    \\centering
    \\includegraphics[width=1\\linewidth]{{Ch05: Simulations/supp_plots/{supp_mat_name}/{plot_name_ext}}}
    \\caption{{{caption}}}
  \\end{{figure}}
  
")
  return(latex_text)
}

add_clear_page <- function(latex_text) {
  glue::glue("
  {latex_text}
  \\clearpage
       ")
}


add_subsection <- function(latex_text, header) {
  glue::glue("
  {latex_text}
  \\subsection{{{header}}}
       ")
}

add_subsubsection <- function(latex_text, header) {
  glue::glue("
  {latex_text}
  \\subsubsection{{{header}}}
       ")
}



## Calibration in the large
latex_text <- latex_text |> add_subsection("Pooled calibration in the large")

calib_itl <- readRDS(file = here(results_folder, "calib_itl.RDS")) |> 
  filter(model != "Random intercept - ML", model != "Hausman - centred") |> 
  ProfacSims:::correct_model_spelling()

### Estimates

  calib_itl$plot_value = calib_itl$est
  for(my_n_studies in studies_vector){
    plot_data <- calib_itl |> filter(n_studies ==my_n_studies) 
    plot <- plot_data |> 
      ProfacSims:::box_plot_cluster_x_sup() + #
      ylab(glue("Calibration In the Large")) +
      scale_y_continuous(limits = c(-0.25,0.25)) 

  plot
  caption <- glue("Calibration in the large, training data {my_n_studies} studies")
  plot_name <- glue("calib_itl{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)

}     



### Hetrogeneity
latex_text <- latex_text |> add_subsection("Between-study hetrogeneity in calibration in the large")

calib_itl$plot_value = sqrt(calib_itl$tau2)
for(my_n_studies in studies_vector){
  plot_data <- calib_itl |> filter(n_studies ==my_n_studies) 
  plot <- plot_data |> 
    ProfacSims:::box_plot_cluster_x_sup() + #
    ylab(glue("Tau")) +
    ylab("Tau")
  
  if(my_n_studies <20){
    plot <- plot +  scale_y_continuous(limits = c(0, 0.5))
  }
  plot
  caption <- glue("Hetrogeneity in Calibration in the large, training data {my_n_studies} studies")
  plot_name <- glue("calib_itl_hetro{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
}     




## Calibration Slope

calib_slope <- readRDS(file = here(results_folder, "calib_slope.RDS")) |> 
  filter(model != "Random intercept - ML", model != "Hausman - centred") |> 
  ProfacSims:::correct_model_spelling()

### Estimates

latex_text <- latex_text |> add_subsection("Pooled calibration slope")

calib_slope$plot_value = calib_slope$est
for(my_n_studies in studies_vector){
  plot_data <- calib_slope |> filter(n_studies ==my_n_studies) 
  plot <- plot_data |> 
    ProfacSims:::box_plot_cluster_x_sup() + #
    ylab(glue("Calibration Slope")) +
    scale_y_continuous(limits = c(0.75, 1.75))+
    geom_hline(yintercept = 1,  linetype = 2)
  
  plot
  caption <- glue("Calibraiton slope, training data {my_n_studies} studies")
  plot_name <- glue("calib_slope{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
}     



### Hetrogeneity
latex_text <- latex_text |> add_subsection("Between-study hetrogeneity in calibration slope")

calib_slope$plot_value = sqrt(calib_slope$tau2)
for(my_n_studies in studies_vector){
  plot_data <- calib_slope |> filter(n_studies ==my_n_studies) 
  plot <- plot_data |> 
    ProfacSims:::box_plot_cluster_x_sup() + #
    ylab(glue("Tau")) +
    ylab("Tau") +  scale_y_continuous(limits = c(0, 0.1)) 
  
  
  plot
  caption <- glue("Between-study hetrogeneity in calibraiton slope, training data {my_n_studies} studies")
  plot_name <- glue("calib_slope_hetro{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
  
}     


## MSE

mse <- readRDS(file = here(results_folder, "mse.RDS")) |> 
  filter(model != "Random intercept - ML", model != "Hausman - centred") |> 
  ProfacSims:::correct_model_spelling()

### Estimates

latex_text <- latex_text |> add_subsection("Pooled MSE")

mse$plot_value = mse$est
for(my_n_studies in studies_vector){
  plot_data <- mse |> filter(n_studies ==my_n_studies) 
  plot <- plot_data |> 
    ProfacSims:::box_plot_cluster_x_sup() + #
    ylab(glue("Mean Squared Error"))
  
  if(my_n_studies ==4){
    plot <- plot+  scale_y_continuous(limits = c(0.575, 0.8)) 
  } else {
    plot <- plot+  scale_y_continuous(limits = c(0.575, 0.7)) 
  }
  plot
  caption <- glue("Mean Squared Error, training data {my_n_studies} studies")
  plot_name <- glue("mse_{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
  
}     



### Hetrogeneity

latex_text <- latex_text |> add_subsection("Between-study hetrogeneity in R-squared")

mse$plot_value = sqrt(mse$tau2)
for(my_n_studies in studies_vector){
  plot_data <- mse |> filter(n_studies ==my_n_studies) 
  plot <- plot_data |> 
    ProfacSims:::box_plot_cluster_x_sup() + #
    ylab(glue("Tau")) +
    ylab("Tau") +  scale_y_continuous(limits = c(0, 0.2)) 
  
  plot
  caption <- glue("Between-study hetrogeneity in mean squared error, training data {my_n_studies} studies")
  plot_name <- glue("mse_hetro{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
  
}     



writeLines(latex_text, con = here(plots_folder, paste0(supp_mat_name, ".tex")))


