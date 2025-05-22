library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Loading files:
sim_name <- "240124-pred-new-data"
results_folder <- here("Results", "Database-extracts", sim_name)
supp_mat_name <- "supp_mat_pt1"
plots_folder <- here("Latex for thesis", "supp_plots", supp_mat_name)
latex_text <- ""
plot_height <- 24
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



studies_vector <- c(4,8,16,32)

var_u <- readRDS(file = here(results_folder, "var_u.RDS"))

plot_data <- var_u |> 
  filter(predict_method == "new0")
plot <- plot_data |> 
  ProfacSims:::box_plot_error_var_u_supp() #

plot
caption <- "Estimated random intercept variance"
plot_name <- "var_u"
plot_name_ext <- paste0(plot_name, ".pdf")
ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = 18, units = "cm")

latex_text <- latex_text |> add_subsection("Random intercept variance")
latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)



## Calibration in the large
latex_text <- latex_text |> add_subsection("Pooled calibration in the large")
calib_itl_stacked <- readRDS(file = here(results_folder, "calib_itl_stacked.RDS"))


### Estimates

for(my_n_studies in studies_vector){
  plot_data <- calib_itl_stacked |> 
    filter(
      predict_method != "new_dynamic", 
      n_studies ==my_n_studies) 
  
  plot <- plot_data |> 
    ProfacSims:::box_plot_by_pred_ss_supp(what = "est") # 
  
  plot <- plot +
    ylab("Calibration in the large")
  plot
  caption <- glue("Calibration in the large, training data {my_n_studies} studies")
  plot_name <- glue("calib_itl{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)

}     



### Hetrogeneity
latex_text <- latex_text |> add_subsection("Between-study hetrogeneity in calibration in the large")

for(my_n_studies in studies_vector){
  plot_data <- calib_itl_stacked |> 
    filter(
      predict_method != "new_dynamic", 
      n_studies ==my_n_studies) 
  
  plot <- plot_data |> 
    ProfacSims:::box_plot_by_pred_ss_supp(what = "tau") #
  
  plot <- plot +
    ylab("Tau")
  plot
  caption <- glue("Hetrogeneity in Calibration in the large, training data {my_n_studies} studies")
  plot_name <- glue("calib_itl_hetro{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
}     




## Calibration Slope

calib_slope_stacked <- readRDS(file = here(results_folder, "calib_slope_stacked.RDS"))


### Estimates

latex_text <- latex_text |> add_subsection("Pooled calibration slope")

for(my_n_studies in studies_vector){
  plot_data <- calib_slope_stacked |> 
    filter(
      predict_method != "new_dynamic", 
      n_studies ==my_n_studies) 
  
  plot <- plot_data |> 
    ProfacSims:::box_plot_by_pred_ss_supp(what = "est") # 
  
  plot <- plot + 
    ylab("Calibration Slope")
  plot
  caption <- glue("Calibraiton slope, training data {my_n_studies} studies")
  plot_name <- glue("calib_slope{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
}     



### Hetrogeneity
latex_text <- latex_text |> add_subsection("Between-study hetrogeneity in calibration slope")


for(my_n_studies in studies_vector){
  plot_data <- calib_slope_stacked |> 
    filter(
      predict_method != "new_dynamic", 
      n_studies ==my_n_studies) 
  
  plot <- plot_data |> 
    ProfacSims:::box_plot_by_pred_ss_supp(what = "tau") #
  
  plot <- plot +
    ylab("Tau")
  plot
  caption <- glue("Between-study hetrogeneity in calibraiton slope, training data {my_n_studies} studies")
  plot_name <- glue("calib_slope_hetro{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
  
}     


## R-squared

r_squared_stacked <- readRDS(file = here(results_folder, "r_squared_stacked.RDS"))

### Estimates

latex_text <- latex_text |> add_subsection("Pooled R-sqaured")

for(my_n_studies in studies_vector){
  plot_data <- r_squared_stacked |> 
    filter(
      predict_method != "new_dynamic", 
      n_studies ==my_n_studies) 
  
  plot <- plot_data |> 
    ProfacSims:::box_plot_by_pred_ss_supp(what = "est") # 
  
  plot <- plot + 
    labs(
      title = "R-Squared",
      subtitle = glue("Training data: {my_n_studies} studies")
    ) +
    ylab("R-Squared") +
    scale_y_continuous(limits = c(0, NA))
  plot
  caption <- glue("R-squared, training data {my_n_studies} studies")
  plot_name <- glue("r2_{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
  
}     



### Hetrogeneity

latex_text <- latex_text |> add_subsection("Between-study hetrogeneity in R-squared")

for(my_n_studies in studies_vector){
  plot_data <- r_squared_stacked |> 
    filter(
      predict_method != "new_dynamic", 
      n_studies ==my_n_studies) 
  
  plot <- plot_data |> 
    ProfacSims:::box_plot_by_pred_ss_supp(what = "tau") #
  
  plot <- plot + 
    labs(
      title = "Hetrogeneity in R-Squared",
      subtitle = glue("Training data: {my_n_studies} studies")
    ) +
    ylab("Tau")
  plot
  caption <- glue("Between-study hetrogeneity in R-Squared, training data {my_n_studies} studies")
  plot_name <- glue("r2_hetro{my_n_studies}")
  plot_name_ext <- paste0(plot_name, ".pdf")
  ggsave(file = here::here(plots_folder, plot_name_ext), width = 18, height = plot_height, units = "cm")
  
  latex_text <- add_figure_to_latex(latex_text, plot_name_ext, caption, supp_mat_name)
  
  
  
  
}     



writeLines(latex_text, con = here(plots_folder, paste0(supp_mat_name, ".tex")))


