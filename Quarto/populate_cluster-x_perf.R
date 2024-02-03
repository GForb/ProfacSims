
wd <- getwd()
if(wd != here::here("Quarto")){
  setwd(here::here("Quarto"))
}
sim_name <- "240131-cluster-x"
file_names = c("calib_itl", "calib_slope", "r_squared")
for (filename in file_names){
  input <- paste0("cluster-x_perf_", filename, ".qmd")
  print(input)
  quarto::quarto_render(
        cache_refresh = TRUE,
        input = input,
        output_file = paste0(sim_name, "_", filename, ".html")
        )
}



#
#
#
# params_df <- data.frame(
#   what = c("Calibration in the large", "Calibration Slope"),
#   file_name = c("calib_itl", "calib_slope, r_squared")
# )
#
# for (i in 1:nrow(params_df)) {
#   unlink("cluster-x_perf_template_files", recursive = TRUE)
#   unlink("cluster-x_perf_template_cache", recursive = TRUE)
#
#   quarto::quarto_render(
#     cache_refresh = TRUE,
#     input = "cluster-x_perf_template.qmd",
#     output_file = paste0(sim_name, "_", params_df$file_name[i], ".html"),
#     execute_params = list(
#       what = params_df$what[i],
#       file_name = params_df$file_name[i],
#       sim_name = sim_name
#     )
#   )
# }
#
#
# , "r_squared"
# , "R-squared"
