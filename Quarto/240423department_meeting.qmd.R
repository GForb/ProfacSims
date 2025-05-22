---
  title: "Prediction with new data for intercepts"
format: html
editor: visual
---
  
```{r echo = FALSE}
library(tidyverse)
library(ggplot2)
library(DBI)
library(RSQLite)
library(here)
library(glue)

# Loading files:
sim_name <- "240124-pred-new-data"
results_folder <- here("Results", "Database-extracts", sim_name)

calib_itl_raw <- readRDS(file = here(results_folder, "calib_itl.RDS"))

calib_itl <- calib_itl_raw |> filter(model != "Not adjusting for study") |> 
  filter(!(predict_method == "new0" & model != "Fixed intercept"))

calib_itl0 <- calib_itl_raw |> filter(predict_method == "new0", intercept_est_sample_size == 10) |> 
  mutate(intercept_est_sample_size = 0)

calib_itl_stacked <- calib_itl_raw |> filter(predict_method != "new0") |> bind_rows(calib_itl0)
```


```{r calib_itl_hetro_large, fig.height = 8}

my_n_studies <- 16        
my_sample_size_train =1000  

plot_data <- calib_itl_stacked |> 
  filter(predict_method != "new_dynamic", n_studies == my_n_studies, study_sample_size_train == my_sample_size_train, ICC != 0.2, R2 ==0.4) |> 
  mutate(tau = sqrt(tau2))

plot <- plot_data |> 
  ProfacSims:::box_plot_by_pred_ss(what = "tau") 

plot <- plot + 
  labs(title = "Hetrogeneity in Calibration in the large",
       subtitle = glue("Training data: {my_n_studies} studies of size {my_sample_size_train} ")) +
  ylab("Tau")
print(plot)


```
