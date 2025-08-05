library(tidyverse)

load(here::here("data",
                "04_rates",
                "SET_rates_and_details.RData"))

# set up the nicer names for comparisons
set_details <- set_details |> 
    mutate(dir_19yr = factor(dir_19yr, 
                             levels = c("dec_sig", "dec_nonsig", "not_enough_info", "inc_nonsig", "inc_sig"),
                             labels = c("No, more confident", "No, less confident", "Not enough info",
                                        "Yes, less confident", "Yes, more confident")),
           dir_slr = factor(dir_slr, 
                            levels = c("dec_sig", "dec_nonsig", "not_enough_info", "inc_nonsig", "inc_sig"),
                            labels = c("No, more confident", "No, less confident", "Not enough info",
                                       "Yes, less confident", "Yes, more confident")))



# make a list of reserve summary details
# make the reserve-level list
reserves <- unique(set_details$reserve)
reserves_list <- list()

# for each reserve, make a list of different SET characteristics to report
for(i in seq_along(reserves)){
    
    res_set_details <- set_details |> 
        filter(reserve == reserves[i])
    
    set_installations <- res_set_details |>
        mutate(start_year = lubridate::year(start_date)) |> 
        summarize(.by = c(reserve, start_year),
                  n = n()) |> 
        arrange(reserve, start_year)
    set_types <- res_set_details |> 
        summarize(.by = c(reserve, set_type),
                  n = n())
    set_vegs <- res_set_details |> 
        summarize(.by = c(reserve, dominant_vegetation),
                  n = n())
    set_slr_rates <- res_set_details |> 
        summarize(.by = reserve,
                  rate_longterm = paste(unique(round(slr_rate, 2)), collapse = ","),
                  rate_19yr = paste(unique(round(slr_19yr, 2)), collapse = ","))
    set_slr_comps <- res_set_details |> 
        summarize(.by = c(reserve, dir_slr),
                  n = n())
    set_19yr_comps <- res_set_details |> 
        summarize(.by = c(reserve, dir_19yr),
                  n = n())
    set_salinities <- res_set_details |> 
        summarize(.by = c(reserve, general_salinity),
                  n = n()) 
    
    res_set_list <- list(
        installations = set_installations,
        types = set_types,
        vegs = set_vegs,
        slr_rates = set_slr_rates,
        slr_comps = set_slr_comps,
        yr19_comps = set_19yr_comps,
        salinities = set_salinities
    )
    
    reserves_list[[i]] <- res_set_list
    names(reserves_list)[i] <- reserves[i]
}


