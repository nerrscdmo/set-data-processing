# after running this script, copy the "images" folder over to the app directory

library(tidyverse)

# data ----
load(here::here("data",
                "04_rates",
                "SET_rates_and_details.RData"))  # this is the same data file to copy to the app
reserve_sets <- reserve_sets |> 
    mutate(outcome = factor(outcome, 
                            levels = c("dec_sig", "dec_nonsig", "inc_nonsig", "inc_sig", "not_enough_info"),
                            labels = c("No, more confident", "No, less confident",
                                       "Yes, less confident", "Yes, more confident", "Not calculated")))

# color palette ----
# cols_slr <- c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6") # RdYlBu from colorbrewer
# cols_slr <- c("#A50026", "#f67e4b", "#feda8b", "#6ea6cd", "#364b9a") # Sunset from Tol
cols_slr <- c("#b2182b", "#f4a582", "#92c5de", "#2166ac", "#ffee99") # BuRd from Tol
names(cols_slr) <- c("No, more confident", "No, less confident",
                     "Yes, less confident", "Yes, more confident", "Not calculated")

save(cols_slr, file = here::here("images", "color_palette.RDS"))

# pie charts ----
ress <- unique(reserve_sets$reserve)

# long-term ----
single_comp <- reserve_sets |> 
    filter(compared_to == "slr")

for(i in seq_along(ress)){
    res <- ress[i]
    tmp <- single_comp |> 
        filter(reserve == res)
    
    pie <- ggplot(tmp) +
        geom_bar(aes(x = "",
                     y = n,
                     fill = outcome),
                 col = "black",
                 stat = "identity",
                 position = "stack",
                 width = 1) +
        scale_fill_manual(values = cols_slr) +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(legend.position = "none")
    
    flnm <- paste0(res, "_longterm.svg")
    
    ggsave(filename = here::here("images", "pie_charts",
                                 flnm),
           plot = pie,
           height = 1,
           width = 1,
           units = "in")
}


# 19-yr
single_comp <- reserve_sets |> 
    filter(compared_to == "19yr")

for(i in seq_along(ress)){
    res <- ress[i]
    tmp <- single_comp |> 
        filter(reserve == res)
    
    pie <- ggplot(tmp) +
        geom_bar(aes(x = "",
                     y = n,
                     fill = outcome),
                 col = "black",
                 stat = "identity",
                 position = "stack",
                 width = 1) +
        scale_fill_manual(values = cols_slr) +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(legend.position = "none")
    
    flnm <- paste0(res, "_19yr.svg")
    
    ggsave(filename = here::here("images", "pie_charts",
                                 flnm),
           plot = pie,
           height = 1,
           width = 1,
           units = "in")
}

# arrows ----

# center on x = 0
cent = 0
# height of base
ht_bs = 1.25
# width of base
wd_bs = .9
# height of arrowhead
ht_ar = ht_bs * 0.75
# width of arrowhead
wd_ar = 2.1 * wd_bs
# thickness of lines
lwd = 8




vertices <- tribble(
    ~order, ~pointid, ~x, ~y,
    1, "bs_btmlt", cent - (wd_bs/2), 0,
    2, "bs_btmrt", cent + (wd_bs/2), 0, 
    7, "bs_uplt",  cent - (wd_bs/2), ht_bs,
    3, "bs_uprt",  cent + (wd_bs/2), ht_bs,
    6, "ar_lt",    cent - (wd_ar/2), ht_bs,
    4, "ar_rt",    cent + (wd_ar/2), ht_bs, 
    5, "ar_pt",    cent,             ht_bs + ht_ar,
    8, "bs_btmlt", cent - (wd_bs/2), 0
) |> 
    arrange(order)


# make the guides ----

# define colors as keeping up, not keeping up, or not enough info
up_cols <- cols_slr[which(stringr::str_starts(names(cols_slr), "Yes"))]
down_cols <- cols_slr[which(stringr::str_starts(names(cols_slr), "No,"))]
not_cols <- cols_slr[which(names(cols_slr) == "Not calculated")]

# make the up arrows
for(i in seq_along(up_cols)){
    p_up <- ggplot(vertices) +
        geom_path(aes(x = x, y = y),
                  lineend = "round",
                  linewidth = lwd,
                  col = up_cols[i]) +
        coord_equal() +
        theme_void()
    flnm <- paste0("up_", ifelse(grepl("less", names(up_cols)[i]), "nonsig", "sig"), ".svg")
    ggsave(filename = here::here("images", "arrows",
                                 flnm),
           plot = p_up,
           height = 3,
           width = 3,
           units = "in")
}

# make the down arrows
for(i in seq_along(down_cols)){
    p_down <- ggplot(vertices) +
        geom_path(aes(x = x, y = y*-1),
                  lineend = "round",
                  linewidth = lwd,
                  col = down_cols[i]) +
        coord_equal() +
        theme_void()
    
    flnm <- paste0("down_", ifelse(grepl("less", names(up_cols)[i]), "nonsig", "sig"), ".svg")
    ggsave(filename = here::here("images", "arrows",
                                 flnm),
           plot = p_down,
           height = 3,
           width = 3,
           units = "in")
}

# make the rectangle for not enough info
not_enoughdf <- tribble(
    ~order, ~x, ~y,
    1, 0, 0,
    2, 1, 0,
    3, 1, 1, 
    4, 0, 1
)
p_not <- ggplot(not_enoughdf) +
    geom_polygon(aes(x = x, y = y),
                 fill = not_cols,
                 col = "gray60") +
    coord_equal() +
    theme_void()
ggsave(filename = here::here("images", "arrows",
                             "notEnoughInfo.svg"),
       plot = p_not,
       height = 1,
       width = 1,
       units = "in")
