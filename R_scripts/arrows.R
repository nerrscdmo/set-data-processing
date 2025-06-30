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
lwd = 10




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


ggplot(vertices) +
    geom_path(aes(x = x, y = y),
              lineend = "round",
              linewidth = lwd) +
    coord_equal() +
    theme_void()

# make the guides ----

# define colors as keeping up, not keeping up, or not enough info
up_cols <- cols_slr[which(stringr::str_starts(names(cols_slr), "Yes"))]
down_cols <- cols_slr[which(stringr::str_starts(names(cols_slr), "No,"))]
not_cols <- cols_slr[which(names(cols_slr) == "Not enough info")]

# make the up arrows
for(i in seq_along(up_cols)){
    p_up <- ggplot(vertices) +
        geom_path(aes(x = x, y = y),
                  lineend = "round",
                  linewidth = lwd,
                  col = up_cols[i]) +
        coord_equal() +
        theme_void()
}

# make the down arrows
for(i in seq_along(down_cols)){
    p_down <- ggplot(vertices) +
        geom_path(aes(x = x, y = y*-1),
                  lineend = "round",
                  linewidth = lwd,
                  col = cols_slr2[i]) +
        coord_equal() +
        theme_void()
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
