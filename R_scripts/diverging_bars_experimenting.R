library(patchwork)

col_palette <- c(
    # from RColorBrewer RdYlBu; supposedly colorblind-safe
    "inc_sig" = "#2c7bb6",
    "inc_nonsig" = "#abd9e9",
    "not_enough_info" = "#ffffbf",
    "dec_nonsig" = "#fdae61",
    "dec_sig" = "#d7191c"
)

lab_palette = c(
    "dec_sig" = "No; more confident",
    "dec_nonsig" = "No; less confident",
    "not_enough_info" = "Not enough information",
    "inc_nonsig" = "Yes; less confident",
    "inc_sig" = "Yes; more confident"
)


dir_slr <- rates_comp_summary |> 
    filter(comparison == "dir_slr") |> 
    mutate(graph_total = case_when(str_starts(value, "dec") ~ total * -1,
                                   .default = total),
           value = factor(value, levels = c("dec_sig",
                                            "dec_nonsig",
                                            "not_enough_info",
                                            "inc_nonsig",
                                            "inc_sig")))

dir_slr2 <- dir_slr |> 
    select(reserve, value, total) |> 
    pivot_wider(names_from = "value",
                values_from = "total",
                values_fill = 0) |> 
    pivot_longer(3:7,
                 names_to = "result",
                 values_to = "count") |> 
    mutate(result = factor(result, levels = c("dec_sig",
                                              "dec_nonsig",
                                              "not_enough_info",
                                              "inc_nonsig",
                                              "inc_sig")))


dir_slr_notKeepingUp <- dir_slr2 |> 
    filter(result %in% c("dec_sig", "dec_nonsig")) |> 
    mutate(graph_count = count * -1,
           result = factor(result, levels = c("dec_nonsig",
                                              "dec_sig")))

dir_slr_yesKeepingUp <- dir_slr2 |> 
    filter(result %in% c("inc_sig", "inc_nonsig")) |> 
    mutate(graph_count = count)




p_labs <- ggplot(dir_slr_notKeepingUp,
                 aes(x = 1,
                     y = reserve,
                     label = reserve)) +
    geom_text() +
    theme_void()

p_left <- ggplot(dir_slr_notKeepingUp,
                 aes(x = graph_count,
                     y = reserve,
                     fill = result)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = col_palette) +
    scale_x_continuous(breaks = scales::pretty_breaks(),
                       labels = function(x) abs(x)) +
    theme_void() +
    theme(axis.text.x = element_text(),
          panel.grid.major.x = element_line(color = "grey90"))
p_right <- ggplot(dir_slr_yesKeepingUp,
                 aes(x = graph_count,
                     y = reserve,
                     fill = result)) +
    geom_col(
             show.legend = TRUE) +
    scale_fill_manual(values = col_palette,
                      labels = lab_palette,
                      drop = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme_void() +
    theme(axis.text.x = element_text(),
          panel.grid.major.x = element_line(color = "grey90"))
p_left + p_labs + p_right +
    plot_layout(guides = "collect",
                widths = c(7, 2, 7))



dir_slr3 <- dir_slr2 |> 
    filter(result != "not_enough_info") |> 
    mutate(graph_count = case_when(str_starts(result, "dec") ~ count * -1,
                                   .default = count),
           result = factor(result, levels = c("dec_nonsig",
                                              "dec_sig",
                                              "not_enough_info",
                                              "inc_nonsig",
                                              "inc_sig")))

res_sets <- dir_slr3 |> 
    ungroup() |> 
    summarize(.by = reserve,
              SETcount = sum(count)) |> 
    arrange(desc(SETcount)) |> 
    mutate(reserveFactor = forcats::fct_inorder(reserve))

dir_slr3 <- left_join(dir_slr3, res_sets)

ggplot(dir_slr3,
       aes(x = graph_count,
           y = reserveFactor,
           fill = result)) +
    geom_col() +
    geom_vline(xintercept = 0) +
    scale_fill_manual(values = col_palette,
                      labels = lab_palette) +
    scale_x_continuous(breaks = scales::pretty_breaks(),
                       labels = function(x) abs(x)) +
    theme_void() +
    theme(axis.text.x = element_text(),
          axis.text.y = element_text(),
          axis.title.x = element_text(),
          axis.title.y = element_text(angle = 90),
          panel.grid.major.x = element_line(color = "gray90")) +
    labs(title = "Are SETs keeping up with Sea Level Rise?",
         x = "# SETs",
         y = "Reserve, arranged by total # of SETs",
         fill = "")
    