library(ggstats)



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

dir_slr2 |> 
    filter(result != "not_enough_info") |>
    ggplot(
        aes(x = count, y = reserve, fill = result)) +
    geom_col(position = "diverging") +
    scale_fill_brewer(palette = "RdYlBu")


# of ones that can be calculated......
dir_slr |> 
    filter(value != "not_enough_info") |>
    ggplot(aes(y = reserve,
               fill = value)) +
    geom_bar(position = "likert", stat = "prop", complete = "fill") +
    scale_fill_brewer(palette = "RdYlBu")


dir_slr |> 
    filter(value != "not_enough_info") |>
    ggplot(aes(x = total,
               y = reserve,
               fill = value)) +
    geom_col(position = position_diverging(complete = "fill")) +
    scale_fill_brewer(palette = "RdYlBu")


ggplot(dir_slr,
       aes(x = total,
           y = reserve,
           fill = value)) +
    geom_col(position = position_diverging()) +
    scale_fill_brewer(palette = "RdYlBu")

dir_slr |> 
    filter(value != "not_enough_info") |> 
    ggplot(aes(x = graph_total,
               y = reserve,
               fill = value)) +
    geom_col() +
    scale_fill_brewer(palette = "RdYlBu")


p1 <- dir_slr |> 
    filter(value %in% c("dec_nonsig", "dec_sig")) |> 
    mutate(value = factor(value, levels = c("dec_sig", "dec_nonsig"))) |> 
    ggplot(aes(x = graph_total,
               y = reserve,
               fill = value)) +
    geom_col() +
    scale_fill_manual(values = c("dec_nonsig" = "#fdae61",
                                 "dec_sig" = "#d7191c"))

p2 <- dir_slr |> 
    filter(value %in% c("inc_nonsig", "inc_sig")) |> 
    ggplot(aes(x = graph_total,
               y = reserve,
               fill = value)) +
    geom_col() +
    scale_fill_brewer(palette = "RdYlBu")

