# Get summary data for K locus
metadata_summary_k_locus_combined <- reactive({
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    filter(! K_locus_confidence %in% c("Low", "None")) %>%
    group_by(K_locus) %>% 
    summarise(prop=n()/nrow(.)) %>% 
    arrange(-prop) %>% 
    mutate(id = row_number())
})
metadata_summary_k_locus_each <- reactive({
  # Get region counts
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    group_by(Region) %>% 
    summarise(region_n = n()) -> region_counts
  # Filter and assign
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    filter(!K_locus_confidence %in% c("Low", "None")) %>%
    group_by(K_locus) %>% 
    summarise(prop = n()/n()) %>% 
    arrange(-prop) %>%
    mutate(id = row_number()) %>% 
    select(K_locus, id) -> K_locus_to_ID
  # Group, get cumulative counts
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    filter(!K_locus_confidence %in% c("Low", "None")) %>%
    group_by(Region, K_locus) %>% 
    summarise(n = n()) %>%
    merge(., region_counts) %>%
    mutate(prop = n/region_n) %>%
    dcast(K_locus ~ Region, value.var = "prop") %>%
    left_join(., K_locus_to_ID) %>%
    arrange(id) -> cumulative_K_by_group
  cumulative_K_by_group[is.na(cumulative_K_by_group)] <- 0
  cumulative_K_by_group %>% mutate_at(vars(-K_locus, -id), cumsum) %>%
    melt(id.vars = c("K_locus", "id")) %>% 
    filter(variable != "NA")
})
# K locus plots
output$cumulative_k_line_each <- renderPlotly({
  g <- ggplot(metadata_summary_k_locus_each(), aes(x=id, y=value)) + geom_step(aes(colour=variable)) +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
output$cumulative_k_line_combined <- renderPlotly({
  g <- ggplot(metadata_summary_k_locus_combined(), aes(x=id, y=cumsum(prop))) + geom_step() +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})


# Get summary data for O locus
metadata_summary_o_locus_combined <- reactive({
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    filter(! O_locus_confidence %in% c("Low", "None")) %>%
    group_by(O_locus) %>% 
    summarise(prop=n()/nrow(.)) %>% 
    arrange(-prop) %>% 
    mutate(id = row_number())
})
metadata_summary_o_locus_each <- reactive({
  # Get region counts
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    group_by(Region) %>% 
    summarise(region_n = n()) -> region_counts
  # Filter and assign
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    filter(!O_locus_confidence %in% c("Low", "None")) %>%
    group_by(O_locus) %>% 
    summarise(prop = n()/n()) %>% 
    arrange(-prop) %>%
    mutate(id = row_number()) %>% 
    select(O_locus, id) -> O_locus_to_ID
  # Group, get cumulative counts
  left_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    filter(!O_locus_confidence %in% c("Low", "None")) %>%
    group_by(Region, O_locus) %>% 
    summarise(n = n()) %>%
    merge(., region_counts) %>%
    mutate(prop = n/region_n) %>%
    dcast(O_locus ~ Region, value.var = "prop") %>%
    left_join(., O_locus_to_ID) %>%
    arrange(id) -> cumulative_O_by_group
  cumulative_O_by_group[is.na(cumulative_O_by_group)] <- 0
  cumulative_O_by_group %>% mutate_at(vars(-O_locus, -id), cumsum) %>%
    melt(id.vars = c("O_locus", "id")) %>% 
    filter(variable != "NA")
})
# O locus plots
output$cumulative_o_line_each <- renderPlotly({
  g <- ggplot(metadata_summary_o_locus_each(), aes(x=id, y=value)) + geom_step(aes(colour=variable)) +
    xlab('Number of O-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
output$cumulative_o_line_combined <- renderPlotly({
  g <- ggplot(metadata_summary_o_locus_combined(), aes(x=id, y=cumsum(prop))) + geom_step() +
    xlab('Number of O-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})