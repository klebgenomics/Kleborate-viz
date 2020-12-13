# Get summary data for K locus
metadata_summary_k_locus_combined <- reactive({
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(K_locus = if_else(K_locus_confidence %in% c('Low', 'None'), 'unknown', K_locus)) %>%
    group_by(K_locus) %>% 
    summarise(prop=n()/nrow(.)) %>%
    filter(K_locus != 'unknown') %>%
    arrange(-prop) %>%
    mutate(cumulative_prevalence = cumsum(prop)) %>% 
    mutate(K_locus_number = row_number())
})
metadata_summary_k_locus_each <- reactive({
  # Mark low confidence loci
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
  mutate(K_locus = if_else(K_locus_confidence %in% c("Low", "None"), "unknown", K_locus)) -> k_high_confidence
  # Get proprotions by region
  as.data.frame(prop.table(table(k_high_confidence$K_locus, k_high_confidence$Region), margin = 2)) %>%
  dcast(Var1 ~ Var2) %>%
  rename(K_locus = Var1) %>%
  mutate(mean_prev = rowMeans(.[2:ncol(.)])) %>%
  arrange(-mean_prev) %>%
  mutate(K_locus_number = row_number()) %>%
  filter(K_locus != "unknown") %>%
  mutate_at(vars(-K_locus, -mean_prev, -K_locus_number), cumsum) %>%
  melt(id.vars = c("K_locus", "K_locus_number")) %>% 
  filter(!variable %in% c("NA", "mean_prev")) %>%
  mutate(cumulative_prevalence = value)
})
# K locus plots
output$cumulative_k_line_each <- renderPlotly({
  g <- ggplot(metadata_summary_k_locus_each(), aes(x=K_locus_number, y=cumulative_prevalence)) + geom_step(aes(colour=variable)) +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw() +
    coord_cartesian(ylim=c(0, 1))
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
output$cumulative_k_line_combined <- renderPlotly({
  g <- ggplot(metadata_summary_k_locus_combined(), aes(x=K_locus_number, y=cumulative_prevalence)) + geom_step() +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()+
    coord_cartesian(ylim=c(0, 1))
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})


# Get summary data for O locus
metadata_summary_o_locus_combined <- reactive({
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(O_locus = if_else(O_locus_confidence %in% c('Low', 'None'), 'unknown', O_locus)) %>%
    group_by(O_locus) %>% 
    summarise(prop=n()/nrow(.)) %>%
    filter(O_locus != 'unknown') %>% 
    arrange(-prop) %>%
    mutate(cumulative_prevalence = cumsum(prop)) %>%  
    mutate(O_locus_number = row_number())
})
metadata_summary_o_locus_each <- reactive({
  # Mark low confidnce loci
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
  mutate(K_locus = if_else(O_locus_confidence %in% c("Low", "None"), "unknown", O_locus)) -> o_high_confidence
  # Get proprotions by region
  as.data.frame(prop.table(table(o_high_confidence$O_locus, o_high_confidence$Region), margin = 2)) %>%
  dcast(Var1 ~ Var2) %>%
  rename(O_locus = Var1) %>%
  mutate(mean_prev = rowMeans(.[2:ncol(.)])) %>%
  arrange(-mean_prev) %>%
  mutate(O_locus_number = row_number()) %>%
  filter(O_locus != "unknown") %>%
  mutate_at(vars(-O_locus, -mean_prev, -O_locus_number), cumsum) %>%
  melt(id.vars = c("O_locus", "O_locus_number")) %>% 
  filter(!variable %in% c("NA", "mean_prev")) %>%
  mutate(cumulative_prevalence = value)
})
# O locus plots
output$cumulative_o_line_each <- renderPlotly({
  g <- ggplot(metadata_summary_o_locus_each(), aes(x=O_locus_number, y=cumulative_prevalence)) + geom_step(aes(colour=variable)) +
    xlab('Number of O-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()+
    coord_cartesian(ylim=c(0, 1))
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})
output$cumulative_o_line_combined <- renderPlotly({
  g <- ggplot(metadata_summary_o_locus_combined(), aes(x=O_locus_number, y=cumulative_prevalence)) + geom_step() +
    xlab('Number of O-loci') +
    ylab('Cumulative prevalence') +
    theme_bw()+
    coord_cartesian(ylim=c(0, 1))
  g <- ggplotly(g, dynamicTicks=TRUE)
  print(g)
})