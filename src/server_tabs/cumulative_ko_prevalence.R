# Group selector
observeEvent(
  data_loaded$metadata,
  {
    v.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata) %in% c('strain', 'Strain')]
    updateSelectInput(session, 'ko_cumulative_var', choices=v.cols, selected=v.cols[1])
  }
)

# Colour generator
ko_cumulative_group_colours <- reactive({
  v.groups <- unique(data_loaded$metadata[[input$ko_cumulative_var]])
  v.groups <- sort(v.groups)
  v.colours <- misc_colour_palette(length(v.groups))
  names(v.colours) <- v.groups
  return(v.colours)
})

# Get summary data for K locus
metadata_summary_k_locus_combined <- reactive({
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(K_locus = if_else(K_locus_confidence %in% c('Low', 'None'), 'unknown', K_locus)) %>%
    group_by(K_locus) %>% 
    summarise(prop=n()/nrow(.)) %>%
    filter(K_locus != 'unknown') %>%
    arrange(-prop) %>%
    mutate(cumulative_prevalence = cumsum(prop)) %>% 
    mutate(K_locus_number = row_number()) %>%
    select(K_locus_number, cumulative_prevalence)
})
metadata_summary_k_locus_each <- reactive({
  # Mark low confidence loci
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(K_locus = if_else(K_locus_confidence %in% c("Low", "None"), "unknown", K_locus)) -> k_high_confidence
  # Get proprotions by variable
  as.data.frame(prop.table(table(k_high_confidence$K_locus, k_high_confidence[[input$ko_cumulative_var]]), margin = 2)) %>%
    dcast(Var1 ~ Var2) %>%
    rename(K_locus = Var1) %>%
    mutate(mean_prev = rowMeans(.[2:ncol(.)])) %>%
    arrange(-mean_prev) %>%
    mutate(K_locus_number = row_number()) %>%
    filter(K_locus != "unknown") %>%
    mutate_at(vars(-K_locus, -mean_prev, -K_locus_number), cumsum) %>%
    melt(id.vars = c("K_locus", "K_locus_number")) %>% 
    filter(!variable %in% c("NA", "mean_prev")) %>%
    mutate(cumulative_prevalence = value) %>%
    select(K_locus_number, cumulative_prevalence, variable)
})

# K locus plots
output$cumulative_k_line_each_plot <- renderPlotly(cumulative_k_line_each_plot())
cumulative_k_line_each_plot <- reactive({
  v.colours <- ko_cumulative_group_colours()
  g <- ggplot(metadata_summary_k_locus_each(), aes(x=K_locus_number, y=cumulative_prevalence)) + geom_step(aes(colour=variable)) +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw() + scale_colour_manual(values=v.colours, breaks=names(v.colours)) +
    coord_cartesian(ylim=c(0, 1))
  ggplotly(g)
})
output$cumulative_k_line_combined_plot <- renderPlotly(cumulative_k_line_combined_plot())
cumulative_k_line_combined_plot <- reactive({
  g <- ggplot(metadata_summary_k_locus_combined(), aes(x=K_locus_number, y=cumulative_prevalence)) + geom_step() +
    xlab('Number of K-loci') +
    ylab('Cumulative prevalence') +
    theme_bw() +
    coord_cartesian(ylim=c(0, 1))
  ggplotly(g)
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
    mutate(O_locus_number = row_number()) %>%
    select(O_locus_number, cumulative_prevalence)
})
metadata_summary_o_locus_each <- reactive({
  # Mark low confidnce loci
  inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(K_locus = if_else(O_locus_confidence %in% c("Low", "None"), "unknown", O_locus)) -> o_high_confidence
  # Get proprotions by variable
  as.data.frame(prop.table(table(o_high_confidence$O_locus, o_high_confidence[[input$ko_cumulative_var]]), margin = 2)) %>%
    dcast(Var1 ~ Var2) %>%
    rename(O_locus = Var1) %>%
    mutate(mean_prev = rowMeans(.[2:ncol(.)])) %>%
    arrange(-mean_prev) %>%
    mutate(O_locus_number = row_number()) %>%
    filter(O_locus != "unknown") %>%
    mutate_at(vars(-O_locus, -mean_prev, -O_locus_number), cumsum) %>%
    melt(id.vars = c("O_locus", "O_locus_number")) %>% 
    filter(!variable %in% c("NA", "mean_prev")) %>%
    mutate(cumulative_prevalence = value) %>%
    select(O_locus_number, cumulative_prevalence, variable)
})

# O locus plots
output$cumulative_o_line_each_plot <- renderPlotly(cumulative_o_line_each_plot())
cumulative_o_line_each_plot <- reactive({
  v.colours <- ko_cumulative_group_colours()
  g <- ggplot(metadata_summary_o_locus_each(), aes(x=O_locus_number, y=cumulative_prevalence)) + geom_step(aes(colour=variable)) +
    xlab('Number of O-loci') +
    ylab('Cumulative prevalence') +
    theme_bw() + scale_colour_manual(values=v.colours, breaks=names(v.colours)) +
    coord_cartesian(ylim=c(0, 1))
  ggplotly(g)
})
output$cumulative_o_line_combined_plot <- renderPlotly(cumulative_o_line_combined_plot())
cumulative_o_line_combined_plot <- reactive({
  g <- ggplot(metadata_summary_o_locus_combined(), aes(x=O_locus_number, y=cumulative_prevalence)) + geom_step() +
    xlab('Number of O-loci') +
    ylab('Cumulative prevalence') +
    theme_bw() +
    coord_cartesian(ylim=c(0, 1))
  ggplotly(g)
})

# Download plot/data
# Overall K prevalence
output$cumulative_k_line_each_data_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'csv')),
  content=function(s.filename) { write.csv(metadata_summary_k_locus_combined(), s.filename, row.names=FALSE) }
)
output$cumulative_k_line_each_plot_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'pdf')),
  content=function(s.filename) { download_plot(cumulative_k_line_each_plot, s.filename) }
)
observeEvent(input$cumulative_k_line_each_plot_download_show, {
  download_modal(downloadButton('cumulative_k_line_each_plot_download', class='btn-primary'))
})
# Individual K prevalence
output$cumulative_k_line_combined_data_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'csv')),
  content=function(s.filename) { write.csv(metadata_summary_k_locus_combined(), s.filename, row.names=FALSE) }
)
output$cumulative_k_line_combined_plot_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'pdf')),
  content=function(s.filename) { download_plot(cumulative_k_line_combined_plot, s.filename) }
)
observeEvent(input$cumulative_k_line_combined_plot_download_show, {
  download_modal(downloadButton('cumulative_k_line_combined_plot_download', class='btn-primary'))
})
# Overall O prevalence
output$cumulative_o_line_each_data_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'csv')),
  content=function(s.filename) { write.csv(metadata_summary_o_locus_combined(), s.filename, row.names=FALSE) }
)
output$cumulative_o_line_each_plot_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'pdf')),
  content=function(s.filename) { download_plot(cumulative_o_line_each_plot, s.filename) }
)
observeEvent(input$cumulative_o_line_each_plot_download_show, {
  download_modal(downloadButton('cumulative_o_line_each_plot_download', class='btn-primary'))
})
# Individual O prevalence
output$cumulative_o_line_combined_data_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'csv')),
  content=function(s.filename) { write.csv(metadata_summary_o_locus_combined(), s.filename, row.names=FALSE) }
)
output$cumulative_o_line_combined_plot_download <- downloadHandler(
  filename=reactive(download_filename('k_locus_prevalence_overall', 'pdf')),
  content=function(s.filename) { download_plot(cumulative_o_line_combined_plot, s.filename) }
)
observeEvent(input$cumulative_o_line_combined_plot_download_show, {
  download_modal(downloadButton('cumulative_o_line_combined_plot_download', class='btn-primary'))
})
