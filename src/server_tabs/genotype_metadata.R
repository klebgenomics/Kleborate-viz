# Group selector
observeEvent(
  data_loaded$metadata,
  {
    v.cols <- colnames(data_loaded$metadata)[!colnames(data_loaded$metadata)%in% c('strain', 'Strain', 'Year', 'year')]
    updateSelectInput(session, 'genotype_metadata_dist_plot_group', choices=v.cols, selected=v.cols[1])
  }
)
# Genome distribution by metadata plot
output$genotype_metadata_dist_plot <- renderPlotly({
  # Return until input ui element renders and has a default value
  if (is.null(input$genotype_metadata_dist_plot_anno) || 
      is.null(input$genotype_metadata_group_count) ||
      is.null(input$genotype_metadata_dist_plot_group)) {
    return()
  }
  # Get configuration for plot type
  d <- inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ])
  v.prep <- get_plot_metadata_annotation(d, input$genotype_metadata_dist_plot_anno)
  d <- v.prep$d
  v.colours <- v.prep$colours
  s.anno_name <- v.prep$anno_name
    # Order by group size and select
  d$group <- d[[input$genotype_metadata_dist_plot_group]]
  v.group_counts <- sort(table(d$group), decreasing=TRUE)
  v.group_order <- names(v.group_counts)
  d$group <- factor(d$group, levels=v.group_order)
  d <- d[d$group %in% v.group_order[1:input$genotype_metadata_group_count], ]
  # Create plot
  g <- ggplot(data=d, aes(x=group, fill=annotation))
  g <- g + geom_bar()
  g <- g + theme(
    axis.text.x=element_text(colour='black', size=12, angle=45, hjust=1),
    axis.text.y=element_text(colour='black', size=12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = 'black', size = 16),
    legend.text = element_text(colour = 'black', size = 12),
    legend.title = element_text(colour = 'black', size = 16),
    
    panel.background=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(colour='black')
  )
  g <- g + ylab('Number of genomes') + xlab(input$genotype_metadata_dist_plot_group)
  g <- g + scale_y_continuous(expand=c(0, 0))
  g <- g + scale_fill_manual(values=v.colours, breaks=names(v.colours), name=s.anno_name, drop=FALSE)
  ggplotly(g)
})
# Group number slider
output$genotype_metadata_group_count <- renderUI({
  d <- inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ])
  v.groups <- unique(d[[input$genotype_metadata_dist_plot_group]])
  sliderInput(
    inputId='genotype_metadata_group_count',
    label='Number of groups:',
    min=1,
    max=length(v.groups),
    value=min(20, length(v.groups)),
    step=1
  )
})