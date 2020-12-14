# Group selector
observeEvent(
  data_loaded$metadata,
  {
    v.cols <- colnames(data_loaded$metadata)[colnames(data_loaded$metadata)!='strain']
    updateSelectInput(session, 'genotype_metadata_dist_plot_group', choices=v.cols, selected=v.cols[1])
  }
)
# Genome distribution by metadata plot
genotype_metadata_dist_plot <- reactive({
  # Return until input ui element renders and has a default value
  if (is.null(input$genotype_metadata_dist_plot_anno)) {
    return()
  }
  # Get configuration for plot type
  d <- inner_join(data_loaded$metadata, data_loaded$kleborate[data_selected$rows, ])
  if (input$genotype_metadata_dist_plot_anno=='virulence_score') {
    # Determine colours and label names
    v.virulence_score_labels <- paste0(names(v.virulence_score_names), ": ", v.virulence_score_names)
    names(v.virulence_score_labels) <- names(v.virulence_score_names)
    v.colours <- v.virulence_score_colours[names(v.virulence_score_labels)]
    names(v.colours) <- v.virulence_score_labels
    # Set annotation column
    d$annotation <- v.virulence_score_labels[as.character(d$virulence_score)]
    s.anno_name <- 'Virulence Score'
  } else if (input$genotype_metadata_dist_plot_anno=='resistance_score') {
    # Determine colours and label names
    v.resistance_score_labels <- paste0(names(v.resistance_score_names), ": ", v.resistance_score_names)
    names(v.resistance_score_labels) <- names(v.resistance_score_names)
    v.colours <- v.resistance_score_colours[names(v.resistance_score_labels)]
    names(v.colours) <- v.resistance_score_labels
    # Set annotation column
    d$annotation <- v.resistance_score_labels[as.character(d$resistance_score)]
    s.anno_name <- 'Resistance Score'
  } else if (input$genotype_metadata_dist_plot_anno=='Bla_ESBL_simplified') {
    d$annotation <- d$Bla_ESBL_simplified
    # NOTE: placeholder for colours
    n <- length(unique(d$annotation))
    v.colours <- hcl(h=seq(15, 375, length=n+1), l=65, c=100)[1:n]
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla ESBL'
  } else if (input$genotype_metadata_dist_plot_anno=='Bla_Carb_simplified') {
    d$annotation <- d$Bla_Carb_simplified
    n <- length(unique(d$annotation))
    v.colours <- hcl(h=seq(15, 375, length=n+1), l=65, c=100)[1:n]
    names(v.colours) <- unique(d$annotation)
    s.anno_name <- 'Bla Carb'
  } else {
    if (input$genotype_metadata_dist_plot_anno %in% v.virulence_loci) {
      v.colours <- c("grey", "#2171b5")
      s.anno_name <- names(v.virulence_loci)[v.virulence_loci==input$genotype_metadata_dist_plot_anno]
    } else if (input$genotype_metadata_dist_plot_anno %in% v.resistance_classes) {
      v.colours <- c("grey", "#ef3b2c")
      s.anno_name <- names(v.resistance_classes)[v.resistance_classes==input$genotype_metadata_dist_plot_anno]
    } else {
      stop('Got bad annotation variable')
    }
    names(v.colours) <- c('absent', 'present')
    # Set annotation column
    d$annotation <- ifelse(d[[input$genotype_metadata_dist_plot_anno]]=='-', 'absent', 'present')
  }
  # Order by group size
  d <<- d
  
  d$group <- d[[input$genotype_metadata_dist_plot_group]]
  v.group_counts <- sort(table(d$group), decreasing=TRUE)
  v.group_order <- names(v.group_counts)
  d$group <- factor(d$group, levels=v.group_order)
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
  return(g)
})
output$genotype_metadata_dist_plot <- renderPlot ({ print(genotype_metadata_dist_plot()) })