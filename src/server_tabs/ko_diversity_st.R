# User ST selection
ko_diversity_st_selected <- reactiveVal()
observeEvent(
  data_loaded$kleborate,
  {
    ko_diversity_st_selected(NULL)
    updateTextInput(session, 'ko_diversity_st_text', value='')
  }
)
observeEvent(
  input$ko_diversity_st_text_button,
  {
    v.sts <- unique(data_loaded$kleborate$ST)
    if (! tolower(input$ko_diversity_st_text) %in% tolower(v.sts)) {
      showNotification(
        paste('Could not find', input$ko_diversity_st_text, 'in provided dataset'),
        type='error',
        duration=NULL
      )
    } else {
      v.selector <- which(tolower(v.sts) %in% tolower(input$ko_diversity_st_text))
      ko_diversity_st_selected(v.sts[v.selector])
    }
  }
)
observeEvent(
  input$ko_diversity_st_reset_button,
  {
    ko_diversity_st_selected(NULL)
    updateTextInput(session, 'ko_diversity_st_text', value='')
  }
)
output$ko_diversity_locus_count <- renderUI({
  v.k_loci <- length(unique(data_loaded$kleborate$K_locus))
  v.o_loci <- length(unique(data_loaded$kleborate$o_locus))
  v.ko_loci_max <- max(v.k_loci, v.o_loci)
  sliderInput(
    inputId='ko_diversity_locus_count',
    label='Number of loci:',
    min=1,
    max=v.ko_loci_max,
    value=min(20, v.ko_loci_max),
    step=1
  )
})
create_locus_barplot <- function(d, s.locus, s.anno_name, v.colours) {
  if (! s.locus %in% c('K_locus', 'O_locus')) {
    stop('Got bad locus')
  }
  d$locus <- d[[s.locus]]
  # Order locus by group size
  v.loci_counts <- sort(table(d$locus), decreasing=TRUE)
  v.loci_order <- names(v.loci_counts)
  d$locus <- factor(d$locus, levels=v.loci_order)
  # Select first n loci
  d <- d[d$locus %in% v.loci_order[1:input$ko_diversity_locus_count], ]
  # Create plot
  g <- ggplot(data=d, aes(x=locus, fill=annotation))
  g <- g + geom_bar()
  g <- g + theme(
    axis.text.x=element_text(colour='black', size=12, angle=45, hjust=1),
    axis.text.y=element_text(colour='black', size=12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = 'black', size = 16),
    legend.text = element_text(colour = 'black', size= 12),
    legend.title = element_text(colour = 'black', size = 16),
    panel.background=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(colour='black')
  )
  g <- g + ylab('Number of genomes') + xlab(str_replace(s.locus, '_', ' '))
  g <- g + scale_y_continuous(expand=c(0, 0))
  g <- g + scale_fill_manual(values=v.colours, breaks=names(v.colours), name=s.anno_name, drop=FALSE)
  ggplotly(g)
}
output$k_locus_barplot <- renderPlotly({
  # Return until input ui element renders and has a default value
  if (is.null(input$ko_diversity_locus_count)) {
    return()
  }
  # Prepare data
  d <- data_loaded$kleborate[data_selected$rows, ]
  if (!is.null(ko_diversity_st_selected())) {
    d <- d[d$ST==ko_diversity_st_selected(), ]
    d <- d[!is.na(d$strain), ]
  }
  d$K_locus_low_filter <- ifelse(d$K_locus_confidence %in% c('None', 'Low'), 'Unknown', d$K_locus_confidence)
  d$O_locus_low_filter <- ifelse(d$K_locus_confidence %in% c('None', 'Low'), 'Unknown', d$O_locus_confidence)
  v.prep <- get_plot_metadata_annotation(d, input$ko_dist_plot_anno)
  d <- v.prep$d
  v.colours <- v.prep$colours
  s.anno_name <- v.prep$anno_name
  # Create plot
  g <- create_locus_barplot(d, 'K_locus', s.anno_name, v.colours)
  return(g)
})
output$o_locus_barplot <- renderPlotly({
  # Return until input ui element renders and has a default value
  if (is.null(input$ko_diversity_locus_count)) {
    return()
  }
  # Prepare data
  d <- data_loaded$kleborate[data_selected$rows, ]
  if (!is.null(ko_diversity_st_selected())) {
    d <- d[d$ST==ko_diversity_st_selected(), ]
    d <- d[!is.na(d$strain), ]
  }
  d$K_locus_low_filter <- ifelse(d$K_locus_confidence %in% c('None', 'Low'), 'Unknown', d$K_locus_confidence)
  d$O_locus_low_filter <- ifelse(d$K_locus_confidence %in% c('None', 'Low'), 'Unknown', d$O_locus_confidence)
  v.prep <- get_plot_metadata_annotation(d, input$ko_dist_plot_anno)
  d <- v.prep$d
  v.colours <- v.prep$colours
  s.anno_name <- v.prep$anno_name
  # Create plot
  g <- create_locus_barplot(d, 'O_locus', s.anno_name, v.colours)
  return(g)
})
# Heatmap
output$ko_diversity_st_heatmap <- renderPlotly({
  # Get data
  d <- data_loaded$kleborate[data_selected$rows, ]
  if (is.null(ko_diversity_st_selected())) {
    main_title <- NULL
  } else {
    d <- d[d$ST==ko_diversity_st_selected(), ]
    main_title <- ko_diversity_st_selected()
  }
  # Format data for plotting
  # NOTE: converting as done below is required to handle corner cases where nrow = 1 or ncol=1
  k_vs_o <- table(d$K_locus, d$O_locus)
  k_vs_o <- k_vs_o[rowSums(k_vs_o)>0,colSums(k_vs_o)>0,drop=FALSE]
  k_vs_o <- matrix(k_vs_o, byrow=TRUE, ncol=ncol(k_vs_o), dimnames=list(rownames(k_vs_o), colnames(k_vs_o)))
  # Ensure we have some data
  if (nrow(k_vs_o) < 1){
    return(NULL)
  }
  # Create heatmap
  heatmaply(
    k_vs_o,
    main=list(text=main_title, color = "#000000"),
    Rowv=NULL,
    Colv=NULL,
    fontsize_row=10,
    fontsize_col=10,
    hide_colorbar=F,
    revC=F,
    key.title = "# genomes",
    showticklabels=c(FALSE, FALSE),
    plot_method='ggplot',
    colors=c('white', colorRampPalette(colors=c('#f1c280', '#e67d77', '#ED6060'))(max(k_vs_o))),
    xlab='O loci',
    ylab='K loci',
    margin = c(10, 10, 10, 10),

  )
})
