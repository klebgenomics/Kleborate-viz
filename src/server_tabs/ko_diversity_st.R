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

# Slider for number of KO to display
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

# Bar plots
# Generalised functions
locus_bar_plot <- function(d, s.locus, s.anno_name, v.colours) {
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
locus_bar_data <- function(s.locus) {
  if (! s.locus %in% c('K_locus', 'O_locus')) {
    stop('Got bad locus')
  }
  # Select data
  d <- data_loaded$kleborate[data_selected$rows, ]
  if (!is.null(ko_diversity_st_selected())) {
    d <- d[d$ST==ko_diversity_st_selected(), ]
    d <- d[!is.na(d$strain), ]
  }
  # Remove low quality loci
  d$K_locus_low_filter <- ifelse(d$K_locus_confidence %in% c('None', 'Low'), 'Unknown', d$K_locus_confidence)
  d$O_locus_low_filter <- ifelse(d$K_locus_confidence %in% c('None', 'Low'), 'Unknown', d$O_locus_confidence)
  # Get annotations
  v.prep <- get_plot_metadata_annotation(d, input$ko_dist_plot_anno)
  d <- v.prep$d
  v.colours <- v.prep$colours
  s.anno_name <- v.prep$anno_name
  # Set locus
  d$locus <- d[[s.locus]]
  # Order locus by group size
  v.loci_counts <- sort(table(d$locus), decreasing=TRUE)
  v.loci_order <- names(v.loci_counts)
  d$locus <- factor(d$locus, levels=v.loci_order)
  # Select first n loci
  d <- d[d$locus %in% v.loci_order[1:input$ko_diversity_locus_count], ]
  # Subset columns for plotting
  d <- d[ ,c('strain', 'locus', 'annotation')]
  return(list(d=d, colours=v.colours, anno_name=s.anno_name))
}
# K locus
output$k_locus_bar_plot <- renderPlotly(k_locus_bar_plot())
k_locus_bar_data <- reactive(locus_bar_data('K_locus'))
k_locus_bar_plot <- reactive({
  # Return until input ui element renders and has a default value
  if (is.null(input$ko_diversity_locus_count)) {
    return()
  }
  # Get data
  v.data <- k_locus_bar_data()
  d <- v.data$d
  s.anno_name <- v.data$anno_name
  v.colours <- v.data$colours
  # Create plot
  locus_bar_plot(d, 'K locus', s.anno_name, v.colours)
})
# O locus
output$o_locus_bar_plot <- renderPlotly(o_locus_bar_plot())
o_locus_bar_data <- reactive(locus_bar_data('O_locus'))
o_locus_bar_plot <- reactive({
  # Return until input ui element renders and has a default value
  if (is.null(input$ko_diversity_locus_count)) {
    return()
  }
  # Get data
  v.data <- o_locus_bar_data()
  d <- v.data$d
  s.anno_name <- v.data$anno_name
  v.colours <- v.data$colours
  # Create plot
  locus_bar_plot(d, 'O locus', s.anno_name, v.colours)
})

# Heatmap
output$ko_diversity_st_heatmap_plot <- renderPlotly(ko_diversity_st_heatmap_plot())
ko_diversity_st_heatmap_data <- reactive({
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
  return(list(d=k_vs_o, main_title=main_title))
})
ko_diversity_st_heatmap_plot <- reactive({
  # Get data
  v.data <- ko_diversity_st_heatmap_data()
  d <- v.data$d
  s.title <- v.data$main_title
  # Ensure we have some data
  if (nrow(d) < 1){
    return(NULL)
  }
  # Create heatmap
  heatmaply(
    d,
    main=list(text=s.title, color = "#000000"),
    Rowv=NULL,
    Colv=NULL,
    fontsize_row=10,
    fontsize_col=10,
    hide_colorbar=F,
    revC=F,
    key.title = "# genomes",
    showticklabels=c(FALSE, FALSE),
    plot_method='ggplot',
    colors=c('white', colorRampPalette(colors=c('#f1c280', '#e67d77', '#ED6060'))(max(d))),
    xlab='O loci',
    ylab='K loci',
    margin = c(10, 10, 10, 10),

  )
})

# Download plot/data
# K locus bar plot
output$k_locus_bar_data_download <- downloadHandler(
  filename=reactive(download_filename(paste0('k_diversity_barplot', input$ko_dist_plot_anno), 'csv')),
  content=function(s.filename) { write.csv(k_locus_bar_data()$d, s.filename, row.names=FALSE) }
)
output$k_locus_bar_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0('k_diversity_barplot', input$ko_dist_plot_anno), 'pdf')),
  content=function(s.filename) { download_plot(k_locus_bar_plot, s.filename) }
)
observeEvent(input$k_locus_bar_plot_download_show, {
  download_modal(downloadButton('k_locus_bar_plot_download', class='btn-primary'))
})
# O locus bar plot
output$o_locus_bar_data_download <- downloadHandler(
  filename=reactive(download_filename(paste0('o_diversity_barplot_', input$ko_dist_plot_anno), 'csv')),
  content=function(s.filename) { write.csv(o_locus_bar_data()$d, s.filename, row.names=FALSE) }
)
output$o_locus_bar_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0('o_diversity_barplot_', input$ko_dist_plot_anno), 'pdf')),
  content=function(s.filename) { download_plot(o_locus_bar_plot, s.filename) }
)
observeEvent(input$o_locus_bar_plot_download_show, {
  download_modal(downloadButton('o_locus_bar_plot_download', class='btn-primary'))
})
# Heatmap plot
output$ko_diversity_st_heatmap_data_download <- downloadHandler(
  filename=reactive(download_filename(paste0('ko_diversity_heatmap_', input$ko_dist_plot_anno), 'csv')),
  content=function(s.filename) { write.csv(ko_diversity_st_heatmap_data()$d, s.filename, row.names=FALSE) }
)
output$ko_diversity_st_heatmap_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0('ko_diversity_heatmap_', input$ko_dist_plot_anno), 'pdf')),
  content=function(s.filename) { download_plot(ko_diversity_st_heatmap_plot, s.filename) }
)
observeEvent(input$ko_diversity_st_heatmap_plot_download_show, {
  download_modal(downloadButton('ko_diversity_st_heatmap_plot_download', class='btn-primary'))
})