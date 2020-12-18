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

# K vs O scatter plot by ST
scatter_plot_data <- reactive({
  data_loaded$kleborate[data_selected$rows, ] %>%
    mutate(K_locus = if_else(K_locus_confidence %in% c('Low', 'None'), 'unknown', K_locus)) %>%
    mutate(O_locus = if_else(O_locus_confidence %in% c('Low', 'None'), 'unknown', O_locus)) -> high_confidence_loci
  # Get diversity
  v.st_k <- table(high_confidence_loci$ST, high_confidence_loci$K_locus)
  v.st_o <- table(high_confidence_loci$ST, high_confidence_loci$O_locus)
  v.div_k <- unlist(diversity(v.st_k, index='simpson'))
  v.div_o <- unlist(diversity(v.st_o, index='simpson'))
  # Filter shared STs and order by ST
  v.st <- unique(c(names(v.div_k), names(v.div_o)))
  v.div_k <- v.div_k[match(names(v.div_k), v.st)]
  v.div_o <- v.div_o[match(names(v.div_o), v.st)]
  # Get coefficients
  d <- data.frame(ST=v.st, div_k=v.div_k, div_o=v.div_o)
  d$eff_k <- 1/(1-d$div_k)
  d$eff_o <- 1/(1-d$div_o)
  d$total <- rowSums(v.st_k)
  return(d)
})
output$ko_diversity_st_scatter <- renderPlotly({
  # Handle click events
  ed <- event_data('plotly_click', source='ko_diversity_scatter')
  if(is.null(ed) == FALSE && ed$curveNumber == 0) {
    ko_diversity_st_selected(ed$key)
    # Immediately clear click event and text input
    runjs("Shiny.onInputChange('plotly_click-ko_diversity_scatter', 'null');")
    updateTextInput(session, 'ko_diversity_scatter', value='')
  }
  # Annotate selected ST
  d <- scatter_plot_data()
  if (!is.null(ko_diversity_st_selected())) {
    d$annotation <- ifelse(d$ST==ko_diversity_st_selected(), 'selected', 'notselected')
  } else {
    d$annotation <- 'notselected'
  }
  v.colours <- c('selected'='red', 'notselected'='black')
  # Render
  g <- ggplot(d, aes(x=eff_k, y=eff_o, size=total*2, colour=annotation, key=ST)) + geom_point(alpha=0.5)
  g <- g + theme_bw() + theme(legend.position='none')
  g <- g + scale_colour_manual(values=v.colours, breaks=names(v.colours))
  g <- g + xlab('K locus diversity (effective Simpson\'s)') + ylab('O locus diversity (effective Simpson\'s')
  ggplotly(g, source='ko_diversity_scatter')
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
    showticklabels=c(TRUE, TRUE),
    plot_method='ggplot',
    colors=c('white', colorRampPalette(colors=c('#f1c280', '#e67d77', '#ED6060'))(max(k_vs_o)))
  )
})
