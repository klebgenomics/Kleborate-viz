# User ST selection
convergence_st_selected <- reactiveVal()
observeEvent(
  data_loaded$kleborate,
  {
    convergence_st_selected(NULL)
    updateTextInput(session, 'convergence_st_text', value='')
  }
)
observeEvent(
  input$convergence_st_text_button,
  {
    v.sts <- unique(data_loaded$kleborate$ST)
    if (! tolower(input$convergence_st_text) %in% tolower(v.sts)) {
      showNotification(
        paste('Could not find', input$convergence_st_text, 'in provided dataset'),
        type='error',
        duration=NULL
      )
    } else {
      v.selector <- which(tolower(v.sts) %in% tolower(input$convergence_st_text))
      convergence_st_selected(v.sts[v.selector])
    }
  }
)
observeEvent(
  input$convergence_st_reset_button,
  {
    convergence_st_selected(NULL)
    updateTextInput(session, 'convergence_st_text', value='')
  }
)

# Scatter plot
output$convergence_st_scatter_plot <- renderPlotly(convergence_st_scatter_plot())
convergence_st_scatter_data <- reactive({
  d <- data_loaded$kleborate[data_selected$rows, ] %>% 
    group_by(ST) %>%
    summarise(
      mean_vir=mean(virulence_score),
      mean_res=mean(resistance_score),
      total=n()
    )
  d <- d[!is.na(d$ST), ]
  # Handle click events
  ed <- event_data('plotly_click', source='convergence_st_scatter')
  if(is.null(ed) == FALSE && ed$curveNumber == 0) {
    convergence_st_selected(ed$key)
    # Immediately clear click event and text input
    runjs("Shiny.onInputChange('plotly_click-convergence_st_scatter', 'null');")
    updateTextInput(session, 'convergence_st_text', value='')
  }
  # Annotate selected ST
  if (!is.null(convergence_st_selected())) {
    d$annotation <- ifelse(d$ST==convergence_st_selected(), 'selected', 'notselected')
  } else {
    d$annotation <- 'notselected'
  }
  v.colours <- c('selected'='red', 'notselected'='black')
  # Subset specific columns for plotting
  d <- d[ ,c('mean_vir', 'mean_res', 'total', 'ST', 'annotation')]
  return(list(d=d, colours=v.colours))
})
convergence_st_scatter_plot <- reactive({
  # Get data
  v.data <- convergence_st_scatter_data()
  d <- v.data$d
  v.colours <- v.data$colours
  # Render
  g <- ggplot(d, aes(x=mean_vir, y=mean_res, size=total, colour=annotation, key=ST)) + geom_point(alpha=0.5)
  g <- g + theme_bw() + theme(legend.position='none')
  g <- g + scale_colour_manual(values=v.colours, breaks=names(v.colours))
  g <- g + xlim(c(0, 5)) + ylim(c(0, 3))
  g <- g + xlab('Mean virulence score') + ylab('Mean resistance score')
  ggplotly(g, source='convergence_st_scatter')
})

# Clustered heatmap
output$convergence_st_heatmap_plot <- renderPlotly(convergence_st_heatmap_plot())
convergence_st_heatmap_data <- reactive({
  #  Set NA ST to string and determine display data
  d <- data_loaded$kleborate[data_selected$rows, ]
  d$ST[is.na(d$ST)] <- 'NA'
  if (! is.null(convergence_st_selected())) {
    # Display user selected
    d <- d[d$ST==convergence_st_selected(), ]
    s.title <- convergence_st_selected()
  } else if (nrow(d) <= 30) {
    # Display all
    s.title <- 'All STs'
  } else if (sum(d$virulence_score>=3 & d$resistance_score >=1)==0) {
    # Too many to display all; select most convergent
    d.con <- d %>%
      group_by(ST) %>%
      summarise(
        mean_vir=mean(virulence_score),
        mean_res=mean(resistance_score),
        total=n()
      )
    v.score_multi <- d.con$mean_vir * d.con$mean_res
    s.st <- d.con$ST[which.max(v.score_multi)]
    d <- d[d$ST==s.st, ]
    s.title <- paste('Auto selected:', s.st)
  } else {
    # Otherwise display strains with aerobactin and clinically significant resistance
    d <- d[d$virulence_score>=3 & d$resistance_score>=1, ]
    s.title <- 'Auto selected: multiple'
  }
  # Select only presence/absence columns to display; also get nice display names
  v.vir_pa <- v.virulence_loci[grepl('_pa$', v.virulence_loci)]
  v.res_pa <- v.resistance_classes[grepl('_pa$', v.resistance_classes)]
  v.vir_pa_col_names <- sub('_pa$', '', v.vir_pa)
  v.res_pa_col_names <- sub('_pa$', '', v.res_pa)
  v.columns <- setNames(c(v.vir_pa_col_names, v.res_pa_col_names), c(names(v.vir_pa), names(v.res_pa)))
  # Select columns
  d <- d[ ,colnames(d) %in% c('strain', v.columns)]
  # Move strain names to rownames
  rownames(d) <- d$strain
  d <- d[ ,-1]
  # Convert to binary matrix
  v.col_vir <- colnames(d) %in% v.vir_pa_col_names
  v.col_res <- colnames(d) %in% v.res_pa_col_names
  d[ ,v.col_vir] <- ifelse(d[ ,v.col_vir]=='-', 0, 1)
  d[ ,v.col_res] <- ifelse(d[ ,v.col_res]=='-', 0, 2)
  # Set nice names for display
  colnames(d) <- names(v.columns)[match(colnames(d), v.columns)]
  return(list(d=d, title=s.title))
})
convergence_st_heatmap_plot <- reactive({
  # Get data
  v.data <- convergence_st_heatmap_data()
  d <- v.data$d
  s.title <- v.data$title
  # Render
  heatmaply(
    d,
    main=s.title,
    limits=c(0, 2),
    colors=c('white', '#1855b7', '#bb363c'),
    Rowv=nrow(d)>=3,
    Colv=FALSE,
    show_dendrogram = c(FALSE, FALSE),
    hide_colorbar=TRUE,
    showticklabels=c(TRUE, FALSE)
  )
})

# Download plot/data
# Scatter
download_filename <- function(s.prefix, s.suffix) { paste0(s.prefix, download_filename_suffix(s.suffix)) }
output$convergence_st_scatter_data_download <- downloadHandler(
  filename=reactive(download_filename('convergence_scatter__', '.csv')),
  content=function(s.filename) { write.csv(convergence_st_scatter_data()$d, s.filename, row.names=FALSE) }
)
output$convergence_st_scatter_plot_download <- downloadHandler(
  filename=reactive(download_filename('convergence_scatter__', '.pdf')),
  content=function(s.filename) { download_plot(convergence_st_scatter_plot, s.filename) }
)
observeEvent(input$convergence_st_scatter_plot_download_show, {
  download_modal(downloadButton('convergence_st_scatter_plot_download', class='btn-primary'))
})
# Heatmap
download_filename <- function(s.prefix, s.suffix) { paste0(s.prefix, download_filename_suffix(s.suffix)) }
output$convergence_st_heatmap_data_download <- downloadHandler(
  filename=reactive(download_filename('convergence_heatmap__', '.csv')),
  content=function(s.filename) { write.csv(convergence_st_heatmap_data()$d, s.filename, row.names=FALSE) }
)
output$convergence_st_heatmap_plot_download <- downloadHandler(
  filename=reactive(download_filename('convergence_heatmap__', '.pdf')),
  content=function(s.filename) { download_plot(convergence_st_heatmap_plot, s.filename) }
)
observeEvent(input$convergence_st_heatmap_plot_download_show, {
  download_modal(downloadButton('convergence_st_heatmap_plot_download', class='btn-primary'))
})