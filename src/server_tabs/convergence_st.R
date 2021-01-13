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
output$convergence_st_scatter <- renderPlotly({
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
  # Render
  g <- ggplot(d, aes(x=mean_vir, y=mean_res, size=total, colour=annotation, key=ST)) + geom_point(alpha=0.5)
  g <- g + theme_bw() + theme(legend.position='none')
  g <- g + scale_colour_manual(values=v.colours, breaks=names(v.colours))
  g <- g + xlim(c(0, 5)) + ylim(c(0, 3))
  g <- g + xlab('Mean virulence score') + ylab('Mean resistance score')
  ggplotly(g, source='convergence_st_scatter')
})
# Clustered heatmap
output$convergence_st_heatmap <- renderPlotly({
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
  # Select columns
  v.columns <- c('strain', v.virulence_loci, v.resistance_classes)
  d <- d[ ,colnames(d) %in% v.columns]
  # Move strain names to rownames
  rownames(d) <- d$strain
  d <- d[ ,-1]
  # Convert to binary matrix
  v.col_vir <- colnames(d) %in% v.virulence_loci
  v.col_res <- colnames(d) %in% v.resistance_classes
  d[ ,v.col_vir] <- ifelse(d[ ,v.col_vir]=='-', 0, 1)
  d[ ,v.col_res] <- ifelse(d[ ,v.col_res]=='-', 0, 2)
  # Set nice names for display
  v.name_map <- c(v.virulence_loci, v.resistance_classes)
  colnames(d) <- names(v.name_map)[match(colnames(d), v.name_map)]
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