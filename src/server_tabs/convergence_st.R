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
  data_by_species <- data_loaded$kleborate[data_selected$rows, ]
  ed <- event_data('plotly_click', source='st_scatter')
  if(! is.null(convergence_st_selected())) {
    data_matrix <- data_by_species[data_by_species$ST %in% convergence_st_selected(), ]
    main_title <- convergence_st_selected()
  } else {
    if (nrow(data_by_species) <= 30) {
      # Display all STs
      data_matrix <- data_by_species
      main_title <- 'All STs'
    } else {
      # Display most convergent ST
      if (sum(data_by_species$virulence_score>=3 & data_by_species$resistance_score >=1)==0) {
        kd <- data_by_species %>% 
          group_by(ST) %>% 
          summarise(
            mean_vir=mean(virulence_score),
            mean_res=mean(resistance_score),
            total=n()
          )
        selected_st <- kd$ST[kd$mean_vir*kd$mean_res==max(kd$mean_vir*kd$mean_res)][1]
        data_matrix <- data_by_species[data_by_species$ST %in% selected_st, ]
        st_name <- as.character(selected_st)
        main_title=paste(st_name)
      } else {
        # If there are strains with aerobactin and clinically significant resistance, report them
        data_matrix <- data_by_species[data_by_species$virulence_score>=3 & data_by_species$resistance_score>=1, ]
        main_title <- ''
      }
    }
  }
  # Convert to binary matrix, format for heatmaply
  rownames(data_matrix) <- data_matrix[ ,1]
  vir_data_matrix <- (data_matrix[ ,as.character(v.virulence_loci)]!='-')*1
  res_data_matrix <- (data_matrix[ ,as.character(v.resistance_classes)]!='-')*2
  st_data_matrix <- as.data.frame.matrix(cbind(vir_data_matrix, res_data_matrix))
  # Ensure we have data to plot
  if (nrow(st_data_matrix) < 1) {
    return(NULL)
  }
  # Create heatmap
  heatmaply(
    st_data_matrix,
    Rowv=nrow(st_data_matrix) >= 3,
    Colv=FALSE,
    show_dendrogram = c(FALSE, FALSE),
    hide_colorbar=TRUE,
    labRow=NULL,
    fontsize_row=10,
    fontsize_col=10,
    revC=F,
    colors=c('white', '#1855b7', '#bb363c'),
    showticklabels=c(TRUE, FALSE),
    main=list(text=main_title, color='#000000', size = 8)
  )
})
