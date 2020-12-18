# Set colours of other species in plots
species_other_colours <- reactive({
  v.other_species <- unique(data_loaded$kleborate$species[! data_loaded$kleborate$species %in% v.kpsc_names])
  v.other_species <- sort(v.other_species)
  v.colours <- other_species_colour_palette(length(v.other_species))
  names(v.colours) <- v.other_species
  return(v.colours)
})

# Data summary
summary_data <- reactive({
  n.vs <- c('Mean virulence score', round(mean(data_loaded$kleborate$virulence_score), 2))
  n.vr <- c('Mean resistance score', round(mean(data_loaded$kleborate$resistance_score), 2))
  n.us <- c('Total unique species', length(unique(data_loaded$kleborate$species)))
  n.st <- c('Total STs', length(unique(data_loaded$kleborate$ST)))
  d <- t(data.frame(n.us, n.st, n.vs, n.vr))
  return(d)
})
output$summary_data <- renderTable(summary_data(), colnames=F)

# Resistance virulence heatmap
output$res_vir_heatmap <- renderPlotly({ 
  # Check for events
  ed <- event_data('plotly_click', source='res_vir_heatmap')
  if(is.null(ed) == FALSE) {
    selected_st <- d[ed$pointNumber+1, ]
    data_selected$resistance_min <- data_selected$resistance_max <- ed$y
    data_selected$virulence_min <- data_selected$virulence_max <- ed$x
    # Immediately clear click event (otherwise it prevents any changes to data_selected res/vir values)
    runjs("Shiny.onInputChange('plotly_click-res_vir_heatmap', 'null');")
    data_selected$rows <- compute_row_selection()
    updateSliderInput(session, 'res_score_range_slider', value=c(data_selected$resistance_min, data_selected$resistance_max))
    updateSliderInput(session, 'vir_score_range_slider', value=c(data_selected$virulence_min, data_selected$virulence_max))
  }
  ed <- event_data('plotly_selected', source='res_vir_heatmap')
  if(is.null(ed) == FALSE && length(ed) > 0 && nrow(ed) > 0) {
    data_selected$resistance_min <- min(ed$y)
    data_selected$resistance_max <- max(ed$y)
    data_selected$virulence_min <- min(ed$x)
    data_selected$virulence_max <- max(ed$x)
    # Immediately clear click event (otherwise it prevents any changes to data_selected res/vir values)
    runjs("Shiny.onInputChange('plotly_selected-res_vir_heatmap', 'null');")
    data_selected$rows <- compute_row_selection()
    updateSliderInput(session, 'res_score_range_slider', value=c(data_selected$resistance_min, data_selected$resistance_max))
    updateSliderInput(session, 'vir_score_range_slider', value=c(data_selected$virulence_min, data_selected$virulence_max))
  }
  
  # Count scores
  d <- table(
    data_loaded$kleborate$resistance_score,
    data_loaded$kleborate$virulence
  )
  d <- melt(d)
  colnames(d) <- c('Resistance Score', 'Virulence Score', 'Count')
  # Set selected and stroke
  v.selector_res <- d$`Resistance Score` >= data_selected$resistance_min & d$`Resistance Score` <= data_selected$resistance_max
  v.selector_vir <- d$`Virulence Score` >= data_selected$virulence_min & d$`Virulence Score` <= data_selected$virulence_max
  d$selected <- v.selector_res & v.selector_vir
  d$stroke <- ifelse(d$selected, 1, 0)
  # Render plot
  v.colours <- colorRampPalette(c('#05668d', '#028090', '#00a896', '#02c39a'))(100)
  # NOTE: ggplotly does not honor fill for pch=21 points, had to add separate layers for point and outline
  g <- ggplot(d, aes(x=`Virulence Score`, y=`Resistance Score`))
  g <- g + geom_point(aes(stroke=stroke), pch=21, size=6.2)
  g <- g + geom_point(aes(colour=Count), size=5.5)
  g <- g + theme_bw()
  g <- g + scale_x_continuous(breaks=0:5)
  g <- g + scale_colour_gradientn(colours=v.colours, limits=c(0, max(d$Count)))
  g <- g + expand_limits(x=c(-0.5, 5.5), y=c(-0.5, 3.5))
  g <- ggplotly(g, source='res_vir_heatmap') %>%
    layout(xaxis=list(fixedrange=TRUE), yaxis=list(fixedrange=TRUE), dragmode='select') %>%
    config(displayModeBar=FALSE)
  g$x$data[[1]]$hoverinfo <- 'none'
  g$x$data[[2]]$hoverinfo <- 'none'
  g
})


output$res_vir_st_barplot <- renderPlotly({
  # Return until input ui element renders and has a default value have we have data
  if (is.null(input$species_selector) | is.null(data_loaded$kleborate) | is.null(input$summary_st_count)) {
    return()
  }
  d <- data_loaded$kleborate[data_selected$rows, ]
  # Get ST by frequency and order
  v.st_counts <- table(d$ST)
  v.st_ordered <- names(v.st_counts)[order(v.st_counts, decreasing=TRUE)]
  d$ST <- factor(d$ST, levels=v.st_ordered)
  # Select ST
  v.st_selected <- v.st_ordered[1:input$summary_st_count]
  d <- d[d$ST %in% v.st_selected, ]
  g <- ggplot(d, aes(y=ST)) + geom_bar()
  g <- g + theme_bw() + theme(axis.text.x=element_text(angle=45))
  g <- g + coord_flip()
  ggplotly(g)
})
# ST number slider
output$summary_st_count <- renderUI({
  sliderInput(
    inputId='summary_st_count',
    label='Number of STs:',
    min=1,
    max=length(unique(data_loaded$kleborate$ST)),
    value=min(20, length(unique(data_loaded$kleborate$ST)))
  )
})

# Resistance virulence barplot
output$res_vir_barplot <- renderPlotly({
  # Return until input ui element renders and has a default value have we have data
  if (is.null(input$species_selector) | is.null(data_loaded$kleborate)) {
    return()
  }
  v.colours <- c(v.kpsc_colours, species_other_colours())
  d <- data_loaded$kleborate[data_selected$rows, ]
  # Order data
  d$resistance_score <- factor(d$resistance_score, levels=0:3)
  d$virulence_score <- factor(d$virulence_score, levels=0:5)
  d$species <- factor(d$species, levels=names(v.colours))
  # Melt data
  d <- melt(d, id.vars=c('strain', 'species'), measure.vars=c('resistance_score', 'virulence_score'), variable.name='score_name', value.name='score')
  # Rename scores for plotting
  v.name_map <- c('resistance_score'='Resistance score', 'virulence_score'='Virulence score')
  d$score_name <- v.name_map[d$score_name]
  # Set dummy data to force display of score ranges; unable to set each axis directly with facet_wrap
  d.dummy_res <- data.frame(strain=NA, species=NA, score_name='Resistance score', score=factor(0:3))
  d.dummy_vir <- data.frame(strain=NA, species=NA, score_name='Virulence score', score=factor(0:5))
  d.dummy <- rbind(d.dummy_res, d.dummy_vir)
  # Plot
  g <- ggplot(data=d, aes(x=score, fill=species)) + geom_bar()
  g <- g + geom_blank(data=d.dummy)
  g <- g + scale_y_continuous(expand=c(0, 0))
  g <- g + scale_fill_manual(values=v.colours)
  g <- g + coord_flip()
  g <- g + theme_bw() + theme(
    axis.title.y=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(size=12)
  )
  g <- g + facet_wrap( ~ score_name, ncol=1, scales='free')
  g <- g + ylab('Number of isolates') + labs(fill='Species')
  ggplotly(g)
})