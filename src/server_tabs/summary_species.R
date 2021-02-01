# Set colours of other species in plots
species_other_colours <- reactive({
  v.other_species <- unique(data_loaded$kleborate$species[! data_loaded$kleborate$species %in% v.kpsc_names])
  v.other_species <- sort(v.other_species)
  v.colours <- other_species_colour_palette(length(v.other_species))
  names(v.colours) <- v.other_species
  return(v.colours)
})

# Resistance virulence barplot
output$res_vir_bar_plot <- renderPlotly(res_vir_bar_plot())
res_vir_bar_data <- reactive({
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
  # Subset columns
  d <- d[ ,c('strain', 'species', 'score', 'score_name')]
  return(list(d=d, dummy=d.dummy, colours=v.colours))
})
res_vir_bar_plot <- reactive({
  # Return until input ui element renders and has a default value have we have data
  if (is.null(input$species_selector) | is.null(data_loaded$kleborate)) {
    return()
  }
  # Get data
  v.data <- res_vir_bar_data()
  d <- v.data$d
  d.dummy <- v.data$dummy
  v.colours <- v.data$colours
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

# Download plot/data
output$res_vir_bar_data_download <- downloadHandler(
  filename=reactive(download_filename('species_summary', 'csv')),
  content=function(s.filename) { write.csv(res_vir_bar_data()$d, s.filename, row.names=FALSE) }
)
output$res_vir_bar_plot_download <- downloadHandler(
  filename=reactive(download_filename('species_summary', 'pdf')),
  content=function(s.filename) { download_plot(res_vir_bar_plot, s.filename) }
)
observeEvent(input$res_vir_bar_plot_download_show, {
  download_modal(downloadButton('res_vir_bar_plot_download', class='btn-primary'))
})