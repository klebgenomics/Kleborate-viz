# Resistance plot
species_resistance_plot <- reactive({
  # Return until input ui element renders and has a default value have we have data
  if (is.null(input$species_selector) | is.null(data_loaded$kleborate)) {
    return()
  }
  v.colours <- c(v.kpsc_colours, species_other_colours())
  d <- data_loaded$kleborate[data_selected$rows, ]
  d$resistance_score <- factor(d$resistance_score, levels=0:3)
  g <- ggplot(data=d, aes(x=resistance_score, fill=species)) + geom_bar()
  g <- g + scale_x_discrete(breaks=0:3, drop=FALSE) + scale_y_continuous(expand=c(0, 0))
  g <- g + scale_fill_manual(values=v.colours)
  g <- g + coord_flip()
  g <- g + theme(
    axis.text.x=element_text(colour='black', size=12),
    axis.text.y=element_text(colour='black', size=12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = 'black', size = 16),
    legend.text = element_text(colour = 'black', size = 12),
    legend.title = element_text(colour = 'black', size = 16),
    panel.background=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(colour='black')
  )
  g <- g + ylab('Number of isolates') + labs(fill='Species')
  return(g)
})
output$species_resistance_plot <- renderPlot({ print(species_resistance_plot()) })
# Virulence plot
species_virluence_plot <- reactive({
  # Return until input ui element renders and has a default value have we have data
  if (is.null(input$species_selector) | is.null(data_loaded$kleborate)) {
    return()
  }
  v.colours <- c(v.kpsc_colours, species_other_colours())
  d <- data_loaded$kleborate[data_selected$rows, ]
  d$virulence_score <- factor(d$virulence_score, levels=0:5)
  g <- ggplot(data=d, aes(x=virulence_score, fill=species)) + geom_bar()
  g <- g + scale_x_discrete(breaks=0:5, drop=FALSE) + scale_y_continuous(expand=c(0, 0))
  g <- g + scale_fill_manual(values=v.colours)
  g <- g + coord_flip()
  g <- g + theme(
    axis.text.x=element_text(colour='black', size=12),
    axis.text.y=element_text(colour='black', size=12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = 'black', size = 16),
    legend.text = element_text(colour = 'black', size = 12),
    legend.title = element_text(colour = 'black', size = 16),
    panel.background=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(colour='black')
  )
  g <- g + ylab('Number of isolates') + xlab('Virulence score') + labs(fill='Species')
  return(g)
})
output$species_virluence_plot <- renderPlot ({ print(species_virluence_plot()) })
# Download species summary plot
output$summary_species_plots_download <- downloadHandler(
  filename=function() { 'summary_species_plots.pdf' },
  content=function(s.filename) {
    pdf(s.filename, width=10, height=6)
    print(resScoreBarBySpecies_reactive())
    print(species_resistance_plot())
    dev.off()
  }
)
