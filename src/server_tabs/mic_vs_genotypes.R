# Colours
v.omp_mutations_colours <- c(
  '-'='#BABABA',
  'OmpK35-trunc'='#80B1D3',
  'OmpK36-trunc'='#B793CC',
  'OmpK36GD'='#000000',
  'OmpK36TD'='#E81CCB',
  'OmpK35-trunc;OmpK36-trunc'='#41AB5D',
  'OmpK35-trunc;OmpK36GD'='#CE2820',
  'OmpK35-trunc;OmpK36TD'='#FDB462'
)
# Ordering
v.amr_gt_order_start <- c(
  '- wt',
  '- mut')
v.amr_gt_order_end <- c(
  'other wt',
  'other mut',
  'multiple wt',
  'multiple mut'
)
v.amr_gt_order_carb <- c(
  'IMP wt',
  'IMP mut',
  'KPC wt',
  'KPC mut',
  'NDM wt',
  'NDM mut',
  'OXA wt',
  'OXA mut',
  'VIM wt',
  'VIM mut'
)
v.amr_gt_order_esbl <- c(
  'CTX-M-14 wt',
  'CTX-M-14 mut',
  'CTX-M-15 wt',
  'CTX-M-15 mut',
  'CTX-M-65 wt',
  'CTX-M-65 mut',
  'CTX-M-other wt',
  'CTX-M-other mut',
  'SHV wt',
  'SHV mut',
  'TEM wt',
  'TEM mut'
)
v.amr_gt_order_bla_acq <- c(
  'DHA wt',
  'DHA mut',
  'LAP wt',
  'LAP mut',
  'OXA wt',
  'OXA mut',
  'TEM wt',
  'TEM mut'
)

# User input
observeEvent(
  data_loaded$mic_data,
  {
    v.cols <- colnames(data_loaded$mic_data)[colnames(data_loaded$mic_data)!='strain']
    updateSelectInput(session, 'amr_profile_mic', choices=v.cols, selected=v.cols[1])
  }
)

# Plot
output$amr_profile_dist_plot <- renderPlotly(amr_profile_dist_plot())
amr_profile_dist_data <- reactive({
  d <- inner_join(data_loaded$mic_data, data_loaded$kleborate[data_selected$rows, ])
  # Set MIC for y-axis
  d$y <- d[[input$amr_profile_mic]]
  d <- d[! d$y %in% c(NA, 'NA', 'Not tested'), ]
  d$y <- sub('[<>≤≥=]', '', d$y)
  d$y <- as.numeric(d$y)
  # Set summary AMR genotype for x-axis
  if (input$amr_profile_geno=='Bla Carb') {
    d$x <- d$carbapenemase_omp_combination
    v.amr_gt_order_x <- v.amr_gt_order_carb
  } else if (input$amr_profile_geno=='Bla ESBL') {
    d$x <- d$ESBL_omp_combination
    v.amr_gt_order_x <- v.amr_gt_order_esbl
  } else if (input$amr_profile_geno=='Bla acquired') {
    d$x <- d$Bla_acquired_omp_combination
    v.amr_gt_order_x <- v.amr_gt_order_bla_acq
  }
  # Set order for x-axis
  v.order <- c(v.amr_gt_order_start, v.amr_gt_order_x, v.amr_gt_order_end)
  d$x <- factor(d$x, levels=v.order)
  return(list(d=d, order=v.order))
})
amr_profile_dist_plot <- reactive({
  # Do not attempt to render without defined input variables
  if (is.null(input$amr_profile_mic)) {
    return()
  }
  # Get data
  v.data <- amr_profile_dist_data()
  d <- v.data$d
  v.order <- v.data$order
  # Plot
  g <- ggplot(d, aes(x=x, y=y))
  g <- g + geom_jitter(aes(colour = Omp_mutations_simplified), width=0.25, height=0.5, alpha= 0.7)
  g <- g + geom_boxplot(outlier.shape=NA)
  g <- g + scale_x_discrete(breaks=unique(v.order), drop=FALSE) + scale_y_log10()
  g <- g + scale_colour_manual('Omp mutations', breaks=names(v.omp_mutations_colours), values=v.omp_mutations_colours)
  g <- g + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1))
  g <- g + xlab('AMR genotype') + ylab('MIC')
  g <- ggplotly(g)
  # Remove outliers
  i <- length(g$x$data)
  g$x$data[i] <- lapply(g$x$data[i], function(d) {
    d$marker <- list(opacity=0)
    return(d)
  })
  g
})

# Download plot/data
output$amr_profile_dist_data_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$amr_profile_geno, '__', input$amr_profile_mic), 'csv')),
  content=function(s.filename) { write.csv(amr_profile_dist_data()$d, s.filename, row.names=FALSE) }
)
output$amr_profile_dist_plot_download <- downloadHandler(
  filename=reactive(download_filename(paste0(input$amr_profile_geno, '__', input$amr_profile_mic), 'pdf')),
  content=function(s.filename) { download_plot(amr_profile_dist_plot, s.filename) }
)
observeEvent(input$amr_profile_dist_plot_download_show, {
  download_modal(downloadButton('amr_profile_dist_plot_download', class='btn-primary'))
})