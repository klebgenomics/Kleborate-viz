# Colours
v.omp_mutations_colours <- c(
  '-'='#000000',
  'OmpK35-trunc'='#ffffb2',
  'OmpK36-trunc'='#fed976',
  'OmpK36GD'='#feb24c',
  'OmpK36TD'='#fd8d3c',
  'OmpK35-trunc;OmpK36-trunc'='#fc4e2a',
  'OmpK35-trunc;OmpK36GD'='#e31a1c',
  'OmpK35-trunc;OmpK36TD'='#b10026'
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
output$amr_profile_dist <- renderPlotly({
  if (is.null(input$amr_profile_mic)) {
    return()
  }
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
  # Plot
  g <- ggplot(d, aes(x=x, y=y)) +
    geom_jitter(aes(colour = Omp_mutations_simplified), width=0.25, height=0.5, alpha = 0.7) +
    geom_boxplot(outlier.shape = NA) +
    scale_x_discrete(breaks=unique(v.order), drop=FALSE) +
    scale_colour_manual("Omp mutations", breaks=names(v.omp_mutations_colours), values=v.omp_mutations_colours) +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust =1)) +
    xlab("AMR genotype") +
    ylab("MIC")
  g <- ggplotly(g)
  # Remove outliers
  i <- length(g$x$data)
  g$x$data[i] <- lapply(g$x$data[i], FUN = function(x){
    x$marker = list(opacity = 0)
    return(x)
  })
  print(g)
})