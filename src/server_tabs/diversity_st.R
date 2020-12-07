# K vs O scatter plot by ST
output$ko_diversity_st_scatter <- renderPlotly({
  # Create dataframe with simpson diversity and total number of genomes per ST
  st_vs_k <- table(kleborate_data()[data_selected$rows, ]$ST, kleborate_data()[data_selected$rows,]$K_locus)
  st_vs_o <- table(kleborate_data()[data_selected$rows, ]$ST, kleborate_data()[data_selected$rows,]$O_locus)
  div_k <- as.data.frame(diversity(st_vs_k, index='simpson'))
  div_o <- as.data.frame(diversity(st_vs_o, index='simpson'))
  div_combined <- merge(div_k, div_o, by=0)
  colnames(div_combined) <- c('ST', 'kdiv', 'odiv')
  div_combined$keff <- 1/(1-div_combined$kdiv)
  div_combined$oeff <- 1/(1-div_combined$odiv)
  div_combined$total <- rowSums(st_vs_k)
  # Create scatterplot
  k_vs_o <- plot_ly(source='ko_diversity_st_scatter') %>%
    add_trace(
      data=div_combined,
      x=~div_combined$keff,
      y=~div_combined$oeff,
      size=~div_combined$total*2,
      text=~paste('ST: ', div_combined$ST)
    ) %>% 
    layout(
      title='K and O diversity (click to show details)',
      xaxis=list(title='K locus'),
      yaxis=list(title='O locus')
    )
  return(k_vs_o)
})
# Heatmap
output$ko_diversity_st_heatmap <- renderPlotly({
  # Get data
  selected_st <- unique(kleborate_data()$ST)
  ed <- event_data('plotly_click', source='ko_diversity_st_scatter')
  if(is.null(ed) == FALSE && ed$curveNumber == 0) {
    selected_st <- levels(as.factor(as.character(kleborate_data()$ST)))[ed$pointNumber+1]
  }
  data_matrix <- kleborate_data()[kleborate_data()$ST==selected_st, ]
  st_name <- paste(as.character(selected_st))
  main_title=paste('Selected strains:', st_name)
  # Format data for plotting
  # NOTE: converting as done below is required to handle corner cases where nrow = 1 or ncol=1
  k_vs_o <- table(data_matrix$K_locus,data_matrix$O_locus)
  k_vs_o <- k_vs_o[rowSums(k_vs_o)>0,colSums(k_vs_o)>0,drop=FALSE]
  k_vs_o <- matrix(k_vs_o, byrow=TRUE, ncol=ncol(k_vs_o), dimnames=list(rownames(k_vs_o), colnames(k_vs_o)))
  # Create heatmap
  heatmaply(
    k_vs_o,
    main=main_title,
    Rowv=NULL,
    Colv=NULL,
    fontsize_row=6,
    fontsize_col=6,
    hide_colorbar=F,
    revC=F,
    showticklabels=c(TRUE, FALSE),
    colors=c('white',colorRampPalette(colors=c('blue', 'red'))(max(k_vs_o)))
  )
})