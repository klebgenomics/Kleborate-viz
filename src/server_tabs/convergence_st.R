# Scatter plot
output$convergence_st_scatter <- renderPlotly({
  d <- kleborate_data()[data_selected$rows, ] %>% 
    group_by(ST) %>%
    summarise(
      mean_vir=mean(virulence_score),
      mean_res=mean(resistance_score),
      total=n()
    )
  marker_function <- function(total) {
    if (nrow(kleborate_data()[data_selected$rows, ])>100) {
      return(log(total, 2) * 4)
    } else {
      return (total * 10)
    }
  }
  # Create scatterplot
  p <- plot_ly(source='st_scatter') %>% 
    add_trace(
      data=d,
      x=~mean_vir,
      y=~mean_res,
      text=~ST, 
      type='scatter',
      mode='markers',
      marker=list(size=~marker_function(total), opacity=0.5),
      name=' '
    ) %>%
    layout(
      title='Mean virulence and resistance score (click to show details)',
      showlegend=FALSE,
      xaxis=list(title='mean virulence score'),
      yaxis=list(title='mean resistance score')
    )
  # Add new trace with colored point if there is event data
  ed <- event_data('plotly_click', source='st_scatter')
  if(is.null(ed) == FALSE && ed$curveNumber == 0) {
    selected_st <- d[ed$pointNumber+1, ]
    p <- p %>% add_trace(
      data=selected_st,
      x=~mean_vir,
      y=~mean_res,
      text=~ST,
      type='scatter',
      mode='markers',
      marker=list(size=~marker_function(total), opacity=0.5),
      name=' '
    )
  }
  return(p)
})
# Clustered heatmap
output$convergence_st_heatmap <- renderPlotly({
  data_by_species <- kleborate_data()[data_selected$rows, ]
  ed <- event_data('plotly_click', source='st_scatter')
  if(is.null(ed) == FALSE && ed$curveNumber == 0) {
    selected_st <- levels(as.factor(as.character(data_by_species$ST)))[ed$pointNumber+1]
    data_matrix <- data_by_species[data_by_species$ST %in% selected_st, ]
    st_name <- as.character(selected_st)
    main_title <- paste('Selected strains:', st_name)
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
        main_title=paste('Most convergent ST:', st_name)
      } else {
        # If there are strains with aerobactin and clinically significant resistance, report them
        data_matrix <- data_by_species[data_by_species$virulence_score>=3 & data_by_species$resistance_score>=1, ]
        main_title <- 'Convergent strains'
      }
    }
  }
  # Convert to binary matrix, format for heatmaply
  rownames(data_matrix) <- data_matrix[ ,1]
  vir_data_matrix <- (data_matrix[ ,c(virulence_locus_columns)]!='-')*1
  res_data_matrix <- (data_matrix[ ,c(resistance_class_columns)]!='-')*2
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
    hide_colorbar=FALSE,
    labRow=NULL,
    fontsize_row=6,
    fontsize_col=7,
    revC=F,
    colors=c('white', '#2171b5', '#ef3b2c'),
    showticklabels=c(TRUE, FALSE),
    main=main_title
  )
})