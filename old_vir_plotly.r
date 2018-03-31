  ## Mean observation of virulence genes within an ST
  # Subset virulence gene columns and group by ST
  kleborate_data.gene_vir_ab <- KleborateData()[ ,c('ST', virulence_locus_columns)]
  for(j in virulence_locus_columns) {
    kleborate_data.gene_vir_ab[ ,j] <- gsub('-', 0, kleborate_data.gene_vir_ab [,j])
    kleborate_data.gene_vir_ab[kleborate_data.gene_vir_ab[ ,j]!=0,j] <- 1
  }
  kleborate_data.gene_vir_ab <- kleborate_data.gene_vir_ab %>% group_by(ST) %>% summarise(ybt = mean(as.numeric(as.character(Yersiniabactin))), clb = mean(as.numeric(as.character(Colibactin))), iuc = mean(as.numeric(as.character(Aerobactin))), iro = mean(as.numeric(as.character(Salmochelin))))

  # Bar plot function, receives event data
  output$st_virulence <- renderPlotly({
    ed <- event_data('plotly_click', source='st_scatter')
    title_base <- 'Mean gene presence'
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- kleborate_data.gene_vir_ab[ed$pointNumber+1, ]
      st_name <- as.character(selected_st[ ,1])
      st_data <- selected_st[ ,2:length(selected_st)]
      title <- paste(title_base, '-', st_name)
      genes <- colnames(st_data)
      values <- as.numeric(st_data)
      st_data <- data.frame(gene=genes, value=values)
      plot_ly(data=st_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title, xaxis=list(range=c(0, 1)))
    }
    else {
      genes <- colnames(kleborate_data.gene_vir_ab)[2:length(kleborate_data.gene_vir_ab)]
      values <- rep(0, length(kleborate_data.gene_vir_ab)-1)
      empty_data <- data.frame(gene=genes, value=values)
      plot_ly(data=empty_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title, yaxis=list(range=c(0, 1)))
    }
  })
