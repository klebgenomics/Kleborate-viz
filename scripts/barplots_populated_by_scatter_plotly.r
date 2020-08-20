  # Bar plot function, receives event data
  output$st_virulence <- renderPlotly({
  
    ## Mean observation of virulence genes within an ST
  # Subset virulence gene columns and group by ST
  kleborate_data.gene_vir <- KleborateData()[ ,c('ST', virulence_locus_columns)]
  for(j in virulence_locus_columns) {
    kleborate_data.gene_vir[ ,j] <- gsub('-', 0, kleborate_data.gene_vir [,j])
    kleborate_data.gene_vir[kleborate_data.gene_vir[ ,j]!=0,j] <- 1
  }
  kleborate_data.gene_vir <- kleborate_data.gene_vir %>% group_by(ST) %>% summarise(ybt = mean(as.numeric(as.character(Yersiniabactin))), clb = mean(as.numeric(as.character(Colibactin))), iuc = mean(as.numeric(as.character(Aerobactin))), iro = mean(as.numeric(as.character(Salmochelin))))

    ed <- event_data('plotly_click', source='st_scatter')
    title_base <- 'Virulence gene frequency'
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- kleborate_data.gene_vir[ed$pointNumber+1, ]
      st_name <- as.character(selected_st[ ,1])
      st_data <- selected_st[ ,2:length(selected_st)]
      title <- paste(title_base, '-', st_name)
      genes <- colnames(st_data)
      values <- as.numeric(st_data)
      st_data <- data.frame(gene=genes, value=values)
      plot_ly(data=st_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title_base, xaxis=list(range=c(0, 1)))
    }
    else {
      genes <- colnames(kleborate_data.gene_vir)[2:length(kleborate_data.gene_vir)]
      values <- rep(0, length(kleborate_data.gene_vir)-1)
      empty_data <- data.frame(gene=genes, value=values)
      plot_ly(data=empty_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title_base, yaxis=list(range=c(0, 1)))
    }
  })
  
  # Bar plot function, receives event data
  output$st_resistance <- renderPlotly({
  
    ## Mean observation of resistance genes within an ST
  # Subset resistance gene columns and group by ST
  kleborate_data.gene_res <- KleborateData()[ ,c('ST', resistance_class_columns)]
  for(j in resistance_class_columns) {
    kleborate_data.gene_res[ ,j] <- gsub('-', 0, kleborate_data.gene_res [,j])
    kleborate_data.gene_res[kleborate_data.gene_res[ ,j]!=0,j] <- 1
  }
  kleborate_data.gene_res <- kleborate_data.gene_res %>% group_by(ST) %>% summarise(Agly = mean(as.numeric(as.character(AGly))), Col = mean(as.numeric(as.character(Col))), Fcyn = mean(as.numeric(as.character(Fcyn))), Flq = mean(as.numeric(as.character(Flq))), Gly = mean(as.numeric(as.character(Gly))), MLS = mean(as.numeric(as.character(MLS))), Ntmdz = mean(as.numeric(as.character(Ntmdz))), Phe = mean(as.numeric(as.character(Phe))), Rif= mean(as.numeric(as.character(Rif))), Sul = mean(as.numeric(as.character(Sul))), Tet = mean(as.numeric(as.character(Tet))), Tmt = mean(as.numeric(as.character(Tmt))), Bla = mean(as.numeric(as.character(Bla))), Bla_Carb = mean(as.numeric(as.character(Bla_Carb))), Bla_ESBL = mean(as.numeric(as.character(Bla_ESBL))))
  
    ed <- event_data('plotly_click', source='st_scatter')
    title_base <- 'Resistance gene frequency'
    if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- kleborate_data.gene_res[ed$pointNumber+1, ]
      st_name <- as.character(selected_st[ ,1])
      st_data <- selected_st[ ,2:length(selected_st)]
      title <- paste(title_base, '-', st_name)
      genes <- colnames(st_data)
      values <- as.numeric(st_data)
      st_data <- data.frame(gene=genes, value=values)
      plot_ly(data=st_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title_base, xaxis=list(range=c(0, 1)))
    }
    else {
      genes <- colnames(kleborate_data.gene_res)[2:length(kleborate_data.gene_res)]
      values <- rep(0, length(kleborate_data.gene_res)-1)
      empty_data <- data.frame(gene=genes, value=values)
      plot_ly(data=empty_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title_base, yaxis=list(range=c(0, 1)))
    }
  })
