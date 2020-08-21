  ## Mean observation of virulence genes within an ST
  # Subset virulence gene columns and group by ST
  kleborate_data.gene_vir <- KleborateData()[ ,c('ST', virulence_locus_columns)]
  for(j in virulence_locus_columns) {
    kleborate_data.gene_vir[ ,j] <- gsub('-', 0, kleborate_data.gene_vir [,j])
    kleborate_data.gene_vir[kleborate_data.gene_vir[ ,j]!=0,j] <- 1
  }
  kleborate_data.gene_vir <- kleborate_data.gene_vir %>% group_by(ST) %>% summarise(ybt = mean(as.numeric(as.character(Yersiniabactin))), clb = mean(as.numeric(as.character(Colibactin))), iuc = mean(as.numeric(as.character(Aerobactin))), iro = mean(as.numeric(as.character(Salmochelin))))

  # Bar plot function, receives event data
  output$st_virulence <- renderPlotly({
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
      plot_ly(data=st_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title, xaxis=list(range=c(0, 1)))
    }
    else {
      genes <- colnames(kleborate_data.gene_vir)[2:length(kleborate_data.gene_vir)]
      values <- rep(0, length(kleborate_data.gene_vir)-1)
      empty_data <- data.frame(gene=genes, value=values)
      plot_ly(data=empty_data, y=~gene, x=~value, type='bar', orientation = 'h') %>% layout(title=title, yaxis=list(range=c(0, 1)))
    }
  })




  ## Mean observation of specific genes within an ST
  # Subset virulence and antibiotic gene columns and group by ST
  #vir_ab_cols <- c('Yersiniabactin', 'Colibactin', 'Aerobactin', 'Salmochelin', 'AGly', 'Col', 'Fcyn', 'Flq', 'Gly', 'MLS', 'Ntmdz', 'Phe', 'Rif', 'Sul', 'Tet', 'Tmt', 'Bla', 'Bla_Carb', 'Bla_ESBL', 'Bla_ESBL_inhR', 'Bla_broad', 'Bla_broad_inhR')
  vir_ab_cols <- c(virulence_locus_columns,resistance_class_columns)
  kleborate_data.gene_vir_ab <- kleborate_data[ ,c('ST', vir_ab_cols)]
  for(j in vir_ab_cols) {
    kleborate_data.gene_vir_ab[ ,j] <- gsub('-', 0, kleborate_data.gene_vir_ab [,j])
    kleborate_data.gene_vir_ab[kleborate_data.gene_vir_ab[ ,j]!=0,j] <- 1
  }
  kleborate_data.gene_vir_ab <- kleborate_data.gene_vir_ab %>% group_by(ST) %>% summarise(ybt = mean(as.numeric(as.character(Yersiniabactin))), clb = mean(as.numeric(as.character(Colibactin))), iuc = mean(as.numeric(as.character(Aerobactin))), iro = mean(as.numeric(as.character(Salmochelin))), Agly = mean(as.numeric(as.character(AGly))), Col = mean(as.numeric(as.character(Col))), Fcyn = mean(as.numeric(as.character(Fcyn))), Flq = mean(as.numeric(as.character(Flq))), Gly = mean(as.numeric(as.character(Gly))), MLS = mean(as.numeric(as.character(MLS))), Ntmdz = mean(as.numeric(as.character(Ntmdz))), Phe = mean(as.numeric(as.character(Phe))), Rif= mean(as.numeric(as.character(Rif))), Sul = mean(as.numeric(as.character(Sul))), Tet = mean(as.numeric(as.character(Tet))), Tmt = mean(as.numeric(as.character(Tmt))), Bla = mean(as.numeric(as.character(Bla))), Bla_Carb = mean(as.numeric(as.character(Bla_Carb))), Bla_ESBL = mean(as.numeric(as.character(Bla_ESBL))), Bla_ESBL_inhR = mean(as.numeric(as.character(Bla_ESBL_inhR))), Bla_broad = mean(as.numeric(as.character(Bla_broad))), Bla_broad_inhR = mean(as.numeric(as.character(Bla_broad_inhR))))

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