  # plot K locus heatmap for selected data
  output$k_o_heatmap_selected <- renderPlotly({
  
  	selected_st_list <- c("ST11","ST258")
  
  ed <- event_data('plotly_click', source='k_vs_o_plotlyBubble')
  	if(is.null(ed) == FALSE && ed$curveNumber == 0) {
      selected_st <- levels(as.factor(as.character(KleborateData()$ST)))[ed$pointNumber+1]
      selected_st_list <- c(selected_st_list, selected_st) # add to list
      data_matrix <- KleborateData()[KleborateData()$ST %in% selected_st_list,]
      st_name <- paste(as.character(selected_st_list))
      main_title = paste("Selected strains:",st_name)
    }
    else {
    	data_matrix <- KleborateData()[KleborateData()$ST %in% c("ST11","ST258"),]
    }

	k_locus_content <- table(data_matrix$ST,data_matrix$K_locus)
	k_locus_content <- k_locus_content[rowSums(k_locus_content)>0,colSums(k_locus_content)>0]
	
    k_locus_matrix <- as.data.frame.matrix(k_locus_content) # format for heatmaply
      
    # draw plot with heatmaply
	heatmaply(k_locus_matrix)

  })