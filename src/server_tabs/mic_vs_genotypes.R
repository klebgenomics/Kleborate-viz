# Summarise data
amr_profile_data <- reactive({
  inner_join(data_loaded$mic_data, data_loaded$kleborate[data_selected$rows, ]) %>%
    # simplify omp
    mutate(Omp_mutations_simplified = str_replace_all(Omp_mutations, "-[0-9]+%", "-trunc"), Omp_simple = if_else(Omp_mutations == "-", "wt", "mut")) %>%
    # simplify carbapenemases and combine with omp
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "IMP"), "IMP", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "KPC"), "KPC", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "OXA"), "OXA", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "NDM"), "NDM", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "VIM"), "VIM", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, ";"), "multiple", Bla_Carb_simplified)) %>%
    mutate(carbapenemase_omp_combination = paste(Bla_Carb_simplified, Omp_simple, sep = " ")) %>%
    # simplify ESBLs and combine with omp
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "CTX-M"), "CTX-M-other", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(Bla_ESBL_acquired == "CTX-M-14", "CTX-M-14", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(Bla_ESBL_acquired == "CTX-M-15", "CTX-M-15", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(Bla_ESBL_acquired == "CTX-M-65", "CTX-M-65", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "SHV"), "SHV", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, "TEM"), "TEM", Bla_ESBL_simplified)) %>%
    mutate(Bla_ESBL_simplified = if_else(str_detect(Bla_ESBL_acquired, ";"), "multiple", Bla_ESBL_simplified)) %>%
    mutate(ESBL_omp_combination = paste(Bla_ESBL_simplified, Omp_simple, sep = " ")) %>%
    # simplify bla acquired and combine with omp
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "TEM"), "TEM", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "OXA"), "OXA", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "LAP"), "LAP", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, "DHA"), "DHA", Bla_acq_simplified)) %>%
    mutate(Bla_acq_simplified = if_else(str_detect(Bla_acquired, ";"), "multiple", Bla_acq_simplified)) %>%
    mutate(Bla_acquired_omp_combination = paste(Bla_acq_simplified, Omp_simple, sep = " "))
})
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
  d <- amr_profile_data()
  # Set MIC for y-axis
  d$y <- d[[input$amr_profile_mic]]
  d <- d[! d$y %in% c(NA, 'NA', 'Not tested'), ]
  d$y <- sub('[<>≤≥=]', '', d$y)
  d$y <- as.numeric(d$y)
  # Set summary AMR genotype for x-axis
  if (input$amr_profile_geno=='Bla Carb') {
    d$x <- d$carbapenemase_omp_combination
  } else if (input$amr_profile_geno=='Bla ESBL') {
    d$x <- d$ESBL_omp_combination
  } else if (input$amr_profile_geno=='Bla acquired') {
    d$x <- d$Bla_acquired_omp_combination
  }
  # Set order for x-axis
  v.amr_genotype_order <- c(
    '- wt',
    '- mut',
    'IMP wt',
    'IMP mut',
    'KPC wt',
    'KPC mut',
    'NDM wt',
    'NDM mut',
    'VIM wt',
    'VIM mut',
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
    'DHA wt',
    'DHA mut',
    'LAP wt',
    'LAP mut',
    'OXA wt',
    'OXA mut',
    'TEM wt',
    'TEM mut',
    'other wt',
    'other mut',
    'multiple wt',
    'multiple mut'
  )
  v.order <- v.amr_genotype_order[v.amr_genotype_order %in% d$x]
  d$x <- factor(d$x, levels=v.order)
  # Plot
  g <- ggplot(d, aes(x=x, y=y)) +
    geom_jitter(aes(colour = Omp_mutations_simplified), width=0.25, height=0.5, alpha = 0.7) +
    geom_boxplot(outlier.shape = NA) +
    scale_x_discrete(breaks=unique(d$x), drop=FALSE) +
    scale_colour_manual("Omp mutations", breaks=c("-", "OmpK35-trunc", "OmpK36-trunc", "OmpK36GD", "OmpK36TD", "OmpK35-trunc;OmpK36-trunc", "OmpK35-trunc;OmpK36GD", "OmpK35-trunc;OmpK36TD"), values=c("#000000", "#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026")) +
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
