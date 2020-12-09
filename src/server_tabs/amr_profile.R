# Summarise data
amr_profile_data <- reactive({
  left_join(data_loaded$metadata, data_loaded$mic_data) %>%
    left_join(., data_loaded$kleborate[data_selected$rows, ]) %>%
    mutate(Omp_mutations_simplified = str_replace_all(Omp_mutations, "-[0-9]+%", "-trunc"), Omp_simple = if_else(Omp_mutations == "-", "wt", "mut")) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "[A-Z]+"), "other", "-")) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "IMP"), "IMP", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "KPC"), "KPC", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "OXA"), "OXA", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "NDM"), "NDM", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, "VIM"), "VIM", Bla_Carb_simplified)) %>%
    mutate(Bla_Carb_simplified = if_else(str_detect(Bla_Carb_acquired, ";"), "multiple", Bla_Carb_simplified)) %>%
    mutate(carbapenemase_omp_combination = paste(Bla_Carb_simplified, Omp_simple, sep = " "))
})
observeEvent(
  data_loaded$mic_data,
  {
    updateSelectInput(
      session,
      'amr_profile_var',
      choices=colnames(data_loaded$mic_data)[colnames(data_loaded$mic_data)!='strain']
    )
  }
)
# Plot
output$amr_profile_dist <- renderPlotly({
  if (is.null(input$amr_profile_var)) {
    return()
  }
  d <- amr_profile_data()
  d$y <- d[[input$amr_profile_var]]
  d <- d[! d$y %in% c(NA, 'NA', 'Not tested'), ]
  d$y <- sub('[<>≤≥=]', '', d$y)
  d$y <- as.numeric(d$y)
  g <- ggplot(d, aes(x=carbapenemase_omp_combination, y=y)) +
    geom_jitter(aes(colour = Omp_mutations_simplified), width=0.25, height=0.5, alpha = 0.5) +
    geom_boxplot(aes(outlier.shape = NA)) +
    scale_x_discrete(limits=c("- wt", "- mut", "IMP wt", "IMP mut", "KPC wt", "KPC mut", "NDM wt", "NDM mut", "OXA wt", "OXA mut", "VIM wt", "VIM mut", "other wt", "other mut", "multiple wt", "multiple mut")) +
    theme(axis.text.x = element_text(angle = 45, hjust =1))
  g <- ggplotly(g)
  print(g)
})
