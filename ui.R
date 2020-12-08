ui <- fluidPage(
  # Initialise shinyjs to allow runjs calls in server code
  # This is required specifically to clear plotly event data
  useShinyjs(),
  # Side bar
  sidebarLayout(
    sidebarPanel(
      # Logo
      div(img(src='logo.png', height=100, width=200)),
      br(),
      # Input files
      fileInput(
        'kleborate_file',
        'Load Kleborate Output File (txt)',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      fileInput(
        'metadata_file',
        'Load Metadata file (csv)',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      fileInput(
        'mic_file',
        'Load MIC table (csv)',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      # Data summary
      h4('Data Summary'),
      tableOutput('summary_data'),
      h4('Subset for Analysis'),
      br(),
      # Species, resistance, virulence selectors
      uiOutput('species_display_radio_list'),
      br(),
      sliderInput(
        inputId='res_score_range_slider',
        label='Resistance scores:',
        min=0, max=3, step=1, value=c(0, 3)
      ),
      sliderInput(
        inputId='vir_score_range_slider',
        label='Virulence scores:',
        min=0, max=5, step=1, value=c(0, 5)
      ),
    ),
    # Tab navigator and main display
    mainPanel(
      tabsetPanel(
        id='primary',
        tabPanel(
          'Summary by species',
          br(),
          plotOutput('species_resistance_plot', height='400'),
          br(),
          plotOutput('species_virluence_plot', height='400'),
          br(),
          div(
            style='position:absolute;right:1em;',
            downloadButton(outputId='summary_species_plots_download', label='Download plots')
          )
        ),
        tabPanel(
          'Genotypes by ST',
          plotOutput('genotype_st_dist_plot', height='400px'),
          br(),
          selectInput(
            inputId='genotype_st_dist_plot_var',
            label='Annotation variable',
            choices=v.genotype_st_var_choices
          ),
          downloadButton(
            outputId='genotype_st_plot_download',
            label='Download the plot'
          ),
          downloadButton(
            outputId='genotype_st_data_download',
            label='Download the data'
          ),
          column(
            8,
            wellPanel(uiOutput('genotype_st_count'))
          ),
          column(
            12,
            h4('Resistance vs virulence across all strains (click to select a subset)'),
            plotlyOutput('res_vir_heatmap', width ='500px', height='400px')
          )
        ),
        tabPanel(
          'Convergence by ST',
          br(),
          plotlyOutput('convergence_st_scatter', height='400px'),
          br(),
          plotlyOutput('convergence_st_heatmap', height='400px')
        ),
        tabPanel(
          'Diversity by ST',
          br(),
          plotlyOutput('ko_diversity_st_scatter', height='400px'),
          br(),
          plotlyOutput('ko_diversity_st_heatmap', height='400px')
        ),
        tabPanel(
          'Prevalence by Year',
          br(),
          plotlyOutput('prevalence_year_virulence_bar', height='400px'),
          br(),
          plotlyOutput('prevalence_year_resistance_bar', height='400px'),
          br(),
          plotlyOutput('prevalence_year_resistance_line', height='400px')
        ),
        tabPanel(
          'Prevalence by Sample',
          br(),
          plotlyOutput('prevalence_sample_scatter', height='400px'),
        ),
        tabPanel(
          'Cumulative K/O locus',
          plotlyOutput('cumulative_k_line', height='400px'),
          br(),
          plotlyOutput('cumulative_o_line', height='400px')
        ),
        tabPanel(
          'AMR profile by AMR classes',
          br(),
          plotlyOutput('amr_profile_dist', height='400px')
        )
      )
    )
  )
)