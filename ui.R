ui <- fluidPage(
  title='Kleborate-viz',
  # Initialise shinyjs to allow runjs calls in server code
  # This is required specifically to clear plotly event data
  useShinyjs(),
  # Side bar
  sidebarLayout(
    sidebarPanel(
      # Logo
      div(img(src='logo.png', height=100, width=200)),
      hr(),
      # Builtin datasets
      h4('Built in datasets'),
      actionButton("dataset_global", "Global dataset"),
      actionButton("dataset_euscape", "EuSCAPE dataset"),
      hr(),
      # Input files
      h4('Upload data'),
      fileInput(
        'kleborate_file',
        'Load Kleborate output file (txt)',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      fileInput(
        'metadata_file',
        'Load Metadata table (csv)',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      fileInput(
        'mic_file',
        'Load MIC table (csv)',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      hr(),
      # Data summary
      h4('Data Summary'),
      tableOutput('summary_data'),
      hr(),
      h4('Subset for Analysis'),
      # Species, resistance, virulence selectors
      uiOutput('species_display_radio_list'),
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
          'K/O diversity by ST',
          br(),
          plotlyOutput('ko_diversity_st_scatter', height='400px'),
          br(),
          plotlyOutput('ko_diversity_st_heatmap', height='400px')
        ),
        tabPanel(
          'Temporal trends',
          br(),
          plotlyOutput('prevalence_year_virulence_bar', height='400px'),
          br(),
          plotlyOutput('prevalence_year_resistance_bar', height='400px'),
          br(),
          plotlyOutput('prevalence_year_resistance_line', height='400px')
        ),
        tabPanel(
          'Sample trends',
          br(),
          plotlyOutput('prevalence_sample_scatter', height='400px'),
        ),
        tabPanel(
          'Cumulative K/O prevalence',
          plotlyOutput('cumulative_k_line_combined', height='200px'),
          plotlyOutput('cumulative_k_line_each', height='400px'),
          br(),
          plotlyOutput('cumulative_o_line_combined', height='200px'),
          plotlyOutput('cumulative_o_line_each', height='400px')
        ),
        tabPanel(
          'AMR profile by AMR classes',
          br(),
          plotlyOutput('amr_profile_dist', height='400px'),
          selectInput(
            inputId='amr_profile_var',
            label='MIC variable',
            choices=NULL
          )
        )
      )
    )
  )
)