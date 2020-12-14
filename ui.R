ui <- fluidPage(
  title='Kleborate-viz',
  # Initialise shinyjs to allow runjs calls in server code
  # This is required specifically to clear plotly event data
  useShinyjs(),
  # Side bar
  sidebarLayout(
    sidebarPanel(
      # Set width
      width=12/4,
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
          h4('Resistance scores'),
          plotOutput('species_resistance_plot', height='400'),
          br(),
          h4('Virulence scores'),
          plotOutput('species_virluence_plot', height='400'),
          br(),
          div(
            style='position:absolute;right:1em;',
            downloadButton(outputId='summary_species_plots_download', label='Download plots')
          )
        ),
        tabPanel(
          'Genotypes by ST',
          br(),
          h4('Count of genomes for common STs'),
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
            h4('Virulence and resistance scores (click to select a subset)'),
            plotlyOutput('res_vir_heatmap', width ='500px', height='400px')
          )
        ),
        tabPanel(
          'Convergence by ST',
          br(),
          h4('Mean virulence and resistance scores by ST (click to select genomes)'),
          plotlyOutput('convergence_st_scatter', height='400px'),
          br(),
          br(),
          h4('Genotypes of selected genomes'),
          plotlyOutput('convergence_st_heatmap', height='400px')
        ),
        tabPanel(
          'K/O diversity by ST',
          br(),
          h4('K and O locus diversity by ST (click to select subset)'),
          plotlyOutput('ko_diversity_st_scatter', height='400px'),
          br(),
          h4('Genotypes of selected genomes'),
          plotlyOutput('ko_diversity_st_heatmap', height='600px')
        ),
        tabPanel(
          'Temporal trends',
          h4('Year selector'),
          fluidRow(
            align='center',
            sliderInput(
              inputId='year_range_slider',
              label='',
              sep='',
              min=0, max=50, step=1, value=c(0, 50)
            ),
          ),
          br(),
          h4('Virulence and resistance scores'),
          plotlyOutput('year_mean_scores_line', height='400px'),
          br(),
          h4('Acquired AMR classes and genes'),
          plotlyOutput('year_mean_resistance_line', height='400px'),
          br(),
          h4('Virulence determinant prevalence'),
          plotlyOutput('virulence_prevalence_year_line', height='400px'),
          br(),
          h4('AMR determinant prevalence'),
          plotlyOutput('AMR_prevalence_year_line', height='400px')
        ),
        tabPanel(
          'Sample trends',
          br(),
          plotlyOutput('prevalence_sample_scatter', height='400px'),
        ),
        tabPanel(
          'Cumulative K/O prevalence',
          br(),
          h4('K locus overall prevalence'),
          plotlyOutput('cumulative_k_line_combined', height='300px'),
          br(),
          h4('O locus overall prevalence'),
          plotlyOutput('cumulative_o_line_combined', height='300px'),
          br(),
          h4('K locus prevalence by group'),
          fluidRow(
            align='center',
            selectInput(
              inputId='ko_cumulative_var',
              label='Group variable',
              choices=NULL
            )
          ),
          plotlyOutput('cumulative_k_line_each', height='400px'),
          br(),
          h4('O locus prevalence by group'),
          plotlyOutput('cumulative_o_line_each', height='400px')
        ),
        tabPanel(
          'MICs by AMR genotype',
          br(),
          plotlyOutput('amr_profile_dist', height='400px'),
          selectInput(
            inputId='amr_profile_mic',
            label='MIC variable',
            choices=NULL
          ),
          selectInput(
            inputId='amr_profile_geno',
            label='AMR genotype',
            choices=c(
              'Bla Carb',
              'Bla ESBL',
              'Bla acquired'
            )
          )
        )
      )
    )
  )
)