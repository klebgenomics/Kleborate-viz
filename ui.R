ui <- fluidPage(
  # Initialise shinyjs to allow runjs calls in server code
  # This is required specifically to clear plotly event data
  useShinyjs(),
  extendShinyjs(script='shiny.js', functions=c()),
  # Title panel
  title='Kleborate-viz',
  fluidRow(
    style='margin-top: 20px',
    column(
      3,
      div(
        align='center',
        h2(
          style='margin-top: 0px; margin-bottom: 20px',
          'Kleborate-viz'),
      ),
      # Side bar
      wellPanel(
        # Input files
        h4('Upload data'),
        fileInput(
          'kleborate_file',
          'Kleborate output (txt)',
          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        fileInput(
          'metadata_file',
          'Metadata table (csv)',
          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        fileInput(
          'mic_file',
          'MIC table (csv)',
          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        hr(),
        # Builtin datasets
        h4('Built in datasets'),
        h5('(click to view)'),
        div(
          align='center',
          actionButton("dataset_global", "Global dataset"),
          actionButton("dataset_euscape", "EuSCAPE dataset"),
        ),
      ),
      wellPanel(
        # Data summary
        h4('Data Summary'),
        tableOutput('summary_data')
      ),
      wellPanel(
        h4('Select species to plot'),
        # Species, resistance, virulence selectors
        uiOutput('species_display_radio_list'),
        br(),
        h4('Select scores to plot'),
        h5('(click and drag)'),
        plotlyOutput('res_var_heatmap', height='300px'),
      ),
    ),
    column(
      9,
      style='margin-top: 5px',
      # Tab navigator and main display
      tabsetPanel(
        id='primary',
        tabPanel(
          'Home',
          div(
            align='center',
            style='margin-top: 20px',
            img(src='logo.png', height=200)
          ),
          div(
            align='center',
            style='margin-top: 40px',
            HTML('Kleborate-viz is a visualisation app for the output of <a href="https://github.com/katholt/Kleborate">Kleborate</a>-'),
            br(),
            HTML('a genotyping tool for <i>Klebsiella pneumoniae</i> and its related species complex.'),
            br(),
            br(),
            HTML('You can upload your own data via the control panel on the left'),
            br(),
            HTML('or view the built in datasets by clicking the <b>Global dataset</b>'),
            br(),
            HTML('or <b>EuSCAPE dataset</b> buttons and selecting one of the plot tabs above.'),
            br(),
            br(),
            HTML('By default plots will include all genomes, but you can use the left side panel'),
            br(),
            HTML('to filter by species and/or <a href="https://github.com/katholt/Kleborate/wiki/Scores-and-counts">Kleborate resistance and virulence scores</a>.'),
            br(),
            HTML('All plots are interactive so you can select the variables to show and/or'),
            br(),
            HTML('hover your curser over the data points to view the underlying data values.'),
            br(),
            br(),
            HTML('For more information, comments and suggestions visit the <a href="https://github.com/kelwyres/Kleborate-viz/wiki/1.-Kleborate-viz-home">Kleborate-viz wiki</a>.'),
          )
        ),
        tabPanel(
          'Summary by species',
          br(),
          plotlyOutput('res_vir_barplot', height='700px'),
        ),
        tabPanel(
          'Genotypes by ST',
          br(),
          h4('Count of genomes for common STs'),
          plotlyOutput('genotype_st_dist_plot', height='400px'),
          br(),
          selectInput(
            inputId='genotype_st_dist_plot_anno',
            label='Annotation variable',
            choices=v.genotype_var_choices
          ),
          downloadButton(
            outputId='genotype_st_data_download',
            label='Download the data'
          ),
          column(
            8,
            wellPanel(uiOutput('genotype_st_count'))
          ),
        ),
        tabPanel(
          'Convergence by ST',
          br(),
          h4('Mean virulence and resistance scores by ST (click to select genomes)'),
          plotlyOutput('convergence_st_scatter', height='400px'),
          br(),
          div(
            style='display: inline-block',
            textInput('convergence_st_text', 'Select ST:', placeholder='e.g. ST512'),
          ),
          div(
            style='display: inline-block',
            actionButton(
              style='margin-bottom: 5px',
              'convergence_st_text_button', 'Select'
            ),
            actionButton(
              style='margin-bottom: 5px',
              'convergence_st_reset_button', 'Reset'
            ),
          ),
          br(),
          h4('Genotypes of selected genomes'),
          plotlyOutput('convergence_st_heatmap', height='400px')
        ),
        tabPanel(
          'K/O diversity',
          br(),
          h4('Count of genomes for common K loci'),
          plotlyOutput('k_locus_barplot', height='400px'),
          br(),
          h4('Count of genomes for common O loci'),
          plotlyOutput('o_locus_barplot', height='400px'),
          br(),
          div(
            style='display: inline-block',
            textInput('ko_diversity_st_text', 'Select ST:', placeholder='e.g. ST512'),
          ),
          div(
            style='display: inline-block',
            actionButton(
              style='margin-bottom: 5px',
              'ko_diversity_st_text_button', 'Select'
            ),
            actionButton(
              style='margin-bottom: 5px',
              'ko_diversity_st_reset_button', 'Reset'
            ),
          ),
          fluidRow(
            column(
              6,
              selectInput(
                inputId='ko_dist_plot_anno',
                label='Annotation variable',
                choices=v.genotype_var_choices
              ),
            ),
            column(
              6,
              uiOutput('ko_diversity_locus_count')
            ),
          ),
          br(),
          h4('Genotypes of selected genomes'),
          plotlyOutput('ko_diversity_st_heatmap', height='600px')
        ),
        tabPanel(
          'Genotypes by metadata',
          br(),
          h4('Genome distributions across metadata'),
          plotlyOutput('genotype_metadata_dist_plot', height='400px'),
          br(),
          selectInput(
            inputId='genotype_metadata_dist_plot_anno',
            label='Annotation variable',
            choices=v.genotype_var_choices
          ),
          fluidRow(
            column(
              6,
              selectInput(
                inputId='genotype_metadata_dist_plot_group',
                label='Group variable',
                choices=NULL
              ),
            ),
            column(
              6,
              uiOutput('genotype_metadata_group_count')
            ),
          ),
        ),
        tabPanel(
          'Sample trends',
          br(),
          fluidRow(
            align='center',
            selectInput(
              inputId='sample_trends_var',
              label='Group variable',
              choices=NULL
            )
          ),
          fluidRow(
            align='center',
            selectInput(
              inputId='sample_trends_col',
              label='Colour variable',
              choices=NULL
            )
          ),
          plotlyOutput('prevalence_sample_scatter', height='400px'),
        ),
        tabPanel(
          'Temporal trends',
          h4('Year selector'),
          fluidRow(
            align='center',
            uiOutput('temporal_trends_year_slider'),
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
          'Cumulative K/O prevalence',
          br(),
          h4('Overall prevalence'),
          fluidRow(
            column(width = 8, offset = 0, plotlyOutput('cumulative_k_line_combined', height='300px')),
            column(width = 4, offset = 0, plotlyOutput('cumulative_o_line_combined', height='300px')),
          ),
          br(),
          fluidRow(h4('K locus prevalence by group')),
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