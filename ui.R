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
        h4('Built in datasets', style='display: inline-block'),
        h5('(click to view)', style='display: inline-block'),
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
        h4('Select scores to plot', style='display: inline-block'),
        h5('(click and drag)', style='display: inline-block'),
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
          h4('Summary by species', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('res_vir_bar_plot_download_show', 'graph_modal'),
            IconButton('res_vir_bar_data_download', 'data_dl'),
          ),
          plotlyOutput('res_vir_bar_plot', height='700px'),
        ),
        tabPanel(
          'Genotypes by ST',
          br(),
          h4('Count of genomes for common STs', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('genotype_st_plot_download_show', 'graph_modal'),
            IconButton('genotype_st_data_download', 'data_dl'),
          ),
          plotlyOutput('genotype_st_dist_plot', height='400px'),
          br(),
          div(
            align='center',
            selectInput(
              inputId='genotype_st_dist_plot_anno',
              label='Annotation variable',
              choices=v.genotype_var_choices
            ),
            uiOutput('genotype_st_count')
          ),
        ),
        tabPanel(
          'Convergence by ST',
          br(),
          h4(
            'Mean virulence and resistance scores by ST (click to select genomes)',
            style='display: inline-block;'
          ),
          div(
            style='display: inline-block;',
            IconButton('convergence_st_scatter_plot_download_show', 'graph_modal'),
            IconButton('convergence_st_scatter_data_download', 'data_dl'),
          ),
          plotlyOutput('convergence_st_scatter_plot', height='400px'),
          br(),
          div(
            align='center',
            div(
              style='display: inline-block',
              textInput(
                'convergence_st_text',
                'Select ST:',
                placeholder='e.g. ST512'
              ),
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
          ),
          br(),
          h4('Genotypes of selected genomes', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('convergence_st_heatmap_plot_download_show', 'graph_modal'),
            IconButton('convergence_st_heatmap_data_download', 'data_dl'),
          ),
          plotlyOutput('convergence_st_heatmap_plot', height='400px')
        ),
        tabPanel(
          'Genotypes vs metadata',
          br(),
          h4('Genome distributions across metadata', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('genotype_metadata_dist_plot_download_show', 'graph_modal'),
            IconButton('genotype_metadata_dist_data_download', 'data_dl'),
          ),
          plotlyOutput('genotype_metadata_dist_plot', height='400px'),
          br(),
          div(
            align='center',
            selectInput(
              inputId='genotype_metadata_dist_plot_anno',
              label='Annotation variable',
              choices=v.genotype_var_choices
            ),
            selectInput(
              inputId='genotype_metadata_dist_plot_group',
              label='Group variable',
              choices=NULL
            ),
            uiOutput('genotype_metadata_group_count')
          ),
        ),
        tabPanel(
          'Convergence vs metadata',
          br(),
          h4('Convergence vs metadata', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('prevalence_sample_scatter_plot_download_show', 'graph_modal'),
            IconButton('prevalence_sample_scatter_data_download', 'data_dl'),
          ),
          plotlyOutput('prevalence_sample_scatter', height='400px'),
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
        ),
        tabPanel(
          'Temporal trends',
          h4('Sampling density by year'),
          fluidRow(
            align='center',
            h5('Drag to select year range'),
            plotlyOutput('temporal_trends_year_hist', width=350, height=200),
          ),
          br(),
          h4('Virulence and resistance scores', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('year_mean_scores_line_plot_download_show', 'graph_modal'),
            IconButton('year_mean_scores_line_data_download', 'data_dl'),
          ),
          plotlyOutput('year_mean_scores_line_plot', height='400px'),
          br(),
          h4('Acquired AMR classes and genes', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('year_mean_resistance_line_plot_download_show', 'graph_modal'),
            IconButton('year_mean_resistance_line_data_download', 'data_dl'),
          ),
          plotlyOutput('year_mean_resistance_line_plot', height='400px'),
          br(),
          h4('Virulence determinant prevalence', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('virulence_prevalence_year_line_plot_download_show', 'graph_modal'),
            IconButton('virulence_prevalence_year_line_data_download', 'data_dl'),
          ),
          plotlyOutput('virulence_prevalence_year_line_plot', height='400px'),
          br(),
          h4('AMR determinant prevalence', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('AMR_prevalence_year_line_plot_download_show', 'graph_modal'),
            IconButton('AMR_prevalence_year_line_data_download', 'data_dl'),
          ),
          plotlyOutput('AMR_prevalence_year_line_plot', height='400px')
        ),
        tabPanel(
          'Cumulative K/O prevalence',
          br(),
          h4('Overall prevalence (K locus', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('cumulative_k_line_combined_plot_download_show', 'graph_modal'),
            IconButton('cumulative_k_line_combined_data_download', 'data_dl'),
            h4(' and  O-locus', style='display: inline-block;'),
            IconButton('cumulative_o_line_combined_plot_download_show', 'graph_modal'),
            IconButton('cumulative_o_line_combined_data_download', 'data_dl'),
            h4(')', style='display: inline-block;')
          ),
          fluidRow(
            column(width = 8, offset = 0, plotlyOutput('cumulative_k_line_combined_plot', height='300px')),
            column(width = 4, offset = 0, plotlyOutput('cumulative_o_line_combined_plot', height='300px')),
          ),
          br(),
          fluidRow(
            h4('K locus prevalence by group', style='display: inline-block;'),
            div(
              style='display: inline-block;',
              IconButton('cumulative_k_line_each_plot_download_show', 'graph_modal'),
              IconButton('cumulative_k_line_each_data_download', 'data_dl'),
            ),
          ),
          fluidRow(
            align='center',
            selectInput(
              inputId='ko_cumulative_var',
              label='Group variable',
              choices=NULL
            )
          ),
          plotlyOutput('cumulative_k_line_each_plot', height='400px'),
          br(),
          h4('O locus prevalence by group', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('cumulative_o_line_each_plot_download_show', 'graph_modal'),
            IconButton('cumulative_o_line_each_data_download', 'data_dl'),
          ),
          plotlyOutput('cumulative_o_line_each_plot', height='400px')
        ),
        tabPanel(
          'K/O diversity',
          br(),
          h4('Count of genomes for common K loci', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('k_locus_bar_plot_download_show', 'graph_modal'),
            IconButton('k_locus_bar_data_download', 'data_dl'),
          ),
          plotlyOutput('k_locus_bar_plot', height='400px'),
          br(),
          h4('Count of genomes for common O loci', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('o_locus_bar_plot_download_show', 'graph_modal'),
            IconButton('o_locus_bar_data_download', 'data_dl'),
          ),
          plotlyOutput('o_locus_bar_plot', height='400px'),
          br(),
          div(
            align='center',
            div(
              style='display: inline-block',
              textInput('ko_diversity_st_text', 'Select ST:', placeholder='e.g. ST512'),
            ),
            div(
              style='display: inline-block',
              actionButton(
                style='margin-bottom: 4px',
                'ko_diversity_st_text_button', 'Select'
              ),
              actionButton(
                style='margin-bottom: 4px',
                'ko_diversity_st_reset_button', 'Reset'
              ),
            ),
            selectInput(
              inputId='ko_dist_plot_anno',
              label='Annotation variable',
              choices=v.genotype_var_choices
            ),
            uiOutput('ko_diversity_locus_count')
          ),
          br(),
          h4('Genotypes of selected genomes', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('ko_diversity_st_heatmap_plot_download_show', 'graph_modal'),
            IconButton('ko_diversity_st_heatmap_data_download', 'data_dl'),
          ),
          plotlyOutput('ko_diversity_st_heatmap_plot', height='600px')
        ),
        tabPanel(
          'MICs by AMR genotype',
          br(),
          h4('MICs by AMR genotype', style='display: inline-block;'),
          div(
            style='display: inline-block;',
            IconButton('amr_profile_dist_plot_download_show', 'graph_modal'),
            IconButton('amr_profile_dist_data_download', 'data_dl'),
          ),
          br(),
          plotlyOutput('amr_profile_dist_plot', height='400px'),
          div(
            align='center',
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
)
