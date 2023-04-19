library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(backports)
library(shinycssloaders)
library(shinymanager)
library(shinyvalidate)
library(Cairo)
library(shinyjs)
library(shinycustomloader)

ui <- dashboardPage(
    skin = 'black',
    title = "Stats Tool",
    dashboardHeader(title = 'Stats Tool for Analysis'
    ),
    dashboardSidebar(sidebarMenu(
        menuItem(
            'Instructions',
            tabName = 'instruction',
            icon = icon ('home', lib = 'glyphicon')
        ),
        menuItem(
            'Correlation Matrix',
            tabName = 'corrmatrixpage',
            icon = icon('th-large', lib = 'glyphicon')
        ),
        menuItem(
            'Pairs Plot',
            tabName = 'pairplotpage',
            icon = icon('stats', lib = 'glyphicon')
        ),
        
        menuItem(
            'Bivariate Analysis',
            tabName = 'bivarpage',
            icon = icon('object-align-bottom', lib = 'glyphicon')
        ),
        menuItem(
            'Correlation Table',
            tabName = 'corrtable',
            icon = icon('floppy-disk', lib = 'glyphicon')
        )
    )),
    dashboardBody(tabItems(
        tabItem(tabName = 'instruction',
                fluidRow(
                    box(
                        title = 'Welcome',
                        status = 'primary',
                        h5(
                            "The function of this app is to facilitated data visualization and quickly get preliminary results. If you have any questions please email me at ",
                            strong('jung.sin@hotmail.com')
                        ),
                        h5(
                            strong("Step (1)"),
                            " Please Import your formatted dataset as a CSV, XLSX, or XLS file"
                        ),
                        h5(
                            strong("Step (2)"),
                            " Write the name of the treatment column correctly (case sensitive)"
                        ),
                        h5(
                            strong("Step (3)"),
                            " For looking at more than 2 variables go to ",
                            strong("Correlation Matrix")
                        ),
                        h5(
                            strong("Step (4)"),
                            " For comparing 2 variables go to ",
                            strong("Bivariate Analysis")
                        ),
                        h5(
                            strong("Step (4a)"),
                            " Under Bivariate Analysis: Correlation: Use the red input boxes to select X and Y variables. If two graphs do not show, check treatment column name."
                        ),
                        h5(
                            strong("Step (4b)"),
                            " Under Bivariate Analysis: Boxplot: Use the blue input box to select the variable."
                        ),
                        h5(
                            strong("Step (5)"),
                            " Correlation Matrix: List out the variables you want to compare in the correlation matrix"
                        ),
                        h5(strong("Step (6)"),
                           " Download Button: Under Construction"),
                    ),
                    box(
                        title = 'File Input',
                        fileInput(
                            "uploaded_file",
                            "Choose File",
                            multiple = TRUE,
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv",
                                '.xlsx',
                                '.xls'
                            )
                        ),
                        uiOutput('checktreatment')
                    )
                )),
        tabItem(tabName = 'corrmatrixpage',
                fluidRow(
                    box(
                        title = "Inputs",
                        width = 3,
                        status = 'warning',
                        uiOutput('checkbox')
                    ),
                    
                    tabBox(
                        title = 'Correlation Matrix',
                        width = 9,
                        tabPanel(
                            'Matrix w/ values',
                            plotOutput('corrmatrix2') %>% withLoader(type = 'html', loader = 'loader5')
                        ),
                        tabPanel(
                            'Matrix simplified',
                            plotOutput('corrmatrix1') %>% withLoader(type = 'html', loader = 'loader5')
                        )
                    ),
                    box(
                        title = 'Correlation Matrix Data Table',
                        tableOutput('SummaryOutputTable') %>% withLoader(type = 'html', loader =
                                                                             'loader5'),
                        width = 12,
                        collapsible = TRUE
                    )
                )),
       # ___ Pairplot Tab _____
         tabItem(tabName = 'pairplotpage',
                fluidRow(
                    box(
                        title = "Inputs",
                        width = 3,
                        status = 'warning',
                        uiOutput('pairplot_inputs'),
                        uiOutput('pair_plot_treatment_check'),
                        uiOutput('check_treatment_name_pairplot')
                    )
                ),
                fluidRow(
                    box(
                        title = 'Pairs Plot of Inputs',
                        width = 12,
                        plotOutput('pair_plot_1', height = 800 ) %>% withLoader(type = 'html', loader = 'loader5'), 
                                                                                style = 'display:block;height:80vh;overflow-y: scroll;'
                        ))
                    
                ),
        tabItem(tabName = 'bivarpage',
                fluidRow(
                    box(
                        title = 'Inputs',
                        width = 12,
                        status = 'warning',
                        column(width = 4,
                               uiOutput('checkbox2')),
                        column(width = 4,
                               uiOutput('checkbox3')),
                        column(width = 4,
                               uiOutput('boxplotvar'))
                    ),
                    tabBox(
                        title = 'Graph',
                        width = 12,
                        tabPanel(
                            'Correlation',
                            h4('Single Correlation Results'),
                            plotOutput('corrgraph2') %>% withLoader(type =
                                                                        'html', loader = 'loader5'),
                            hr(),
                            h4('Multi-Group Correlation Graph'),
                            plotOutput('corrgraph3') %>% withLoader(type =
                                                                        'html', loader = 'loader5'),
                            hr(),
                            fluidRow(
                                column(
                                    h4('Single Correlation Table'),
                                    width = 6,
                                    uiOutput('corrcoeff') %>% withLoader(type =
                                                                             'html', loader = 'loader5')
                                ),
                                column(
                                    h4('Multi-Group Correlation Table'),
                                    width = 6,
                                    uiOutput('corrcoeff_bygroup') %>% withLoader(type =
                                                                                     'html', loader = 'loader5')
                                )
                                
                            )
                            
                        ),
                        tabPanel(
                            'Box Plot',
                            h5('Please use the Blue drop down menu'),
                            plotOutput('compgraph2') %>% withLoader(type =
                                                                        'html', loader = 'loader5'),
                            hr(),
                            h5('Statistical Results'),
                            uiOutput('ttestoutput2') %>% withLoader(type = 'html', loader =
                                                                        'loader5'),
                            hr(),
                            uiOutput('ttestoutput') %>% withLoader(type = 'html', loader =
                                                                       'loader5')
                        )
                    )
                )),
        tabItem(tabName = 'corrtable',
                fluidRow(
                    box(
                        title = 'Select Variables',
                        width = 3,
                        status = 'warning',
                        uiOutput('corr_table_var')
                        
                    ),
                    box(
                        title = 'Download Table',
                        width = 3,
                        status = 'warning',
                        downloadButton('download', 'Download Correlation Table')
                    )
                ),
                fluidRow(
                    box(
                        title = 'Correlation Table',
                        status = 'warning',
                        width = 12,
                        DT::dataTableOutput('corr_table_output') %>% withLoader(type = 'html', loader =
                                                                                    'loader5')
                    )
                ))
    ))
)
