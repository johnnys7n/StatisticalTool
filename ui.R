library(dplyr, warn.conflicts = FALSE)
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(backports)
library(stringi)
library(xlsx)
library(shinycssloaders)
library(viridis)
library(gridExtra)
library(ggpubr)
library(car)
library(statsExpressions)
library(qqplotr)
library(ggstatsplot)
library(stargazer)
library(pander)
library(jtools)
library(shinymanager)
library(plotly)
library(tibble)
library(shinyvalidate)
library(Cairo)
library(shinyjs)
library(shinycustomloader)
library(ggsignif)
library(gsubfn)

Logged = FALSE
my_username <- 'test'
my_password <- 'test'

ui <- dashboardPage(
  skin = 'black',
  title = "Statistics Tool",
  dashboardHeader(title = span(
    img(
      src = 'https://avatars.githubusercontent.com/u/103721580?v=4',
      width = '200px',
      height = '400px',
      align = 'middle',
      alt = 'johnnys7n'
    )
  )),
  dashboardSidebar(sidebarMenu(
    menuItem(
      'Instructions',
      tabName = 'instruction',
      icon = icon ('wrench', lib = 'glyphicon')
    ),
    menuItem(
      'Correlation Matrix',
      tabName = 'corrmatrixpage',
      icon = icon('th', lib = 'glyphicon')
    ),
    menuItem(
      'Correlation Table',
      tabName = 'corrtablepage',
      icon = icon('list', lib = 'glyphicon')
    ),
    menuItem(
      'Bivariate Analysis',
      tabName = 'bivarpage',
      icon = icon('object-align-bottom', lib = 'glyphicon')
    ),
    menuItem(
      'Download',
      tabName = 'downloadplots',
      icon = icon('save', lib = 'glyphicon')
    )#glyphicon icon list
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = 'instruction',
      box(
        title = 'Welcome',
        width = 4,
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
        h5(
          strong("Step (6)"),
          " Download Button: Under Construction"
        ),
      ),
      box(
        title = 'File Input',
        width = 3,
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
        uiOutput('proNumber'),
        uiOutput('checktreatment'),
        uiOutput('subject')
      )
    ),
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
                width = 12
              )
            )),
    tabItem(tabName = 'corrtablepage',
            fluidRow(
              box(
                title = "Inputs",
                width = 3,
                status = 'warning',
                uiOutput('corrgraph_varbox'),
                uiOutput('corrgraph_checkbox')
              ),
              box(
                title = 'Correlations',
                width = 9,
                tableOutput('corrgraphx1') %>% withLoader(type = 'html', loader = 'loader5')
                )
              )
            ),
    tabItem(tabName = 'bivarpage',
            fluidRow(
              box(
                title = 'Inputs',
                width = 12,
                status = 'warning',
                column(width = 4,
                  uiOutput('checkbox2')
                ),
                column(width = 4,
                       uiOutput('checkbox3')
                       ),
                column(width = 4,
                       uiOutput('boxplotvar')
                )
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
                    column(h4('Single Correlation Table'),
                         width = 6,
                         uiOutput('corrcoeff') %>% withLoader(type =
                                                                'html', loader = 'loader5')
                         ),
                    column(h4('Multi-Group Correlation Table'),
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
    tabItem(
      tabName = 'downloadplots',
      box(
        title = 'Download Plots into Report',
        downloadBttn(
          outputId = 'downloadPlot',
          label = 'Download',
          color = 'success',
          size = 'md',
          block = FALSE,
          no_outline = TRUE,
          icon = shiny::icon('download')
        )
      )
    )
    
  ))
)
