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
library(tools)
library(data.table)
library(plotfunctions)
library(flextable)
library(GGally)
library(rstatix)

server <- function(input, output, session) {
    options(shiny.maxRequestSize = 30 * 1024 ^ 2,
            shiny.usecairo = T)
    # Read file ---- Only select numeric variables
    
    outvals <- reactiveValues()
    
    iv <- InputValidator$new()
    iv$add_rule('uploaded_file',
                sv_required(message = 'Please Upload a File'))
    iv$enable()
    ##____________________________________________________________________________
    # Instructions Page
    # create the original data table with all the features
    df_all <- reactive({
        req(input$uploaded_file)
        inFile <- input$uploaded_file
        extension <- tools::file_ext(inFile$name)
        filepath <- inFile$datapath
        
        df_all1 <- switch(
            extension,
            csv = read.csv(
                filepath,
                header = TRUE,
                sep = ',',
                skipNul = TRUE,
                blank.lines.skip = TRUE,
                check.names = TRUE
            ),
            xls = readxl::read_excel(
                path = filepath,
                col_names = TRUE,
                col_types = 'guess',
                na = "0",
                .name_repair = 'universal'
            ),
            xlsx = readxl::read_excel(
                path = filepath,
                col_names = TRUE,
                col_types = 'guess',
                na = "0",
                .name_repair = 'universal'
            )
        )
        df_all1
        
    })
    
    
    # temp data table of only numeric values
    df_num <- reactive({
        req(input$uploaded_file)
        gg1 <- df_all()
        dplyr::select_if(gg1, is.numeric)
        
    })
    # temp data table of only numeric values
    df_char <- reactive({
        req(input$uploaded_file)
        gg1 <- df_all()
        dplyr::select_if(gg1, negate(is.numeric))
        
    })
    # input for treatment column name
    output$checktreatment <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'treatmentcol',
            label = 'Treatment Column Name',
            choices = c(names(df_char()), 'NA'),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = FALSE
        )
    })
    treatmentvar <- reactive({
        req(input$uploaded_file)
        treatmentvar <- df_all() %>% select(input$treatmentcol)
    })
    ## Input for animal ID only table 2
    output$animal_ID <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'animal_input_1',
            label = 'Select Animal ID Column Name',
            choices = c(names(df_char()), 'NA'),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = FALSE
        )
    })
    
    output$checkbox <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'select_var',
            label = 'Select 2+ Variables for Correlation Matrix',
            choices = names(df_num()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = TRUE
        )
    })
    # xvariable assignment
    output$checkbox2 <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'select_var2',
            label = 'Select X Variable',
            choices = names(df_num()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                style = 'btn-danger'
                
            ),
            multiple = FALSE,
            
        )
    })
    xvar <- reactive({
        req(input$select_var2)
        input$select_var2
    })
    #y-variable assignment
    output$checkbox3 <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'select_var3',
            label = 'Select Y Variable',
            choices = names(df_num()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                style = 'btn-danger'
                
            ),
            
            multiple = FALSE
        )
    })
    yvar <- reactive({
        req(input$select_var3)
        input$select_var3
    })
    # variable assignment for boxplot
    output$boxplotvar <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'boxvariable',
            label = 'Select Variable for Boxplot Comparison',
            choices = names(df_num()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                style = 'btn-primary'
                
            ),
            
            multiple = FALSE
        )
    })
    boxvar <- reactive({
        req(input$boxvariable)
        input$boxvariable
    })
    # other assignments
    
    df_yvar <- reactive({
        req(input$uploaded_file)
        df_yvar <- df_all() %>% select(input$select_var3)
    })
    df_xvar <- reactive({
        req(input$uploaded_file)
        df_xvar <- df_all() %>% select(input$select_var2)
    })
    treatmentcolname <- reactive({
        req(input$treatmentcol)
        input$treatmentcol
    })
    ##__________________________________________________________________________
    ## Identifying Outliers
    # New Dataset created after removing all Outliers from data
    # input of the treatment column name
    output$outliercheckbox <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'outlier_inputs',
            label = 'Select 2+ Variables for Outliers Table',
            choices = names(df_all()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = TRUE
        )
    })
    ## Input for outliers only table 2
    output$outliercheckbox2 <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'outlier_inputs_2',
            label = 'Select 1 Variable to check for Outliers',
            choices = names(df_num()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = FALSE
        )
    })
    
    ## table of only selected values for correlation Matrix along with treatment!
    df_sel_outlier_all <- reactive({
        req(input$outlier_inputs)
        df_all() %>% select(input$outlier_inputs, input$treatmentcol)
    })
    df_sel_outlier_one <- reactive({
        req(input$outlier_inputs_2)
        df_all() %>% select(input$animal_input_1, input$outlier_inputs_2, input$treatmentcol)
    })
    
    
    # outlier_table_1 <- reactive({
    #     outliers_table <- function(data_1){
    #
    #         # create a list of all the treatment groups
    #
    #         unique_groups <- unique(data_1[[input$treatmentcol]])
    #         for(i in 1:length(unique_groups)){
    #             data_new <- data_1 %>% filter(.data[[input$treatmentcol]] == unique_groups[i])
    #             print(unique_groups[i])
    #             print(sapply(data_new, function(x) if(class(x) == 'numeric'){
    #                 if(length(boxplot(x, plot = FALSE)$out)>0){
    #                     list(paste(boxplot(x, plot = FALSE)$out))
    #                 }}))}}
    #     outliers_table(df_sel_outlier_all())
    # })
    
    ## Displays Main Data Table with Outliers Removed
    outlier_table_1 <- reactive({
        req(input$outlier_inputs)
        outliers <- function(x) {
            Q1 <- quantile(x, probs = .25, na.rm = TRUE)
            Q3 <- quantile(x, probs = .75, na.rm = TRUE)
            iqr = Q3 - Q1
            
            upper_limit = Q3 + (iqr * 1.5)
            lower_limit = Q1 - (iqr * 1.5)
            
            x > upper_limit | x < lower_limit
        }
        # function to only remove outliers using the outliers function in numerical
        # columns
        remove_outliers <- function(df, cols = names(df)) {
            for (col in cols) {
                if(class(df[[col]])=='numeric'){
                    df <- df[!outliers(df[[col]]), ]
                } 
            }
            df
        }
        
        # creating a new list to append all dataframes created from slicing
        new_dt <- list()
        
        data_1 <- df_sel_outlier_all()
        
        # create a list of the unique treatment group names
        unique_groups <- unique(data_1[[input$treatmentcol]])
        for (i in 1:length(unique_groups)) {
            data_new <-
                df_sel_outlier_all() %>% filter(.data[[input$treatmentcol]] == unique_groups[i])
            new_dt[[i]] <- remove_outliers(data_new)
            
        }
        do.call(rbind, new_dt)
        
        
    })
    output$outlier_removed_table <- DT::renderDataTable({
        req(iv$is_valid())
        print(outlier_table_1())
    })
    output$download_outliers <- downloadHandler(
        filename = "outliers_removed_table.csv",
        content = function(fname) {
            write.csv(outlier_table_1(), fname)
        }
    )
    
    ## Displays Outliers One at a Time
    outlier_table_2 <- reactive({
        req(input$outlier_inputs_2)
        req(input$animal_input_1)
        
        df_outliers <- df_sel_outlier_one() %>% select(.data[[input$treatmentcol]], .data[[input$outlier_inputs_2]], .data[[input$animal_input_1]]) %>%
            group_by(.data[[input$treatmentcol]]) %>% 
            identify_outliers(.data[[input$outlier_inputs_2]])
        df_outliers
        
        
    })
    
    
    output$outlier_table <- DT::renderDataTable({
        req(iv$is_valid())
        print(outlier_table_2())
    })
    output$download_outliers <- downloadHandler(
        filename = "outliers_removed_table.csv",
        content = function(fname) {
            write.csv(outlier_table_1(), fname)
        }
    )
    ##____________________________________________________________________________
    # Correlation Matrix
    ## table of only selected values for correlation Matrix along with treatment!
    df_sel_all <- reactive({
        req(input$select_var)
        df_sel_all <-
            df_all() %>% select(input$select_var, input$treatmentcol)
    })
    ## table of only selected values for correlation Matrix
    df_sel <- reactive({
        req(input$select_var)
        df_sel <- df_num() %>% select(input$select_var)
    })
    df_sel2 <- reactive({
        req(input$select_var2, input$select_var3)
        df_sel2 <-
            df_num() %>% select(input$select_var2, input$select_var3)
    })
    
    
    
    corrPlot1 <- reactive({
        req(input$select_var)
        corr_mat = cor(df_sel(),
                       method = 's',
                       use = "pairwise.complete.obs")
        corrplot(
            corr_mat,
            type = 'upper',
            tl.srt = 45,
            sig.level = 0.05,
            order = 'hclust',
            insig = 'blank'
        )
    })
    #output of the simplified correlation plot
    output$corrmatrix1 <- renderPlot({
        req(iv$is_valid())
        print(corrPlot1())
    })
    
    # correlation matrix plot
    corrmatrixPlot <- reactive({
        req(input$select_var)
        validate(need(
            length(input$select_var) >= 2,
            'Select 2 or more variables'
        ))
        corr.data = function(data) {
            # Get correlations
            cor.vals = cor(data, use = "complete.obs")
            
            # Get p-values
            cor.p = cor.mtest(data, conf.level = 0.95, method = 'spearman')$p
            rownames(cor.p) = rownames(cor.vals)
            colnames(cor.p) = colnames(cor.vals)
            
            cbind(rowvars = rownames(cor.vals), data.frame(cor.vals)) %>%
                gather(colvars, corr,-rowvars) %>%
                left_join(
                    cbind(rowvars = rownames(cor.p), data.frame(cor.p)) %>%
                        gather(colvars, p.value,-rowvars)
                )
        }
        corr.data(df_sel()) %>%
            ggplot(aes(colvars, fct_rev(rowvars))) +
            geom_tile(colour = "grey20",
                      aes(fill = corr),
                      size = 0.5) +
            #geom_point(aes(size=p.value, colour=cut(abs(corr), c(0, 0.01, 0.05, 1), include.lowest=TRUE)), pch=15) +
            geom_text(
                aes(label = sprintf("%1.2f", corr)),
                position = position_nudge(y = 0.2),
                size = 4,
                colour = "white"
            ) +
            geom_text(
                aes(label = paste0(
                    "(", sprintf("%1.2f", p.value), ")"
                )),
                position = position_nudge(y = -0.2),
                size = 3,
                colour = "white"
            ) +
            scale_fill_gradient2(
                low = "red",
                mid = "yellow2",
                high = "blue",
                midpoint = 0,
                limits = c(-1, 1)
            ) +
            scale_size_continuous(range = c(8, 12)) +
            labs(x = "", y = "") +
            theme_classic() +
            coord_fixed() +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) + NULL
        
    })
    
    # correlation matrix output with pvalues
    
    output$corrmatrix2 <- renderPlot({
        req(iv$is_valid())
        outvals$table1 <- corrmatrixPlot()
        print(corrmatrixPlot())
    })
    # summary Table
    custom_table <- reactive({
        req(input$select_var, input$treatmentcol)
        if (input$treatmentcol == 'NA') {
            vtable::st(df_sel(),
                       out = 'return')
        } else {
            vtable::st(
                df_sel_all(),
                group = input$treatmentcol,
                group.long = TRUE,
                out = 'return'
            )
        }
    })
    
    # output of the summary table
    output$SummaryOutputTable <- renderTable({
        req(iv$is_valid())
        custom_table()
    })
    
    ##__________________________________________________________________________
    #Bivariate Analysis
    # Correlation Tab
    #corrlation plot for 2 variables ex: treatment
    bivar_corr_plot <- reactive({
        alk.palette6 <-
            c(
                rgb(10, 25, 84, maxColorValue = 255),
                rgb(18, 134, 166, maxColorValue = 255),
                rgb(100, 203, 215, maxColorValue = 255),
                rgb(200, 200, 200, maxColorValue = 255),
                rgb(255, 255, 204, maxColorValue = 255),
                rgb(255, 255, 255, maxColorValue = 255)
            )
        
        
        gg3 <-
            ggplot(data = df_all(), aes_string(x = xvar(), y = yvar()))
        gg3 + geom_point(size = 2) + stat_smooth(method = 'lm', se = TRUE) + theme_bw() + ylab(char(yvar())) + xlab(char(xvar())) + labs(title =
                                                                                                                                             paste(yvar(), "versus", xvar())) + theme(text = element_text(size = 12)) + scale_color_manual(values =
                                                                                                                                                                                                                                               alk.palette6)
    })
    
    
    output$corrgraph2 <- renderPlot({
        req(iv$is_valid())
        outvals$plot2 <- bivar_corr_plot()
        bivar_corr_plot()
    })
    #Produce plot 2 of correlation graph (bivariate analysis)
    #corrlation plot for 2 variables in: treatment
    
    bivar_corr_plot_treat <- reactive({
        alk.palette6 <-
            c(
                rgb(10, 25, 84, maxColorValue = 255),
                rgb(18, 134, 166, maxColorValue = 255),
                rgb(100, 203, 215, maxColorValue = 255),
                rgb(200, 200, 200, maxColorValue = 255),
                rgb(255, 255, 204, maxColorValue = 255),
                rgb(255, 255, 255, maxColorValue = 255)
            )
        
        x <- c('Treatment Has not been called')
        y <- data.frame(x)
        if (input$treatmentcol == 'NA') {
            return(y)
        } else{
            gg13 <-
                ggplot(data = df_all(),
                       aes_string(
                           x = xvar(),
                           y = yvar(),
                           color = treatmentcolname()
                       ))
            gg13 + geom_point(size = 2) + stat_smooth(method = 'lm', se = TRUE) + theme_bw() + ylab(char(yvar())) + xlab(char(xvar())) + labs(title =
                                                                                                                                                  paste(
                                                                                                                                                      yvar(),
                                                                                                                                                      "versus",
                                                                                                                                                      xvar(),
                                                                                                                                                      "by",
                                                                                                                                                      treatmentcolname(),
                                                                                                                                                      sep = ' '
                                                                                                                                                  )) + theme(text = element_text(size = 12)) + scale_color_manual(values =
                                                                                                                                                                                                                      alk.palette6)
        }
    })
    
    output$corrgraph3 <- renderPlot({
        req(iv$is_valid())
        bivar_corr_plot_treat()
    })
    #OUTPUT RESULTS INTO A TABLE that has correlation by treatment, also called multiplot table
    
    Bivar_Table1 <- reactive({
        validate(need(
            input$treatmentcol,
            'Please specify treatment column first!'
        ))
        corrpvalue <- function(x1, x2) {
            corr1 <- cor.test(x1, x2, method = 'spearman')
            return(corr1$p.value)
        }
        R2value <- function(x1, x2) {
            r2v <- lm(x = x1,
                      y = x2,
                      method = "")
            return(r2v$p.value)
        }
        slope <- function(x3, x4) {
            slopemodel <- lm(x4 ~ x3)
            slopenum <- summary(slopemodel)
            return(slopenum$coefficients[2, 1])
        }
        datacoeff <- df_all() %>%
            dplyr::select(.data[[input$select_var2]], .data[[input$select_var3]]) %>%
            dplyr::summarise(
                count = n(),
                MeanX = round(mean(.data[[input$select_var2]], na.rm = TRUE), digits = 2),
                MeanY = round(mean(.data[[input$select_var3]], na.rm = TRUE), digits = 2),
                Slope = round(slope(.data[[input$select_var2]], .data[[input$select_var3]]), digits = 2),
                Spearman_R = round(
                    cor(.data[[input$select_var3]], .data[[input$select_var2]], method = 'spearman', use = 'complete.obs'),
                    digits = 2
                ),
                Spearman_R.squared = round(abs(
                    cor(.data[[input$select_var3]], .data[[input$select_var2]], method = 'spearman', use = 'complete.obs')
                ) ^ 2, digits = 2),
                Spearman_P.value = round(corrpvalue(.data[[input$select_var2]], .data[[input$select_var3]]), digits = 2)
            )
        datacoeff2 <-
            data.table::transpose(datacoeff, keep.names = 'Stats')
        tbl <- flextable(datacoeff2)
        tbl <- colformat_double(tbl, digits = 3)
        htmltools_value(tbl)
        
    })
    output$corrcoeff <- renderUI({
        req(iv$is_valid())
        Bivar_Table1()
        
    })
    # OUTPUT TABLE for spearmen correlation by treatment
    Bivar_Table2 <- reactive({
        validate(need(
            input$treatmentcol,
            'Please specify treatment column first!'
        ))
        
        corrpvalue <- function(x1, x2) {
            corr1 <- cor.test(x1, x2, method = 'spearman')
            return(corr1$p.value)
        }
        slope2 <- function(x3, x4) {
            coef(lm(x4 ~ x3))[[2]]
        }
        datacoeff <- df_all() %>%
            dplyr::select(.data[[input$select_var2]], .data[[input$select_var3]], .data[[input$treatmentcol]]) %>%
            dplyr::group_by(.data[[input$treatmentcol]]) %>%
            dplyr::summarise(
                count = n(),
                MeanX = round(mean(.data[[input$select_var2]], na.rm = TRUE), digits = 2),
                MeanY = round(mean(.data[[input$select_var3]], na.rm = TRUE), digits = 2),
                Slope = round(slope2(.data[[input$select_var2]], .data[[input$select_var3]]), digits = 3),
                Spearman_R = round(
                    cor(.data[[input$select_var3]], .data[[input$select_var2]], method = 'spearman', use = 'complete.obs'),
                    digits = 3
                ),
                Spearman_R.squared = round(abs(
                    cor(.data[[input$select_var3]], .data[[input$select_var2]], method = 'spearman', use = 'complete.obs')
                ) ^ 2, digits = 3),
                Spearman_P.value = round(corrpvalue(.data[[input$select_var2]], .data[[input$select_var3]]), digits = 3)
            )
        datacoeff2 <-
            data.table::transpose(datacoeff, keep.names = 'Stats')
        tbl <- flextable(datacoeff2)
        tbl <-
            colformat_double(
                x = tbl,
                i = c('MeanX', 'MeanY', 'SpearmanR', 'R2', 'pvalue'),
                digits = 3
            )
        htmltools_value(tbl)
        
        
    })
    output$corrcoeff_bygroup <- renderUI({
        req(iv$is_valid())
        Bivar_Table2()
    })
    ## BOX PLOT Tab
    #comparing the selected Yvariable against the treatment groups
    Boxplot1 <- reactive({
        alk.palette6 <-
            c(
                rgb(10, 25, 84, maxColorValue = 255),
                rgb(18, 134, 166, maxColorValue = 255),
                rgb(100, 203, 215, maxColorValue = 255),
                rgb(200, 200, 200, maxColorValue = 255),
                rgb(255, 255, 204, maxColorValue = 255),
                rgb(255, 255, 255, maxColorValue = 255)
            )
        datadf <- df_all()
        treatdf <- treatmentcolname()
        if (data.table::uniqueN(datadf[[treatdf]]) <= 2) {
            ggplot2::ggplot(
                data = df_all(),
                aes_string(
                    x = treatmentcolname(),
                    y = boxvar(),
                    fill = treatmentcolname()
                )
            ) +
                geom_boxplot(
                    stat = 'boxplot',
                    outlier.colour = 'red',
                    show.legend = TRUE
                ) +
                ylab(char(boxvar())) + xlab(char(treatmentcolname())) +
                labs(title = paste('Comparing ', boxvar(), " by treatment")) +
                theme_bw() + theme(text = element_text(size = 12)) +
                scale_fill_manual(values = alk.palette6) +
                stat_compare_means(
                    method = 'wilcox.test',
                    paired = FALSE,
                    label.x.npc = 'left',
                    label.y.npc = 'bottom'
                ) +
                stat_compare_means(
                    method = 'wilcox.test',
                    paired = FALSE,
                    label = 'p.signif',
                    label.x = 1.5,
                    line.color = 'gray',
                    line.size = 0.4
                ) +
                geom_jitter(
                    color = 'black',
                    size = 2,
                    na.rm = TRUE,
                    alpha = 0.9
                )
        } else{
            ggplot2::ggplot(
                data = df_all(),
                aes_string(
                    x = treatmentcolname(),
                    y = boxvar(),
                    fill = treatmentcolname()
                )
            ) +
                geom_boxplot(
                    stat = 'boxplot',
                    outlier.colour = 'red',
                    show.legend = TRUE
                ) +
                ylab(char(boxvar())) + xlab(char(treatmentcolname())) +
                labs(title = paste('Comparing ', boxvar(), " by treatment")) +
                theme_bw() + theme(text = element_text(size = 12)) +
                scale_fill_manual(values = alk.palette6) +
                geom_jitter(
                    color = 'black',
                    size = 2,
                    na.rm = TRUE,
                    alpha = 0.9
                )
            
        }
        
        
        
    })
    output$compgraph2 <- renderPlot({
        req(iv$is_valid())
        outvals$plot4 <- Boxplot1()
        Boxplot1()
    })
    #Outputting T test / ANOVA results to tabbox results
    ttest_output_table <- reactive({
        validate(need(
            input$treatmentcol,
            'Please specify treatment column first!'
        ))
        ttest_pvarF <- function(x1, x2, data) {
            ttest <-
                t.test(
                    formula = as.formula(paste(x1, x2, sep = "~")),
                    var.equal = FALSE,
                    data = data
                )
            return(ttest$p.value)
        }
        ttest_pvarT <- function(x1, x2, data) {
            ttest <-
                t.test(
                    formula = as.formula(paste(x1, x2, sep = "~")),
                    var.equal = TRUE,
                    data = data
                )
            return(ttest$p.value)
        }
        if (length(unique(df_all()[[treatmentcolname()]])) == 2) {
            ttestpvarF <- ttest_pvarF(boxvar(), treatmentcolname(), df_all())
            ttestpvarT <-
                ttest_pvarT(boxvar(), treatmentcolname(), df_all())
            
            Tests <- c("T.Test Pvalue: Unequal Variance",
                       "T.Test Pvalue: Equal Variance")
            Results <- c(ttestpvarF,
                         ttestpvarT)
            datacoeff <- data.frame(Tests, Results)
            
            tbl <- flextable(head(datacoeff))
            tbl <- width(tbl, width = 2.5)
            tbl <- colformat_double(tbl, digits = 5)
            
            htmltools_value(tbl)
        } else {
            return()
            
        }
        
    })
    output$ttestoutput <- renderUI({
        req(iv$is_valid())
        ttest_output_table()
    })
    #table of results from the Mann-Whitney comparisons in boxplot
    Table7 <- reactive({
        mantestP <- function(x1, x2, data) {
            manvar <-
                wilcox.test(as.formula(paste(x1, x2, sep = "~")), data = data)
            return(manvar$p.value)
        }
        anova_p <- function(x1, x2, data) {
            anovap <-
                aov(formula = as.formula(paste(x1, x2, sep = "~")), data = data)
            return(summary(anovap)[[1]][["Pr(>F)"]][1])
        }
        
        if (length(unique(df_all()[[treatmentcolname()]])) == 2) {
            mantest <- mantestP(boxvar(), treatmentcolname(), df_all())
            Tests <- c("Mann-Whitney Test: P-value")
            Results <- c(mantest)
            datacoeff <- data.frame(Tests, Results)
            
            tbl <- flextable(head(datacoeff))
            tbl <- width(tbl, width = 2.5)
            tbl <- colformat_double(tbl, digits = 5)
            
            htmltools_value(tbl)
        }
        else{
            anovapvar <- anova_p(boxvar(), treatmentcolname(), df_all())
            Tests <- c("ANOVA")
            Results <- c(anovapvar)
            datacoeff <- data.frame(Tests, Results)
            
            tbl <- flextable(head(datacoeff))
            tbl <- width(tbl, width = 2.5)
            tbl <- colformat_double(tbl, digits = 5)
            
            htmltools_value(tbl)
            
        }
    })
    
    output$ttestoutput2 <- renderUI({
        req(iv$is_valid())
        Table7()
    })
    
    ##______________________________________________________________________________
    ## Correlation Table
    
    output$corr_table_var <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'select_corr_table_var',
            label = 'Select 2+ Variables for Correlation Table',
            choices = names(df_num()),
            options = list(
                size = 10,
                `actions-box` = TRUE,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = TRUE
        )
    })
    
    df_corrtable <- reactive({
        req(input$uploaded_file)
        df_num() %>% select(input$select_corr_table_var)
    })
    
    # Table of P-value comparisons
    corr_table_1 <- reactive({
        req(input$select_corr_table_var)
        validate(need(
            length(input$select_corr_table_var) >= 2,
            'Please select 2+ Variables'
        ))
        
        df <- data.frame()
        
        corr.data <- function(data) {
            # Get correlations
            cor.vals <-
                cor(data, method = 's', use = "complete.obs")
            
            # Get p-values
            cor.p <-
                cor.mtest(data, conf.level = 0.95, method = 'spearman')$p
            rownames(cor.p) = rownames(cor.vals)
            colnames(cor.p) = colnames(cor.vals)
            
            cbind(rowvars = rownames(cor.vals), data.frame(cor.vals)) %>%
                gather(colvars, corr,-rowvars) %>%
                left_join(
                    cbind(rowvars = rownames(cor.p), data.frame(cor.p)) %>%
                        gather(colvars, p.value,-rowvars)
                )
        }
        df <- corr.data(df_corrtable())
        df
    })
    
    
    output$corr_table_output <- DT::renderDataTable({
        print(corr_table_1())
        
    })
    
    output$download <- downloadHandler(
        filename = "correlation_table.csv",
        content = function(fname) {
            write.csv(corr_table_1(), fname)
        }
    )
    # _______________________ Pairplot ______________________________
    # feature added 4/2023
    # input of the treatment column name
    output$pairplot_inputs <- renderUI({
        req(input$uploaded_file)
        pickerInput(
            inputId = 'pairplot_inputs',
            label = 'Select 2+ Variables for Pairs Plot',
            choices = names(df_num()),
            options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = 'all',
                style = 'btn-primary'
            ),
            multiple = TRUE
        )
    })
    
    # ## UI Output for checking treatment:
    #
    output$pair_plot_treatment_check <- renderUI({
        req(input$uploaded_file)
        checkboxInput(inputId = 'pair_plot_treatment_check',
                      label = 'Color by Treatment?',)
    })
    #
    ## table of only selected values for correlation Matrix along with treatment!
    df_sel_pp_all <- reactive({
        req(input$pairplot_inputs)
        df_sel_pp <-
            df_all() %>% select(input$pairplot_inputs, input$treatmentcol)
    })
    ## table of only selected values for correlation Matrix
    df_sel_pp <- reactive({
        req(input$pairplot_inputs)
        df_sel <- df_num() %>% select(input$pairplot_inputs)
    })
    
    
    pair_plot_1 <- reactive({
        req(input$pairplot_inputs)
        # if true output the pairplot colored by treatment else
        # put the pairplot without the treatment
        alk.palette6 <-
            c(
                rgb(10, 25, 84, maxColorValue = 255),
                rgb(18, 134, 166, maxColorValue = 255),
                rgb(100, 203, 215, maxColorValue = 255),
                rgb(200, 200, 200, maxColorValue = 255),
                rgb(255, 255, 204, maxColorValue = 255),
                rgb(255, 255, 255, maxColorValue = 255)
            )
        if (input$pair_plot_treatment_check == TRUE) {
            GGally::ggpairs(df_sel_pp_all(),
                            mapping = ggplot2::aes(color = .data[[input$treatmentcol]], alpha = 0.5)) +
                scale_fill_manual(values = alk.palette6)
            
            
        } else{
            GGally::ggpairs(df_sel_pp(),
                            upper = list(combo = 'facetdensity')) + theme_bw()
            
        }
        
    })
    
    
    output$pair_plot_1 <- renderPlot({
        req(iv$is_valid())
        print(pair_plot_1())
    })
    
    
    
    
}
