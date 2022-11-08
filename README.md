# StatisticalTool

A R Shiny application compiling all basic statistical tools for bivariate or multivariate modeling and analysis.

### Introduction:
* The application takes in a structured table as an input (.csv, .xlsx., or .xls) and outputs different statistical tests mainly used for comparisons in treatment groups. The detailed instructions are written directly in the home page of the application.
* Clarification of terms: **Subject**: the independent variable **Treatment**: the groups

### Features:
1. Correlation matrix: 
    * *Treatment/Group specification is not necessary*
    * Select two or more variables to input and output two different correlation matrices (heatmap with p-values per box or simplified heatmap for easier patterning).
    *  Correlation matrix data table will output descriptive statistics for every variable selected. (*note: selecting treatment/group will group variables in the data table*)

2. Correlation Table:
    * **currently under construction**
    * Proposed feature: to select many variables and output a Latex table of coefficient of determination and p-values to numerically rank the correlation between dependent variables.

3. Bivariate Analysis:
    * *Treatment/Group specification is not necessary*
    * Selecting a variable of interest after using the correlation matrices and tables to screen. 
    * In order 
    * To access the full functionality of the boxplot and multi-group correlation graph and table, please input the grouping specification.
    * Statistical tests used: **Boxplot**: one-way ANOVA, Kruskal Wallis Test | **Correlation** Spearman rank correlation test.
  
### Currently Under Construction
1. Download button to output a report

