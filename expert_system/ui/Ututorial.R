tabPanel("Tutorial", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("What's MetaboPro?"),
                     p("MetaboPro is an integrated workflow for untargeted metabolomics data analysis.
                        MetaboPro provides multiple options in each steps of data processing, 
                        including missing value imputation, batch effects removal, sample normalization, 
                        transformation, and scaling. In each step, multiple approaches are implemented, 
                       and systematic evaluations are provided to make sure the robustness of statistical outcome."),
                     
                     tags$img(src = "metabopro.png", width = "900px", height = "300px"),
                     hr(),
                     h2("Source code and example data"),
                     p("The source code can be found on https://github.com/zhanlongmei/metabopro"),
                     p("An example data is avaiable for testing all the functions of Metabopro:"),
                     downloadButton("downloadData2", label = "Download"),
                     hr(),
                     h2("How to use Metabopro?"),
                     p("The usage of Metabopro is very simple. Users need to provide a sample-by-feature matrix and sample list file and follow the instructions in each tab panel"),
                     h4("Sample list"),
                     p("The sample list is a txt file containing the sample information, it should contain four colums: \"sample	batch	class	order\". "),
                     p("sample refers to sample name; batch refers to the batch information; class refers to the biological group, the QC group must be named as \"QC\", as shown in our example data; order refers to the injetion order."),
                     tags$img(src = "sample_list.png",width="400px",height="500px"),
                     h4("Sample-by-feature matrix"),
                     p("The sample-by-feature matrix is a csv file containing the signal intensity of MS features, the first columns must be mz and RT, the rest columns are sample names. Each row represents a mz"),
                     p("The sample names in the peak table must be the same with sample names in the sample list file."),
                     tags$img(src = "sample_feature_matrix.png",width="800px",height="500px"),
                     hr(),
                     h2("Contact Us"),
                     p("If you have any questions, suggestions or remarks, please contact meizhanlong@genomics.cn.")
                     
                   )
         )
)
