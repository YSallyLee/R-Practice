intro_content <- tags$div(
  tags$h2("This app supports exploratory data analysis of Building Energy Performance in Washington, DC."),
  p("The data used in this app are a subset of a dataset used by the city of Washington, DC Department of Energy & Environment (DOEE). 
        It is data on public and private building energy performance disclosed under the Building Energy Performance Standards (BEPS) Program."),
  br(),
  p("The data set for the app was downloaded in October 2023 from:"),
  p(tags$a(href = "https://opendata.dc.gov/datasets/DCGIS::building-energy-performance/about",
           "DC Open Data on Building Energy Performance", 
           target = "_blank")),
  br(),
  p("The data set was filtered to remove data on non-compliant reports and columns not of interest. 
  The data was further reduced by eliminating records where there appeared to be large discrepancies between the square footage of the building recorded for tax purposes and as reported by the owners in their reports. 
  These discrepancies created extreme values that could hinder the analysis. 
  Finally, the variables were renamed to be more human readable. The meta-data definitions can be found at:
"),
  br(),
  p(tags$a(href = "#", "Building Energy Performance Metadata")),
  p(tags$a("This data is meant to be in accordance with the US Government Energy Star Program. For help in interpreting the variables suggest:"),
  p(tags$a(tags$a(href = "#", "EPA Energy Star Program"))
  ))
)
  
  