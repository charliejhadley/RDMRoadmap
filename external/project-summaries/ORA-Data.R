tabPanel(
  "ORA Data Summary",
  wellPanel(HTML(
    paste(
      "<p>The ORA Data project is comprised of three sub projects: VIPR1, VIPR2 and VIPR3.</p>",
      "<p>Note that in the Projects Spreadsheet the project has been separated into it's constituent parts,
      to update the content of this page please email david.tomkins@bodleian.ox.ac.uk or martin.hadley@it.ox.ac.uk</p>"
      ,sep = ""
    )
  )),
  plotOutput("oraData_DepositsAndPublishedPlot"),
  plotOutput("oraData_FundersPlot")
  )