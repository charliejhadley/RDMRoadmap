# Timeline for RDM Roadmap Dashboard

## Note that a more complicated version of this could be built from http://stackoverflow.com/q/19779665/1659890

navbarMenu(
  "RDM Services",
  tabPanel(
    "About RDM Services",
    wellPanel(HTML(paste(
      "<p><h1>About RDM Services</h1></p>",
      "<p>Most <em>RDM Projects</em> are funded with the goal of creating an <em>RDM Service</em> which will provide infrastructure or information services to assist
      researchers and the University in meeting their RDM commitments - subject to 
      <a href=http://researchdata.ox.ac.uk/university-of-oxford-policy-on-the-management-of-research-data-and-records/>Oxford's Policy on RDM</a>. This section of the RDM Roadmap 
      serves to provide information and statistics on the use of services from RDM Project that have been delivered, the following services are currently included in the report:</p>",
      "<ul>",
      "<li>ORA Data</li>",
      "</ul>",
      sep="")
    ))
  ),
  tabPanel(
    "ORA Data Summary",
    fluidPage(wellPanel(HTML(
      paste(
        "<h1>ORA Data Summary</h1>",
        "<p>The ORA Data project is comprised of three sub projects: VIPR1, VIPR2 and VIPR3.</p>",
        "<p>Note that in the Projects Spreadsheet the project has been separated into it's constituent parts,
        to update the content of this page please email david.tomkins@bodleian.ox.ac.uk or martin.hadley@it.ox.ac.uk</p>"
        ,sep = ""
      )
    )),
    plotOutput("oraData_DepositsAndPublishedPlot"),
    plotOutput("oraData_FundersPlot")),
    value = 'oradata'
    ),
  tabPanel(
    "ORDS Data Summary",
    fluidPage(wellPanel(HTML(
      paste(
        "<h1>ORDS Summary</h1>",
        "<p>ORDS is the University's repository for \"live\" research data, datasets that are being actively developed and collaborated on before
        being submitted to the long-term repository; ORA Data.</p>",
        sep = ""
      )
    )),
    plotOutput("ords_FullandTrialPlot")),
    value = 'ordsdata'
    )
  )