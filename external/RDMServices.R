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
    source("external/project-summaries/ORA-Data.R", local = TRUE)$value
    ),
  tabPanel(
    "ORDS Summary",
    source("external/project-summaries/ORDS-Data.R", local = TRUE)$value,
    value = 'ordssummary'
  )
  )