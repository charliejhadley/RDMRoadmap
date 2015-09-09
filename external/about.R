# About Section for RDM Roadmap Dashboard

tabPanel("About",
         tabsetPanel(
           "foo",
           tabPanel("RDM at Oxford",
                    fluidPage(HTML(
                      paste(
                        "<p>
                        <p>Welcome to the Research Data Management delivery group 'roadmap'.</p>
                        
                        Here we bring together information about projects, service improvement efforts, communication activity, and events such as taught courses.</p>
                        
                        We are using google spreadsheets and forms to make it easy for everyone in the delivery group to keep the information that underpins this site accurate and up to date. We are using a visualization service (","<a href='http://www.shinyapps.io'>shinyapps.io</a>",") to read the data i.e. using Gantt chart views of projects, heat maps of teaching activity, and tree maps to show where our effort is focused.</p>
                        
                        For more information about RDM at Oxford please see the research data website: ","<a href=http://researchdata.ox.ac.uk/home/introduction-to-rdm/>researchdata.ox.ac.uk/home/introduction-to-rdm/</a></p>",
                        "<strong>Update: 9/9/2015</strong> - this is our first draft, we really want to improve the design, and to show how we can use RDM to help coordinate our efforts, and report progress to the RDM working group.",sep =
                          ""
                      )
                    ))),
           tabPanel("Research Data Lifecycle",
                    HTML(
                      paste(
                        "<p></p>",
                        "<p>The succesful curation and preservation of research data is a multi-faceted concern for researchers and funders alike.
                        Visualising the lifecycle of research data can be difficult, correctly identifying roles and responsibilities requires a
                        full model of how research data is generated, stored, accessed and updated.</p>",
                        "To help with RDM at Oxford, we have embedded the DCC Curation Lifecycle Model diagram below</p>",
                        "<p><img src=http://www.dcc.ac.uk/sites/default/files/lifecycle_web.png text-align='center'></p>",
                        "Attribution: <a href=http://www.dcc.ac.uk/resources/curation-lifecycle-model>http://www.dcc.ac.uk/resources/curation-lifecycle-model</a>"
                        ,sep = ""
                      )
                    )),
           tabPanel("Adding data to these plans",
                    HTML(
                      paste(
                        "<p></p>",
                        "<p>Projects can be created, updated and deleted using the following Google Form: ","<a href=http://goo.gl/forms/Q4LKIJ7DZP>http://goo.gl/forms/Q4LKIJ7DZP</a></p>"
                        ,sep = ""
                      )
                    )),
           tabPanel("About this tool",
                    fluidPage(HTML(
                      paste(
                        "<p>This app has been developed by the Academic Support Services team using the web service Shiny, allowing interactive apps utilising
                        the R development stack to be deployed and shared easily.</p>"
                      )
                      ),
                      fluidRow(column(
                        12,
                        p(
                          HTML('<ul>'),
                          HTML('<li>'),"Contact: Martin John Hadley (martin.hadley@it.ox.ac.uk)",HTML('</li>'),
                          HTML('<li>'),a('Developed using R', href =
                                           "http://www.r-project.org/", target = "_blank"),HTML('</li>'),
                          HTML('<li>'),a('Available on Github', href =
                                           "https://github.com/martinjhnhadley/RDMRoadmap", target = "_blank"),HTML('</li>'),
                          HTML('<li>'),a('Hosted by Shiny', href = "http://www.rstudio.com/shiny/", target =
                                           "_blank"),HTML('</li>'),
                          HTML('<li>'),"Inspiration for App Structure:",a(
                            'Alaska / Western Canada Historical and Projected Climate by Matthew Leonawicz', href =
                              "http://shiny.snap.uaf.edu/akcan_climate/", target = "_blank"
                          ),HTML('</li>')
                          #                              HTML('<li>'),"Primary supporting R packages",HTML('</li>')
                          #                              HTML('<ul>'),
                          #                              HTML('<li>'),a('ggplot2', href="http://ggplot2.org", target="_blank"),HTML('</li>'),
                          #                              HTML('<li>'),a('Hmisc', href="http://cran.r-project.org/web/packages/Hmisc/index.html", target="_blank"),HTML('</li>'),
                          #                              HTML('<ul>'),
                          #                              HTML('</ul>')
                        )
                      ))))
           ))