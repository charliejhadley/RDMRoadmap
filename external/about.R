# About Section for RDM Roadmap Dashboard

tabPanel("About this tool",
         tabsetPanel("foo",
           tabPanel("RDM at Oxford",
         fluidPage(HTML('
              <p style="text-align:justify">This R Shiny web app is currently under development.
              The data in the app concern Research Data Management projects undertaken by University of Oxford.</p>
              
              <p style="text-align:justify">The app currently has many features.
              Lorem ipsum can easily be added to this section, as indeed it should be.</p>
              
              <p style="text-align:justify"><strong>Suggestions:</strong>This app is very much in an alpha build.</p>'),
         
         fluidRow(
           column(4,
                  strong('References'),
                  p(HTML('<ul>'),
                    HTML('<li>'),a('Coded in R', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
                    HTML('<li>'),a('Built with the Shiny package', href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
                    HTML('<li>'),"Primary supporting R packages",HTML('</li>'),
                    HTML('<ul>'),
                    HTML('<li>'),a('ggplot2', href="http://ggplot2.org", target="_blank"),HTML('</li>'),
                    HTML('<li>'),a('Hmisc', href="http://cran.r-project.org/web/packages/Hmisc/index.html", target="_blank"),HTML('</li>'),
                    HTML('<ul>'),
                    HTML('</ul>'))
           )
         )
         )),
         tabPanel("Adding data to these plans",
                  "I've not been implemented yet")
         )
)