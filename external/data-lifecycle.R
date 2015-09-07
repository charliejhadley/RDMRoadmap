


tabPanel("Lifecycle",
         fluidPage(tags$head(HTML(
           "<link type='text/css' rel='stylesheet' href='../external/all_v3.2.css' />
           <script type='text/javascript' src='http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js'></script>
           <script type='text/javascript' src='http://www.data-archive.ac.uk/scripts/webui_v3.2.js'></script>
           <script type='text/javascript' src='http://www.data-archive.ac.uk/scripts/websiteui_v3.2.js'></script>
           <script type='text/javascript' src='http://www.data-archive.ac.uk/scripts/common_v3.2.js'></script>"
         )),
           htmlOutput("lifeCycle")))