tabPanel(
  "ORDS Data Summary",
  wellPanel(HTML(
    paste(
      "<h1>ORDS Summary</h1>",
      "<p>ORDS is the University's repository for \"live\" research data, datasets that are being actively developed and collaborated on before
      being submitted to the long-term repository; ORA Data.</p>",
      sep = ""
    )
  )),
  plotOutput("ords_FullandTrialPlot")
  )