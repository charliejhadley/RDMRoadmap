# App body for RDM Roadmap Dashboard

# Primary Outputs


output$plotui <- renderUI({
  plotOutput("plot", height = 300,
             click = "plot_click")
})

output$plot <- renderPlot({
  dat <- projects.df
  base <- ggplot(
    dat,
    aes(
      Project.Short.Name, Project.Start.Date, ymin = Project.Start.Date,color =
        as.factor(IT.Board),ymax = Project.End.Date,xticks
    )
  ) + xlab("Project.Short.Name")
  base + geom_linerange(size = 4,alpha = .7) +  coord_flip()  + scale_colour_brewer(palette = "Spectral")
})

# Print the rows of the data frame which match the x value
output$selecProject <- renderDataTable({
  if (is.null(input$plot_click$y))
    return()
  projects.df[round(input$plot_click$y),]
})

## clicked points:
output$projSummary <- renderUI({
  if (is.null(input$plot_click$y))
    return()
  projData <- projects.df[round(input$plot_click$y),]
  mainPanel(titlePanel(projData$Project.Short.Name),
            pre(projData$Project.Summary))
})
