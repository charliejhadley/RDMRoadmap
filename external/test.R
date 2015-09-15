proj.df <- projects.df

base <-
  ggplot(proj.df, aes(
    x = Project.Start.Date, y = projectID, color = as.factor(IT.Board)
  ))
gantt <- {
  base +
    scale_y_discrete(breaks = proj.df$projectID, labels = proj.df$Project.Short.Name) +
    geom_segment(aes(xend = Project.End.Date, y = projectID, yend = projectID), size = 5)
}

gantt <-
  gantt + ylab(NULL) + xlab(NULL) + labs(color = "IT Board") + guides(color = guide_legend(title.hjust = 0.5))
gantt <-
  gantt + scale_x_datetime(breaks = "3 month", labels = date_format("%Y-%b"), minor_breaks = "3 month") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

milestone.df <- data.frame(milestone = c(as.POSIXct("2010-10-01"),as.POSIXct("2012-07-01"),
                                         as.POSIXct("2014-08-01"),as.POSIXct("2014-12-01"),as.POSIXct(today())),
                           descrip =c("RDM Website Launched","RDM Policy Ratified","ORA-Data Launched","ORDS Launched","Today"))

gantt + geom_vline(data = milestone.df, aes(xintercept = as.integer(milestone)), linetype = "dashed") +
  geom_text(data = milestone.df, aes(x = milestone, 
                                     y = nrow(proj.df) - nchar(as.vector(milestone.df$descrip))/5, label=descrip),
            angle = 90, colour = "black", text = element_text(size = 14)) 

## Old (new is below!)

gantt + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(
    xintercept = as.integer(as.POSIXct(today())), linetype = "dashed", color = "black"
  ) +
  #   geom_text(
  #     aes(
  #       x = as.POSIXct(today()), label = "today", y = nrow(proj.df) - 2
  #     ), angle = 90, colour = "blue", text = element_text(size = 11)
  #   ) +
  geom_vline(
    xintercept = as.integer(as.POSIXct("2014-08-01")), linetype = "dashed", color =
      "blue"
  ) +
  geom_text(
    aes(
      x = c(as.POSIXct("2014-08-01"),as.POSIXct(today())), label = c("today","ORDS Launched"), 
      y = c(nrow(proj.df) - 2,nrow(proj.df) - 4)
    ), angle = 90, colour = "blue", text = element_text(size = 11)
  ) +
  geom_vline(
    xintercept = as.integer(as.POSIXct("2014-12-01")), linetype = "dashed", color = "red"
  ) +
  geom_vline(xintercept = as.integer(as.POSIXct("2012-07-01")), linetype = "dashed") +
  geom_vline(xintercept = as.integer(as.POSIXct("2010-10-01")), linetype = "dashed")


milestone.df <- data.frame(milestone = c(as.POSIXct("2010-10-01"),as.POSIXct("2012-07-01"),
                                    as.POSIXct("2014-08-01"),as.POSIXct("2014-12-01"),as.POSIXct(today())),
                           descrip =c("RDM Website Launched","RDM Policy Ratified","ORA-Data Launched","ORDS Launched","Today"))

gantt + geom_vline(data = milestone.df, aes(xintercept = as.integer(milestone)), linetype = "dashed") +
  geom_text(data = milestone.df, aes(x = milestone, 
                                     y = nrow(proj.df) - nchar(as.vector(milestone.df$descrip))/5, label=descrip),
            angle = 90, colour = "black", text = element_text(size = 14)) 

### A neater solution from http://stackoverflow.com/a/7662520/1659890

GeomText2 <- proto(ggplot2::GeomText, {
  objname <- "text2"
  draw <- function(., data, scales, coordinates, ..., parse = FALSE,
                   expand = 1.2, bgcol = "grey50", bgfill = NA, bgalpha = 1) {
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }
    with(coordinates$transform(data, scales), {
      sizes <- llply(1:nrow(data),
                     function(i) with(data[i, ], {
                       grobs <- textGrob(lab[i], default.units="native", rot=angle, gp=gpar(fontsize=size * .pt))
                       list(w = grobWidth(grobs), h = grobHeight(grobs))
                     }))
      
      gList(rectGrob(x, y,
                     width = do.call(unit.c, lapply(sizes, "[[", "w")) * expand,
                     height = do.call(unit.c, lapply(sizes, "[[", "h")) * expand,
                     gp = gpar(col = alpha(bgcol, bgalpha), fill = alpha(bgfill, bgalpha))),
            .super$draw(., data, scales, coordinates, ..., parse))
    })
  }
})


geom_text2 <- GeomText2$build_accessor()

gantt + geom_vline(data = milestone.df, aes(xintercept = as.integer(milestone)), linetype = "dashed") +
  geom_text2(data = milestone.df, aes(x = milestone, 
                                     y = nrow(proj.df) - nchar(as.vector(milestone.df$descrip))/5, label=descrip),
            angle = 90, colour = "black", text = element_text(size = 14),size = 5, expand = 1.5, bgcol = "green", bgfill = "skyblue", bgalpha = 0.8)

