## Timeline test file

library(googlesheets)
library(ggplot2)

# Load data
projects.sheet <- gs_key("1I6Z94prfJrmSSmD_mwqazkp8Qx8AUmmsp9hAW6bF8yQ",
                         visibility = "public")
projects.df <- gs_read(projects.sheet)
projects.df$Project.Start.Date <- as.POSIXct(projects.df$Project.Start.Date)
projects.df$Project.End.Date <- as.POSIXct(projects.df$Project.End.Date)
projects.df$Project.Short.Name <- as.character(projects.df$Project.Short.Name)
projects.df$IT.Board <- as.factor(projects.df$IT.Board)
projects.df$Project.Sponsor<- as.factor(projects.df$Project.Sponsor)
projects.df$Project.Manager <- as.factor(projects.df$Project.Manager)
projects.df$Senior.Supplier <- as.factor(projects.df$Senior.Supplier)
projects.df$Senior.User <- as.factor(projects.df$Senior.User)

## Order the factors

projects.df$Project.Short.Name <- factor(projects.df$Project.Short.Name, 
                                         levels=projects.df[order(projects.df$Budget.Requested),]$Project.Short.Name)
## ggplot2


base <- ggplot(projects.df,
               aes(Project.Short.Name, Project.Start.Date, order=Budget.Requested,ymin=Project.Start.Date,color=as.factor(IT.Board),ymax=Project.End.Date,xticks)) + xlab("Project.Short.Name")
base + geom_linerange(size=4,alpha=.7) + coord_flip() + scale_colour_brewer(palette="Spectral")
