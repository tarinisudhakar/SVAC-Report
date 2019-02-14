setwd("~/Documents/CCS/SVAC/Latex/SVAC-Report")
#install.packages("reshape2")
#install.packages("grid")
#install.packages("RGraphics")
#install.packages("tikzDevice")
library(tikzDevice)
options(tz="IST")
library(reshape2)
library(RGraphics)
library(ggplot2)
library(grid)
library(scales)
data <- read.csv("mandatevsreality.csv")
data <- data[1:7,1:3]
data
data$order <- 100*data$order/sum(data$order)
data$mm <- 100*data$meeting_minutes/sum(data$meeting_minutes)
data <- data[,c(1,2,4)]
data.new <- melt(data, id.vars = c("desc"))
colnames(data.new)[2] <- "source"
colnames(data.new)[3] <- "share_number"
data.new[,3] <- round(data.new[,3], 1)
levels(data.new$desc) <- gsub(" ", "\n", levels(data.new$desc))
data.new


grob <- grobTree(textGrob("Representation from street vendors\nand vendor associations",
                          x=0.45,  y=0.85, hjust=0, vjust = 0.3,
                          gp=gpar(col="black", fontsize=10, fontface="italic")))
myCurve<-curveGrob(0.75, 0.93, 0.95, 0.75, default.units = "npc",
                   curvature = -0.5, angle = 90, ncp = 20, shape = 1,
                   square = FALSE, squareShape = 1,
                   inflect = FALSE, arrow = arrow(length = unit(0.2, "cm")), 
                   open = TRUE,
                   debug = FALSE,
                   name = NULL, gp = gpar(), vp = NULL)
x <- NULL
tikz(file = "m_vs_r.tex", width = 6.5, height = 7)
#?tikz
x <- ggplot(data.new[order(data.new$share_number, decreasing = F), ], 
            aes(x = factor(source), y = share_number, 
                fill = desc, label = share_number)) +
        labs(title = "Percentage of Members in TVCs") +
        scale_x_discrete(position = "top", labels = c("Mandate of\nMCG Order", 
                                                      "Reality from\nMeeting Minutes")) + 
        geom_bar(stat = "identity", width = 0.35)  + 
        geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        theme(axis.text = element_text(size = 10)) + 
        coord_flip() + scale_y_reverse() + 
        theme(aspect.ratio = 1/2,
              legend.position=c(0.52,0),
              legend.title = element_blank(),
              legend.key = element_rect(size = 5),
              legend.key.size = unit(1.5, 'pt'),
              plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 10, margin = margin(l = 0 , r =0)),
              axis.title = element_blank(),
              legend.text = element_text(margin = margin(l = 1 , r =10), size = 10),
              legend.margin = margin(r=10, unit = "pt")) + 
        guides(fill = guide_legend(nrow = 1, keywidth = 1)) + 
        annotation_custom(grob=myCurve) + 
        annotation_custom(grob) + 
        scale_fill_manual(values=c("#3f75a6", "#498bc4", "#5c9bd4", "#99bce5", "#b8ceeb", "#d7e5f8","#f0f6ff"))
print(x)
dev.off()
x

