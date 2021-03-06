#
#Ref.: https://www.r-bloggers.com/building-barplots-with-error-bars/
#Ref.2: http://stackoverflow.com/questions/29768219/grouped-barplot-in-r-with-error-bars
myData <- aggregate(mtcars$mpg,
    by = list(cyl = mtcars$cyl, gears = mtcars$gear),
    FUN = function(x) c(mean = mean(x), sd = sd(x),
                        n = length(x)))

myData <- do.call(data.frame, myData)

myData$se <- myData$x.sd / sqrt(myData$x.n)

colnames(myData) <- c("cyl", "gears", "mean", "sd", "n", "se")

myData$names <- c(paste(myData$cyl, "cyl /",
                        myData$gears, " gear"))


#Barplots using ggplot2
#install ggplot2
#Ref. (if fails): http://stackoverflow.com/questions/30636670/ggplot2-fails-to-install-on-r-3-0-2 
#install.packages("ggplot2")

library(ggplot2)

#Simple barplot
dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)

p <- ggplot(data = myData, aes(x = names, y = mean, fill = names))

p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank())

#Grouped barplots



limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)

p <- ggplot(data = myData, aes(x = factor(cyl), y = mean,
               fill = factor(gears)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "No. Cylinders", y = "Miles Per Gallon") +
  ggtitle("Mileage by No. Cylindersnand No. Gears") +
  scale_fill_discrete(name = "No. Gears")

#run with: 
#Rscript <filename.r>