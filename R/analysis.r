setwd("~/pagefocus/R/")  # set working directory

source("pagefocus.r")  # load pagefocus R functions

pagefocus.data <- read.csv("data.csv", na.strings="")  # load data from csv file
pages <- c("demo_page", "another_page")  # specify variables holding the pagefocus raw data


# run pagefocus analysis and add results to pagefocus.data
pagefocus.data <- cbind(pagefocus.data, pagefocus.analysis(pagefocus.data, pages, statistics=c("count", "duration")))


# number of page defocusing events per page 
pagefocus.count.means <- colMeans(pagefocus.data[,paste0(pages, "_count")])  # calculate means

bar.midpoints <- barplot(pagefocus.count.means, main="Number of page defocusing events per page", xlab="Page", ylim=c(0,2), names.arg=pages, col="cornflowerblue", las=1)
text(x=bar.midpoints, y=pagefocus.count.means + 0.1, labels=as.character(round(pagefocus.count.means,2)), xpd=TRUE)  # add values


# mean absence duration per page (seconds)
pagefocus.duration.means <- colMeans(pagefocus.data[,paste0(pages, "_duration")], na.rm=TRUE)  # calculate means

bar.midpoints <- barplot(pagefocus.duration.means, main="Mean absence duration per page (seconds)", xlab="Page", ylim=c(0,60), names.arg=pages, col="cornflowerblue", las=1)
text(x=bar.midpoints, y=pagefocus.duration.means + 3, labels=paste(round(pagefocus.duration.means,2), "s"), xpd=TRUE)  # add values
