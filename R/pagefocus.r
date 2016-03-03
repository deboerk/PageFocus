rtrim <- function (x) sub("\\s+$", "", x)  # returns string w/o trailing whitespace

winsorize <- function(x, q=.05, lower.winsorize=TRUE, upper.winsorize=TRUE) {
 extrema <- quantile(x, c(q, 1 - q))
 if(lower.winsorize) x[x < extrema[1]] <- extrema[1]
 if(upper.winsorize) x[x > extrema[2]] <- extrema[2]
 x
}

pagefocus <- function(data, page, statistics=NULL, lower.cut=-Inf, upper.cut=Inf, lower.winsorize=FALSE, upper.winsorize=FALSE, winsorize.percentil=.05) {  # lower.cut, upper.cut, upper.winsorize in milliseconds
  if(length(data) == 0) stop("No data found")
  if(length(page) == 0) stop("No page found")
  if(is.null(statistics)) statistics <- c("loss", "count", "error", "exclusions", "duration", "duration.min", "duration.mean", "duration.max", "duration.log.sum", "duration.log.mean", "duration.log.max", "duration.log.min")

  if(length(page) == 1) {  # analyze one page
    if(is.factor(data)) data <- as.character(data)
    data <- rtrim(data)

    absence.count <-  absence.durations <- NULL

    result <- list()
    for(d in 1:length(data)) {  # iterate through all participants
      result[[d]] <- {
        if(!is.null(data[d]) && !is.na(data[d]) && grepl("^(D[[:digit:]]*;R[[:digit:]]*;)+$", data[d])) {
          clicks.raw <- strsplit(data[d], ";")[[1]]

          timestamp <- c()
          for(x in clicks.raw) {
            timestamp <- c(timestamp, as.numeric(substr(x,2,nchar(x))))
          }
          timestamp.diffs <- diff(timestamp)
          absence.duration <- timestamp.diffs[seq(1, length(timestamp.diffs), by=2)]

          ## cut-offs
          include <- absence.duration > lower.cut & absence.duration < upper.cut
          absence.duration <- absence.duration[include]

          absence.durations <- c(absence.durations, absence.duration)  # collect durations for winsorizing (use the same percentile value for all participants on a page)

          list(
            absence.duration=absence.duration,
            absence.count=length(absence.duration),
            exclusions=sum(!include),
            error=0
          )
        } else if(is.na(data[d]) || grepl("^-(66|77|99)*$", data[d])) {  ## no pagefocus lost
          list(
            absence.count=0,
            exclusions=0,
            error=0
          )
        } else {  ## error occured
          list(
            absence.count=NA,
            exclusions=NA,
            error=1
          )
        }
      }
    }

    ## winsorize
    if(!is.null(absence.durations) && (upper.winsorize || lower.winsorize)) {
      lim <- quantile(absence.durations, c(winsorize.percentil, 1 - winsorize.percentil))  # use the same percentile value for all participants on a page
      absence.durations <- NULL

      for(d in 1:length(result)) {  # iterate through all participants
        if(!is.null(absence.duration)) {
          x <- result[[d]]$absence.duration
          if(lower.winsorize) x[x < lim[1]] <- lim[1]
          if(upper.winsorize) x[x > lim[2]] <- lim[2]
          result[[d]]$absence.duration <- x
          absence.durations <- c(absence.durations, x)
        }
      }
    }

    result.table <- NULL

    for(d in 1:length(result)) {  # iterate through all participants
      x <- result[[d]]$absence.duration

      if(is.na(result[[d]]$absence.count) || result[[d]]$absence.count == 0) {  # if there are no pagefocus losses or an error occured
        absence.duration <- absence.duration.min <- absence.duration.mean <- absence.duration.max <- absence.duration.log.sum <- absence.duration.log.min <- absence.duration.log.mean <- absence.duration.log.max <- NA
      } else {  # there have been pagefocus losses
        x <- x/1000  # convert milliseconds to seconds
        absence.duration <- sum(x)
        absence.duration.min <- min(x)
        absence.duration.mean <- mean(x)
        absence.duration.max <- max(x)
        absence.duration.log.sum <- sum(log(x))
        absence.duration.log.min <- min(log(x))
        absence.duration.log.mean <- mean(log(x))
        absence.duration.log.max <- max(log(x))
      }
      result.table <- rbind(result.table, c(loss=result[[d]]$absence.count > 0, count=result[[d]]$absence.count, error=result[[d]]$error, exclusions=result[[d]]$exclusions, duration=absence.duration, duration.min=absence.duration.min, duration.mean=absence.duration.mean, duration.max=absence.duration.max, duration.log.sum=absence.duration.log.sum, duration.log.min=absence.duration.log.min, duration.log.mean=absence.duration.log.mean, duration.log.max=absence.duration.log.max)[statistics])
    }

    colnames(result.table) <- paste(page, statistics, sep="_")
  } else {
    stop("This function can only analyze one page at a time. Please use pagefocus.analysis().")
  }

  result.table
}

pagefocus.analysis <- function(data, pages, statistics=NULL, lower.cut=-Inf, upper.cut=Inf, lower.winsorize=FALSE, upper.winsorize=FALSE, winsorize.percentil=.05) {
  result <- NULL

  for(v in 1:length(pages)) {
    if(length(data[[pages[v]]]) == 0) stop(paste0("Could not find data for page '", pages[v], "'"))
    result <- cbind(result, pagefocus(data[[pages[v]]], pages[v], statistics, lower.cut=lower.cut, upper.cut=upper.cut, lower.winsorize=lower.winsorize, upper.winsorize=upper.winsorize, winsorize.percentil=winsorize.percentil))
  }

  result
}
