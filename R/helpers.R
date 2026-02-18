fixTime.internal <-  function(time){

  is_numeric_like <- function(x) {
    !is.na(suppressWarnings(as.numeric(x)))
  }

 # Check if we have the correct format, first check if colons are present
  if(length(grep(":", time))>0){

    # We assume dd-hh:mm format
    if(length(grep("-", time))>0){
      days <- as.numeric(strsplit(time, "-")[[1]][1])
      rest <- strsplit(time, "-")[[1]][2]
      hours <- as.numeric(strsplit(rest, ":")[[1]][1])
      minutes <- as.numeric(strsplit(rest, ":")[[1]][2])
      if(nchar(minutes)==1) minutes <- paste0("0", minutes)
      out <- paste0(days*8 + hours, ":", minutes)
    } else {
    # Now we catch the case dd:hh:mm
      if(grep(":", time)==2){
        days <- as.numeric(strsplit(time, ":")[[1]][1])
        hours <- as.numeric(strsplit(time, ":")[[1]][2])
        minutes <- as.numeric(strsplit(time, ":")[[1]][3])
        if(nchar(minutes)==1) minutes <- paste0("0", minutes)
        out <- paste0(days*8 + hours, ":", minutes)
      } else {
      # Now we assume hh:dd format
        if(grep(":", time)==1){
          days <- 0
          hours <- as.numeric(strsplit(time, ":")[[1]][1])
          minutes <- as.numeric(strsplit(time, ":")[[1]][2])
          if(nchar(minutes)==1) minutes <- paste0("0", minutes)
          out <- paste0(days*8 + hours, ":", minutes)
        } else {
          stop("Malformatted time entry: ", time)
        }
      }
    }
  } else {
    # expect single value followed by h
    if(length(grep("h", time))==1){
      out <- paste0(gsub("h", "", time), ":00")
    } else if(length(grep("m", time))==1){
          minutes <- as.numeric(gsub("m", "", time))
          hours <- minutes%/%60
          minutes <- minutes%%60
          if(nchar(minutes)==1) minutes <- paste0("0", minutes)
          out <- paste0(hours, ":", minutes)
    } else if(length(grep("d", time))==1){
            out <- paste0(as.numeric(gsub("d", "", time))*8, ":00")
    } else if(is_numeric_like(time)){
    # We assume a single numeric value representing hours
    # Format a numeric values of hours into hh:mm format
      time <- as.numeric(time)
      minutes <- time * 60
      hours <- minutes%/%60
      minutes <- minutes%%60
      if(nchar(minutes)==1) minutes <- paste0("0", minutes)
      out <- paste0(hours, ":", minutes)
    } else {
    stop("Malformatted time entry: ", time)
    }
  }

  out
}


#' Fix time values
#'
#' This functions fixes inconsistent time entries
#' @param time Sting of time entry
#' @return A hh:mm format string
#' @export

fixTime <-  function(time){
  out <- rep("", length(time))

  if(length(time)>1){
    for(i in 1:length(out)){
      out[i] <- fixTime.internal(time[i])
    }
  } else {
    out[i] <- time
  }

  out
}


timeInMinutes <- function(x){
  hours <- as.numeric(strsplit(x, ":")[[1]][1])
  minutes <- as.numeric(strsplit(x, ":")[[1]][2])

  out <- hours * 60 + minutes

  out
}
