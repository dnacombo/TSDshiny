# This is the Blursday database server source code.
# The live application can be accessed at 
# https://dnacombo.shinyapps.io/Blursday/
# This code is publicly available at
# https://github.com/dnacombo/TSDshiny

#     Copyright (C) 2021  Maximilien Chaumon
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

humanReadable <- function (x, units = "auto", standard = c("IEC", "SI", "Unix"), 
                           digits = 1, width = NULL, sep = " ", justify = c("right", 
                                                                            "left")) 
{
  suffix.SI <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", 
                 "ZB", "YB")
  suffix.IEC <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", 
                  "EiB", "ZiB", "YiB")
  suffix.Unix <- c("B", "K", "M", "G", "T", "P", "E", "Z", 
                   "Y")
  standard <- match.arg(standard)
  if (length(justify) == 1) 
    justify <- c(justify, justify)
  .applyHuman <- function(x, base, suffix, digits, width, 
                          sep) {
    n <- length(suffix)
    i <- pmax(pmin(floor(log(x, base)), n - 1), 0)
    if (!is.finite(i)) 
      i <- 0
    x <- x/base^i
    if (is.null(width)) 
      x <- format(round(x = x, digits = digits), nsmall = digits)
    else {
      lenX <- nchar(x)
      if (lenX > width) {
        digits <- pmax(width - nchar(round(x)) - 1, 
                       0)
      }
      if (i == 0) 
        digits <- 0
      x <- round(x, digits = digits)
    }
    c(x, suffix[i + 1])
  }
  if (any(x < 0)) 
    stop("'x' must be positive")
  if (standard == "SI") {
    suffix <- suffix.SI
    base <- 10^3
  }
  else if (standard == "IEC") {
    suffix <- suffix.IEC
    base <- 2^10
  }
  else {
    suffix <- suffix.Unix
    base <- 2^10
  }
  if (!missing(units) && units == "bytes") {
    retval <- rbind(x, "bytes")
  }
  else if (!missing(units) && units != "auto") {
    units <- suffix[match(toupper(units), toupper(suffix))]
    power <- match(units, suffix) - 1
    X <- x/(base^power)
    X <- format.default(x = X, digits = digits, nsmall = digits)
    retval <- rbind(X, rep(units, length(X)))
  }
  else retval <- sapply(X = x, FUN = ".applyHuman", base = base, 
                        suffix = suffix, digits = digits, width = width, sep = sep)
  if (all(justify == "none")) 
    paste(trimws(retval[1, ]), trimws(retval[2, ]), sep = sep)
  else paste(format(trimws(retval[1, ]), justify = justify[1]), 
             format(trimws(retval[2, ]), justify = justify[2]), sep = sep)
}

gimmeRdata <- function(DataDir = getwd(), UniqueName = '[^_]+', 
                       ExperimentID = '[0-9]{5}', Country = '[^_]+', 
                       Session = 'S[^_]+', Run = '[^_]*', ...,
                       progress = F,
                       fast = T,
                       verbose = T)
  {
  
  
  if (length(UniqueName) > 1){
    UniqueName <- paste0('(',paste(UniqueName,collapse = '|'),')')
  }
  if (length(Country) > 1) {
    Country <- paste0('(',paste(Country,collapse = '|'),')')
  }
  if (length(Session) > 1) {
    Session <- paste0('(',paste(Session,collapse = '|'),')')
  }
  
  
  fs <- list.files(path = DataDir, pattern = paste0('^TSD_',Country,'_',Session,'_',UniqueName,'.RData'), full.names = T, recursive = F)
  if (verbose) {
    cat(paste0('Loading ', humanReadable(sum(file.info(fs)$size)), ' from ', length(fs), ' files.\n'))
  }
  if (length(fs) == 0) return(tibble())
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(fs), initial = 0, char = "=",
                         width = 40, style = 3)
    i <- 0
  }
  if (fast) {
    d <- sapply(fs,
                function(x){
                  if (progress) {
                    i <<- i + 1
                    setTxtProgressBar(pb,i)
                  }
                  mget(load(x))
                })
    D <- bind_rows(d)%>%
      filter(grepl(!!ExperimentID,Experiment_ID),
             grepl(!!Run,Run))
    
  } else {
    D <- tibble()
    for (f in fs) {
      load(f)
      D <- d %>%
        filter(grepl(!!ExperimentID,Experiment_ID),
               grepl(!!Run,Run)) %>%
        bind_rows(D)
      if (progress) {
        i <- i + 1
        setTxtProgressBar(pb,i)
      }
    }
  }
  return(D)
}

source('add_covariates.R')
