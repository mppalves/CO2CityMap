#' @name readFiles
#' @description
#' @param
#' @details
#' @export
#' @import stringr
#' @import foreign
#' @import dplyr
#' @import tidyr

readFiles <- function(dir, type = NULL) {

  files <- list.files(dir)
  files_shape <- grep("network.dbf", files, value = T)

  if (!(type %in% c("tables", "network"))) {
    stop("You must select either type 'tables' or 'network")
  }

  if(type == "tables") {
    files_table <- grep("[0-9]{2}.dbf", files, value = T)
    time_series <- separate(as.data.frame(files_table), col = 1, into = c("start", "to", "rest"), sep = " ",extra = "drop") %>%
      separate(col = "rest", into = c("end", "seq"),sep = "_", extra = "drop") %>%
      select(c(start, end, seq)) %>%
      mutate(start = as.Date(start), end = as.Date(end), seq = as.numeric(seq))
    files_table <- files_table[order(time_series[, 1], time_series[, 2], time_series[, 3])]
    y <- NULL
    for (i in files_table) {
      print(paste("reading file", i))
      time <- strsplit(i, " ") %>% unlist()
      start <- time[1]
      end <- time[3] %>% strsplit(., "_") %>% unlist() %>% .[1]
      order <- as.numeric(time[3] %>% strsplit(., "-|\\.") %>% unlist() %>% .[4] %>% gsub("_", "", .))
      x <- read.dbf(file.path(dir, i))[, c(1, 5, 9)]
      colnames(x) <- c("Id", "AvgSpeed", "Hits")
      x <- data.frame(x, "start" = start, "end" = end, "order" = order)
      y <- rbind(y, x)
    }
  }
  if (type == "network") {
    print(paste("reading file", files_shape))
    y <- read.dbf(file.path(dir, files_shape))
  }
  return(y)
}
