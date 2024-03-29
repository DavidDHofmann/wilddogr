# ################################################################################
# #### TESTING
# ################################################################################
# library(rdrop2)
# library(pbapply)
# library(lubridate)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(readxl)
# library(tidyverse)
# library(chron)
# library(parsedate)
# library(openxlsx)

################################################################################
#### Load Dependencies
################################################################################
#' @import ggplot2
#' @importFrom pbapply pbsapply
#' @importFrom rdrop2 drop_exists drop_dir drop_download
#' @importFrom lubridate year hours minutes ymd_hms dmy_hm with_tz round_date
#' @importFrom dplyr group_by summarize arrange mutate lag select
#' @importFrom readxl read_excel
#' @importFrom chron times
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom parsedate parse_date
#' @importFrom openxlsx convertToDateTime
NULL

################################################################################
#### Level 1 Functions
################################################################################
#' Display all files containing GPS data on Dropbox
#'
#' This function is used to retrieve information on all files containing GPS
#' data in the KML-Dispersers folder on Dropbox. The returned dataframe can be
#' used to identify files that should be downloaded using \code{dog_download()}.
#' The first time the function is run, you'll need to allow access to your
#' dropbox.
#' @export
#' @param rvc logical indicating wheter "RVC GPS data" should be searched too.
#' @return data.frame Data frame containing information on all files containing
#' GPS data on Dropbox.
#' @examples
#' # Check all available files on Dropbbox
#' dog_files(rvc = T)
# dog_files(rvc = T)
dog_files <- function(rvc = F) {

  # Check if the KML-Files folder is in the top directory
  if (drop_exists("KML-Files")) {
      path <- "KML-Files"
    } else {
      path <- drop_dir(recursive = T)
      path <- path$path_display[path$name == "KML-Files"]
  }

  # Identify all files in the kml folder
  files <- drop_dir(path, recursive = T)

  # If desired, extract rvc data
  if (rvc) {

    # Find gps data
    files_1 <- files[grepl(files$path_display, pattern = "GPS_data_RVC"), ]
    files_1 <- files_1[grepl(files_1$name, pattern = ".txt"), ]

    # Extract dog names and add NA columns for the collar and timestamp
    files_1$DogName   <- str_extract(files_1$name, pattern = "(?<= - )\\w*()")
    files_1$Timestamp <- as.POSIXct(NA)
    files_1$Collar    <- as.numeric(NA)

    # Rename columns
    files_1$fullname <- files_1$path_display
    files_1$filepath <- dirname(files_1$path_display)
    files_1$filename <- files_1$name
    files_1$filetype <- ".txt"

    # Remove unnecessary columns
    files_1 <- dplyr::select(files_1
      , c(DogName, Collar, Timestamp, fullname, filepath, filename, filetype)
    )

  }

  # Subset to csv files that contain GPS data
  files <- files[grepl(files$name, pattern = ".csv$"), ]
  files <- files[grepl(files$name, pattern = "GPS"), ]

  # Ignore all "performance" files and files from the "dispersers" folder (they
  # are copies of files that are stored elsewhere)
  files <- files[!(grepl(files$path_lower, pattern = "performance")), ]
  files <- files[!(grepl(files$path_lower, pattern = "dispersers")), ]

  # Prepare a dataframe with relevant information on each file
  info <- data.frame(
      fullname         = files$path_display
    , filepath         = dirname(files$path_display)
    , filename         = files$name
    , stringsAsFactors = F
  )

  # Assign the name of the dog, collar number, and date to each file. In some
  # cases the information is given in the filename, in other cases in the
  # folders. Let's split the files accordingly.
  files_2 <- info[grepl(info$filepath, pattern = "inactive collars"), ]
  # files_3 <- info[!(grepl(info$filepath, pattern = "inactive collars")), ]

  # Identify dog name, collar, and date for second set of files
  files_2 <- files_2 %>%
    mutate(filename = gsub(x = filename, pattern = "__", replacement = "_")) %>%
    mutate(filepath = gsub(x = filepath, pattern = "__", replacement = "_")) %>%
    separate(
        col   = filepath
      , sep   = "_"
      , into  = c(NA, "DogName", "Collar")
      , extra = "drop"
    ) %>%
    dplyr::select(-Collar) %>%
    separate(
        col   = filename
      , sep   = "_"
      , into  = c(NA, "Collar", "Timestamp")
      , extra = "drop"
    ) %>%
    dplyr::select(c(Collar, DogName, Timestamp)) %>%
    cbind(files_2, .)

  # # Identify dog name, collar, and date for third set of files
  # files_3 <- files_3 %>%
  #   mutate(filename = gsub(x = filename, pattern = "__", replacement = "_")) %>%
  #   mutate(filepath = gsub(x = filepath, pattern = "__", replacement = "_")) %>%
  #   separate(
  #       col = fullname
  #     , sep = "_"
  #     , c(NA, "DogName", NA, "Collar", "Timestamp")
  #   ) %>%
  #   dplyr::select(c(Collar, DogName, Timestamp)) %>%
  #   cbind(files_3, .)

  # Put all back together and do some cleaning
  # files_2 <- rbind(files_2, files_3) %>%
  files_2 <- files_2 %>%
    separate(
        col = Timestamp
      , sep = c(4, 6, 8, 10, 12, 14)
      , c("Year", "Month", "Day", "Hour", "Minutes", "Seconds")
    ) %>%
    mutate(Date = paste(Year, Month, Day, sep = "-")) %>%
    mutate(Time = paste(Hour, Minutes, Seconds, sep = ":")) %>%
    mutate(Timestamp = paste(Date, Time)) %>%
    mutate(Timestamp = as.POSIXct(Timestamp)) %>%
    mutate(Collar = str_extract(Collar, pattern = "\\d.*")) %>%
    mutate(filetype = ".csv") %>%
    dplyr::select(DogName, Collar, Timestamp, fullname, filepath, filename, filetype)

  # Add rvc data
  if (rvc) {
      info <- rbind(files_1, files_2)
    } else {
      info <- files_2
  }

  # Arrange nicely
  info <- info %>%
    as_tibble() %>%
    arrange(DogName, Timestamp)

  # Add "file-counter"
  info <- info %>%
      group_by(DogName, Collar, Timestamp) %>%
      mutate(Counter = row_number())

  # Prepare new (clean) filenames
  info$newfilename <- paste(
      info$DogName
    , paste0("Collar", replace_na(info$Collar, "XXXXX"))
    , replace_na(gsub(as.character(info$Timestamp), pattern = "-|\\s|:", replacement = ""), rep("XXXXXXXXXXXXXX"))
    , formatC(info$Counter, width = 2, format = "d", flag = "0")
    , sep = "_"
  )

  # Add file extension
  info$newfilename <- paste0(info$newfilename, info$filetype)

  # Return the file information
  return(info)

}

#' Download GPS data from Dropbox
#'
#' This function allows you to download the files you subsetted using
#' \code{dog_files()}. Files can either be downloaded in their raw format (as
#' they are on Dropbox) using \code{clean = F}. Alternatively, the files can be
#' pre-processed using \code{clean = T}. This will: (1) make sure all files are
#' converted to .csv files with the same separator, (2) special characters such
#' as '°' are removed, (3) a column is indicating if an individual was
#' dispersing or resident is added, (4) collar handling periods are removed, (5)
#' perfect duplicates are removed, (6) unnecessary columns are removed, (6) all
#' files are finally merged into a single .csv file.
#' @export
#' @param x data.frame (or a subset of it) as returned from \code{dog_files()}
#' containing the files you'd like to download. If you simply want to download
#' all files, consider using \code{dog_download_all()}.
#' @param overwrite logical indicating if you wish to overwrite already existing
#' files
#' @param clean logical indicating if you wish to pre-process the data. If set
#' to \code{FALSE}, raw data will be stored. If set to \code{TRUE}, the files
#' will be checked for inconsistencies and they will be corrected (e.g. if there
#' are differing separators). In addition, collar handling periods will be
#' removed and dispersal/residency dates will be assigned to each fix. Insteady
#' of the raw data, you will get a single cleaned dataset. I highly recommend
#' that you set this option to \code{TRUE}.
#' @param outdir character directory where you'd like to store the downloaded
#' files. By default this is set to your working directory (i.e. to the
#' directory that is shown by \code{getwd()})
#' @param printpath should the path to the final file(s) be returned after the
#' operations are finished? This is handy if you want to fruther handle the
#' files after the download.
#' @return character filepath to the downloaded files
#' @examples
#' \dontrun{
#' # Check all available files on Dropbbox
#' files <- dog_files(rvc = T)
#'
#' # Randomly select three of the files
#' files <- files[sample(nrow(files), 3), ]
#'
#' # Download them
#' dog_download(files)
#'}
dog_download <- function(x
    , overwrite = F
    , clean     = F
    , outdir    = getwd()
    , printpath = T
    , legacy    = F
  ) {

  # setwd("/home/david/Schreibtisch")
  # x <- dog_files(rvc = F)
  # x <- subset(x, DogName == "Aspen" & Collar == "22506")
  # overwrite <- T
  # clean <- T
  # legacy <- F

  # Make sure a dataframe is provided
  if (missing(x)) {
    stop("Please provide a dataframe containing the files you want to download")
  }

  # If not allowed to overwrite, check which files already exist and remove them
  # from the list
  if (!overwrite & clean) {
    if (file.exists(file.path(outdir, "Cleaned_GPSData.csv"))) {
      warning("Cleaned file already exists.")
      return(file.exists(file.path(outdir, "Cleaned_GPSData.csv")))
    }
  }
  if (!overwrite) {
    index <- file.exists(file.path(outdir, x$newfilename))
    x <- x[!index, ]
  }

  # In case the list has no rows anymore. Stop
  if (nrow(x) == 0 | is.null(x)) {
    return(warning("All files already exist. Nothing to download."))
  }

  # Raw files will go to this temporary folder
  if (clean) {
    tmp <- tempdir()
  }

  # Download files
  cat("Downloading files from Dropbox. This may take a while...\n")
  tmpfles <- pbsapply(1:nrow(x), function(i) {

    # In case no cleaning is desired, simply download the files to the output
    # directory
    if (!clean) {

        # Download file to output directory
        tmpfles <- file.path(outdir, x$newfilename[i])
        drop_download(
            path       = x$fullname[i]
          , local_path = tmpfles
          , overwrite  = overwrite
        )

        # Return the filepath
        return(tmpfles)

      # In case cleaning is desired...
      } else {

        # Download files to temporary directory
        tmpfles <- file.path(tmp, x$newfilename[i])
        drop_download(
            path       = x$fullname[i]
          , local_path = tmpfles
          , overwrite  = T
        )

        # Clean the file
        tmpfles <- suppressMessages(
          suppressWarnings(
            .dogClean(tmpfles)
          )
        )

        # Return the filepath
        return(tmpfles)
    }
  })

  # Print update
  cat("All files downloaded...\n")

  # Further cleaning (if desired)
  if (clean) {

    # Print update
    cat("Files being cleaned and merged...\n")

    # Combine all datasets into a single one
    dat <- lapply(tmpfles, function(x) {
      read_csv(x, col_types = cols())
    }) %>% do.call(rbind, .)

    # Remove duplicates
    dups_complete <- duplicated(dat)
    dat <- subset(dat, !dups_complete)

    # # Use dispersal dates to determine dispersal phases
    # # cut <- ext$DispersalPeriods
    # cut <- dispersalDates()
    #
    # # Now loop through all dogs and use the table to specify whether a fix was
    # # taken during dispersal or not
    # dat$State <- "Resident"
    # names <- unique(cut$DogName)
    # for (i in seq_along(names)) {
    #   cutoff <- subset(cut, DogName == names[i])
    #   index <- which(dat$DogName == names[i])
    #   for (h in 1:nrow(cutoff)) {
    #     dat$State[index][dat$Timestamp[index] >= cutoff$FirstDate[h] &
    #     dat$Timestamp[index] <= cutoff$LastDate[h]] <- "Disperser"
    #   }
    # }

    # Use phases dates to determine different behavioral phases
    if (legacy) {
        cut <- dispersalDates()
        cut$Phase <- "Disperser"
      } else {
        cut <- phaseDates()
    }

    # Loop through the dogs and assign the different behavioural phases
    if (legacy) {
        dat$State <- "Resident"
      } else {
        dat$State <- NA
    }
    names <- unique(cut$DogName)
    for (i in seq_along(names)) {
      cutoff <- subset(cut, DogName == names[i])
      index <- which(dat$DogName == names[i])
      for (h in 1:nrow(cutoff)) {
        dat$State[index][dat$Timestamp[index] >= cutoff$FirstDate[h] &
        dat$Timestamp[index] <= cutoff$LastDate[h]] <- cutoff$Phase[h]
      }
    }

    # Use collar dates to remove data before/after collaring
    # cut <- ext$CollarPeriods
    cut <- collarDates()

    # Create a column that indicates if the respective fix lies within the first
    # and last date. Note, for some individuals we're missing these dates, so
    # we'll keep their data if FirstDate or LastDate are NA
    dat <- left_join(dat, cut, by = c("CollarID", "DogName"))
    dat$Keep <- ifelse(
        test  = is.na(dat$FirstDate) & is.na(dat$LastDate)
      , yes   = T
      , no    = dat$Timestamp >= dat$FirstDate & dat$Timestamp <= dat$LastDate
    )

    # Subset data and remove unnecessary columns
    dat <- subset(dat, Keep)
    dat <- dplyr::select(dat, -c("FirstDate", "LastDate", "Keep", "DogCode"))

    # Coerce to correct type
    dat <- dat %>% mutate(
        DogName   = as.character(DogName)
      , CollarID  = as.numeric(CollarID)
      , x         = as.numeric(x)
      , y         = as.numeric(y)
      , Timestamp = Timestamp
      , DOP       = as.numeric(DOP)
    )

    # Update filepath
    tmpfles <- file.path(outdir, "Cleaned_GPSData.csv")

    # Store the final dataframe to output directory
    write_csv(dat, tmpfles)

    # Print update
    cat("Finished!...\n")
  }

  # Return the final filepath(s) if desired
  if (printpath) {
    return(tmpfles)
  }
}

#' Download all GPS data from Dropbox
#'
#' This is a wrapper function around \code{dog_download} and allows you to
#' quickly download all files available on Dropbox.
#' @export
#' @param rvc logical indicating wheter "RVC GPS data" should be searched too.
#' @param overwrite logical indicating if you wish to overwrite already existing
#' files
#' @param clean logical indicating if you wish to pre-process the data
#' @param outdir character directory where you'd like to store the downloaded
#' files. By default this is set to your working directory (i.e. \code{getwd()})
#' @param printpath should the path to the final file(s) be returned after the
#' operations are finished?
#' @return character filepath to the downloaded files
#' @examples
#' \dontrun{
#' # Download them
#' dog_download_all(rvc = T)
#'}
dog_download_all <- function(
      rvc       = T
    , overwrite = F
    , clean     = T
    , outdir    = getwd()
    , printpath = T
    , legacy    = F
  ) {

    # Identify all files
    files <- dog_files(rvc = rvc)

    # Download them
    dog_download(files
      , overwrite = overwrite
      , clean     = clean
      , outdir    = outdir
      , printpath = printpath
      , legacy    = legacy
    )

}

#' Get an overview of the downloaded GPS data
#'
#' Function to create plots that give an overview of each dog's behavioral
#' phases based on the GPS data
#' @export
#' @param x data.frame or tibble containing GPS data
#' @param nrow numeric value indicating the number of desired rows
#' @param ncol numeric value indicating the number of desired columns
#' @return plot of the data
#' @examples
#' \dontrun{
#' # Download GPS data and create overview plot
#' data <- dog_download_all(rvc = F)
#'
#' # Create overview plot
#' dog_overview(data)
#'}
dog_overview <- function(x, nrow = NULL, ncol = NULL) {

  # # Testing
  # x <- dog_files(rvc = F)
  # x <- subset(x, DogName == "Aramis")
  # x <- dog_download(x, overwrite = T, outdir = tempdir(), clean = T)
  # dat <- read_csv(x)
  # x <- dat

  # Identify start and enpoint of behavioral phases for each dog
  phases <- x %>%
    nest(Data = -DogName) %>%
    mutate(Data = map(Data, function(y) {
      y$State[is.na(y$State)] <- "Undefined"
      y$switchstate <- lag(y$State) != y$State
      y$switchstate[is.na(y$switchstate)] <- F
      y$StateCounter <- cumsum(y$switchstate)
      phases <- y %>%
        group_by(StateCounter) %>%
        summarize(First = min(Timestamp), Last = max(Timestamp), State = unique(State)) %>%
        arrange(First)
      return(phases)
    })) %>% unnest(Data)

  # Identify start and endtimes for each collar for each dog
  collars <- x %>%
    nest(Data = -DogName) %>%
    mutate(Data = map(Data, function(y) {
      collars <- y %>%
        group_by(CollarID) %>%
        summarize(First = min(Timestamp), Last = max(Timestamp), CollarID = unique(CollarID)) %>%
        arrange(First) %>%
        mutate(CollarID = as.factor(CollarID))
      return(collars)
    })) %>% unnest(Data)

  # Determine if there has been a change of behavior
  ggplot() +
    geom_rect(data = phases, aes(xmin = First, xmax = Last, ymin = 0, ymax = 1, fill = State), color = NA) +
    geom_segment(data = collars, size = 0.5, aes(x = First, xend = Last, y = 1, yend = 1, lty = CollarID), show.legend = F) +
    geom_point(data = collars, aes(x = First, y = 1), col = "green", pch = 15, size = 2) +
    geom_point(data = collars, aes(x = Last, y = 1), col = "red", pch = 4, size = 2) +
    geom_density(data = x, aes(x = Timestamp, y = after_stat(ndensity)), adjust = 0.25, show.legend = F) +
    theme_minimal() +
    ylim(c(0, 1)) +
    facet_wrap(~ DogName, scales = "free", ncol = ncol, nrow = nrow) +
    xlab("") +
    ylab("") +
    # scale_fill_brewer(palette = "Set1", alpha = 0.75)
    scale_fill_viridis_d(alpha = 0.75) +
    scale_x_datetime(date_labels = "%b %Y") +
    theme(
        axis.text.x = element_text(size = 5)
      , axis.text.y = element_blank()
    )

}

#' Resample GPS fixes
#'
#' Function to resample fixes to a coarser resolution
#' @export
#' @param data Data should be resampled
#' @param hours Aspired time between fixes in hours
#' @param start Hour at which the resampled tracks should start
#' @param tol tolerance (in hours) with regards to the hour at which fixes
#' should be sampled
#' @return \code{data.frame}
resampleFixes <- function(data, hours, start, tol = 0.5) {

  # In case there are multiple individuals, run through them
  dat <- nest(data, Data = -c(DogName, CollarID))
  dat$Data <- lapply(dat$Data, function(x) {
    resampled <- .resampleFixes(x, hours, start, tol)
    return(resampled)
  })

  # Remove NA's and unnest the list
  keep      <- sapply(dat$Data, is.data.frame)
  dat       <- dat[keep, ]
  resampled <- unnest(dat, Data)

  # Return the resampled fixes
  return(resampled)
}


#' Function to download and show the collar dates for each individual
#'
#' This function is used to download and show the file that contains the
#' dates at which collars were deployed and when they stopped working
#' @export
#' @return data.frame Data frame containing the collar dates
#' @examples
#' # Check collar dates
#' collarDates()
collarDates <- function() {

  # Download updated collar handling dates
  drop_download(
      path       = "KML-Files/Collar Settings.xlsx"
    , local_path = tempdir()
    , overwrite  = T
  )

  # Download file and keep relevant columns
  collar_periods <- file.path(tempdir(), "Collar Settings.xlsx") %>%
    read_excel(skip = 1) %>%
    subset(!is.na(`Collar Nr.`) & !is.na(`Dog Name`)) %>%
    dplyr::select(
        CollarID  = `Collar Nr.`
      , DogName   = `Dog Name`
      , DogCode   = `Dog Code`
      , Sex       = `Sex`
      , FirstDate = `Collaring.Date.Text`
      , LastDate1 = `Stop.Recording.Date.Text`
      , LastDate2 = `Last fix date`
    )

  # Some dates are not parsed correctly, let's do so now
  collar_periods <- collar_periods %>%
    mutate(
        FirstDate = .parseDate(FirstDate) # Excel is stupid
      , LastDate1 = .parseDate(LastDate1) # Excel is stupid
      , LastDate2 = .parseDate(LastDate2) # Excel is stupid
    )

  # collar_periods$LastDate2 <- lapply(collar_periods$LastDate2, function(x) {
  #   if (is.na(x)) {
  #       timestamp <- NA
  #     } else if (is.na(suppressWarnings(as.numeric(x)))) {
  #       timestamp <- paste0(x, ":00")
  #       timestamp <- lubridate::dmy_hms(x, tz = "UTC")
  #     } else {
  #       timestamp <- as.Date(as.numeric(x), origin = "1899-12-30", tz = "UTC")
  #       timestamp <- as.POSIXct(timestamp)
  #       timestamp <- with_tz(timestamp, "UTC")
  #   }
  #   return(as.POSIXct(timestamp, tz = "UTC"))
  # }) %>% do.call(c, .)

  # Convert local times to UTC (think about this again!!!)
  collar_periods <- collar_periods %>%
    mutate(
        FirstDate = ymd_hms(FirstDate) - hours(2) + minutes(5) # Subtract 2 hours to get utc time, add 5 mins for tolerance
      , LastDate1 = ymd_hms(LastDate1) - hours(2) + minutes(5) # Subtract 2 hours to get utc time, add 5 mins for tolerance
      , LastDate2 = LastDate2 - hours(2) + minutes(5) # Subtract 2 hours to get utc time, add 5 mins for tolerance
    ) %>%
    mutate(LastDate = pmax(LastDate1, LastDate2, na.rm = T)) %>%
    dplyr::select(-c(LastDate1, LastDate2)) %>%
    arrange(CollarID, DogName)

  # Return the final dataframe
  return(collar_periods)
}

#' Function to download and show the segmentation dates for the different
#' behavioral phases
#'
#' This function is used to download and show the file that contains the
#' segmentation dates that are used to classify the GPS data into different
#' behavioral phases / states
#' @export
#' @return data.frame Data frame containing the segmentation dates
#' @examples
#' # Check segmentation dates
#' phaseDates()
phaseDates <- function() {

  # Download phases file
  drop_download(
      path       = "KML-Files/Dogs_traj_segmentation.xlsx"
    , local_path = tempdir()
    , overwrite  = T
  )

  # Load it
  phase_dates <- file.path(tempdir(), "Dogs_traj_segmentation.xlsx") %>%
    {suppressMessages(read_excel(., skip = 10))}

  # Do some cleaning
  phase_dates <- phase_dates %>%
    subset(Ind != ".." & Ind != "..." & Phase != ".." & Phase != "...") %>%
    {suppressWarnings(
      mutate(.
        , Start_phase_date = as.Date(as.numeric(Start_phase_date), origin = "1899-12-30")   # Excel is stupid
        , Start_phase_time = times(as.numeric(Start_phase_time))                            # Excel is stupid
        , Stop_phase_date  = as.Date(as.numeric(Stop_phase_date), origin  = "1899-12-30")   # Excel is stupid
        , Stop_phase_time  = times(as.numeric(Stop_phase_time))                             # Excel is stupid
      )
    )} %>%
    {suppressWarnings(
      mutate(.
        , FirstDate = round_date(ymd_hms(paste(Start_phase_date, Start_phase_time)), unit = "30 min")
        , LastDate  = round_date(ymd_hms(paste(Stop_phase_date, Stop_phase_time)), unit = "30 min")
      )
    )} %>%
    select(DogName = Ind, CollarID = CollarID, Phase, FirstDate, LastDate) %>%
    mutate(.
      , Phase = gsub(Phase, pattern = "Set$", replacement = "Setl")
      , Phase = gsub(Phase, pattern = "Set$", replacement = "Setl")
    ) %>%
    mutate(
      LastDate = if_else(is.na(LastDate), ymd_hms(Sys.time()), LastDate)
    ) %>%
    mutate(
      Phase = case_when(
          Phase == "Res" ~ "Resident"
        , Phase == "Trans" ~ "Disperser"
        , Phase == "Disp" ~ "Disperser"
        , Phase == "StopOver" ~ "StopOver"
        , Phase == "Setl" ~ "Settlement"
        , Phase == "Expl" ~ "Explorer"
        , TRUE ~ Phase
      )
    )

  # Return it
  return(phase_dates)
}

#' (OLD!!!) Function to download and show the segmentation dates for the
#' different behavioral phases
#'
#' This function is used to download and show the file that contains the
#' segmentation dates that are used to classify the GPS data into residents vs.
#' dispersers
#' @export
#' @return data.frame Data frame containing the segmentation dates
#' @examples
#' # Check segmentation dates
#' dispersalDates()
# Helper to download collar handling dates
dispersalDates <- function() {

  # Download updated cutoff dates
  drop_download(
      path       = "KML-Files/DISPERSERS/overview dispersal dates.xlsx"
    , local_path = tempdir()
    , overwrite  = T
  )

  # Do some cleaning
  dispersal_periods <- file.path(tempdir(), "overview dispersal dates.xlsx") %>%
    read_excel() %>%
    subset(!is.na(CollarID) & !is.na(DogName)) %>%
    dplyr::select(
        CollarID
      , DogName
      , FirstDate = StartDate_UTC
      , LastDate  = EndDate_UTC
      , DispersalNo
    ) %>%
    mutate(
        FirstDate = ymd_hms(FirstDate)
      , LastDate  = ymd_hms(LastDate)
    )

  # Return the final dataframe
  return(dispersal_periods)

}

################################################################################
#### Level 2 Functions
################################################################################
# Custom function to parse a vector of dates in various formats
# x <- c("21.07.2018 10:00", "2021-11-02 09:15", "43055.38", NA)
.parseDate <- function(x) {
  parsed_dates <- lapply(x, function(z) {
    z_numeric <- suppressWarnings(as.numeric(z))
    parsed <- parsedate::parse_date(z, approx = F)
    if (is.na(parsed) & !is.na(z_numeric)) {
      parsed <- convertToDateTime(z_numeric, tz = "UTC")
      parsed <- with_tz(parsed, "UTC")
    }
    return(parsed)
  })
  parsed_dates <- do.call(c, parsed_dates)
  return(parsed_dates)
}

# Helper to clean files
.dogClean <- function(x) {

  # Testing
  # x <- tmpfles

  # Extract information from filenames
  info <- basename(x)
  info <- str_split(info, pattern = "_|\\.")
  info <- data.frame(do.call(rbind, info), stringsAsFactors = F)
  if (ncol(info) == 4){
      names(info) <- c("DogName", "Collar", "Date", "filetype")
    } else {
      names(info) <- c("DogName", "Collar", "Date", "FileNo", "filetype")
  }
  info <- cbind(x, info)
  info$filename <- as.character(info$x)
  info$x <- NULL

  # Loop through the files and clean them
  for (i in 1:nrow(info)) {
    if (info$filetype[i] == "txt") {
      cleaned <- .cleanTXT(
          x         = info$filename[i]
        , DogName   = info$DogName[i]
        , Collar    = info$Collar[i]
        , Timestamp = info$Timestamp[i]
      )
    } else if (info$filetype[i] == "csv") {
      cleaned <- .cleanCSV(
          x         = info$filename[i]
        , DogName   = info$DogName[i]
        , Collar    = info$Collar[i]
        , Timestamp = info$Timestamp[i]
      )
    } else {
      stop("Can't clean this file. Neither a .csv nor a .txt")
    }
  }
  return(cleaned)
}

# Helper to clean .csv files
.cleanCSV <- function(x, DogName, Collar, Timestamp) {

  # Identify separator
  sep <- readLines(x, n = 1)
  sep <- if_else(grepl(x = sep, pattern = ";", useBytes = T), ";", ",")

  # Load the file as plain text and remove funny characters
  dat <- read_file(x, local = locale(encoding = "latin1"))
  if (sep == ",") {
      dat <- gsub(dat, pattern = ",", replacement = ";")
    } else {
      dat <- gsub(dat, pattern = ",", replacement = ".")
  }
  dat <- gsub(dat, pattern = "°", replacement = "")
  dat <- gsub(dat, pattern = "/", replacement = ".")
  dat <- gsub(dat, pattern = "\\[[^][]*]", replacement = "")
  dat <- gsub(dat, pattern = " ", replacement = "")
  write(dat, x)

  # Load file as data frame and do some more cleaning
  dat <- read_delim(x, local = locale(encoding = "latin1"), delim = ";") %>%

    # Add Dog Name
    mutate(DogName = DogName) %>%

    # Retrieve timestamp
    mutate(Timestamp = as.POSIXct(
      paste(UTC_Date, UTC_Time), tz = "UTC", format = "%d.%m.%Y %H:%M:%S")
    ) %>%

    # # Remove special characters like [°]
    # setNames(gsub(names(.), pattern = " \\[*", replacement = "")) %>%
    # setNames(gsub(names(.), pattern = "\\]", replacement = "")) %>%

    # Keep only desired columns
    dplyr::select(.
      , DogName   = DogName
      , CollarID  = CollarID
      , x         = Longitude
      , y         = Latitude
      , Timestamp = Timestamp
      , DOP       = DOP
    )

  # Store the cleaned data to file
  write_csv(dat, x)

  # Return the filepath
  return(x)

}

# Helper to clean .txt files
.cleanTXT <- function(x, DogName, Collar, Timestamp) {

  # Load the file
  dat <- read_delim(x, delim = "\t", skip = 2) %>%

    # Keep only the columns of interest
    dplyr::select(
        x                   = `Longitude (deg)`
      , y                   = `Latitude (deg)`
      , Timestamp           = `UTC time (yyyy-mm-dd HH:MM:SS)`
      , Height              = `Height above MSL (m)`
      , HorizontalAccuracy  = `Horizontal accuracy (m)`
      , VerticalAccuracy    = `Vertical accuracy (m)`
    ) %>%

    # Add dog name
    mutate(DogName = DogName) %>%

    # Prepare columns present in dat1
    mutate(., CollarID = NA, DOP = NA) %>%

    # Remove rows with missing fixes
    filter(., !is.na(x)) %>%

    # Order the data by timestamp
    arrange(., Timestamp) %>%

    # Create Column that indicates the timelag and distance between two fixes
    # This allows us to also calculate the speed
    mutate(x,
        dt = as.numeric(Timestamp - lag(Timestamp), units = "hours")
      , dl = sqrt((x - lag(x))**2 + (y - lag(y)) ** 2) * 111
      , speed = dl / dt
    ) %>%
    filter(., Height > quantile(Height, 0.1)) %>%
    filter(., Height < quantile(Height, 0.9)) %>%
    filter(., VerticalAccuracy < quantile(VerticalAccuracy, 0.9)) %>%
    filter(., HorizontalAccuracy < quantile(HorizontalAccuracy, 0.9)) %>%
    filter(., year(Timestamp) != 1970) %>%
    filter(., speed < quantile(speed, 0.9, na.rm = TRUE)) %>%
    dplyr::select(c("DogName", "CollarID", "x", "y", "Timestamp", "DOP"))

  # Create new filename
  x <- paste0(substr(x, start = 0, stop = nchar(x) - 4), ".csv")

  # Write the cleaned data to file
  write_csv(dat, x)

  # Return the filepath
  return(x)

}

# Helper function to resample fixes for a single individual
.resampleFixes <- function(data, hours, start, tol = 0.5) {

  # Identify the first date at which a fix was taken
  first <- range(data$Timestamp)[1] %>%

    # Update the time to the start time specified in the function
    update(., hour = start, min = 0, sec = 0)

  # Identify the last date at which a fix was taken
  last <- range(data$Timestamp)[2] %>%

    # Update the time to the end time specified in the function
    update(., hour = 24, min = 0, sec = 0)

  # Prepare a range of dates for which we would expect to find data according
  # to the specified sampling scheme
  dates <- seq(first, last, by = paste0(hours, " hours")) %>%

    # Coerce the data to a dataframe
    as.data.frame() %>%

    # Give the column a nicer name
    set_names("Timestamp")

  # For each Timestamp we now identify the closest fix
  closest <- sapply(1:nrow(dates), function(x) {

    # Identify the index of the closest fix
    index <- which.min(abs(dates$Timestamp[x] - data$Timestamp))[1]

    # Check if the time difference is smaller than 30 mins
    close <- as.numeric(abs(dates$Timestamp[x] - data$Timestamp[index]), units = "hours") <= tol

    # In case the fix is close enough, return its index
    if (close) {
      return(index)
    } else {
      return(NA)
    }
  })

  # Remove NAs
  closest <- na.omit(closest)

  # Return respective fixes
  if (length(closest) > 0) {
    return(data[closest, ])
  } else {
    return(NA)
  }
}
