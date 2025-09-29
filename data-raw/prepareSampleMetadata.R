require(tidyverse)
require(DBI)
require(RPostgres)
require(lubridate)
metaRaw <- read.csv("../../database_struct/Standardtabeller/metadata_sklinna_2016.csv", skip = 1,
                    fileEncoding = "windows-1252", stringsAsFactors =  F)

metaRaw <- metaRaw[1:169,]

head(metaRaw)
tail(metaRaw)
names(metaRaw)[38] <- "other"
names(metaRaw)
str(metaRaw)


metaRaw$weight <- as.numeric(metaRaw$weight)
metaRaw[c(16:19, 22)] <- sapply(metaRaw[c(16:19, 22)], as.numeric)
metaRaw[c(21, 28, 29)] <- sapply(metaRaw[c(21, 28, 29)] , as.numeric)

metaRaw$hatching_success <- as.logical(as.numeric(metaRaw$hatching_success))
metaRaw$breeding_success <- as.logical(as.numeric(metaRaw$breeding_success))
metaRaw$age <- as.numeric(metaRaw$age)

cap <- function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2),
        sep = "", collapse = " ")
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = "[- ]"), cap, USE.NAMES = !is.null(names(s)))
}

metaRaw$data_responsible <- capwords(metaRaw$data_responsible)
metaRaw$data_responsible <- gsub("Svein Håkon", "Svein-Håkon", metaRaw$data_responsible)

metaRaw$species <- sapply(metaRaw$species, cap)
metaRaw$colony <- sapply(metaRaw$colony, cap)
#require(devtools)

metaRaw$logger_model_retrieved[metaRaw$logger_model_retrieved == "Mk4093"] <- "mk4093"
#metaRaw$logger_model_retrieved[metaRaw$logger_model_retrieved == "Mk4083"] <- "mk4083"

#
# metaRaw$subspecies[metaRaw$subspecies == ""] <- NA
# metaRaw$sex[metaRaw$sex == ""] <- NA
# metaRaw$sexing_method[metaRaw$sexing_method == ""] <- NA
# metaRaw$logger_id_deployed[metaRaw$logger_id_deployed == ""] <- NA
# metaRaw$logger_model_deployed[metaRaw$logger_model_deployed == ""] <- NA
# metaRaw$logger_id_retrieved[metaRaw$logger_id_retrieved == ""] <- NA
# metaRaw$logger_id_retrieved[metaRaw$logger_id_retrieved == ""] <- NA
# metaRaw$breeding_success_criterion[metaRaw$breeding_success_criterion == ""] <- NA
# metaRaw$colony[metaRaw$colony == ""]
metaRaw[metaRaw == "NA"] <- NA
metaRaw[metaRaw == ""] <- NA

metaRaw$date <- as.Date(metaRaw$date, format = "%d/%m/%Y")
#metaRaw <-
metaRaw <- metaRaw[order(metaRaw$date),]
metaRaw$data_responsible[metaRaw$data_responsible == "Signe Christensen Dalsgaard"] <- "Signe Christensen-Dalsgaard"
metaRaw <-metaRaw[is.na(metaRaw$logger_id_retrieved), ] ##Remove all retrievals in this data, since none of them are deployed. We need to add some retrievals
metaRaw$date[is.na(metaRaw$date)] <- "2016-03-01"

##Make up some retrievals artificially that matches the deployments

tempRetr <- metaRaw[head(!is.na(metaRaw$logger_id_deployed), 20), ]
tempRetr$logger_id_retrieved <- tempRetr$logger_id_deployed
tempRetr$logger_model_retrieved <- tempRetr$logger_model_deployed
tempRetr$logger_id_deployed <- NA
tempRetr$logger_model_deployed <- NA
tempRetr$date <- tempRetr$date + 365


sampleMetadata <- rbind(metaRaw, tempRetr)
sampleMetadata <- as_tibble(sampleMetadata)
# sampleMetadata %>%
#   filter(!is.na(logger_id_deployed))
#
# sampleMetadata %>%
#   filter(!is.na(logger_id_retrieved))
#
# sampleMetadata$date[!is.na(sampleMetadata$logger_id_retrieved)] <- sampleMetadata$date[!is.na(sampleMetadata$logger_id_retrieved)] + month(1)

dep <- sampleMetadata %>% filter(date < '2017-01-01')
ret <- sampleMetadata %>% filter(date >= '2017-01-01')

##add deployment data for the duplicate sessions.
dupDep <- dep[1:2, ]
dupDep$ring_number <- c("1234", "5678")
dupDep$date <- as.Date(c("2017-03-04"))

dupRet <- ret[1:2, ]
dupRet$ring_number <- c("1234", "5678")
dupRet$date <- as.Date(c("2018-03-04"))

sampleMetadata <- bind_rows(dep, dupDep, dupRet, ret)

devtools::use_data(sampleMetadata, overwrite = T)

sampleIndividInfo <- sampleMetadata[!duplicated(sampleMetadata[c(2:3)]),c(2:3, 10, 4, 11, 12, 13:15)]
sampleIndividInfo <- as_tibble(sampleIndividInfo)
#devtools::use_data(sampleIndividInfo, overwrite = T)

tempLoggers <- rbind(sampleMetadata[c(7, 6)], setNames(sampleMetadata[c(9, 8)], names(sampleMetadata[c(7, 6)])))

sampleLoggerInfo <- tempLoggers[!duplicated(tempLoggers),]
sampleLoggerInfo <- sampleLoggerInfo[sampleLoggerInfo$logger_id_retrieved != "",]
sampleLoggerInfo <- sampleLoggerInfo[!is.na(sampleLoggerInfo$logger_id_retrieved),]

sampleLoggerInfo$producer <- "Biotrack"
sampleLoggerInfo$producer[grep("c", sampleLoggerInfo$logger_model_retrieved)] <- "Migrate Technology"
sampleLoggerInfo$producer[grep("f", sampleLoggerInfo$logger_model_retrieved)] <- "Migrate Technology"
sampleLoggerInfo$producer[grep("w", sampleLoggerInfo$logger_model_retrieved)] <- "Migrate Technology"

sampleLoggerInfo$producer[grep("mk15", sampleLoggerInfo$logger_model_retrieved)] <- "BAS"
sampleLoggerInfo$logger_model_retrieved[sampleLoggerInfo$logger_model_retrieved == "Mk4083"] <- "mk4083"

sampleLoggerInfo$production_year <- 2013
sampleLoggerInfo$project <- "seatrack"
sampleLoggerInfo <- sampleLoggerInfo[c(1, 3, 4, 2, 5)]
names(sampleLoggerInfo)[c(1, 4)] <- c("logger_serial_no", "logger_model")

##On second thought, we won't write to logger_info, but include this in logger_import instead
require(lubridate)
sampleLoggerImport <- sampleLoggerInfo
sampleLoggerImport$starttime_gmt <- as.Date('2015-01-01') + years(1)
sampleLoggerImport$logging_mode <- 1
sampleLoggerImport$started_by <- "Jens Åström"
sampleLoggerImport$started_where <- "NINA"
sampleLoggerImport$days_delayed <- 10
sampleLoggerImport$programmed_gmt_time <- sampleLoggerImport$starttime_gmt + days(2)
sampleLoggerImport$intended_species <- "Little auk"
sampleLoggerImport$intended_location <- "Bjørnøya"
sampleLoggerImport$intended_deployer <- "Vegard Sandøy Bråthen"
#sampleLoggerImport$data_responsible <- "Jens Åström"
sampleLoggerImport$shutdown_session <- F
sampleLoggerImport$shutdown_date <- NA
sampleLoggerImport$field_status <- NA
sampleLoggerImport$downloaded_by <- NA
sampleLoggerImport$download_date <- NA
sampleLoggerImport$download_type <- NA
sampleLoggerImport$decomissioned <- NA
sampleLoggerImport$comment <- NA

##Add another startup to test multiple active sessions
temp <- sampleLoggerImport %>%
  filter(logger_serial_no %in% c("Z236", "Z231"))
temp$starttime_gmt <-  as.Date(c("2017-01-01", "2017-01-01"))
temp$programmed_gmt_time <- as.Date(c("2017-01-01", "2017-01-01"))

sampleLoggerImport <- sampleLoggerImport %>%
  bind_rows(temp)


sampleLoggerImport <- sampleLoggerImport[c("logger_serial_no", "logger_model", "producer",
                                           "production_year", "project", "starttime_gmt",
                                           "logging_mode", "started_by", "started_where",
                                           "days_delayed", "programmed_gmt_time", "intended_species",
                                           "intended_location", "intended_deployer",
                                           "shutdown_session", "shutdown_date","field_status", "downloaded_by", "download_type",
                                           "download_date", "decomissioned", "comment")]

sampleLoggerImport <- as_tibble(sampleLoggerImport)

devtools::use_data(sampleLoggerImport, overwrite = T)

sampleLoggerShutdown <- sampleLoggerImport

sampleLoggerShutdown <-  sampleLoggerShutdown[sampleLoggerShutdown$logger_serial_no %in% sampleMetadata$logger_id_retrieved[!is.na(sampleMetadata$logger_id_retrieved)], ]
names(sampleLoggerShutdown)
sampleLoggerShutdown[c(3:5, 7:15)] <- NA ##keep starttime_gmt to allow multiple open sessions
sampleLoggerShutdown$shutdown_session = T
sampleLoggerShutdown$download_type[1:30] <- "Successfully downloaded"
sampleLoggerShutdown$download_type[31:nrow(sampleLoggerShutdown)] <- "Nonresponsive"
sampleLoggerShutdown$field_status[1:30] <- "OK"
sampleLoggerShutdown$field_status[31:nrow(sampleLoggerShutdown)] <- "Error"
sampleLoggerShutdown$downloaded_by <- "Jens Åström"
sampleLoggerShutdown$download_date <- Sys.Date()
sampleLoggerShutdown$shutdown_date <- Sys.Date()
sampleLoggerShutdown$decomissioned <- F
sampleLoggerShutdown$download_type[20:40] <- "Reconstructed"


sampleLoggerShutdown <- as_tibble(sampleLoggerShutdown)
devtools::use_data(sampleLoggerShutdown, overwrite = T)


sampleLoggerModels <- sampleLoggerInfo[!duplicated(sampleLoggerInfo[c(2, 4)]), c(2, 4)]
names(sampleLoggerModels) <- c("producer", "model")
devtools::use_data(sampleLoggerModels, overwrite = T)

# tmpLoggerInfo <- dbGetQuery(con, "SELECT * FROM loggers.logger_info")
# sampleLoggerImport<- tmpLoggerInfo["logger_id"]
# sampleLoggerImport$startdate_gmt <- Sys.Date()
# sampleLoggerImport$starttime_gmt <- Sys.time()
# sampleLoggerImport$logging_mode <- "testmode"
# sampleLoggerImport$started_by <- "Jens Åström"
# sampleLoggerImport$started_where <- "NINA"
# sampleLoggerImport$days_delayed <- 0
# sampleLoggerImport$programmed_gmt_date <- Sys.Date()
# sampleLoggerImport$programmed_gmt_time <- Sys.time()
#
# DBI::dbSendQuery(con, "SET search_path TO imports, public")
# DBI::dbWriteTable(con, "logger_import", sampleLoggerImport, append = T, overwrite = F)

 # oldLoggerInfo <- DBI::dbGetQuery(con, "SELECT * FROM loggers.logger_info")
 # newLoggerInfo <- anti_join(sampleLoggerInfo, oldLoggerInfo)
 # writeLoggerInfo(newLoggerInfo)





# metadata <- metaRaw
# # metadata <- metadata[!compareNA(metadata$logger_id_retrieved, "v2014037"),]
# # metadata <- metadata[!compareNA(metadata$logger_id_retrieved, "v2014025"),]
# # metadata <- metadata[!compareNA(metadata$logger_id_retrieved, "c406"),]
# # metadata <- metadata[!compareNA(metadata$logger_id_retrieved, "C406"),]
# # metadata <- metadata[!compareNA(metadata$logger_id_retrieved, "C415"),]
# # metadata <- metadata[!compareNA(metadata$logger_id_retrieved, "C393"),]
#
#
# #No loggers retrieved after deployment in this data set.
# ##Test setting retrieval date one year later
#
# ##add hoc to compensate for earlier insert
# metadata <- metadata[-which(metadata$logger_id_deployed %in% c("T089", "B1263")), ]
#
# metaRetr <- metaRaw[metaRaw$logger_id_retrieved %in% retr$logger_id_retrieved,]
# #metaRetr$date <-
# metaRetr$date <- metaRetr$date + 365
# metaRetr$logger_id_deployed <- NA
# metaRetr$logger_model_deployed <- NA
#
#
# writeMetadata(metadata)
# writeMetadata(metaRetr)


##########
##Posdata

connectSeatrack("testreader", "testreader")

posdata <- getPosdata(limit = 1000)
use_data(posdata)

#Metadata
#This is a bit of a hack to save the metadata from the production database to the testing version.
connectSeatrack()

toLoad <- list("breeding_stages",
            "breeding_success_criterion",
            "colony",
            "download_types",
            "euring_codes",
            "import_types",
            "location",
            "logger_fate",
            "logger_producers",
            "logger_models",
            "logger_files",
            "logging_modes",
            "mounting_types",
            "people",
            "retrieval_type",
            "sex",
            "sexing_method",
            "species",
            "subspecies")

loadFun <- function(x){
  tmp <- dbReadTable(con,  Id(schema = "metadata", table = x))
  assign(x, tmp, envir = .GlobalEnv)
}

lapply(toLoad, loadFun)


# breeding_stages <- dbReadTable(con, Id(schema = "metadata", table = "breeding_stages"))
# breeding_success_criterion <- dbReadTable(con, Id(schema = "metadata", table = "breeding_success_criterion"))
# colony <- dbReadTable(con, Id(schema = "metadata", table = "colony"))
# download_types <- dbReadTable(con, Id(schema = "metadata", table = "download_types"))
# euring_codes <- dbReadTable(con, Id(schema = "metadata", table = "euring_codes"))
# import_types <- dbReadTable(con, Id(schema = "metadata", table = "import_types"))
# location <- dbReadTable(con, Id(schema = "metadata", table = "location"))
# logger_fate <- dbReadTable(con, Id(schema = "metadata", table = "logger_fate"))
# logger_files <- dbReadTable(con, Id(schema = "metadata", table = "logger_files"))
# logger_models <- dbReadTable(con, Id(schema = "metadata", table = "logger_models"))
# logger_producers <- dbReadTable(con, Id(schema = "metadata", table = "logger_producers"))
# logging_modes <- dbReadTable(con, Id(schema = "metadata", table = "logging_modes"))
# mounting_types <- dbReadTable(con, Id(schema = "metadata", table = "mounting_types"))
# people <- dbReadTable(con, Id(schema = "metadata", table = "people"))
# retrieval_type <- dbReadTable(con, Id(schema = "metadata", table = "retrieval_type"))
# sex <- dbReadTable(con, Id(schema = "metadata", table = "sex"))
# sexing_method <- dbReadTable(con, Id(schema = "metadata", table = "sexing_method"))
# species <- dbReadTable(con, Id(schema = "metadata", table = "species"))
# subspecies <- dbReadTable(con, Id(schema = "metadata", table = "subspecies"))


save(breeding_stages,
     breeding_success_criterion,
     colony,
     download_types,
     euring_codes,
     import_types,
     location,
     logger_fate,
     logger_models,
     logger_files,
     logger_producers,
     logging_modes,
     mounting_types,
     people,
     retrieval_type,
     sex,
     sexing_method,
     species,
     subspecies, file = "data/metadata.rda")

#connectSeatrack(dbname = "seatrack_devel", Username = "seatrack_admin", Password = "")

writeFun <- function(x){
    dbWriteTable(con,  Id(schema = "metadata", table = x), append = T, get(x))

}

lapply(toLoad, writeFun)


##Activity data
lightRaw <- read_delim("../../database_struct/Standardtabeller/light_BT_overview.txt", delim = "\t")
lightRaw

filenames <- dbReadTable(con, Id(schema = "loggers", table = "file_archive"))

sampleLight <- lightRaw %>%
  slice(1:100) %>%
  mutate(filename = filenames$filename[1],
         date_time = as_datetime(date_time)) %>%
  select(filename,
         date_time,
         clipped,
         raw_light,
         std_light)


sampleLight

writeRecordings(lightData = sampleLight)


activityRaw <- read_delim("../../database_struct/Standardtabeller/activity_BT_overview.txt", delim = "\t")
sampleActivity <- activityRaw %>%
  slice(1:100) %>%
  mutate(filename = filenames$filename[1],
         date_time = as_datetime(date_time)) %>%
  select(filename,
         date_time,
         conductivity,
         std_conductivity)

writeRecordings(activityData = sampleActivity)


temperatureRaw <- read_delim("../../database_struct/Standardtabeller/temperature_BT_overview.txt", delim = "\t")

sampleTemperature <- temperatureRaw %>%
  slice(1:100) %>%
  mutate(filename = filenames$filename[1],
         date_time = as_datetime(date_time)) %>%
  select(filename,
         date_time,
         wet_temp_min,
         wet_temp_max,
         wet_temp_mean,
         num_samples)

writeRecordings(temperatureData = sampleTemperature)


#sampleRingHistory (requires some data in the database)

rn <- getIndividInfo() %>%
  group_by(individ_id) %>%
  select(individ_id,
         current_euring_code = euring_code,
         current_ring_number = ring_number,
         current_ring_color = color_ring) %>%
  slice(1) %>%
   head(100)

rn <- rn %>%
  mutate(date_current_ring = "2017-01-01",
         old_euring_code = "OLD",
         old_ring_number = paste('OLD', current_ring_number),
         old_ring_color = "BLACK",
         date_old_ring = "1999-12-31")


sampleRingHistory <- rn
use_data(sampleRingHistory, overwrite = T)
