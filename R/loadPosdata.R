#' Load posdata files into R.
#'
#' This function loads posdata files from disk into R for further import to database.
#'
#' @param files A character vector of posdata files to import. Should not include file endings, i.e. should have a format like "posdata_FULGLA_eynhallow_2014"
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#'  files<-c("posdata_FULGLA_eynhallow_2014",
#' "posdata_FULGLA_eynhallow_2013",
#' "posdata_FULGLA_eynhallow_2012",
#' "posdata_FULGLA_eynhallow_2011",
#' "posdata_FULGLA_eynhallow_2010",
#' "posdata_FULGLA_eynhallow_2009",
#' "posdata_FULGLA_eynhallow_2007"
#' )
#'
#' toImport <- loadPosdata(files)
#'
#' summary(toImport)
#' }
#'
#'
#'

loadPosdata <- function(files,
                        originFolder = "../Rawdata"){

  stripnames <- gsub("(posdata_)(.*)", "\\2", files)

  ##load new file versions in a list
  fileList <- list()
  for(i in 1:length(stripnames)) {
    fileList[[i]] <- read.csv(paste0(originFolder, "/", files[i], ".txt", sep=""), sep="\t", as.is=T, fileEncoding="windows-1252")
  }
  names(fileList) <- stripnames

  class(fileList) <- c("posdata", "list")

  return(fileList)
}

#' @export
summary.posdata <- function(posdata){
  ##Check encodings and posdatafile
  #This is a manual quality check to spot common errors. But it does not spot everything...

  outList<-list()
  for(i in 1:length(posdata)){
    outList[[i]]<-data.frame("data_responsible"=unique(posdata[[i]]$data_responsible),
                             "file"=unique(posdata[[i]]$posdata_file),
                             "species"=unique(posdata[[i]]$species),
                             "colony"=unique(posdata[[i]]$colony))
  }

  names(outList) <- names(posdata)

  niceTab <- data.frame(outList[[1]])
  if(length(outList) > 1){
  for(j in 2:length(outList)){
    niceTab <- rbind(niceTab, outList[[j]])
  }
  }

  #rownames(niceTab) <- names(outList)

  return(niceTab)

}
