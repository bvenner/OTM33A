#' Read data from text file and return data.table
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return data.table containing data contained in file.name, with headers and attributes read from the file
#' @export
#' @examples
#' read.OTM33A(file.name,numskip=33)


read.OTM33A <- function(file.name, numskip=33) {
  # save mast heading
  header = readLines.mvb(file.name,n=-1,EOF="<<<DATA>>>",line.count=T)
  numskip=attr(header,"line.count")
  rawdat <- read.table(file.name, header=T, sep="\t",skip=numskip)
  rawdat <- data.table(rawdat)
  rawdat <- rawdat[!is.na(rawdat$Time)]
  DateTime = as.POSIXct(strptime(as.character(rawdat$Time),format = "%m/%d/%y %H:%M:%S"))
  rawdat[,DateTime := DateTime]
  rawdat[,sub := !is.na(DateTime)]
    # Only keep rows with a timestamp (removes extra rows at end of file)
  setattr(rawdat,"distance",as.numeric(stringr::str_split_fixed(header[grep("Distance to Source",header)],"\t",n=2)[2]))
#  setattr(rawdat,"distance",header[Name=="Distance to Source",as.numeric(as.character(Value))])
  setattr(rawdat,"heading",as.numeric(stringr::str_split_fixed(header[grep("Mast Heading",header)],"\t",n=2)[2]))
  setattr(rawdat,"file.name",file.name)
  rawdat
}
