#' deserialize_data
#'
#' A function to deserialize the protobuf data
#'
#' @param rg_content the $content of a request in octet-stream
#' @return A S3 object which contains the spectral data associated with each spectrum
#' @export
#'

deserialize_data <- function(rg_content) {
  file = system.file("message", "message.proto", package = "parquetMS")
  RProtoBuf::readProtoFiles(file)
  protoenv = as.environment("RProtoBuf:DescriptorPool")
  alldata = rg_content
  n = 1
  i = 1
  outputlist = list()
  while (n < length(alldata)) {
    pos = DecodeVarint32(alldata,as.integer(n))
    clen = pos[[1]]
    n = pos[[2]]
    nend = n + clen - 1
    tmp = alldata[n:nend]
    outputlist[[i]] = RProtoBuf::read(protoenv$Spectrum, tmp)
    n = nend + 1
    i = i+1
  }
  return(outputlist)
}
