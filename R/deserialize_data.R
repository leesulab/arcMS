#' Deserialize data
#'
#' A function to deserialize the protobuf data
#'
#' @param rg_content the $content of a request in octet-stream
#' @return A S3 object which contains the spectral data associated with each spectrum
#' @keywords internal

deserialize_data <- function(rg_content) {
  file = system.file("message", "message.proto", package = "arcMS")
  if (!nzchar(file)) {
    # fallback: try the inst/ path directly (e.g. when using devtools::load_all)
    file = system.file("inst", "message", "message.proto", package = "arcMS")
  }
  if (!nzchar(file)) {
    # last resort: look relative to package source
    pkg_path = find.package("arcMS", quiet = TRUE)
    if (length(pkg_path) > 0) {
      candidate = file.path(pkg_path, "inst", "message", "message.proto")
      if (file.exists(candidate)) file = candidate
    }
  }
  if (!nzchar(file) || !file.exists(file)) {
    stop("Cannot find message.proto file for protobuf deserialization.")
  }
  RProtoBuf::readProtoFiles(file)
  Spectrum <- RProtoBuf::P("Spectrum")
  if (is.null(Spectrum)) {
    # Try with full package qualifier (if proto file has package declaration)
    Spectrum <- RProtoBuf::P("Waters.WebApi.Common.Models.Spectrum")
  }
  if (is.null(Spectrum)) {
    stop("Failed to load Spectrum protobuf descriptor. Check that RProtoBuf is properly installed.")
  }
  alldata = rg_content
  n = 1
  i = 1
  outputlist = list()
  while (n < length(alldata)) {
    pos = DecodeVarint32(alldata, as.integer(n))
    clen = pos[[1]]
    n = pos[[2]]
    nend = n + clen - 1
    tmp = alldata[n:nend]
    outputlist[[i]] = RProtoBuf::read(Spectrum, tmp)
    n = nend + 1
    i = i + 1
  }
  return(outputlist)
}
