#' DecodeVarint32
#'
#'@description
#' A function to decode a buffer data.
#'
#' @references
#' \itemize{ Translated from Python:
#'   \item Encoder - https://github.com/protocolbuffers/protobuf/blob/master/python/google/protobuf/internal/encoder.py
#'   \item Decoder - https://raw.githubusercontent.com/protocolbuffers/protobuf/master/python/google/protobuf/internal/decoder.py
#'   \item Author - Fred K. Gruber <fred@gnshealthcare.com>
#' }
#'
#' @param buffer The buffer data
#' @param pos The position of the beginning of the message
#' @return Decoded message position
#' @export
#'

DecodeVarint32 = function(buffer, pos){
  mask = (2 ^ 32 - 1)
  result = 0 %>% as.raw
  shift = 0
  while(TRUE){
    b = buffer[pos]
    result = bitOr(as.numeric(result),
                   bitShiftL(
                     bitAnd(as.numeric(b), as.numeric(0x7f)),
                     shift))
    pos = pos + 1
    if(!bitAnd(b,0x80)){
      result = bitAnd(result,mask)
      result = as.integer(result)
      return(list(result, pos))
    }
    shift = shift + 7
    if(shift >= 64){
      stop("Too  many bytes when decoding varint")
    }

  }
}
