library(mzR)
filename = "/media/julien/Data/SynologyDrive/example-data/E-2022_01-Clichy-D_1_A,2_1.mzML"


mzml_to_dt <- function(filename, chunksize = 10000) {
  aa <- mzR::openMSfile(filename, backend = "pwiz")
  scanCount = length(aa)
  chk = 1
  chunks = list()
  nchunks = ceiling(scanCount / chunksize)
  for (i in 1:nchunks) {
    chunks[[i]] = chk
    chk = chk + chunksize
  }
  
  retrieveHeaders <- function(chunks, chunksize, p) {
    start = chunks
    end = chunks + chunksize - 1
    
    if(end>scanCount) {
      end = scanCount
    }
    res = header(aa, start:end)
    p()
    return(res)
  }

  respheaders = function(chunks) {
    p <- progressor(along = chunks)
    response = lapply(chunks, retrieveHeaders, chunksize = chunksize, p = p)
    return(response)
  }
  responseheaders = with_progress(respheaders(chunks))
  
  hds = rbindlist(responseheaders)
  hds.expanded <- hds[ ,list(freq=rep(1,peaksCount)),by=c("seqNum", "msLevel", "retentionTime","ionMobilityDriftTime")]
  hds.expanded[ ,freq := NULL]
  
  retrievePeaks <- function(chunks, chunksize, p) {
    start = chunks
    end = chunks + chunksize - 1
    
    if(end>scanCount) {
      end = scanCount
    }
    res = peaks(aa, start:end)
    p()
    return(res)
  }
  resppeaks = function(chunks) {
    p <- progressor(along = chunks)
    response = lapply(chunks, retrievePeaks, chunksize = chunksize, p = p)
    return(response)
  }
  responsepeaks = with_progress(resppeaks(chunks))
  
  dt = unlist(responsepeaks, recursive = F)
  dt2 = do.call(rbind, dt)
  rm(dt)
  dt = as.data.table(dt2)
  dt = cbind(dt, hds.expanded)
  return(dt)
}
