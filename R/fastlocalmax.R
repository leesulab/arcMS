chrom = tic
minHighestPoint = 20
smoothing = T
minimumConsecutiveScans = 8
fastFeatures <- function(peaksi, mzTolerance, minimumConsecutiveScans, minHighestPoint) {
  function(chrom, chromThreshold, hws = 4L, minDataPoints, xRange, searchXWidth, minRatio, minAbsoluteHeight, minRelativeHeight, smoothing = TRUE) {
    chrom = as.data.table(chrom)
    if (smoothing == T & nrow(chrom) > 4) {
      chrom$intsmoothed = MsCoreUtils::smooth(chrom$int, MsCoreUtils::coefSG(hws = 2, k = 3L)) 
      chromsm = copy(chrom)
      chromsm = chromsm[, int := intsmoothed]
      chromsm = chromsm[, int:=ifelse(int<0, 0, int)]
      chrom = chromsm
    }  
  library(dbscan)
  chrom = chrom |> filter(int > minHighestPoint)
  chrom$groups <- dbscan(as.matrix(chrom$rt), eps=0.9, minPts=minimumConsecutiveScans)$cluster
  length(unique(data$groups))
  data2 = data[ , maxInt:=max(intensity), by=groups]
  data2 = data2[intensity == maxInt,]
  return(list(data = data, flist = data2))
}