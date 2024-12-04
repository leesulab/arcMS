chrom = data.table(dt = cr2$dt, int = cr2$intsm)
chrom = tics
chrom = chromrttic
chrom = tic
chrom = cr2
chrom = imstrace
chrom = napro2
chrom=g209
chromThreshold = 0.50 #50%
minAbsoluteHeight = 150
minRelativeHeight = 0.05
minDataPoints = 4
minRatio = 2
xRange = c(0.1, 2)
searchXWidth = 0.05
# xRange = peak duration range (min/mobility) = range of acceptable peak length (in mins or mobility unit) - to filter noisy features (too short or long duration) (PEAK_DURATION)
# searchXWidth = minimum search range (RT/mobility, absolute) = size of RT or mobility window examined (SEARCH_RT_RANGE)
# min ratio : min ratio of peak top/edge 

# pour aller vite, deux étapes : 1) fast locmax : si un seul pic, on ne fait rien, si plusieurs pics alors on scinde la feature en plusieurs 
# puis pour chaque pic il faut checker la forme du pic

MinimumSearchFeatureResolver <- function(chrom, chromThreshold, hws = 4L, minDataPoints, xRange, searchXWidth, minRatio, minAbsoluteHeight, minRelativeHeight, smoothing = TRUE) {
  chrom = as.data.table(chrom)
  if (smoothing == T & nrow(chrom) > 4) {
    chrom$intsmoothed = MsCoreUtils::smooth(chrom$int, MsCoreUtils::coefSG(hws = 2, k = 3L)) 
    chromsm = copy(chrom)
    chromsm = chromsm[, int := intsmoothed]
    chromsm = chromsm[, int:=ifelse(int<0, 0, int)]
    chrom = chromsm
  }
  
  #
  #   @param x domain values of the data to be resolved
  # @param y range values of the data to be resolved. Values have to be <b>strictly monotonically
  #          increasing</b> (e.g. RT or mobility). The values inside this array are set to 0 if
  #          they fall below the chromatographicThresholdLevel.
  # @return List of x values for each resolved peak
  #
  # valueCount = nrow(chrom[,1])
  # lastScan = valueCount - 1
  
  # First, remove all data points below chromatographic threshold.
  # threshold is the Percentage of data points in the EIC removed before local minima search.
  chromatographicThresholdLevel = quantile(chrom$int, chromThreshold)
  maxY = 0
  
  chrom = chrom[,int := ifelse(int < chromatographicThresholdLevel, 0, int)]
  maxInt = max(chrom$int)
  
  minHeight = max(minAbsoluteHeight, minRelativeHeight * maxInt)

  # local maxima detection
  chrom$locmax = MsCoreUtils::localMaxima(chrom$int, hws = hws)
  chrom$locmin = MsCoreUtils::localMaxima(1/chrom$int, hws = hws - 2)
  
  chrom = chrom[, nextLocmin := data.table::shift(locmin, type="lead", fill = 0)]
  
  chrom = chrom[, nextIntensity := data.table::shift(int, type="lead", fill = 0)]
  chrom = chrom[, prevIntensity := data.table::shift(int, type="lag", fill = 0)]
  chrom = chrom[, nextIsZero := nextIntensity == 0]
  chrom = chrom[, currentIsZero := int == 0]
  chrom = chrom[, prevIsZero := prevIntensity == 0]
  
  chrom = chrom[, nextLocmin2 := (nextLocmin | nextIsZero) & !currentIsZero]
  
  chrom = chrom[, rightpart := cumsum(locmax), by = rleid(locmin == 0L)]
  chrom = chrom[, rightpart := ifelse(prevIsZero & nextIsZero & !currentIsZero, 1, rightpart)]
  # get last right point of peak
  chrom = chrom[, rightpoint := ifelse(
    (rightpart == 1 | rightpart == 2) &
      nextLocmin2 == T, 1, 0)]
  
# faire idem sur partie left
# puis length = id rightpoint 
  firstcol = names(chrom[,1])
  
  # with all EICs in same chrom
  #chrom = chrom |> group_by(.id) |> arrange(desc(chrom[[firstcol]]), .by_group = T) 
  #chrom = as.data.table(chrom)
  
  # with one EIC
  chrom = chrom[order(-chrom[[firstcol]]),]
  
  #chrom3 = chrom[order(-dt)]
  #leftpart includes locmax, not locmin


  chrom = chrom[, nextLocmin3 := data.table::shift(locmin, type="lead", fill = 0)]
  chrom = chrom[, nextLocmin4 := (nextLocmin3 | prevIsZero) & !currentIsZero]
  
  chrom = chrom[, leftpart := cumsum(locmax), by = rleid(locmin == 0L)]
  chrom = chrom[, leftpart := ifelse(prevIsZero & nextIsZero & !currentIsZero, 1, leftpart)]
  # get last left point of peak
  chrom = chrom[, leftpoint := ifelse(
    (leftpart == 1 | leftpart == 2) &
      nextLocmin4 == T, 1, 0)]
  
  # with all EICs in same chrom
  #chrom = chrom |> group_by(.id) |> arrange(chrom[[firstcol]], .by_group = T) 
  #chrom = as.data.table(chrom)
  
  # with one EIC
  chrom = chrom[order(chrom[[firstcol]])]
 
  # if (sum(chrom[,leftpoint]) != sum(chrom[,rightpoint])) {
  #   # get missing left points for each right point
  #   mins = chrom[ , .SD[which.min(int)], by = rleid(rightpart)]
  #   chrom = chrom[, minsright := min(int), by = rightpart]
  #   
  #   chrom = chrom[, leftpoint2 := ifelse(rightpart == 1 & int == min(int), 1, 0), by = rleid(rightpart)]
  # } 
  
  idsleft = as.numeric(unlist(chrom[leftpoint == 1, 1]))
  idsright = as.numeric(unlist(chrom[rightpoint == 1, 1]))
  
  # Checks if the current start and end points of each peak are non-zero but 
  # have zero values next to
  # them. If yes, these zeros will be included in the region of the peak.
  # adjust start and end points of signals to produce better peak shapes
  # (e.g. include 0 intensity points on the edges.), as start and end points of this
  # resolver will never be 0.
  for (i in seq_along(idsleft)) {
    rowindex = which(chrom[[firstcol]] == idsleft[i])
    
  if (chrom[rowindex, int] != 0 & chrom[max(rowindex-1,1), int] == 0) {
    idsleft[i] = chrom[[firstcol]][rowindex-1]
  }
  }
  for (i in seq_along(idsright)) {
    rowindex = which(chrom[[firstcol]] == idsright[i])
    
    if (chrom[rowindex, int] != 0 & chrom[min(rowindex+1,nrow(chrom)), int] == 0) {
      idsright[i] = chrom[[firstcol]][rowindex+1]
    }
  }
  


  peaks = list()

    for (i in seq_along(idsleft)) {
      if(!is.na(idsright[i]) & idsleft[i] <= idsright[i]) {
      peak = chrom[chrom[[firstcol]] >= idsleft[i] & chrom[[firstcol]] <= idsright[i],]
      
      # Check the shape of the peak.
      if (nrow(peak) >= minDataPoints &
          max(peak[,int]) >= minHeight &
          max(peak[,int]) >= peak[1, int] * minRatio &
          max(peak[,int]) >= peak[nrow(peak), int] * minRatio &
          peak[nrow(peak), 1] - peak[1, 1] > xRange[1] &
          peak[nrow(peak), 1] - peak[1, 1] < xRange[2]
          ) {
       
        peaks[[i]] = peak
      }
    
    }
  }
  # remove NULL peaks
  peaks = Filter(Negate(is.null), peaks)
  
  return(peaks)

} 

  
