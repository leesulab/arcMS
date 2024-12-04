# mzmine parameters doc
#
#public static final IntegerParameter minimumConsecutiveScans = new IntegerParameter(
#   "Minimum consecutive scans", """
#       This number of scans needs to be above the specified 'Minimum intensity for consecutive scans' to detect EICs.
#       The optimal value depends on the chromatography system setup. The best way to set this parameter
#       is by studying the raw data and determining what is the typical time span (number of data points) of chromatographic features.""",
#   5, true, 1, null);
# 
# public static final DoubleParameter minGroupIntensity = new DoubleParameter(
#   "Minimum intensity for consecutive scans", """
#       This threshold is only used to find consecutive scans (data points) above a certain intensity.
#       All data points, even below this level can be added to a chromatogram but at least N consecutive scans need to be above.
#       """, MZmineCore.getConfiguration().getIntensityFormat(), 0d);
# 
# public static final MZToleranceParameter mzTolerance = new MZToleranceParameter(
#   ToleranceType.SCAN_TO_SCAN, 0.002, 10);
# 
# public static final StringParameter suffix = new StringParameter("Suffix",
#                                                                  "This string is added to filename as suffix", "chromatograms");
# 
# 
# public static final DoubleParameter minHighestPoint = new DoubleParameter(
#   "Minimum absolute height",
#   "Points below this intensity will not be considered in starting a new chromatogram",
#   MZmineCore.getConfiguration().getIntensityFormat());
# 
# public static final HiddenParameter<Map<String, Boolean>> allowSingleScans = new HiddenParameter<>(
#   new OptOutParameter("Allow single scan chromatograms",
#                       "Allows selection of single scans as chromatograms. This is useful for "
#                       + "feature table generation if MALDI point measurements."));
# 

data = pharmapeaks
mzTolerance = 0.01
minHighestPoint = 10
data = detectedmzs
data = det4


mzTolerance = 1
minimumConsecutiveScans = 1
ADAPChromBuilderFast <- function(data, mzTolerance, minimumConsecutiveScans, minHighestPoint) {
  
library(dbscan)
  data = data |> filter(intensity > minHighestPoint)
data$groups <- dbscan(as.matrix(data$exactMass), eps=mzTolerance, minPts=minimumConsecutiveScans)$cluster
length(unique(data$groups))
data2 = data[ , maxInt:=max(intensity), by=groups]
data2 = data2[intensity == maxInt,]
return(list(data = data, flist = data2))
}
# construire les EIC à partir des données brutes ou seulement de la liste des masses détectées ?
# mzmine : This module connects data points from mass lists and builds chromatograms.

ADAPChromBuilder <- function(data, mzTolerance, minimumConsecutiveScans, minGroupIntensity, minHighestPoint) {

  # make a list of all the data points
  # sort data points by intensity
  # loop through list
  # add data point to chromatogrm or make new one
  # update mz avg and other stuff
    
  # map the mz tolerance to chromatograms
  #RangeMap<Double, ADAPChromatogram> rangeToChromMap = TreeRangeMap.create();
  
  # make a list of all the data points
  #allMzValues = s.getMassList();
  # sort data points by intensity

data =  data |> arrange(desc(intensity))
data = data |> filter(intensity > minHighestPoint)


chroms = list()
mzs = vector()
rts = vector()

for (i in seq_along(data$rt)) {
  mzi = data$exactMass[i]
  if(length(mzs) == 0) {mzs[[i]] = mzi}
  if(length(mzs) == 1 || !(any(abs(mzi - mzs) <= mzTolerance, na.rm = T))) {
    lowmz = mzi - mzTolerance
    highmz = mzi + mzTolerance
    EIC = data |>
      arrange(rt) |>
      filter(exactMass > lowmz & exactMass < highmz)
    chroms[[i]] = EIC
    names(chroms)[i] = paste0("EIC", round(mzi,4))
    mzs[[i]] = mzi
    rts[[i]] = data$rt[i]
    print(paste(i, "/", length(data$rt)))
  }
}
mzs = mzs[-1]
rts = rts[-1]
chroms = chroms[-1]
mzs = mzs[!is.na(mzs)]
rts = rts[!is.na(rts)]
chroms = Filter(Negate(is.null), chroms)
df = data.frame(rt = rts, mz =mzs)
chroms[["allpoints"]] = df 
return(chroms)
}



length(rts)

