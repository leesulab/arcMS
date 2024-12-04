


IMStrace1D = spectrum |>
  group_by(bin) |>
  summarise(sum_int = sum(intensity))

IMStrace1D = IMStrace1D[order(IMStrace1D$bin),]

plotly::plot_ly(IMStrace1D,
                      y=~sum_int,
                      x=~bin,
                      type = 'scatter',
                      mode = 'lines',
                      line = list(color = 'rgba(0,0,0,1)', width = 1),
                      line = list(shape = 'spline', smoothing = 1)
  ) %>% layout(
    xaxis=list(title= "Retention Time (min)"
    ),
    yaxis = list(title="Intensity"
    )) %>% hide_legend()



massdetection = function(spectrum, noiseLevel) {
  mzpeaks = data.frame()
  for (i in seq_along(unique(spectrum$bin))) {
    imscan = spectrum |> filter(bin == i)
    mzpeaksi = exactmassdetector(imscan, noiseLevel)
    if (nrow(mzpeaksi) != 0) {
      mzpeaksi$rt = imscan$rt[1]
      mzpeaksi$scanid = imscan$scanid[1]
      mzpeaksi$bin = imscan$bin[1]
      mzpeaksi$dt = imscan$dt[1]
      mzpeaks = rbind(mzpeaks, mzpeaksi)
    }
  }
  return(mzpeaks)
}


fullmassdetection = function(data, noiseLevel) {
  scans = data |>
    filter(mslevel == "1") |>
    group_by(scanid) |>
    summarise(scanid) |>
    collect()
  
  mzpeaks = data.frame()
  for (i in seq_along(unique(scans$scanid))) {
  spectrum = raw_data |> 
    to_duckdb() |> 
    filter(mslevel == "1") |>
    filter(scanid == i)  |>
    collect()
  mzpeaksi = massdetection(spectrum, noiseLevel)
  mzpeaks = rbind(mzpeaks, mzpeaksi)
  }
  return(mzpeaks)
}

detectedmzs = fullmassdetection(raw_data, 20)
detectedmzspec = massdetection(spectrum, 20)

imscansel2 = detectedmzs |> 
  filter(round(rt, 5) == 10.59274)  |>
  filter(bin == 68)

p %>% add_trace(x = imscansel2$mz, y = imscansel2$intensity, type = "scatter")
p %>% add_trace(x = imscan_sm$mz, y = imscan_sm$intensity, mode = "lines")

tic = detectedmzs |>
      group_by(rt) |>
      summarise(sum_int = sum(intensity))

tic2 = raw_data |>
  to_duckdb() |>
  filter(mslevel == "1") |>
  group_by(rt, scanid) |>
  summarise(sum_int = sum(intensity)) |>
  arrange(rt) |>
  collect()
tic2 = as.data.table(tic2)

plot_ly(data = tic, x=~rt, y=~sum_int, type="scatter", mode="lines") %>% add_trace(tic2, x=~rt+0.1, y=~sum_int, mode="lines", col="orange")

plot_ly(data = tic2, x=~rt, y=~sum_int, mode="lines")




exactmassdetector <- function(imscan, noiseLevel) {
  #First get all candidate peaks (local maximum)
  datapoints = vector()
  ascending = TRUE
  mzs = vector()
  intensities = vector()
  # Iterate through all data points
  for (i in seq_along(imscan$mz)) {
    intensity = imscan$intensity[i]
    nextIntensity = imscan$intensity[i + 1]
    
    nextIsBigger = nextIntensity > intensity
    nextIsZero = nextIntensity == 0
    currentIsZero = intensity == 0
    
    # Ignore zero intensity regions
    if (!currentIsZero) {
      # Add current (non-zero) data point to the current m/z peak
      datapoints = append(datapoints, i)
      
    # Check for local maximum
    if (ascending & !nextIsBigger) {
      localMaximumIndex = i
      ascending = FALSE
    }
  
    # Check for the end of the peak
    if (!ascending & nextIsBigger || nextIsZero) {
      
      # Calculate the exact mass
      #print(paste("maxindex:", localMaximumIndex, "dp:", datapoints))
       if (length(datapoints > 1)) {
         exactMz = calculateExactMass(imscan, localMaximumIndex, datapoints)
       } else {
        exactMz = imscan$mz[localMaximumIndex]
       }
      # Add the m/z peak if it is above the noise level or m/z value corresponds to isotope mass
      if (imscan$intensity[localMaximumIndex] > noiseLevel) {
        # Add data point to lists
        mzs = append(mzs, exactMz)
        intensities = append(intensities, imscan$intensity[localMaximumIndex])
      }
      
      # Reset and start with new peak
      ascending = TRUE
      datapoints = vector()
    }
  }
  }
  # Return a dataframe of detected MzPeaks sorted by MZ
  df = data.frame(mz = mzs, intensity = intensities)
  return(df)
}
  
  #
  # This method calculates the exact mass of a peak using the FWHM concept and 
  # linear equation (y = mx + b).
  #


topIndex = localMaximumIndex
topIndex = 151
datapoints = c(147,148,149,150,151,152,153,154)
calculateExactMass(imscan, topIndex, datapoints)

  calculateExactMass = function(imscan, topIndex, datapoints) {
    
    # According with the FWHM concept, the exact mass of this peak is the half point of FWHM. In
    # order to get the points in the curve that define the FWHM, we use the linear equation.
    #
    # First we look for, in left side of the peak, 2 data points together that have an intensity
    # less (first data point) and bigger (second data point) than half of total intensity. Then we
    # calculate the slope of the line defined by this two data points. At least, we calculate the
    # point in this line that has an intensity equal to the half of total intensity
    #
    # We repeat the same process in the right side
      xRight = -1
      xLeft = -1
      halfIntensity = imscan$intensity[topIndex] / 2
      
      for (i in seq_along(datapoints)) {
        
        # Left side of the curve
        if (!is.na(imscan$intensity[datapoints[i+1]]) &
          imscan$intensity[datapoints[i]] <= halfIntensity &
          imscan$mz[datapoints[i]] < imscan$mz[topIndex] & 
          imscan$intensity[datapoints[i+1]] >= halfIntensity) {
          # First point with intensity just less than half of total intensity
          leftY1 = imscan$intensity[datapoints[i]]
          leftX1 = imscan$mz[datapoints[i]]
          
          # Second point with intensity just bigger than half of total intensity
          leftY2 = imscan$intensity[datapoints[i+1]]
          leftX2 = imscan$mz[datapoints[i+1]]
          
          # We calculate the slope with formula m = Y1 - Y2 / X1 - X2
          mLeft = (leftY1 - leftY2) / (leftX1 - leftX2)
          
          if (mLeft == 0.0) {
            # If slope is zero, we calculate the desired point as the middle point
            xLeft = (leftX1 + leftX2) / 2
          } else {
            # We calculate the desired point (at half intensity) with the linear equation
            # X = X1 + [(Y - Y1) / m ]
            # where Y = half of total intensity
            xLeft = leftX1 + (((halfIntensity) - leftY1) / mLeft)
          }
          
        }
        
        # Right side of the curve
        
        if (!is.na(imscan$intensity[datapoints[i+1]]) &
            imscan$intensity[datapoints[i]] >= halfIntensity &
            imscan$mz[datapoints[i]] > imscan$mz[topIndex] & 
            imscan$intensity[datapoints[i+1]] <= halfIntensity) {
     
          # First point with intensity just bigger than half of total intensity
          rightY1 = imscan$intensity[datapoints[i]]
          rightX1 = imscan$mz[datapoints[i]]
          
          # Second point with intensity just less than half of total intensity
          rightY2 = imscan$intensity[datapoints[i+1]]
          rightX2 = imscan$mz[datapoints[i+1]]
          
          # We calculate the slope with formula m = Y1 - Y2 / X1 - X2
          mRight = (rightY1 - rightY2) / (rightX1 - rightX2)
          
          if (mRight == 0.0) {
            # If slope is zero, we calculate the desired point as the middle point
            xRight = (rightX1 + rightX2) / 2
          } else {
            # We calculate the desired point (at half intensity) with the linear equation
            # X = X1 + [(Y - Y1) / m ], where Y = half of total
            # intensity
            xRight = rightX1 + (((halfIntensity) - rightY1) / mRight)
            print(paste("xRight:", xRight))
          }
          break
        }
        
      }
      
      # We verify the values to confirm we find the desired points. If not we
      # return the same mass value.
      if ((xRight == -1) || (xLeft == -1)) {
        return(imscan$mz[topIndex])
      }
      
      # The center of left and right points is the exact mass of our peak.
      exactMass = (xLeft + xRight) / 2
      print(paste("exactmass: ", exactMass))
      
      return(exactMass)
  }
  
  
  
  library(tictoc)
  tic()
  det = exactmassdetector2(data, 20)
  toc()
    # 4.5 min avec NoiseLevel = 200
  det2 = exactmassdetector(dataf, 20)
  
  p %>% add_trace(det2, ~mz, ~intensity, type = "scatter")
  
  exactmassdetector2 <- function(data, noiseLevel) {
    #First get all candidate peaks (local maximum)
    datapoints = vector()
    ascending = TRUE
    mzs = vector()
    intensities = vector()
    # Iterate through all data points
    for (i in seq_along(data$rt)) {
      intensity = data$intensity[i]
      nextIntensity = data$intensity[i + 1]
      
      nextIsBigger = nextIntensity > intensity
      nextIsZero = nextIntensity == 0
      currentIsZero = intensity == 0
      
      # Ignore zero intensity regions
      if (!currentIsZero) {
        # Add current (non-zero) data point to the current m/z peak
        datapoints = append(datapoints, i)
        
        # Check for local maximum
        if (ascending & !nextIsBigger) {
          localMaximumIndex = i
          ascending = FALSE
        }
        
        # Check for the end of the peak
        if (!ascending & nextIsBigger || nextIsZero) {
          
          # Calculate the exact mass
          #print(paste("maxindex:", localMaximumIndex, "dp:", datapoints))
          if (length(datapoints > 1)) {
            exactMz = calculateExactMass(data, localMaximumIndex, datapoints)
          } else {
            exactMz = data$mz[localMaximumIndex]
          }
          # Add the m/z peak if it is above the noise level or m/z value corresponds to isotope mass
          if (data$intensity[localMaximumIndex] > noiseLevel) {
            # Add data point to lists
            mzs = append(mzs, exactMz)
            intensities = append(intensities, data$intensity[localMaximumIndex])
          }
          
          # Reset and start with new peak
          ascending = TRUE
          datapoints = vector()
        }
      }
    }
    # Return a dataframe of detected MzPeaks sorted by MZ
    df = data.frame(mz = mzs, intensity = intensities)
    return(df)
  }
  
  
  noiseLevel = 20
  data = dataMS1
  
  length(data$rt) / 4
  2811267 * 4
  data_p1 = data[1:2811267,]
  data_p2 = data[2811268:5622534, ]
  data_p3 = data[5622535:8433801, ]
  data_p4 = data[8433801:11245069, ]
  det3 = exactmassdetector3(data_p1, 20)
  det4 = exactmassdetector3(data_p2, 20)
  det5 = exactmassdetector3(data_p3, 20)
  det6 = exactmassdetector3(data_p4, 20)
  
 
data = det4
  data = spectrum
  exactmassdetector3 <- function(data, noiseLevel, smoothing = T) {
    
    if (smoothing == T) {
      data$intsmoothed = MsCoreUtils::smooth(data$intensity, MsCoreUtils::coefSG(2, k = 3L)) 
      datasm = copy(data)
      datasm = datasm[, intensity := intsmoothed]
      datasm = datasm[, intensity:=ifelse(intensity<0, 0, intensity)]
      data = datasm
    }

    data = data[order(rt, bin, mz)]
    # cumsum steps are the longest

    data2 = data[, nextIntensity := data.table::shift(intensity, type="lead", fill = 0)]
    data2 = data2[, prevIntensity := data.table::shift(intensity, type="lag", fill = 0)]
    data2 = data2[, nextMz := data.table::shift(mz, type="lead", fill = 0)]
    #tail(data2)
    data2 = data2[, nextIsBigger := nextIntensity > intensity]
    data2 = data2[, prevIsBigger := prevIntensity >= intensity]
    #head(data2)
    data2 = data2[, nextIsZero := nextIntensity == 0]
    data2 = data2[, currentIsZero := intensity == 0]
    data2 = data2[, prevIsZero := prevIntensity == 0]
    
    data2 = data2[, descending := !currentIsZero & !nextIsBigger]
    #setnames(data2, "locmax", "descending")
    data2 = data2[, locmax := descending & !prevIsBigger]

    
  #  dataf = data2 |> filter(round(rt,1) == 10.6) |> filter(round(mz,1) == 748.5) |> filter(bin > 67 & bin < 69)
   # dataf =  data2 |> 
    #  filter(mslevel == "1") |>
    #  filter(round(rt, 5) == 10.59274)  |>
    #  filter(bin ==68)

    
    data2 = data2[, cleft := ifelse(!currentIsZero & nextIsBigger, 1, 0)]
    data2 = data2[, cright := ifelse(!currentIsZero & !nextIsBigger, 1, 0)]
    data2 = data2[, cpeak := cleft + cright]

    data2 = data2[, halfIntensity := ifelse(locmax, intensity / 2, 0)]
    data2 = data2[, halfIntensity2 := cumsum(halfIntensity), by = rleid(cright == 0L)]

    data2 = data2[, cleftcum := cumsum(cleft), by = rleid(halfIntensity != 0L)]
    data2 = data2[, crightcum := cumsum(cright), by = rleid(cright == 0L)]
    
    data2 = data2[, topmz := ifelse(locmax, mz, 0)]
    data2 = data2[, topmz2 := cumsum(topmz), by = rleid(cright == 0L)]
    
    
    # check last right point of peak - (premiere condition sert pas à grand chose, et avec la derniere on checke juste quand on atteint zero) 
    # attention ici on obtient le dernier point du pic, pas le rightpoint, vérifier les calculs
    data2 = data2[, rightpoint := ifelse(
      intensity < prevIntensity & # avoid double rightpoint in specific cases (same intensity than toppoint)
      intensity >= halfIntensity2 & # get to intensity = 0
      mz > topmz2 & # mz > 0 (ou changer topmz par topmz2?)
      nextIntensity <= halfIntensity2, 1, 0)]
      # shift this new column up
    #data2 = data2[, rightpoint2 := data.table::shift(rightpoint, type = "lead", fill = 0)]
    data2 = data2[, mRight := ifelse(rightpoint == 1, (intensity - nextIntensity) / (mz - nextMz), 0)]
    # rajouter colonne nextMz : on aura sur la même ligne mz du point i (1) et du point i+1 (2 - nextMz)
    # puis mRight = (intensity - nextIntensity) / (mz - nextMz)
    
    data2 = data2[, xright := ifelse(mRight == 0 & rightpoint == 1, (mz+nextMz) / 2, 0)]
    data2 = data2[, xright := ifelse(mRight != 0 & rightpoint == 1, (mz+((halfIntensity2 - intensity) / mRight)), 0)]

    
    # reverse order for left part of peaks
    data3 = data[order(-rt, -bin, -mz)]
    data3 = data3[, nextIntensity := data.table::shift(intensity, type="lead", fill = 0)]
    data3 = data3[, prevIntensity := data.table::shift(intensity, type="lag", fill = 0)]
    data3 = data3[, nextMz := data.table::shift(mz, type="lead", fill = 0)]
    data3 = data3[, nextIsBigger := nextIntensity > intensity]
    data3 = data3[, prevIsBigger := prevIntensity >= intensity]
    data3 = data3[, nextIsZero := nextIntensity == 0]
    data3 = data3[, currentIsZero := intensity == 0]
    data3 = data3[, prevIsZero := prevIntensity == 0]

    data3 = data3[, descending := !currentIsZero & !nextIsBigger]
    data3 = data3[, locmax := descending & !prevIsBigger]
    
   # datafr = data3 |> filter(mslevel == "1") |>
    #  filter(round(rt, 5) == 10.59274)  |>
     # filter(bin ==68)
    
    data3 = data3[, cleft := ifelse(!currentIsZero & nextIsBigger, 1, 0)]
    data3 = data3[, cright := ifelse(!currentIsZero & !nextIsBigger, 1, 0)]
    data3 = data3[, cpeak := cleft + cright]
    
    data3 = data3[, halfIntensity := ifelse(locmax, intensity / 2, 0)]
    data3 = data3[, halfIntensity2 := cumsum(halfIntensity), by = rleid(cright == 0L)]
    
    #data3 = data3[, cleftcum := cumsum(cleft), by = rleid(halfIntensity != 0L)]
    #data3 = data3[, crightcum := cumsum(cright), by = rleid(cright == 0L)]
    
    data3 = data3[, topmz := ifelse(locmax, mz, 0)]
    data3 = data3[, topmz2 := cumsum(topmz), by = rleid(cright == 0L)]
    
    # check left point of peak
    data3 = data3[, leftpoint := ifelse(intensity < prevIntensity &
                                          intensity >= halfIntensity2 &
                                          mz < topmz2 & 
                                          nextIntensity <= halfIntensity2, 1, 0)]
    data3 = data3[, mLeft := ifelse(leftpoint == 1, (nextIntensity - intensity) / (nextMz - mz), 0)]
    
    data3 = data3[, xleft := ifelse(mLeft == 0 & leftpoint == 1, (mz+nextMz) / 2, 0)]
    data3 = data3[, xleft := ifelse(mLeft != 0 & leftpoint == 1, (nextMz+((halfIntensity2 - nextIntensity) / mLeft)), 0)]
    
    data4 = data3[order(rt, bin, mz)]
    #data2$xleft = data4$xleft

    #copy xleft value down the whole peak values to get a common line with both xleft and xright values
    data4 = data4[, xleftcum := cumsum(xleft), by = rleid(topmz2 == 0L)]
    # reverse order to do the same with xright
    data4 = data4[order(-rt, -bin, -mz)]
    data4 = data4[, xrightcum := cumsum(xright), by = rleid(crightcum == 0L)]
    data4 = data4[order(rt, bin, mz)]
    # calculate mean of xleft and xright masses
    data4 = data4[, exactMass1 := ifelse(xleftcum != 0 & xrightcum != 0 & topmz != 0, (xleftcum + xrightcum) / 2, 0)]
    data4 = data4[, exactMass := ifelse(topmz != 0 & exactMass1 == 0, topmz, exactMass1)]  
    
    # keep only peaks above chosen noise level
    data4 = data4[exactMass > noiseLevel,]
    data4 = data4[intensity > noiseLevel,]
    
    # check for out of range masses (artifacts due to cumsum steps)
    data4 = data4[, exactMass := ifelse(exactMass > (mz + 1), mz, exactMass)]
    return(data4)

  }
  
