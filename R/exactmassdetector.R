

  exactmassdetector3 <- function(data, noiseLevel, smoothing = T) {
    
    if (smoothing == T) {
      data$intsmoothed = MsCoreUtils::smooth(data$intensity, MsCoreUtils::coefSG(2, k = 3L)) 
      datasm = copy(data)
      datasm[, intensity := intsmoothed]
      datasm[, intensity:=ifelse(intensity<0, 0, intensity)]
      data = datasm
    }

    data = data[order(rt, bin, mz)]
    # cumsum steps are the longest

    data2 = data[, nextIntensity := data.table::shift(intensity, type="lead", fill = 0)]
    data2[, prevIntensity := data.table::shift(intensity, type="lag", fill = 0)]
    data2[, nextMz := data.table::shift(mz, type="lead", fill = 0)]
    data2[, nextIsBigger := nextIntensity > intensity]
    data2[, prevIsBigger := prevIntensity >= intensity]
    data2[, nextIsZero := nextIntensity == 0]
    data2[, currentIsZero := intensity == 0]
    data2[, prevIsZero := prevIntensity == 0]
    
    data2[, descending := !currentIsZero & !nextIsBigger]
    data2[, locmax := descending & !prevIsBigger]

    data2[, cleft := ifelse(!currentIsZero & nextIsBigger, 1, 0)]
    data2[, cright := ifelse(!currentIsZero & !nextIsBigger, 1, 0)]
    data2[, cpeak := cleft + cright]

    data2[, halfIntensity := ifelse(locmax, intensity / 2, 0)]
    data2[, halfIntensity2 := cumsum(halfIntensity), by = rleid(cright == 0L)]

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
  
