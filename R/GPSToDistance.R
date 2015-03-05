#latitude is the parallels
##longitude is the east/westness
##This is not currently used directly, but may be used to either
##convert GPS for local areas, or to adapt a new cost function.
##
## See the following URLs for reference:
## http://fmepedia.safe.com/articles/How_To/Calculating-accurate-length-in-meters-for-lat-long-coordinate-systems
## http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.HTM
## http://www-pord.ucsd.edu/~matlab/coord.htm

GPSToDistance <- function(lats,lons)
  {

    ##find mean lat/lon te estimate constant-length.
    mlat.deg <- (mean(lats))
    mlon.deg <- (mean(lons))

    mlat <- degtorad(mlat.deg)
    mlon <- degtorad(mlon.deg)
    
    ##set up estimation constants
    m1 <- 111132.92             # latitude calculation term 1
    m2 <- -559.82;		# latitude calculation term 2
    m3 <- 1.175;		# latitude calculation term 3
    m4 <- -0.0023;		# latitude calculation term 4
    
    p1 <- 111412.84;		# longitude calculation term 1
    p2 <- -93.5;		# longitude calculation term 2
    p3 <- 0.118;		# longitude calculation term 3


    
 ## Calculate the length of a degree of latitude and longitude in meters

    latlen  <- m1 +  (m2 * cos(2 * mlat)) +  (m3 * cos(4 * mlat)) +   (m4 * cos(6 * mlat))
    lonlen <- (p1 * cos(mlat)) +  (p2 * cos(3 * mlat)) + (p3 * cos(5 * mlat))

   
    ##Convert the gps to meters from the center of the path.
    latinmeters <- (lats -mlat.deg)*latlen
    loninmeters <- (lons -mlon.deg)*lonlen

   data.frame(lat=latinmeters,lon=loninmeters)
}
