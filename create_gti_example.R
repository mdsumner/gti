library(terra)

s <- sds::nsidc_seaice()
n <- sds::nsidc_seaice(hemisphere = "north")

## create a polygon footprint from a 
footprint <- function(x, pps = 10) {
  ex <- as.vector(terra::ext(x))
  p <- terra::rast(x[[1L]])
  dim(p) <- c(1L, 1L, 1L)
  crs0 <- terra::crs(x)  ## save it so we can drop it so we aren't subject to units
  crs(p) <- "+proj=laea"  ## dummy, we can't densify without it being set ...
  ## divide in pps
  out <- terra::as.polygons(p)
    if (pps > 2) {
    subdivide <- min(diff(ex)[c(1, 3)] / pps)
    out <- terra::densify(out, subdivide)
  }
  crs(out) <- crs0
  out
}


plot(rbind(project(footprint(rast(s)), "+proj=tmerc"), project(footprint(rast(n), 20), "+proj=tmerc +lon_0=147")))

gti <- function(x, gridspec = list(crs = "+proj=tmerc +lon_0=147"),  pps = 10) {
  out <- do.call(rbind, lapply(x, function(.x) project(footprint(terra::rast(.x), pps = pps), gridspec$crs)))
  out$location <- x
  out
} 

v <- gti(c(s, n))

writeVector(v, tf <- tempfile(fileext = ".fgb"), filetype = "FlatGeoBuf")

tf2 <- file.path(tempdir(), "tmerc_seaice.fgb")
system(sprintf("ogr2ogr %s %s -mo RESX=25000 -mo RESY=25000", tf2, tf))

#dir.create("inst/extdata", recursive  = T)
file.copy(tf2, "inst/extdata/")
r <- rast(sprintf("GTI:%s", "inst/extdata/tmerc_seaice.fgb"))
r
plot(r)



