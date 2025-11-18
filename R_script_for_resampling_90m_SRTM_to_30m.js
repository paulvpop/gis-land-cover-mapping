//The ~90 m resolution SRTM imagery can be interpolated/resampled to 30 m resolution, and downloaded to Google Drive by running the following GEE script:

// Load the void-filled 90 m resolution SRTM DEM layer downloaded from the https://srtm.csi.cgiar.org/srtmdata/
var srtm1 = ee.Image("projects/ee-paul_pop/assets/DEM/srtm_55_07");
var srtm2 = ee.Image("projects/ee-paul_pop/assets/DEM/srtm_56_07");

print('CRS:', srtm1.projection().crs());
print('CRS:', srtm2.projection().crs());

// Resample the two srtm imageries
var reproj_srtm1 = srtm1.resample('bicubic').reproject({
  crs: "EPSG:4326",
  scale: 30,
});

var reproj_srtm2 = srtm2.resample('bicubic').reproject({
  crs: "EPSG:4326",
  scale: 30,
});

/*// Another way to change crs:
var reprojected = srtm[x].resample('bicubic').reproject({
  crs: elevation.projection().crs(),
  scale: 30,
});*/

// Display the reprojected and resampled SRTM data
Map.addLayer(reproj_srtm1, {min: 100, max: 5020}, 'SRTM1 Reprojected and Resampled');
Map.centerObject(reproj_srtm1, 8);

Map.addLayer(reproj_srtm2, {min: 100, max: 5020}, 'SRTM2 Reprojected and Resampled');
Map.centerObject(reproj_srtm2, 8);

//Combine them both
var merged = ee.ImageCollection.fromImages([reproj_srtm1, reproj_srtm2]).mosaic()

print('CRS:', merged.projection().crs());

//Load the region of interest (the exact boundary of the Siang study area)
var roi = ee.FeatureCollection("projects/ee-paul_pop/assets/siang");

//Clip the DEM to region of interest (don't use "select('b1')" i.e. var elevation = merged.select('b1').clip(roi);
//since the image only contains one band and it may mess up the output)

var elevation = merged.clip(roi);

// Get the projection information for the clipped and merged 90 m void-filled SRTM DEM
print('CRS:', elevation.projection().crs());
//It should show "CRS: <break> EPSG:4326" in JSON which is the coordinate system we desire 

/*IMPORTANT: Order of arguments is very important in GEE as my attempt at first merging the void-filled 90 m
DEM and clipping them according to the Siang study area resulted in this error (when saving the final resampled 
reprojected output): "Error: Projection error: Unable to compute intersection of geometries in projections EPSG:4326:
affine [1.0, 0.0, 0.0, 0.0, 1.0, 0.0] and EPSG:4326: affine [1.0, 0.0, 0.0, 0.0, 1.0, 0.0] (abstract projection). 
(Error code: 3)".   This error couldn't be resolved despite various methods (including selecting a rectangular study
area with ony four vertices). So, always the resampling and reprojection of individual imagery needs to
be done before merging them and clipping them to the study region.*/

// Export the individual interpolated images (if needed)
Export.image.toDrive({
  image: reproj_srtm1,
  description: 'spline_interpolated_srtm1',
  crs: "EPSG:4326",
  scale: 30,
  maxPixels: 1e13  //Since it's a large area and contains a lot of pixels
});

Export.image.toDrive({
  image: reproj_srtm2,
  description: 'spline_interpolated_srtm2',
  crs: "EPSG:4326",
  scale: 30,
  maxPixels: 1e13  //Since it's a large area and contains a lot of pixels
});

/*Export the interpolated merged and clipped image.
Note that without the "region: roi" argument, it will download a imagery with 0 data, but with it, it will 
result in no-data values outside the exact study area, but within the rectangle bounding the roi
being shown as 0 (not important since it will be clipped for thetopographic correction
step. However, it can be rectified by specifying no data value as below*/

//Define no data value, then unmask the merged and clipped image and supply the value to it:
var noDataVal = -9999;
var unmaskedImage = elevation.unmask({value: noDataVal, sameFootprint: false});

Export.image.toDrive({
  image: unmaskedImage,
  region: roi,
  description: 'spline_interpolated_srtm_new',
  crs: "EPSG:4326",
  scale: 30,
  maxPixels: 1e13,  //Since it's a large area and contains a lot of pixels
  fileFormat: 'GeoTIFF',
  formatOptions: {
    noData: noDataVal
  }
});

//Visualise the elevation (note that this changes the projection to maps mercator so that it is visible in GEE map.
//So, do it only after saving the image)
var palettes = require('users/gena/packages:palettes');
var visParam = {
  crs: 'EPSG:4326',
  min: 100,
  max: 5020,
  palette: palettes.kovesi.linear_kry_5_98_c75[7]
};
Map.addLayer(elevation, visParam, 'Elevation');

/*Saving srtm1 to drive took nearly 8 minutes (started 2025-03-24 17:16:49 +0530; 561.2390 EECU-seconds)
and saving srtm2 took 9 min (started 2025-03-24 17:35:48 +0530; 645.2406 EECU-seconds) the first time and 
7 min (started 2025-03-25 11:17:41 +0530; 528.0871 EECU-seconds) the second time (showing the slightly variable time 
duration of doing the same process). Saving srtm(merged and clipped) which is only around 5% of the file size took 
11 minutes (started 2025-03-25 10:42:17 +0530; 102.8910 EECU-seconds) showing that time taken is not dependent on 
file size, but the processes involved, although computing seconds for the Earth Engine (EECU) was itself low. 
After defining no data value, it took 11 min (started 2025-03-25 12:16:52 +0530; 64.8410 EECU-seconds). */
