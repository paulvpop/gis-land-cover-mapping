//GEE script for downloading Copernicus DEM

//Upload the study area shapefile as an asset in GEE

//Load in the Copernicus 30 m resolution DEM (GLO-30) on to GEE
var dataset = ee.ImageCollection('COPERNICUS/DEM/GLO30');

//Check the size of the image collection
print('GLO-30 Collection size :',dataset.size())

//Set projection:
var demx = dataset.mosaic().setDefaultProjection('EPSG:4326', null, 30)

//Select the DEM elevation band
var dem = demx.select('DEM');

print('CRS:', dem.projection().crs());

//Load the region of interest
var roi = ee.FeatureCollection("projects/ee-paul_pop/assets/rectangleStudyArea");

//Clip the DEM to region of interest 
var elev = dem.clip(roi);

//Visualise the elevation
var palettes = require('users/gena/packages:palettes');
var visParam = {
  crs: 'EPSG:4326',
  min: 100,
  max: 5020,
  palette: palettes.kovesi.linear_kry_5_98_c75[7]
};
Map.addLayer(elev, visParam, 'DEM')
Map.centerObject(elev, 8);

//Define no data value, then unmask the merged and clipped image and supply the value to it
var noDataVal = -9999;
var unmaskedImage = elev.unmask({value: noDataVal, sameFootprint: false});

//Save image to drive
Export.image.toDrive({
  image: unmaskedImage,
  region: roi,
  description: 'GLO30',
  crs: "EPSG:4326",
  scale: 30,
  maxPixels: 1e13,  //Since it's a large area and contains a lot of pixels
  fileFormat: 'GeoTIFF',
  formatOptions: {
    noData: noDataVal
  }
});
