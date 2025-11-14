//The following Google Earth Engine script can be used to download (into Google Drive) cloud-optimised Sentinel imagery between the dates 1st January 2024 and 31st December 2024 after some cloud-masking

/**
 * Function to mask clouds using the Sentinel-2 QA band
 * @param {ee.Image} image Sentinel-2 image
 * @return {ee.Image} cloud masked Sentinel-2 image
 */
function maskS2clouds(image) {
  var qa = image.select('QA60');

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask).divide(10000);
}

// Import the study area
var studyarea = ee.FeatureCollection("projects/ee-paulpop/assets/siang");
Map.centerObject(studyarea, 8);

//Map.addLayer(points);

var dataset = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
                  .filterDate('2024-01-01', '2024-12-31')
                  // Pre-filter to get less cloudy granules (10%).
                  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE',10))
                  .map(maskS2clouds)
                  .filter(ee.Filter.bounds(studyarea));
                  //.median();
                  
// Select specific bands
var selected_bands = dataset.select(['B2', 'B3', 'B4']);

// Get the median image of the selected bands
var median_image = selected_bands.median();

var visualization = {
  min: 0.0,
  max: 0.3,
  bands: ['B4', 'B3', 'B2'],
};

Map.addLayer(dataset, visualization, 'RGB');

// Export the result
Export.image.toDrive({
  image: median_image,
  description: 'Sentinel_data_2024',
  folder: 'GEE image downloads',
  region: studyarea.geometry(),
  scale: 10,
  maxPixels: 1e13,
  formatOptions: {
    cloudOptimized: true
  }
});

