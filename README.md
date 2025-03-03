# Malaria Spatio-Temporal Dashboard

A Shiny app for visualizing and analyzing malaria incidence and intervention effectiveness in Ghana. This app provides interactive maps, temporal trend analysis, and spatial autocorrelation tools to explore malaria data.

## Features
- Interactive maps of malaria hotspots by region and year.
- Temporal trends in malaria incidence over time.
- Spatial autocorrelation analysis using Moran's I and Local Moran's I.
- Comparison of malaria incidence before and after interventions.
- Variogram analysis for spatial modelling.

## Usage
1. Pull the image:
   ```bash
   docker pull iprince/malariaanalysisgh-dashboard

2. Run the container:

   ```bash
   docker run -d --rm -p 3838:3838 iprince/malariaanalysisgh-dashboard

3. Access the app in your browser at http://localhost:3838

# Data Sources
- Ghana shapefile: Administrative boundaries of Ghana.

- Malaria incidence data: Subnational malaria case data.

- Intervention data: Malaria intervention coverage data.

## Dependencies
R packages: tidyverse, sf, tmap, shiny, leaflet, spdep, gstat, spatialreg,
            ggplot2, plotly, DT, dplyr and sp

