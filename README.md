# Ghana Spatio-Temporal Malaria Data Analysis

## Overview
This repository investigates malaria incidence patterns in Ghana, focusing on spatial and temporal trends, evaluating intervention effectiveness, and analyzing spatial dependencies. Key objectives include:

- Analyzing temporal trends and intervention impacts.
- Identifying spatial distributions and malaria hotspots.
- Evaluating spatial correlations and autocorrelations.
- Integrating findings into an interactive dashboard.

## Data Sources
The analysis uses publicly available datasets from [Malaria Atlas Project](https://data.malariaatlas.org/):

1. **Malaria Incidence Data**
   - Subnational unit-level data including incidence rates and malaria treatment metrics.
   - Fields: Region, Year, Incidence Rate, Treatment Effectiveness.

2. **Intervention Data**
   - Metrics on coverage and usage of malaria interventions.

3. **Ghana Shapefile**
   - GIS data containing boundaries for Ghana’s administrative regions.

4. **Merged Dataset**
   - Combines malaria, intervention, and treatment data for spatial analysis.

## Key Analyses

### 1. Data Preparation and Cleaning
- Address column name mismatches for accurate data merging.
- Remove extra spaces and handle missing/null values.
- Aggregate malaria data by region and year for hotspot analysis.

### 2. Exploratory Analysis
- **Descriptive Statistics**: Summarize malaria incidence and treatment data.
- **Temporal Trends**:
  - Analyze yearly changes in incidence and treatment coverage.
  - Create interactive line plots using `ggplot2` and `plotly`.
- **Spatial Trends**:
  - Visualize malaria hotspots with choropleth maps using `tmap` and `leaflet`.

### 3. Hotspot Identification
- Aggregate incidence rates by region and year.
- Develop interactive maps highlighting hotspots for specific years (e.g., 2015).

### 4. Impact of Interventions
- **Comparison of Periods**:
  - Calculate average malaria incidence before and after 2012 (intervention year).
  - Assess changes in treatment coverage and malaria incidence.
- **Visualization**:
  - Generate bar charts and interactive visualizations comparing intervention periods.

### 5. Spatial Correlation and Modeling
- **Spatial Autocorrelation**:
  - Use Moran's I to assess global spatial dependencies.
  - Apply Local Moran’s I to identify spatial clusters of high/low incidence.
  - Conduct variogram analysis for spatial dependence modeling.
- **Spatial Lag Model**:
  - Evaluate how neighboring regions influence malaria incidence.
  - Analyze residuals and marginal effects for interventions.
  - Add predicted values to the spatial dataframe.

### 6. Dashboard Development
- Develop a Shiny dashboard with:
  - **Interactive Map**: Visualize temporal and spatial trends.
  - **Trend Analysis**: Interactive plots for incidence and treatment data.
  - **Comparison Table**: Pre- and post-intervention periods summary.

## Deliverables
1. **Descriptive Analysis**:
   - Summary tables and charts for malaria trends and intervention coverage.
2. **Interactive Visualizations**:
   - Temporal trends using `plotly`.
   - Spatial maps using `tmap` and `leaflet`.
3. **Statistical Findings**:
   - Moran’s I and Local Moran’s I results.
   - Fitted variograms and spatial lag model summaries.
4. **Impact Evaluation**:
   - Quantify changes in incidence rates due to interventions.
   - Compare treatment and incidence metrics before and after 2012.
5. **Dashboard**:
   - User-friendly interface integrating maps, trends, and summary data for stakeholders.

## Methodology

### Libraries
- **Data Handling**: `dplyr`, `tidyverse`, `sf`
- **Visualization**: `ggplot2`, `plotly`, `tmap`, `leaflet`
- **Spatial Analysis**: `spdep`, `gstat`
- **Dashboard Development**: `shiny`

### Workflow
1. Preprocess and merge datasets.
2. Perform exploratory and spatial analysis.
3. Develop and validate statistical models.
4. Create interactive visualizations and dashboards.

### Validation
- Ensure data consistency post-merging.
- Cross-validate spatial model predictions with observed data.
- Test dashboard usability and interactivity.

---

### How to Use

1. **Clone the Repository**
   ```bash
   git clone https://github.com/yourusername/ghana-malaria-analysis.git
   ```

2. **Install Required Libraries**
   Ensure you have the necessary R packages installed:
   ```R
   install.packages(c("dplyr", "tidyverse", "sf", "ggplot2", "plotly", "tmap", "leaflet", "spdep", "gstat", "shiny"))
   ```

3. **Run the Analysis**
   Use the provided R scripts to preprocess data, perform analysis, and generate visualizations.

4. **Launch the Dashboard**
   Navigate to the `dashboard` directory and launch the Shiny app:
   ```R
   shiny::runApp("dashboard")
   ```

