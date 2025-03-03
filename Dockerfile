# Use the official R image from the Rocker project
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install remotes to install specific versions of packages
RUN R -e "install.packages('remotes')"

# Install tmap version 3.3.3
RUN R -e "remotes::install_version('tmap', version = '3.3.3')"

# Install other required R packages
RUN R -e "install.packages(c('tidyverse', 'sf', 'ggplot2', 'dplyr', 'shiny', 'DT', 'leaflet', 'plotly', 'spdep', 'gstat', 'sp', 'spatialreg'), dependencies=TRUE)"

# Set the working directory
WORKDIR /app

# Copy the application files into the container
COPY . /app

# Expose the port that the Shiny app will run on
EXPOSE 3838

# Command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]