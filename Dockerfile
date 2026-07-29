FROM rocker/r-base:latest

# Install necessary system dependencies for other packages
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev \
  libsodium-dev \
  && rm -rf /var/lib/apt/lists/*

# Create application directory
WORKDIR /app

# Install R packages required by our engine and API
RUN R -e "install.packages(c('plumber', 'jsonlite', 'dplyr'), repos='https://cloud.r-project.org/')"

# Copy application files
COPY engine.R plumber.R run.R /app/
COPY public /app/public

# Run the Plumber application
CMD ["Rscript", "run.R"]
