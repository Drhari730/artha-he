# Pinned R version so builds are reproducible.
FROM rocker/r-ver:4.4.1

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libssl-dev \
  libcurl4-gnutls-dev \
  libsodium-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

# rocker/r-ver installs packages from a dated CRAN snapshot (Posit Package Manager),
# so these versions are fixed by the image tag above.
RUN R -e "install.packages(c('plumber', 'jsonlite', 'testthat'))"

COPY engine.R plumber.R run.R /app/
COPY tests /app/tests
COPY validation /app/validation
COPY public /app/public

# Fail the build if the engine tests fail.
RUN Rscript tests/run_tests.R

CMD ["Rscript", "run.R"]
