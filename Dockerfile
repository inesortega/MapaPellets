# Use an official R runtime as a parent image
FROM r-base:4.3.3

WORKDIR /app

RUN apt-get update && \
  apt-get install -y build-essential libssl-dev libcurl4-openssl-dev libxml2-dev libmysqlclient-dev       

RUN apt-get install -y libgdal-dev

RUN apt-get install -y libproj-dev

RUN apt-get install -y libgeos-dev

RUN apt-get install -y libudunits2-dev

RUN apt-get install -y netcdf-bin

RUN apt-get install -y automake libtool autoconf m4 perl 

RUN apt-get install -y \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

RUN apt-get update

# Clean up package registry
RUN rm -rf /var/lib/apt/lists/*

# Configura un directorio de cacheo de paquetes en /root/.cache/R
ENV R_LIBS_USER="/app/Rlibs"
RUN mkdir -p $R_LIBS_USER

# Agrega variables de entorno para R
ENV RENV_PATHS_CACHE="/app/.cache/R"
RUN mkdir -p $RENV_PATHS_CACHE

# Install remaining packages from source
COPY ./requirements-src.R .
RUN Rscript requirements-src.R

COPY ./ /app
# Make port 3838 available to the world outside this container
EXPOSE 3838

# Define environment variable
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Run app.R when the container launches
CMD ["R", "-e", "shiny::runApp('/app', port = 3838)"]