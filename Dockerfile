# Use an official R runtime as a parent image
FROM rocker/r-apt:bionic

WORKDIR /app

RUN apt-get update && \
  apt-get install -y libxml2-dev libmysqlclient-dev

RUN apt-get install -y libgdal-dev

RUN apt-get install -y libproj-dev

RUN apt-get install -y libgeos-dev

RUN apt-get install -y libudunits2-dev

RUN apt-get install -y netcdf-bin

RUN apt-get update

# Install remaining packages from source
COPY ./requirements-src.R .
RUN Rscript requirements-src.R

# Clean up package registry
RUN rm -rf /var/lib/apt/lists/*

COPY ./ /app
# Make port 3838 available to the world outside this container
EXPOSE 3838

# Define environment variable
ENV LC_ALL C.UTF-8
ENV LANG C.UTF-8

# Run app.R when the container launches
CMD ["R", "-e", "shiny::runApp('/app', port = 3838)"]
