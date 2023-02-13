# Base image
FROM rocker/shiny:4.1.2

# Install required linux librairies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends libpq-dev \ 
                                               libssl-dev \
                                               libxml2-dev \
                                               gdal-bin \
                                               libgdal-dev \
                                               espeak
RUN sudo mkdir /usr/share/mbrola
RUN sudo wget https://github.com/numediart/MBROLA-voices/blob/master/data/fr1/fr1?raw=true -O /usr/share/mbrola/fr1
                                           

# Install R package and its dependencies
RUN install2.r remotes
COPY applatin2french/ ./applatin2french
RUN Rscript -e 'remotes::install_deps("./applatin2french")'
RUN Rscript -e 'install.packages("./applatin2french", repos = NULL, type="source")'

# Expose port where shiny app will broadcast
ARG SHINY_PORT=3838
EXPOSE $SHINY_PORT
RUN echo "local({options(shiny.port = ${SHINY_PORT}, shiny.host = '0.0.0.0')})" >> /usr/local/lib/R/etc/Rprofile.site

# Endpoint
CMD ["Rscript", "-e", "applatin2french::runApp()"]
