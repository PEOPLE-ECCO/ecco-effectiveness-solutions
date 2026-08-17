# use rocker/geospatial (features R, GDAL/GEOS/PROJ, sf/terra)
FROM rocker/geospatial:4.6.1

# Install additional deps
RUN install2.r --error --skipinstalled \
    shiny \
    bslib \
    leaflet \
    data.table \
    MatchIt \
    cobalt \
    ragg \
    && rm -rf /tmp/downloaded_packages

WORKDIR /srv/shiny-server/PEOPLE-ECCO-BACI

COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/PEOPLE-ECCO-BACI', host = '0.0.0.0', port = 3838)"]
