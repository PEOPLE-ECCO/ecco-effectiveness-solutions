# PEOPLE-ECCO Effectiveness solutions

## BACI

This repo contains **PEOPLE-ECCO-BACI**, an R Shiny app implementing a
spatial Before-After Control-Impact (BACI) analysis workflow for
evaluating the effectiveness of conservation interventions. It statistically
matches impact (treatment) units to similar control units, then applies a
before/after difference-in-differences contrast to isolate the ecological
effect of a conservation action from the counterfactual scenario.

Usage details are provided in [instructions.md](PEOPLE-ECCO-BACI/instructions.md).

BACI is one of six PEOPLE-ECCO Solutions; see the
[PEOPLE-ECCO user handbook](https://people-ecco.github.io) for the full
platform and Solutions overview.

## Running locally

The app lives in [PEOPLE-ECCO-BACI](PEOPLE-ECCO-BACI). Requirements:

* R (matching the version used in [PEOPLE-ECCO-BACI/Dockerfile](PEOPLE-ECCO-BACI/Dockerfile), currently 4.6.1)
* geospatial system libraries GDAL, GEOS, and PROJ

Install the required R packages:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "data.table", "MatchIt",
  "cobalt", "ragg", "sf", "terra", "exactextractr",
  "jsonlite", "openeo"
))
```

Then run the app from the repo root:

```r
shiny::runApp("PEOPLE-ECCO-BACI", host = "0.0.0.0", port = 3838)
```

The app will be available at http://localhost:3838.

## Running with Docker

A [Dockerfile](PEOPLE-ECCO-BACI/Dockerfile) and
[docker-compose.yml](docker-compose.yml) are provided.

Using Docker Compose (from the repo root):

```sh
docker compose up --build
```

Or building/running the image directly:

```sh
docker build -t people-ecco-baci ./PEOPLE-ECCO-BACI
docker run --rm -p 3838:3838 people-ecco-baci
```

The app will be available at http://localhost:3838.

## License

Licensed under the GNU General Public License v3.0 (GPL-3.0-only) — see
[LICENSE](LICENSE). See [NOTICE](NOTICE) for third-party package licenses.
