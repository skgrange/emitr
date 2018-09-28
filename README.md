# **emitr**

[![Build Status](https://travis-ci.org/skgrange/emitr.svg?branch=master)](https://travis-ci.org/skgrange/emitr)

**emitr** is an R package to help organise and analyse on-road vehicle emissions data. 

## Installation

The development version: 

```
# Install dependencies
devtools::install_github("skgrange/threadr")
devtools::install_github("skgrange/databaser")

# Install emitr
devtools::install_github("skgrange/emitr")
```

## Usage

**emitr** was primarily developed so data from a number of field campaigns could be integrated into a single database and interacted with in a consistent way. This represents a significant challenge because the units used, naming conventions, and what variables are used during a vehicle capture and what details are retrieved using a registration plate based data service can be diverse. 

This package has been developed with [SQLite](https://www.sqlite.org/index.html) and [PostgreSQL](https://www.postgresql.org/) databases. 

### Importing by vehicle

Importing vehicle capture and detail data by vehicle registraion is easy. A vector of registrations can be used and the function will handle the rest. 

```
# Get a random sample of vehicle registrations
registrations <- sample_registrations(con, 10)

# Get details for these registrations
data_emissions <- import_vehicle_emissions(con, registration = registrations)

# Quite a wide table
ncol(data_emissions)

[1] 100
```

### Importing by monitoring location or field session

More useful for on-road emission data analysis is the ability to import data based on monitoring location (a site) or monitoring sessions (generally a site/day/instrument triad). 

```
# Get site information
data_sites <- import_sites(con)

# Get session information
data_sessions <- import_sessions(con)

# Get details for two sites
data_emissions_two_sites <- import_by_site(con, site = c(1, 101))

# Get details for a session
data_emssions_three_sessions <- import_by_session(con, session = c(10, 320, 532))
```

## Database schema

[Here](docs/emitr_schema.png).


## Current size
