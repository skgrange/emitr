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

**emitr** was primarily developed so data from a number of field campaigns could be integrated into a single database and interacted with in a consistent way. This represents a significant challenge because the units used, naming conventions, and what variables are used during a vehicle capture and what details are retrieved using a registration plate based data service can be diverse. However, when complete, the importing functions: `import_vehicle_captures` and `import_vehicle_details` makes it very easy to retrieve standardised data. There is also a higher level function `import_vehicle_emissions` which will join the capture and details data together to make a table ready for analysis. 

This package has been developed with [SQLite](https://www.sqlite.org/index.html) and [PostgreSQL](https://www.postgresql.org/) databases. 

## Database schema

[Here](docs/emitr_schema.png).
