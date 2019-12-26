# Vlaky

Web scraping for zelpage.cz to turn it into an API.

## Running

This project was built using [sbt](https://www.scala-sbt.org/) and requires it for building and executing.

### Local

`$ sbt run`

### Docker

1. `$ sbt docker:publishLocal`
2. `$ docker run vlaky-api`

## API Routes

- GET `/composition/{year}/train/{identifier}`

  Get train composition and details for a specific train. Train identifier format is along the lines of `cd-4725`, `rj-1032`, etc.