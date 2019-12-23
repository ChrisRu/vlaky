# Vlaky

Web scraping for zelpage.cz to turn it into a real API.

## Running

This project was built using [sbt](https://www.scala-sbt.org/) and requires it for building and executing.

### Local

`$ sbt run`

### Docker

1. `$ sbt docker:publishLocal`
2. `$ docker run vlaky-api`

## API Routes

- GET `/composition/{year}/train/{route}`

  Get train composition and details for a specific train.
