#!/bin/bash

# Build the docker image
docker build -t list-func-solver .

# Run the docker image
docker run --rm -it list-func-solver
