#!/bin/sh

ls /app
Rscript /app/update_historical.R &
Rscript /app/app.R & 