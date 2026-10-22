# Shiny runs every file in an app's R/ folder at startup. This folder holds
# the data pipeline (01-04), which must only run when called on purpose, so
# this file's presence turns that off. app.R sources what it needs itself.
