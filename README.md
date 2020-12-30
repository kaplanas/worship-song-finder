# Worship Song Finder

This project consists of two parts:

* A database of Christian hymns and other worship songs, focused especially on four-part a cappella arrangements used in the Churches of Christ
* A Shiny app that provides a searchable interface to the database

The app is deployed live at <a href="https://worshipsongfinder.com/worship-song-finder/">https://worshipsongfinder.com/worship-song-finder/</a>.

## The database

If you want to download the database yourself and explore, you can find the raw data under [database_files](database_files).

Warning: this is _not_ just a single flat table!  Check out the [ERD](database_files/wsf_erd.pdf) for a guide to how things are structured.  It's not quite 3NF, but it's close.

* If you're comfortable in SQL, you can get a dump of the database.  There are two dump files:
  - [wsf.sql](database_files/wsf.sql): The raw database, as laid out in the ERD.
  - [wsf_shiny.sql](database_files/wsf_shiny.sql): Denormalized tables in a format convenient for the Shiny app.  You can re-create this yourself by firing up MySQL, loading [wsf.sql](database_files/wsf.sql) into a schema called `wsf`, and then running [create_tables_for_shiny.sql](other_scripts/create_tables_for_shiny.sql).  (I haven't tested this script with any other SQL flavors.)
* If you just want to look at the raw tables with low overhead, you can download a [zip file](database_files/csv_dumps.zip) of each raw table as a csv.  (Not explicitly shown in the ERD is the fact that each many-to-many relationship - and there are a _lot_ of them - gets its own bridge table.)

## The app

If you want to run the Shiny app locally, you'll need:

* R, RStudio, and Shiny installed on your local machine (of course)
* All the code in this repository, except for [other_scripts](other_scripts)
* Access to a MySQL database
  - Load the contents of [database_files/wsf_shiny.sql](database_files/wsf_shiny.sql) into a schema called `wsf_shiny`
  - Edit [R_files/database_connection_local.R](R_files/database_connection_local.R) with your host, username, and password