seatrackR - R package for utilizing the seatrack database
==============

Much of the basic functionality is now in place, but will continue to be developed. Please take a look at the vignettes for a guide to what is available `help(package = "seatrackR")`. Note that all functions might not be showed in the vignettes. 


Functionality that exists:
*  Connect to the database
*  Retrive data (Individ info, Logger info, Positions, File archive list, Active logging session list)
*  Check the consistency of new data with current database, before insertion. (of "metadata" field sheets)
*  Insert data into the database. (logger data and "metadata" fieldsheets)
*  Interact with the FTP file archive (list files, upload, download, delete)



Installation
============
Install the package by

```
devtools::install_github("NINAnor/seatrack-db/seatrackR", build_vignettes = T)
```
