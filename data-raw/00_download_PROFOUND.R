# download and unzip the profound database in a desired directory

# set path where to store the data
path <- "./data/"

# download ASCII zip file
download.file(
  url = "https://www.pik-potsdam.de/data/doi/10.5880/PIK.2020.006/ProfoundData_ASCII.zip",
  destfile = file.path(tempdir(), 'profound.zip'),
)

# unzip the data to the path above
unzip(
  file.path(tempdir(), 'profound.zip'),
  exdir = path,
  junkpaths = TRUE
)
