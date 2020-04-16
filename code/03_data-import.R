#json file urls------------------------------------------
json_file_urls <- c(
  "premier-league" = "https://datahub.io/sports-data/english-premier-league/datapackage.json",
  "serie-a" = "https://datahub.io/sports-data/italian-serie-a/datapackage.json",
  "la-liga" = "https://datahub.io/sports-data/spanish-la-liga/datapackage.json",
  "bundesliga" = "https://datahub.io/sports-data/german-bundesliga/datapackage.json"
)

#csv file locations-------------------------------------
CsvFileLocations <- lapply(json_file_urls, FileLocationFn) %>%
  unlist()

#data import-------------------------------------
AllData <- lapply(CsvFileLocations, ReadFn) %>%
  bind_rows()  %>%
  select(-(referee:psca)) %>%
  select(-(lbh:sba))
