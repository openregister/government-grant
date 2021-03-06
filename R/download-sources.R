download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/663589/GGIS_Grant_Awards_2016_to_2017_2017-10-27_1621.xlsx",
              "lists/government-grant-awards-2016-17.xlsx",
              mode = "wb")
download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/655239/GGIS_Grant_Schemes_2016_to_2017.csv",
              "lists/government-grant-schemes-2016-17.csv")
download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/655237/GGIS_Grant_Schemes_2015_to_2016.csv",
              "lists/government-grant-schemes-2015-16.csv")
download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/502415/Grants_Register.February16.csv",
              "lists/government-grant-schemes-2014-15-dirty.csv")
download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/405234/2013-14_Government_Grants.csv",
              "lists/government-grant-schemes-2013-14-dirty.csv")

system("sed -i \"s/\\r/\\n/g\" lists/government-grant-schemes-2014-15-dirty.csv")
system("sed -i \"s/\\r/\\n/g\" lists/government-grant-schemes-2013-14-dirty.csv")
system("iconv -f ISO-8859-1 -t UTF-8 -o lists/government-grant-schemes-2013-14.csv lists/government-grant-schemes-2013-14-dirty.csv")
system("iconv -f ISO-8859-1 -t UTF-8 -o lists/government-grant-schemes-2014-15.csv lists/government-grant-schemes-2014-15-dirty.csv")

file.remove("lists/government-grant-schemes-2013-14-dirty.csv")
file.remove("lists/government-grant-schemes-2014-15-dirty.csv")
