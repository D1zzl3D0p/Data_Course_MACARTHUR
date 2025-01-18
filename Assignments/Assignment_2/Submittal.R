
# Read in the CSV files
csv_files=list.files(path="Data",pattern=".csv")
# Get the amount of CSV files in the Data directory
length(csv_files)

# read wingspan vs mass into df
df = read.csv("Data/wingspan_vs_mass.csv")
print(df[5,])

# get list of files whos name starts with b (there's like 3 of them)
b_files=list.files(path="Data",recursive=TRUE,pattern="^b",full.names=TRUE)
for (i in b_files){
  temp=read.csv(i)
  # print out the first lines of each of the b starting csv files
  print(temp[1,])
}

# get list of all csv files in Data
all_csv_files=list.files(path="Data",pattern=".csv$",full.names = TRUE,recursive = TRUE)
for (i in all_csv_files){
  temp=read.csv(i)
  # print the first lines of the csv files
  print(temp[1,])
}
