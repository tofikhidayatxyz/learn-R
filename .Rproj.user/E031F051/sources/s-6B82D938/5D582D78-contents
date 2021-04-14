data = read.csv2('./data/siswa.csv')
library(shiny)
library(DT)


colorsData = c(
  "#f0c929",
  "#ff8882",
  "#29bb89",
  "#93329e",
  "#f5c0c0",
  "#7868e6"
)

# jenis kelamin
sexData <- data[1:100, 2]
uniqueSex <- unique(sexData, incomparables = FALSE)

sexVal = c()

for(itm in uniqueSex) {
  sexVal <- append(sexVal,
           length(sexData[sexData == itm]),
            after=length(sexVal)
            )
}


pie(sexVal,
    labels = uniqueSex,
    main = "Diagram Jenis Kelamin",
    col= colorsData,
    border="#ffffff"
)

# kelas
classData <- data[1:100, 3]
uniqueClass <- unique(classData, incomparables = FALSE)
classVal = c()

for(itm in uniqueClass) {
  classVal <- append(classVal,
                     length(classData[classData == itm]),
                     after = length(classVal)
                     )
}

pie(classVal,
    labels = uniqueClass,
    main = "Diagram Kelas",
    col= colorsData[2:5],
    border="#ffffff"
)

# Jenis sekolah
schoolData <- data[1:100, 4]
uniqueSchool <- unique(schoolData, incomparables = FALSE)
schoolVal = c()

for(itm in uniqueSchool) {
  schoolVal <- append(schoolVal,
                      length(schoolData[schoolData == itm]),
                      after= length(schoolVal)
                      )
}

pie(schoolVal,
    labels = uniqueSchool,
    main = "Diagram Jenis Sekolah",
    col= colorsData[4:6],
    border="#ffffff"
)



# Keterangan
infoData <- data[1:100, 9]
uniqueInfo <- unique(infoData, incomparables = FALSE)
infoVal = c()

for(itm in uniqueInfo) {
  infoVal <- append(infoVal,
                      length(infoData[infoData == itm]),
                      after= length(infoVal)
                      )
}

pie(infoVal,
    labels = uniqueInfo,
    main = "Diagram Keterangan",
    col= colorsData[2:4],
    border="#ffffff"
)

# rata rata nilai b indo, mtk, b inggris

study.indo <- mean(data[1:100, 5])
study.math <- mean(data[1:100, 7])
study.ingr <- mean(data[1:100, 6])

barplot(
  c(study.indo, study.math, study.ingr),
  names.arg = c("B Indonesia", "Matematika", "B Inggris"),
  main = "Rata-Rata Nilai Siswa",
  col = colorsData,
  xlab = "Mata Pelajaran",
  ylab = "Rata Rata Nilai"
  )


# mosaicplot(study.indo,main="Smokers",xlab="Status",ylab="Economic Class", dir=c("v","h"))

## https://www.statmethods.net/graphs/bar.html
## https://www.tutorialspoint.com/r/r_bar_charts.htm