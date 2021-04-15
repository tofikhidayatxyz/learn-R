library(shiny)
library(DT)
library(plyr)

data = read.csv2('./data/siswa.csv')

colorsData = c(
  "#f0c929",
  "#ff8882",
  "#29bb89",
  "#93329e",
  "#f5c0c0",
  "#7868e6"
)

# average lulus
graduatedStudent = subset(data, data[9] == "Lulus")[1:100, 8]
graduatedStudent = graduatedStudent[!is.na(graduatedStudent)]
notGraduatedStudent =  subset(data, data[9] == "Tidak Lulus")[1:100, 8]
notGraduatedStudent = notGraduatedStudent[!is.na(notGraduatedStudent)]

# averageGraduation <- data.frame(
#   Lulus = graduatedStudent,
#   TidakLulus = notGraduatedStudent
# )

# print(paste("Nilai Maksimum Lulus", max(graduatedStudent)))
# print(paste("Nilai Mininum Lulus", min(graduatedStudent)))
# print(paste("Nilai Median Lulus", median(graduatedStudent)))
# print(paste("Nilai Mean Lulus", mean(notGraduatedStudent)))
# print(paste("Nilai Quartil Lulus", quantile(graduatedStudent)))

boxStats = boxplot.stats(graduatedStudent)

summary(graduatedStudent)

plotData <- rbind.fill(data.frame(Lulus = graduatedStudent),
                       data.frame(TidakLulus = notGraduatedStudent))

plotEx = boxplot.stats(plotData)

boxplot(plotData, col = colorsData[3:6], main = "Rata Rata siswa")
# boxplot(notGraduatedStudent, col = colorsData[6], main = "Siswa Tidak Lulus")


# print(plotEx)

# jenis kelamin
sexData <- data[1:100, 2]
uniqueSex <- unique(sexData, incomparables = FALSE)

sexVal = c()
sexLabel = c()

for(itm in uniqueSex) {
  dataSize <- length(sexData[sexData == itm])
  sexVal <- append(sexVal,
            dataSize,
            after=length(sexVal)
          )
  sexLabel <- append(sexLabel,
                    paste(itm, ",",
                          dataSize,
                          "Orang,",
                          dataSize * 100 / length(sexData), "%"),
                    after = length(sexLabel))
}


pie(sexVal,
    labels = sexLabel,
    main = "Diagram Jenis Kelamin",
    col= colorsData,
    border="#ffffff"
)

# kelas
classData <- data[1:100, 3]
uniqueClass <- unique(classData, incomparables = FALSE)
classVal = c()
classLabel = c()

for(itm in uniqueClass) {
  dataSize <- length(classData[classData == itm])
  classVal <- append(classVal,
                     dataSize,
                     after = length(classVal)
                     )
  classLabel <- append(classLabel, 
                        paste("Kelas",
                            itm,
                            ",",
                            dataSize,
                            "Orang,",
                            dataSize * 100 / length(classData),
                            "%"
                            ),
                        after = length(classLabel)
                      )
}

pie(classVal,
    labels = classLabel,
    main = "Diagram Kelas",
    col= colorsData[2:5],
    border="#ffffff"
)

# Jenis sekolah
schoolData <- data[1:100, 4]
uniqueSchool <- unique(schoolData, incomparables = FALSE)
schoolVal = c()
schoolLabel = c()

for(itm in uniqueSchool) {
  dataSize <- length(schoolData[schoolData == itm])
  schoolVal <- append(schoolVal,
                      dataSize,
                      after= length(schoolVal)
                      )
  schoolLabel <- append(schoolLabel,
                    paste(itm,
                        ",",
                        dataSize,
                        "Orang",
                        ",",
                        dataSize * 100 / length(schoolData),
                        "%"
                        ),
                      after= length(schoolLabel))
}

pie(schoolVal,
    labels = schoolLabel,
    main = "Diagram Jenis Sekolah",
    col= colorsData[4:6],
    border="#ffffff"
)



# Keterangan
infoData <- data[1:100, 9]
uniqueInfo <- unique(infoData, incomparables = FALSE)
infoVal = c()
infoLabel = c()

for(itm in uniqueInfo) {
  dataSize <- length(infoData[infoData == itm])
  infoVal <- append(infoVal,
                      dataSize,
                      after= length(infoVal)
                      )
  infoLabel <- append(infoLabel, 
                    paste(itm,
                          ",",
                          dataSize,
                          "Orang,",
                          dataSize * 100 / length(infoData),
                          "%"
                          ),
                    after = length(infoLabel)
                  )

}

pie(infoVal,
    labels = infoLabel,
    main = "Diagram Keterangan",
    col= colorsData[2:4],
    border="#ffffff"
)

# rata rata nilai b indo, mtk, b inggris

study.indo <- mean(data[1:100, 5])
study.math <- mean(data[1:100, 7])
study.ingr <- mean(data[1:100, 6])

studyData = c(study.indo, study.math, study.ingr)

infoBar <- barplot(
        studyData,
        names.arg = c("B Indonesia", "Matematika", "B Inggris"),
        main = "Rata-Rata Nilai Siswa",
        col = colorsData,
        xlab = "Mata Pelajaran",
        ylab = "Rata Rata Nilai",
        density= 15,
        ylim = c(0, 100)
  )

text(infoBar, studyData, paste(studyData), cex=1)





# mosaicplot(study.indo,main="Smokers",xlab="Status",ylab="Economic Class", dir=c("v","h"))

## https://www.statmethods.net/graphs/bar.html
## https://www.tutorialspoint.com/r/r_bar_charts.htm