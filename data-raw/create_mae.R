set.seed(100)

#Starting with the example SE object
object <- summarized_experiment

#Subset the example SE object into 3 different SEs
object1 <- object[1000:2000, 1:5]
object2 <- object[1000:3500, 6:14]
object3 <- object[1500:2800, 15:20]

#Randomly adding NAs into the counts of the 3 SE objects
counts_matrix1 <- assays(object1)$counts
indices1 <- arrayInd(sample(length(counts_matrix1), length(counts_matrix1)*0.1), dim(counts_matrix1))
counts_matrix1[indices1] <- NA

counts_matrix2 <- assays(object2)$counts
indices2 <- arrayInd(sample(length(counts_matrix2), length(counts_matrix2)*0.15), dim(counts_matrix2))
counts_matrix2[indices2] <- NA

counts_matrix3 <- assays(object3)$counts
indices3 <- arrayInd(sample(length(counts_matrix3), length(counts_matrix3)*0.25), dim(counts_matrix3))
counts_matrix3[indices3] <- NA

#Create sample maps for each experiment
object1map <- data.frame(
  primary = c("Jack", "Jill", "Barbara", "Bob", "John"),
  colname = c("06520011B0023R", "06520067C0018R", "06520063C0043R", "06520105C0017R", "06520092C0017R"),
  stringsAsFactors = FALSE
)

object2map <- data.frame(
  primary = c("Jack", "Jill", "Barbara", "Bob", "John", "Jane", "Claire", "Mike", "Kate"),
  colname = c("06520103C0017R", "06520001B0023R", "06520022C0017R", "06520062C0017R", "06520046C0018R", 
              "06520101B0017R", "06520047C0017R", "06520024B0014R", "06520080B0023R"),
  stringsAsFactors = FALSE
)

object3map <- data.frame(
  primary = c("Jack", "Jill", "Barbara", "Bob", "John", "Jane"),
  colname = c("06520093C0017R", "06520070C0018R", "06520023C0018R", "06520099B0017R", "06520015C0016R", "06520019C0023R"),
  stringsAsFactors = FALSE
)

maplist <- list(object1 = object1map, object2 = object2map, object3 = object3map)
sampMap <- listToMap(maplist)

#Create an example phenotype data
colDat <- data.frame(sex = c("M", "F", "F", "M", "M", "F", "F", "M", "F"), 
                     age = 35:43,
                     row.names = c("Jack", "Jill", "Barbara", "Bob", "John", "Jane", "Claire", "Mike", "Kate"))

#Create a named experiment list
assaylist <- list(object1 = object1, object2 = object2, object3 = object3)
ExpList <- ExperimentList(assaylist)

#Create a MultiAssayExperiment object
mae <- MultiAssayExperiment(
  experiments = ExpList,
  colData = colDat,
  sampleMap = sampMap
)
