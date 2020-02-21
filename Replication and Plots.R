##############################
# PLSC 505 - Political Networks
# Replication and Extension - Part 2
# Seth Warner
##############################


require(readxl)
require(igraph)
require(signnet)

#### Prep ####

# Recreate sheet names in Excel
H <- rep("H",22)
S <- rep("S",22)
Congress <- 93:114

sheetH <- paste(H,Congress, sep="")
sheetS <- paste(S,Congress, sep="")

# Prepare vectors to capture network stats
Smod <- rep(0,22)
Stindex <- rep(0,22)
Hmod <- rep(0,22)
Htindex <- rep(0,22)
time <- 1:22

# Vertex name must include "-D" or be Indy exception to categorize as Dem 
Dem <- "-D|Sanders, B.|King, A.|Lieberman, J.|Jeffords, J. (VT-I)"


#### Modularity ####

# Senate - modularity
for (i in 1:22){
  # Read sheet
  tmp <- as.matrix(read_excel("Penn State/Spring 2020/Networks/Replication and Extension/a_sign_of_the_times1.xlsx", sheet=sheetS[i]))
  tmp <- tmp[,-1] #Remove names from first row
  tmp <- apply(tmp, 2, as.numeric) #Change cells to numeric
  rownames(tmp) <- colnames(tmp) #Add names
  
  tmpnet <- graph_from_adjacency_matrix(tmp, mode = "undirected") #Create network
  
  party <- ifelse(grepl(Dem,rownames(tmp)),1,2) #ID parties
  V(tmpnet)$party <- party
  
  Smod[i] <- modularity(tmpnet, party) #Capture membership-based moduarlity
}

cor(Smod,time,method = "spearman")

# House - modularity
for (i in 1:22){
  tmp <- as.matrix(read_excel("Penn State/Spring 2020/Networks/Replication and Extension/a_sign_of_the_times1.xlsx", sheet=sheetH[i]))
  tmp <- tmp[,-1]
  tmp <- apply(tmp, 2, as.numeric)
  rownames(tmp) <- colnames(tmp)
  
  tmpnet <- graph_from_adjacency_matrix(tmp, mode = "undirected")
  
  party <- ifelse(grepl(Dem,rownames(tmp)),1,2)
  V(tmpnet)$party <- party
  
  Hmod[i] <- modularity(tmpnet, party)
}

#Note: Neal only analyzes House from 96th Congress on, due to change in sponsorship rules
cor.test(Hmod[4:22],time[4:22],method = "spearman")

par(mfrow=c(1,2), oma=c(0,0,2,0))

plot(Congress[4:22],Hmod[4:22],
     xlab="Session of Congress",
     ylab="Modularity",type="l",
     main="House")
plot(Congress,Smod,
     xlab="Session of Congress",
     ylab="Modularity",type="l",
     main="Senate")

title("Figure 1. Modularity of Congressional Networks over Time",outer=TRUE)


#### Triangle Index ####

# Senate - triangle index
for (i in 1:22){
  # Read sheet
  tmp <- as.matrix(read_excel("Penn State/Spring 2020/Networks/Replication and Extension/a_sign_of_the_times1.xlsx", sheet=sheetS[i]))
  tmp <- tmp[,-1]
  tmp <- apply(tmp, 2, as.numeric)
  
  # Weighted graph keeps signs
  tmpnet <- graph_from_adjacency_matrix(tmp, mode = "undirected", weighted = T)
  
  # Add "sign" attribute to work with signnet package
  signs <- as.vector(edge.attributes(tmpnet))
  signs <- signs$weight
  E(tmpnet)$sign <- signs
  
  # Count triangles and calc index
  count <- count_signed_triangles(tmpnet)
  Stindex[i] <- as.numeric( (count[1] + count[3]) / sum(count) )
}

cor(Stindex,time,method = "spearman")


# WARNING - HOUSE TRIANGLES TAKES MIN. 1 HOUR

# House - triangle index
for (i in 1:22){
  tmp <- as.matrix(read_excel("Penn State/Spring 2020/Networks/Replication and Extension/a_sign_of_the_times1.xlsx", sheet=sheetH[i]))
  tmp <- tmp[,-1]
  tmp <- apply(tmp, 2, as.numeric)
  rownames(tmp) <- colnames(tmp)
  
  tmpnet <- graph_from_adjacency_matrix(tmp, mode = "undirected", weighted = T)
  
  signs <- as.vector(edge.attributes(tmpnet))
  signs <- signs$weight
  E(tmpnet)$sign <- signs
  
  count <- count_signed_triangles(tmpnet)
  Htindex[i] <- as.numeric( (count[1] + count[3]) / sum(count) ) 
}

cor.test(Htindex[4:22],time[4:22],method = "spearman")

par(mfrow=c(1,2), oma=c(0,0,2,0))

plot(Congress[4:22],Htindex[4:22],
     xlab="Session of Congress",
     ylab="Triangle Index",
     type="l",
     main="House")
plot(Congress,Stindex,
     xlab="Session of Congress",
     ylab="Triangle Index",
     type="l",
     main="Senate")

title("Figure 2. Triangle Index of Congressional Networks over Time",outer=TRUE)


#### Graph and Histogram ####


# Get House network for 114th Congress (2015-16)
House <- as.matrix(read_excel("Penn State/Spring 2020/Networks/Replication and Extension/a_sign_of_the_times1.xlsx", sheet="H114"))
House <- House[,-1] #Remove names from first row
House <- apply(House, 2, as.numeric) #Change cells to numeric
rownames(House) <- colnames(House) #Add names
Dems <- ifelse(grepl(Dem,rownames(House)),1,0)
HouseGOP <- House[Dems==0,Dems==0]

HGOPnet <- graph_from_adjacency_matrix(HouseGOP, mode = "undirected")
plot(HGOPnet, main="Figure 3. House GOP Network (114th Congress)")

#Get and plot Senate
Senate <- as.matrix(read_excel("Penn State/Spring 2020/Networks/Replication and Extension/a_sign_of_the_times1.xlsx", sheet="S114"))
Senate <- Senate[,-1] #Remove names from first row
Senate <- apply(Senate, 2, as.numeric) #Change cells to numeric
rownames(Senate) <- colnames(Senate) #Add names
Dems <- ifelse(grepl(Dem,rownames(Senate)),1,0)
SenateGOP <- Senate[Dems==0,Dems==0]

SGOPnet <- graph_from_adjacency_matrix(SenateGOP, mode = "undirected")
plot(SGOPnet, main="Figure 4. Senate GOP Network (114th Congress)")

# Match seniority to name
names <- V(SGOPnet)$name
names
seniority <- c(12,37,18,13,16,18,12,34,38,21,28,30,13,18,18,28,4,4,10,0,2,
               10,5,4,4,4,10,8,10,4,4,8,8,0,6,0,0,2,4,4,4,4,4,4,13,0,2,2,
               0,0,0,0,0,0)

# Add attributes
V(SGOPnet)$seniority <- seniority

#Histogram
hist(V(SGOPnet)$seniority, main = "Figure 5. Seniority of GOP Senators (114th Congress)",
     xlab="Years of Senate Experience, 2015")
