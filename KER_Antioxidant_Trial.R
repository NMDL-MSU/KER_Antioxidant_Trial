
#' # KER Antioxidant Trial

#' ### Description:  
#' Kentucky equine research project for sport and race Thoroughbred looking at 
#' different antioxidant concentrations including: GSH, Cysteine, Homo-Cysteine, 
#' Total AOP.  
#'   
#' ***  
#' **Code:**  
#' Parent Directory:  
#' 
#' >&nbsp;&nbsp;&nbsp;&nbsp;/mnt/research/NMDL/KER_Antioxidant_Trial  
#'   
#' Directory/File:  
#'  
#' &nbsp;&nbsp;&nbsp;&nbsp;/KER_Antioxidant_Trial.R  
#'  
#' **Input files:**  
#' Directory/File:  
#'   
#' >&nbsp;&nbsp;&nbsp;&nbsp;/mnt/research/NMDL/Data_for_Stats_RER_Antioxidant_Normalized.txt  
#'   
#' **Output files:**  
#'   
#' Files:
#' 
#' >&nbsp;&nbsp;&nbsp;&nbsp;/KER_Antioxidant_Trial.Rdata    
#'  
#' Render R Script  
#'   
#' > &nbsp;&nbsp;&nbsp;&nbsp;KER_Antioxidant_Trial.qsub  
#' 
#' ***  

#' ### Code  

#' Load required libraries

#' Clear environment
rm(list=ls())


#'  ### Summary Function
summSD <- function(x, dig=4) round(c(summary(x),
     Std.Dev.=sd(x)), dig)[c("Min.", "1st Qu.", "Median", "Mean", 
    "Std.Dev.", "3rd Qu.", "Max.")]


#' Load data file for sport and race horses
dir <- "/mnt/research/NMDL"
dataM <- read.table(paste(dir, "Data_for_Stats_RER_Antioxidant_Normalized.txt", sep="/"), 
    header=TRUE, sep="\t")

#' Move input file to R directory
system("mv /mnt/research/NMDL/Data_for_Stats_RER_Antioxidant_Normalized.txt /mnt/research/NMDL/KER_Antioxidant_Trial")

#' Add GSH level factor to data matrix (cutoff 10)
cutoff <- 10
dataM <- data.frame(dataM, GSH.level=rep("high", nrow(dataM)))
dataM$GSH.level <- as.character(dataM$GSH.level)


#' Index animals below GSH cutoff and assign GSH level (low or high)
idx <- as.character(dataM$Animal[dataM$Assay == "GSH" & 
    dataM$Treatment == "Control" & dataM$Pre < cutoff]) 
dataM$GSH.level[as.character(dataM$Animal) %in% idx] <- "low"
dataM$GSH.level <- as.factor(dataM$GSH.level)


#' Check categories of low and high GSH:
# All GSH Pre concentrations
summSD(dataM[dataM$Assay == "GSH", "Pre"])

# Low GSH in Control animals
summSD (dataM[dataM$GSH.level == "low" & dataM$Assay == "GSH" 
    & dataM$Treatment == "Control", "Pre"])

# High GSH in Control animals
summSD (dataM[dataM$GSH.level == "high" & dataM$Assay == "GSH" 
    & dataM$Treatment == "Control", "Pre"])


#' ### Run regression: ANOVA

#' > GSH

# GSH Assay Data
gsh <- dataM[dataM$Assay == "GSH",]
nrow(gsh)

# Test Normality
shapiro.test(gsh$Pre)

#+ histGSHraw, fig.align='center', fig.width=7, fig.height=7, dpi=300
hist(gsh$Pre)

# Log transforme GSH pre values
gsh$Pre <- log(gsh$Pre)
shapiro.test(gsh$Pre)

#+ histGSHlog, fig.align='center', fig.width=7, fig.height=7, dpi=300
hist(gsh$Pre)


# Treatment:Animal effect + GSH Level
gsh.rst <- aov(Pre~Animal + Treatment:GSH.level, data=gsh)
anova(gsh.rst)
TukeyHSD(gsh.rst)["Treatment:GSH.level"]

# Type + Treatment:GSH Level
gsh2.rst <- aov(Pre~Type + Treatment:GSH.level, data=gsh)
anova(gsh2.rst)
TukeyHSD(gsh2.rst)

# Treatment:GSH Level
gshM.rst <- aov(Pre~Treatment:GSH.level, data=gsh)
anova(gshM.rst)
TukeyHSD(gshM.rst)




#' > Cysteine

#' Cysteine Assay Data
cys <- dataM[dataM$Assay == "Cysteine",]
nrow(cys)

# Test Normality
shapiro.test(cys$Pre)

#+ histCYSraw, fig.align='center', fig.width=7, fig.height=7, dpi=300
hist(cys$Pre)

# Log transforme GSH pre values
cys$Pre <- log(cys$Pre)
shapiro.test(cys$Pre)

#+ histCYSlog, fig.align='center', fig.width=7, fig.height=7, dpi=300
hist(cys$Pre)

# Treatment:Animal effect + cys Level
cys.rst <- aov(Pre~Animal + Treatment:GSH.level, data=cys)
anova(cys.rst)
TukeyHSD(cys.rst)["Treatment:GSH.level"]

# Type + Treatment:cys Level
cys2.rst <- aov(Pre~Type + Treatment:GSH.level, data=cys)
anova(cys2.rst)
TukeyHSD(cys2.rst)

# Treatment:cys Level
cysM.rst <- aov(Pre~Treatment:GSH.level, data=cys)
anova(cysM.rst)
TukeyHSD(cysM.rst)



#' > H-Cysteine

#' Homo.Cysteine Assay Data
h.cys <- dataM[dataM$Assay == "Homo-Cysteine",]
nrow(h.cys)

# Test Normality
shapiro.test(h.cys$Pre)

#+ histHCYSraw, fig.align='center', fig.width=7, fig.height=7, dpi=300
hist(h.cys$Pre)

# Log transforme GSH pre values
h.cys$Pre <- log(h.cys$Pre)
shapiro.test(h.cys$Pre)

#+ histHCYSlog, fig.align='center', fig.width=7, fig.height=7, dpi=300
hist(h.cys$Pre)

# Treatment:Animal effect + h.cys Level
h.cys.rst <- aov(Pre~Animal + Treatment:GSH.level, data=h.cys)
anova(h.cys.rst)
TukeyHSD(h.cys.rst)["Treatment:GSH.level"]

# Type + Treatment:h.cys Level
h.cys2.rst <- aov(Pre~Type + Treatment:GSH.level, data=h.cys)
anova(h.cys2.rst)
TukeyHSD(h.cys2.rst)

# Treatment:h.cys Level
h.cysM.rst <- aov(Pre~Treatment:GSH.level, data=h.cys)
anova(h.cysM.rst)
TukeyHSD(h.cysM.rst)


#' ### Run R Script
#+ eval = FALSE
htmlRunR
KER_Antioxidant_Trial.R nodes=1,cpus-per-task=1,time=03:00:00,mem=10G \
+KER Antioxidant Trial

