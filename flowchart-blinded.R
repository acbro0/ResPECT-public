require(diagram)


par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
##initialize new grphics device
openplotmat()
##number of elements per row
elpos<-coordinates (c(1,2,2,1 ))
elpos

##draw arrows from each row to next row
treearrow(from=elpos[1,],to=elpos[2:3,],lwd=6)  
treearrow(from=elpos[2,],to=elpos[4:5,],lwd=6) 
treearrow(from=elpos[4,],to=elpos[6,],lwd=6)  
#treearrow(from=elpos[4,],to=elpos[8,],lwd=6)  

require(dplyr)

##get the data.
demog <- read.csv("/users/nreich/respect/all-years-data-current/ResPECT-final-datasets/ResPECT-demographics.csv", stringsAsFactors=FALSE)

nrow(demog)

##create a generic 3-lined label for each textbox
labels = vector(length=6)
labels[1] = paste(c("Assessed for eligibility (n=5266)"), collapse="") 

labels[3] = paste(c("Ineligible (n=86)", "\n", "Failed to meet inclusion criteria"), collapse="")

#nrow(demog)

labels[2] = paste(c("Recieved intervention (n=5180)", "\n", "Intention to treat cohort"), collapse="") 

labels[4] = paste(c("Completed study (n=4689)", "\n", "Per protocol cohort"), collapse="") 

labels[5] = paste(c("Discontinued intervention", "\n", "Change in work location (n=256)", "\n", "Other reason (n=235)" ), collapse="") 

labels[6] <-  paste("Outcomes analyzed")

labels

##plot text boxes
for ( i in 1:6) textrect (elpos[i,], radx=0.2, rady=0.06, lab=labels[i], shadow.size = 0)




## example of autogenerating a consort diagram in R
nodetext <- structure(
  c('Approached', '', 'Ineligible', '',
    'Enrolled', '', '', '',
    'Control', 'Weightloss', 'Smoking', 'Smoking + WL',
    'Person Month FU', 'Person Month FU', 'Person Month FU', 'Person Month FU'),
  dim=c(4, 4))
nodelab <- structure(letters[seq(nodetext)], dim=c(4,4))
cbind(c(nodetext),c(nodelab))
nodelink <- '
\\path (a)--(c);
\\path (a)--(e);
\\path (e)-|(i);
\\path (e)-|(j);
\\path (e)-|(k);
\\path (e)-|(l);
\\path (i)--(m);
\\path (j)--(n);
\\path (k)--(o);
\\path (l)--(p);
'
consort <- function(nodetext, nodelab=NULL, nodelink=NULL, outfile) {
  nodetext <- t(nodetext)
  nodelab <- t(nodelab)
  
  opts <- "\\tikzset{
  block/.style={rectangle, draw, fill=blue!20, 
  text width=0.85in, text centered, rounded corners, minimum height=4em},
  line/.style ={draw, thick, -latex', shorten >=0pt}
     }"
  start <- '\\begin{tikzpicture}[node distance = .15in, auto]'
  end <- '\\end{tikzpicture}'
  
  if (is.null(nodelab)) {
    nodelab <- letters[seq(nodetext)]
  }
  nodes <- paste0('\\node ', ifelse(nodetext=='', '[draw=none] ', '[block]'), '(', nodelab, ') {', nodetext, '};')
  nodes <- structure(nodes, dim=dim(nodetext))
  nodes <- paste(apply(nodes, 1, paste, collapse=' & '), '\\\\')
  matrix <- nodes
  
  startmatrix <- '\\matrix [column sep=0.85in, row sep=0.85in] {'
  endmatrix <- '};'

  path <- nodelink
  startpath <- '\\begin{scope}[on background layer, every path/.style=line]'
  endpath <- '\\end{scope}'
  
  graphic <- c(start, opts, startmatrix, matrix, endmatrix, startpath, path, endpath, end)
  
  write(graphic, file=)
}
##consort(nodetext, nodelab=nodelab, nodelink=nodelink, outfile='Output/consort.tex')


