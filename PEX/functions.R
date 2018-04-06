##HELPERS#############################################################################
# load or install the necessary packages
load_lib <- function(package, update=F) {
  if(update)
    update.packages(ask=FALSE,checkBuilt=TRUE)
  for(i in package){
    if(!require(i,character.only=T)){
      install.packages(i)
      library(i,character.only=T)
    }
  }
}

# The CKD-EPI equation, expressed as a single equation, is:
#
#   GFR = 141 * min(Scr/κ,1)^α * max(Scr/κ, 1)^-1.209 * 0.993^Age * 1.018 [if female] * 1.159 [if black]
#
# Scr is serum creatinine (mg/dL), κ is 0.7 for females and 0.9 for males, α is -0.329 for females and -0.411 for
# males,
# min indicates the minimum of Scr/κ or 1, and max indicates the maximum of Scr/κ or 1.
ckd.epi<-function(creatinin,age,sex){
  den<-ifelse(sex=="M",0.9,0.7)
  alpha<-ifelse(sex=="M",-0.411,-0.329)
  mul<-ifelse(sex=="M",1,1.018)
  gfr<-141*(min(creatinin/den,1)^alpha)*(max(creatinin/den,1)^-1.209)*(0.993^age)*mul
  return(gfr)
}
