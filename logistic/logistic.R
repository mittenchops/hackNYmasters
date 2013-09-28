library(rpart)

catdat <- read.csv(file='cats.csv', header=T, stringsAsFactors=F)

head(catdat)
summary(catdat)

# CUT OUT SEMANTIC FIELDS
cdata <- catdat[,-c(2,9)]

last <- dim(cdata)[1]

# CONVERT MM:SS to SECONDS
mmss2sec <- function(x){
    s <- strsplit(x,":")[[1]]
    as.numeric(s[1])*60+as.numeric(s[2])
    }

# LOGISTIC WORKS BETTER WHEN NORMALIZED
normer <- function(x){
    m <- max(x)
    sapply(x, function(y){y/m})
    }

# NORMALIZE
cdata$length <- sapply(cdata$length,mmss2sec)
cdata$length <- normer(cdata$length)
cdata$numofcats <- normer(cdata$numofcats)
cdata$views <- normer(cdata$views)

# Simplify
cdata <- cdata[,-c(2,3,4,6)]
leftout <- cdata[last,]

# logistic regression
lfit <- glm(Y~., data=cdata[-last,], family=binomial(link="logit")) # <- bad model here!
predict(lfit,leftout, type='response')  # notice, does not converge---get more data
# 15 
#  1 

# regression tree
rfit <- rpart(Y ~ ., data=cdata[-last,])
predict(rfit, leftout[,-1])
#        15 
# 0.6428571 

