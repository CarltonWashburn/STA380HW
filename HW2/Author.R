#Code to test reading in files
library(tm)
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
# Remember to source in the "reader" wrapper function

## Rolling two directories together into a single corpus
author_train = Sys.glob('./ReutersC50/C50train/*')
author_test = Sys.glob('./ReutersC50/C50tests/*')
author_dirs = c(author_train,author_test)
#author_dirs = author_dirs[1:2]
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=23)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = file_list

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM # some basic summary statistics

class(DTM)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTM[1:10,1:20])
DTM = removeSparseTerms(DTM, 0.975)
DTM

# Now a dense matrix
X = as.matrix(DTM)
#Creating a list of Authors
authors = levels(as.factor(labels))
head(authors)
# Naive Bayes
i = 0;
w = NULL
smooth_count = 1/nrow(X)
for (author in authors) {
  col = NULL
  train = X[((i*50)+1):((i*50)+50),]
  col = colSums(train + smooth_count)
  names(col) = colnames(train)
  temp = col/sum(col)
  w = cbind(w,temp)
  i = i+1
}
colnames(w) = authors

# fitting the data to the model
fit = matrix(nrow = 2500, ncol = 50)
test = X[2501:5000,]
for (i in seq(1:2500)){
  for ( j in seq(1:50)){
    fit[i,j] = sum(test[i,]*log(w[,j]))
  }
}
colnames(fit) = authors
labels_fit = apply(fit,1,which.max)
author_fit = authors[labels_fit]


#Checking accuracy
good = 0
for (i in seq(1, 2500)){
  if (author_fit[i] == labels[2500+i]){
    good = good + 1
  }
}
good/2500

#Now a different model
library(randomForest)
train = X[1:2500,]
test = X[2501:5000,]

pca = prcomp(train)

test.p = predict(pca,newdata = test)

train.pca = pca$x[,1:100]
forest = randomForest(train.pca,y = as.factor(labels[1:2500]),ntree = 200)

predicted = predict(forest,test.p,type = 'response')
good = 0
for (i in seq(1:2500)){
  if (predicted[i] == labels[2500+i]) {
    good = good + 1
  }
}
good/2500
