
# FIRST TIME USER, MAKE SURE YOU HAVE LIBRARIES
# OTHERWISE, UNCOMMENT AND RUN install BELOW
# install.packages("rpart")
# install.packages("cvTools")
# install.packages("tm")
# install.packages("plyr")
# install.packages("SnowballC")
# library(rpart)
# library(plyr)


############################
# PART - 0a IMPORTS
############################
library(cvTools)
library(tm)
library(kernlab)
library(SnowballC)


######!!!IMPORTANT!!!#######
# TO USER - YOU NEED TO
# PUT THE 3 files
#   Spam_detector.R
#   unknown_emails.txt
#   known_all.txt
# Into your own /Users/.../path-to-folder
# And update the setwd line below
############################


# Set directory and load data
setwd("/Users/toshiA/Documents/data")

# The unknown file (ONLY GIVE TO YOU ON TOURNAMENT DAY)
TOURN_EMAIL_FILENAME = 'unknown_emails.txt'

############################
# PART - 0b GETTING READY
############################

# Load the KNOWN data, exactly as appears on Word Doc
f1 = 'known_all.txt'
allemail = readChar(f1,file.info(f1)$size)
allemail = tolower(allemail)

# Load the UNKNOWN (tournament) data
f2 = TOURN_EMAIL_FILENAME
allemail_c = readChar(f2,file.info(f2)$size)
allemail_c = tolower(allemail_c)


############################
# PART - 1
############################

#------------------------
# GETTING READY
# Define delimiter functions etc
#------------------------

delimit_email_regex = "email #([[:digit:]]{1,2})"
email_parser=function(x) unlist(strsplit(x, delimit_email_regex))
space_tokenizer = function(x) unlist(strsplit(x, "[[:space:]]+"))



#----------------
# Create FIRST DTF data
# Similar to A3, i.e. "count","buy","custom","look","price","product"
# PLUS add EXCLM
#----------------

raw_email_arr = email_parser(allemail)
raw_email_arr_c = email_parser(allemail_c) #####fixed from compmail

DIC_ASG_ORI = c("buy","custom","look","price","product", "exclm")
LEN_DIC_ORI = length(DIC_ASG_ORI) # 5+1 for asg list

#DATA CLEANING
# at end of above, you get array[61] because of the "30 good/non-spam..." headers
raw_email_arr=raw_email_arr[-1]
raw_email_arr_c=raw_email_arr_c[-1]

# numbers of TOURNAMENT emails to be processed:
LENEM = length(raw_email_arr_c)


#PREPARING THE DTF matrix
COL_NAMES = c("count", DIC_ASG_ORI)
Dtf = matrix(0,nrow=60,ncol=length(COL_NAMES),dimnames=list( c(1:60), COL_NAMES))
Dtf_c = matrix(0,nrow=LENEM,ncol=length(COL_NAMES),dimnames=list( c(1:LENEM), COL_NAMES))

MaxAnyTerm = matrix(0, nrow=60, ncol=1, dimnames=list(c(1:60), c("max")))
MaxAnyTerm_c = matrix(0, nrow=LENEM, ncol=1, dimnames=list(c(1:LENEM), c("max")))

#Make DTF for the 60 known (non tournament) emails
for (i in 1:60) {
    
    #TOURNAMENT ADDITION: count each "!" as extra word "exclm"
    raw_email_arr[i] = gsub("!", " exclm", raw_email_arr[i])
    
    email_i_array_token = space_tokenizer(raw_email_arr[i])

    len = length(email_i_array_token)
    wordcount = 0
    for(j in 1:len) {
        if (email_i_array_token[j] != "") {
            wordcount = wordcount + 1
        }
    }
    Dtf[i,1] = wordcount
    

    eic = Corpus(VectorSource(email_i_array_token))
    eic_clean = tm_map(eic, removePunctuation)
    eic_stem = tm_map(eic_clean, stemDocument)
    
    
    document = paste(c(eic_stem),collapse=" ")
    myCorpus = Corpus(VectorSource(document))
    tf=termFreq(myCorpus[[1]])
    MaxAnyTerm[i,"max"] = max(tf)
    
    Dtf[i,2] = tf[COL_NAMES[2]]
    Dtf[i,3] = tf[COL_NAMES[3]]
    Dtf[i,4] = tf[COL_NAMES[4]]
    Dtf[i,5] = tf[COL_NAMES[5]]
    Dtf[i,6] = tf[COL_NAMES[6]]
    Dtf[i,7] = tf[COL_NAMES[7]]
    
}

#Make DTF_C for the LENEM unknown (tournament) emails
for (i in 1:LENEM) {
  raw_email_arr_c[i] = gsub("!", " exclm", raw_email_arr_c[i])
  
  email_i_array_token_c = space_tokenizer(raw_email_arr_c[i])
  
  len_c = length(email_i_array_token_c)
  wordcount = 0
  for(j in 1:len_c) {
    if (email_i_array_token_c[j] != "") {
      wordcount = wordcount + 1
    }
  }
  Dtf_c[i,1] = wordcount
  
  eic_c = Corpus(VectorSource(email_i_array_token_c))
  eic_clean_c = tm_map(eic_c, removePunctuation)
  eic_stem_c = tm_map(eic_clean_c, stemDocument)
  
  document_c = paste(c(eic_stem_c),collapse=" ")
  myCorpus_c = Corpus(VectorSource(document_c))
  tf_c=termFreq(myCorpus_c[[1]])
  MaxAnyTerm_c[i,"max"] = max(tf_c)
  
  Dtf_c[i,2] = tf_c[COL_NAMES[2]]
  Dtf_c[i,3] = tf_c[COL_NAMES[3]]
  Dtf_c[i,4] = tf_c[COL_NAMES[4]]
  Dtf_c[i,5] = tf_c[COL_NAMES[5]]
  Dtf_c[i,6] = tf_c[COL_NAMES[6]]
  Dtf_c[i,7] = tf_c[COL_NAMES[7]]
}

# Convert DTF NA to zero
Dtf[is.na(Dtf)] <-0
Dtf_c[is.na(Dtf_c)] <-0

# Update DTF to use AUG
for(i in 1:60) {
    
    MaxForMe = MaxAnyTerm[i,"max"]
    
    for(j in 2:length(COL_NAMES)) {
        Dtf[i,j] = (0.5 + (0.5*Dtf[i,j])/MaxForMe)
    }
    
}

# Update DTF_C to use AUG
for(i in 1:LENEM) {
  
  MaxForMe_c = MaxAnyTerm_c[i,"max"]
  
  for(j in 2:length(COL_NAMES)) {
    Dtf_c[i,j] = (0.5 + (0.5*Dtf_c[i,j])/MaxForMe_c)
  }
  
}


#----------------
# SECOND DTF - using dynamically-found
# Most common words that distinguish GOOD vs SPAM
# In this one we did not change ! to EXCLM
#----------------

MaxAnyTerm = matrix(0, nrow=60, ncol=1, dimnames=list(c(1:60), c("max")))
MaxAnyTerm_c = matrix(0, nrow=LENEM, ncol=1, dimnames=list(c(1:LENEM), c("max")))

copy2_email_arr = email_parser(allemail)
copy2_email_arr = copy2_email_arr[-1] #-1 becasue first line is Raymond's instruction

copy2_email_arr_c = email_parser(allemail_c)
copy2_email_arr_c = copy2_email_arr_c[-1] #-1 becasue first line is Raymond's instruction

CUR_LEN = 0
STARTTHRESHOLD = 20
i=1
for (i in 1:20) {
    
    MYTHRESHOLD = STARTTHRESHOLD - i
    
    #Find most frequent terms in GOOD emails
    copy2_good_email = copy2_email_arr[1:30] ##TODO
    cge2_corpus = Corpus(VectorSource(copy2_good_email))
    cge2_corpus_processed = tm_map(cge2_corpus, stemDocument)
    cge2_corpus_processed = tm_map(cge2_corpus_processed, removeWords, stopwords("english"))
    dtm2_good = DocumentTermMatrix(cge2_corpus_processed)
    fw_good = findFreqTerms(dtm2_good, MYTHRESHOLD)
    
    
    #Find most frequent terms in SPAM emails
    copy2_spam_email = copy2_email_arr[30:60] ###TODO DO NOT HARDCODE
    cse2_corpus = Corpus(VectorSource(copy2_spam_email))
    cse2_corpus_processed = tm_map(cse2_corpus, stemDocument)
    cse2_corpus_processed = tm_map(cse2_corpus_processed, removeWords, stopwords("english"))
    dtm2_spam = DocumentTermMatrix(cse2_corpus_processed)
    fw_spam = findFreqTerms(dtm2_spam, MYTHRESHOLD)
    
    #Find UNION, INTERSECT, DIFF
    fw_union = union(fw_good, fw_spam)
    fw_intersect = intersect(fw_good, fw_spam)
    fw_proposed = setdiff(fw_union, fw_intersect)
    
    CUR_LEN = length(fw_proposed)
    print("LEN is")
    print(CUR_LEN)
    print("and th is")
    print(MYTHRESHOLD)
    
    if (CUR_LEN > 8) {
        break
    }
    
}

DIC_ATTEMPT2 = fw_proposed
LEN_DIC_ATTEMPT2 = length(fw_proposed)
Dtf2 = matrix(0,nrow=60,ncol=length(DIC_ATTEMPT2),dimnames=list( c(1:60), DIC_ATTEMPT2))
Dtf2_c = matrix(0,nrow=LENEM,ncol=length(DIC_ATTEMPT2),dimnames=list( c(1:LENEM), DIC_ATTEMPT2))

for (i in 1:60) {
    
    email_i_array_token = space_tokenizer(copy2_email_arr[i])
    
    # make corpus of e.g. 49 text documents (wow, you treat each word as document)
    # this is because I find STEMMING and REMOVING only works well this way
    eic = Corpus(VectorSource(email_i_array_token))
    eic_clean = tm_map(eic, removePunctuation)
    eic_stem = tm_map(eic_clean, stemDocument)

    # We combine the xx words in ONE email back into one big corpus
    document = paste(c(eic_stem),collapse=" ")
    myCorpus = Corpus(VectorSource(document))
    tf=termFreq(myCorpus[[1]])
    
    MaxAnyTerm[i,"max"] = max(tf)
    
    for (j in 1:LEN_DIC_ATTEMPT2) {
        Dtf2[i,j] = tf[DIC_ATTEMPT2[j]]
    }
}

for (i in 1:LENEM) {
  
  email_i_array_token_c = space_tokenizer(copy2_email_arr_c[i])
  
  eic_c = Corpus(VectorSource(email_i_array_token_c))
  eic_clean_c = tm_map(eic_c, removePunctuation)
  eic_stem_c = tm_map(eic_clean_c, stemDocument)
    
  document_c = paste(c(eic_stem_c),collapse=" ")
  myCorpus_c = Corpus(VectorSource(document_c))
  tf_c=termFreq(myCorpus_c[[1]])
  
  MaxAnyTerm_c[i,"max"] = max(tf_c)
  
  for (j in 1:LEN_DIC_ATTEMPT2) {
    Dtf2_c[i,j] = tf_c[DIC_ATTEMPT2[j]]
  }
}

Dtf2[is.na(Dtf2)] <-0
Dtf2_c[is.na(Dtf2_c)] <-0

# Update to use AUG
for(i in 1:60) {
    
    MaxForMe = MaxAnyTerm[i,"max"]
    
    for(j in 1:length(fw_proposed)) {
        Dtf2[i,j] = (0.5 + (0.5*Dtf2[i,j])/MaxForMe)
    }
    
}

for(i in 1:LENEM) {
  
  MaxForMe_c = MaxAnyTerm_c[i,"max"]
  
  for(j in 1:length(fw_proposed)) {
    Dtf2_c[i,j] = (0.5 + (0.5*Dtf2_c[i,j])/MaxForMe_c)
  }
  
}



#----------------
# THIRD DTF - only using dynamic features from SPAM
# In this one we did not change ! to EXCLM
#----------------
MaxAnyTerm = matrix(0, nrow=60, ncol=1, dimnames=list(c(1:60), c("max")))
MaxAnyTerm_c = matrix(0, nrow=LENEM, ncol=1, dimnames=list(c(1:LENEM), c("max")))

copy3_email_arr = email_parser(allemail)
copy3_email_arr = copy3_email_arr[-1]

copy3_email_arr_c = email_parser(allemail_c)
copy3_email_arr_c = copy3_email_arr_c[-1]

CUR_LEN = 0
STARTTHRESHOLD = 20
i=1
for (i in 1:20) {
    
    MYTHRESHOLD = STARTTHRESHOLD - i
    

    #Find most frequent terms in SPAM emails
    copy3_spam_email = copy3_email_arr[30:60] ###TODO DO NOT HARDCODE
    cse3_corpus = Corpus(VectorSource(copy3_spam_email))
    cse3_corpus_processed = tm_map(cse3_corpus, stemDocument)
    cse3_corpus_processed = tm_map(cse3_corpus_processed, removeWords, stopwords("english"))
    dtm3_spam = DocumentTermMatrix(cse3_corpus_processed)
    
    fw_spam = findFreqTerms(dtm3_spam, MYTHRESHOLD)
 
    CUR_LEN = length(fw_spam)
    print("LEN is")
    print(CUR_LEN)
    print("and th is")
    print(MYTHRESHOLD)
    
    if (CUR_LEN > 8) {
        break
    }
    
}

DIC_ATTEMPT3 = fw_spam
LEN_DIC_ATTEMPT3 = length(fw_spam)
Dtf3 = matrix(0,nrow=60,ncol=length(DIC_ATTEMPT3),dimnames=list( c(1:60), DIC_ATTEMPT3))
Dtf3_c = matrix(0,nrow=LENEM,ncol=length(DIC_ATTEMPT3),dimnames=list( c(1:LENEM), DIC_ATTEMPT3))

for (i in 1:60) {
    
    email_i_array_token = space_tokenizer(copy3_email_arr[i])
    
    # make corpus of e.g. 49 text documents (wow, you treat each word as document)
    # this is because I find STEMMING and REMOVING only works well this way
    eic = Corpus(VectorSource(email_i_array_token))
    eic_clean = tm_map(eic, removePunctuation)
    eic_stem = tm_map(eic_clean, stemDocument)
    
    # Now, after all the trouble having one-email corpus of 49 text docs
    # We combine the 49 words back into one big corpus
    # For counting term frequency of "buy" etc
    document = paste(c(eic_stem),collapse=" ")
    myCorpus = Corpus(VectorSource(document))
    tf=termFreq(myCorpus[[1]])
    MaxAnyTerm[i,"max"] = max(tf)
    
    for (j in 1:LEN_DIC_ATTEMPT3) {
        Dtf3[i,j] = tf[DIC_ATTEMPT3[j]]
    }
    
}

for (i in 1:LENEM) {
  
  email_i_array_token_c = space_tokenizer(copy3_email_arr_c[i])
  
  eic_c = Corpus(VectorSource(email_i_array_token_c))
  eic_clean_c = tm_map(eic_c, removePunctuation)
  eic_stem_c = tm_map(eic_clean_c, stemDocument)
  
  document_c = paste(c(eic_stem_c),collapse=" ")
  myCorpus_c = Corpus(VectorSource(document_c))
  tf_c=termFreq(myCorpus_c[[1]])
  MaxAnyTerm_c[i,"max"] = max(tf_c)
  
  for (j in 1:LEN_DIC_ATTEMPT3) {
    Dtf3_c[i,j] = tf_c[DIC_ATTEMPT3[j]]
  }
  
}


Dtf3[is.na(Dtf3)] <-0
Dtf3_c[is.na(Dtf3_c)] <-0

for(i in 1:60) {
    
    MaxForMe = MaxAnyTerm[i,"max"]
    
    for(j in 1:LEN_DIC_ATTEMPT3) {
        Dtf3[i,j] = (0.5 + (0.5*Dtf3[i,j])/MaxForMe)
    }
    
}

for(i in 1:LENEM) {
  
  MaxForMe_c = MaxAnyTerm_c[i,"max"]
  
  for(j in 1:LEN_DIC_ATTEMPT3) {
    Dtf3_c[i,j] = (0.5 + (0.5*Dtf3_c[i,j])/MaxForMe_c)
  }
  
}


############################
# PART - 2
############################


#----------------
# BUILD MODEL WITH Dtf, Dtf2, Dtf3
# Use Ensemble to predict the UNKNWON TEST EMAIL
#----------------


# Known emails that go into my learning
zdata1=Dtf
zdata2=Dtf2
zdata3=Dtf3
zlabel=matrix(c(rep(1,30),rep(-1,30)))

set.seed(340)
svm_model1 = ksvm(zdata1, zlabel,type="C-svc",kernel='vanilladot',C=100, scaled=c())
svm_model2 = ksvm(zdata2, zlabel,type="C-svc",kernel='vanilladot',C=100, scaled=c())
svm_model3 = ksvm(zdata3, zlabel,type="C-svc",kernel='vanilladot',C=100, scaled=c())

# Unknown emails to test
unkdata1 = Dtf_c
unkdata2 = Dtf2_c
unkdata3 = Dtf3_c

# Use our model to predict the unknown response labels
pred_label1=predict(svm_model1,unkdata1)
pred_label2=predict(svm_model2,unkdata2)
pred_label3=predict(svm_model3,unkdata3)


# Build an overall prediction by MAJORITY VOTE ensemble
pred_label_final = cbind(pred_label1, pred_label2, pred_label3)

final_answer = pred_label_final[,1] + pred_label_final[,2] + pred_label_final[,3]
for (i in 1:length(final_answer)) {
    if (final_answer[i] > 0)
        final_answer[i] = 1
    else
        final_answer[i] = -1
    
}
print("FINAL ANSWER IS")
print(final_answer)



####################################################
####################################################
####################################################

## MORE ELABORATE DOCUMENTATION IN PROGRESS

####################################################
####################################################
####################################################





