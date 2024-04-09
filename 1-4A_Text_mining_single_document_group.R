#####################################################################################
## Template code for reproducible scientific text mining. This workflow can be 
## described in the following steps
## 1) Downloading articles as .pdf or abstract&reference data as BibTex
## 2) Loading and preprocessing documents and metadeta
## 3) Processing data
## 4) Analysis and visulation
## Author: N. A. Zondervan & F. T. Zondervan, contact nazondervan@gmail.com, or Github
#####################################################################################

## Uncomment to install all dependencies/packages
#install.packages("pdftools")
#install.packages("tm")
#install.packages ("stringr)
#install.packages ("tidyr)
#install.packages ("stringr")
#install.packages("DataCombine")
#install.packages("ggplot2")
#install.packages("wordcloud2")
#install.packages("pheatmap")
#install.packages("dendextend")
#install.packages("qgraph")
#install.packages("dplyr")
#install.packages("viridis")

## Load all libraries
library("pdftools")
library("tm")
library("stringr")
library("tidyr")
library("DataCombine")
library("ggplot2")
library('wordcloud2')
library("pheatmap")
library("dendextend")
library("qgraph")
library("dplyr")
library('viridis')

## helper function to go from df -> matrix -> triplets
matrix_to_triplets <- function(orig) {
  triplets <- matrix(0, nrow = length(which(!is.na(orig))), ncol = 3)
  count <- 1
  
  for (y in seq_len(nrow(orig))) {
    for (x in seq_len(ncol(orig))) {
      if (!is.na(orig[y, x])) {
        triplets[count, ] <- c(y, x, orig[y, x])
        count <- count + 1
      }
    }
  }
  
  return(triplets)
}


#####################################################################################
## 1A) Load files from multiple pdf's into a single text corpus using the tm library
## for loading abstracts from a dataframe/TSV, scroll down to 1B)
#####################################################################################

# ## Set Working directory containing articles/abstracts, if using RStudio, select in the menu:
# ## Session > SetWorkingDirecotory > SourceFileLocation
# articles_directory = 'articles/' # Directory to put your pdf articles in 
# files <- list.files(path = articles_directory, pattern = "pdf$")
# files <- paste(articles_directory,files,sep='')
# 
# ## Load all PDF's in the directory in a list and store them as as a Tm text corpus
# corpus <-  Corpus(URISource(files),
#                   readerControl = list(reader = readPDF))
# 
# ## Step 1B Load corpus, without filtering or altering text
# corpus_crude <- tm_map(corpus)


#####################################################################################
## 1B) Load text from a dataframe/Tab-Seperated-File, containing for example abstracts
## for loading PDF's scroll up to 1B)
#####################################################################################
## Tm VCorpus function, load a dataframe, col1 must be 'doc_id', col2 must be 'text', rest is metadata, must be utf-8
## load files
df <- read.csv(file='results_per_base_url4.tsv',sep='\t')

## reorder columns to start with doc_id and text, then rest
## rename and reorder columns to comply with tm package. Alternatively use the full title as 'doc_id'
df2 <- rename(df,doc_id=base_url,text=text)
df2 <- df2 %>% select(doc_id, text,everything()) # Change order, to comply with tm

## Load df2 which is now in the right format for the tm package, as corpus
corpus <- VCorpus(DataframeSource(df2))


#####################################################################################
## 2) Loading & pre-processing data
## Load text from corpus into a Term-Document matrix
#####################################################################################
## Step 2, get TDM (Term Document Matrix), also cleans up the text

## Map and clean the corpus text
garbage <- c("â","â_and","â_one","â_the","â_we","â¦","â¢","â©â","ã©ã©n","ã©real","â¬","â","â_the","â_one","â_and","â_we","â¬","â","â©â","ã©ã©n","ã©ã©n","ã©real","ã©n","â","â¦","â","âm","âs","âslow","â","â","â¢","â","â","âcookiesâ","âit","âI","âs","ã¼ber","contact","website","email","store","home","privacy","cookie","cookies")
words_to_remove <- c("etc","ie", "eg", stopwords("english"),garbage)
content.tdm <- TermDocumentMatrix(corpus, 
                                  control = 
                                    list(stopwords = FALSE,
                                         tolower = TRUE,
                                         stemming = FALSE,
                                         removeNumbers = FALSE,
                                         bounds = list(
                                                       removeWords, words_to_remove, ## This line you can manually specify stopwords to drop
                                                       content_transformer(tolower),
                                                       tm_map(corpus, content_transformer(tolower)
                                                       #tm_map(corpus,removePunctuation, preserve_intra_word_dashes = TRUE)
                                                       #removePunctuation(corpus,preserve_intra_word_dashes = TRUE,preserve_intra_word_contractions = TRUE)
                                         ))))


content.tdm <- tm_map(corpus,removePunctuation, preserve_intra_word_dashes = TRUE, preserve_intra_word_contractions = TRUE)
## START ANALYSIS FROM A COPY OF THE AGGREGATED DATA
load(file="data.RData")

## To get a feelling for the data, explore the data
## Have a look at a part or the whole of the Term Document Matrix
#inspect(content.tdm[1:10,])
#View(as.matrix(content.tdm)) # For if you use RStudio

## In case one wants to autmatically determine the optimal number of clusters/groups in a number of articles
## https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters#36729465

## We can normalize the data by making the TDM binary, this corrects for text being of different sizes
## Additionally it is convenient when looking at co-occurance of terms and allows us to perfrom binomial statistics later on
tdm.binary <- content.tdm # Create a copy to binarize
tdm.binary[content.tdm>=1] <- 1 # Replace the count of terms with a 1, only look at presence absence of terms



#####################################################################################
## 3) Processing: Frequency count and creating a Terms-Terms matrix
#####################################################################################
## Check frequent terms in the 'Term Document Matrix'. 
## If needed, use the word list from the frequency count to only select top 100 frequent terms 
## Play with the parameters below and document them in your metahdology
## These treshold should be adjusted to the amount of text you analyse

treshold_low = 0 # Lower treshold, document its value or share the code
treshold_up = Inf # Upper treshold, if there are common words with a high frequency still in your data set, you can filter them out
findFreqTerms(content.tdm, lowfreq = treshold_up, highfreq = treshold_up)

## define a function to get frequent terms from tdm, this time as dataframe
find_freq_terms_fun <- function(tdm){
  freq_terms <- findFreqTerms(content.tdm)[1:max(tdm$nrow)]
  terms_grouped <- tdm[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}

# find_freq_terms_fun2 <- function(data){
#   df_new <- data.frame()
#   df_new$Frequency <- rowSums(data, na.rm=FALSE)
#   df_new
# }
## run the find_freq_terms_fun, get a dataframe of the Frequent terms
df_terms_freq <- find_freq_terms_fun(tdm.binary)
rownames(df_terms_freq) <- rownames(tdm.binary)

############
## Agregate rows with same name, based on manual translation table merging similar words
## Note since text mining in R uses sparse triplet matrixes, back and for conversion triplet_matrix <-> matrix <-> dataframe
#df_terms_freq$name = rownames(df_terms_freq)
df_terms_freq_backup <- df_terms_freq

### TRANSLATING a) AND CONSOLIDATING WORDS
## Some usefull  code if you want to transform words back from their common 'stemming' root to something more readbale in a publication
## Make a dataframe with two collumns, words as they are due to stemming, and words as you would like to see them in your final analysis and plots
rename_words <- read.csv(file='Translation_table_final.txt',sep='\t')
colnames(rename_words)<- c("not_correct","term_en","correct") 
df_terms_freq <- FindReplace(data = df_terms_freq, Var = "Term", replaceData = rename_words,from = "not_correct", to = "correct", exact = TRUE)

## Aggregating b) rows with words that were consolidated are summed together
select_columns = c("Frequency","prop_term_to_total_terms")
df_terms_freq_agg  <- aggregate(df_terms_freq[select_columns],by=df_terms_freq[c('Term')],FUN=sum)
#rownames(df_terms_freq_agg) <- df_terms_freq_agg$Term
#df_terms_freq_agg <- df_terms_freq[, !names(df_terms_freq) %in% c("Term"), drop = F]
rownames(df_terms_freq_agg) <- df_terms_freq_agg$Term
#df_terms_freq_agg %>% select(Term, everything()) #Make sure the Term column is first
df_terms_freq <- df_terms_freq_agg
## Cleaning
df_terms_freq <- df_terms_freq[!(df_terms_freq$Term %in% words_to_remove), ]
############

## Inspect the results. The relative word frequency can be used to crate word clouds, discover trends, compare importance of Terms in multiple groups of articles
head(df_terms_freq)
tail(df_terms_freq)
#View(df_terms_freq)

## IMPORTANT, You can regenerate the TDM, only looking for words you selected based on their frequency,
## to filter you can use 'control = list(dictionary = words_selection)', with words_selection being the selection of words youy are interested in
words_selection <- head(df_terms_freq$Term,50) ## EXAMPLE, take top 50  terms. Youu can also use manually selected Terms, terms of interest to your study!
                     
## Filter both the normal and the binary TDM on words you are interestd in 
tdm.select <- content.tdm[rownames(content.tdm) %in% words_selection,]
tdm.binary.select <- tdm.binary[rownames(tdm.binary) %in% words_selection,]

## Remove NaN, remove rows and columns with only zero values, usefull in case you searched for a Term and it was not found
m <- as.matrix(tdm.binary.select)
m[is.nan(m)] = 0
m <- m[rowSums(m[])>0,]
m <- m[,colSums(m[])>0]



#####################################################################################
## 4) Final analysis and Visualisations
#####################################################################################

#### VISULATION 1 ####
## Create a word cloud with size based on frequency, nice for presentations
## WARNING: The Wordcloud package generates html code, you have to use R-Studio, click on export, to save as .png
treshold <- 50 ## Set this treshold and document it, all words with lower count will not be shown
wordcloud2(df_terms_freq,minSize = 50)

## Look at correlation between terms, first Convert to Term_Term matrix, to see which words are co-occuring a lot
m_backup <- m
term_term_matrix <- m %*% t(m) # Get Term-Term matrix, so how many documents terms co-occur
distance_matrix <- as.matrix(dist(term_term_matrix)) # Euclidian distance, nice for plotting the distance between terms in a heatmap
cor.matrix <- cor(term_term_matrix,method="pearson") # Get the correlation matrix based on correlation/co-occurance of Terms in the the binary matrix

#### VISULATION 2 ####
## Dendrogram
## Plot a dendogram from the hierarchically clustered terms based on the Euclidian distance
## Mostly redundant since dendogram is also present in the heatmap, but you can extract the clustering of terms from the horizontal clustering
m_h_clust_terms <- hclust(dist(m), method = "complete")
png("4A dendogram_horizontally_clustered_terms.png",width = 450, height = 600, units='mm', res = 300)
as.dendrogram(m_h_clust_terms) %>%
  plot(horiz = TRUE)
dev.off() 

#### VISULATION 2 ####
## Plot Terms-Terms matrix, get grouping as annotation from the dendrogram basd on the Euclidian distance
## NOTE: Heatmaps of clustering are one of the most usefull visualisations for accademic analysis and publishing
dist_mat_annotation  <- data.frame(cluster = as.factor(cutree(tree = as.dendrogram(m_h_clust_terms), k = 3)))
dist_mat_annotation$frequency  <-  df_terms_freq$Frequency[match(row.names(distance_matrix),df_terms_freq$Term)]
png("4A Heatmap with horizontal clustering of Terms euclidian distance.png",width = 300, height = 300, units='mm', res = 300)
  pheatmap(distance_matrix, annotation_row = dist_mat_annotation, color = viridis(n=100),fontsize=13)  
dev.off() 

#### VISULATION 2 ####
## Plot Terms-Terms matrix, get grouping as annotation from the dendrogram from the heatmap itself, so based on Pearson correlation
## NOTE: Heatmaps of clustering are one of the most usefull visualisations for accademic analysis and publishing 
result <-  pheatmap(cor.matrix,fontsize=13)
dev.off()
pearson_annotation <- data.frame(cluster =  as.factor(cutree(result$tree_row, k = 3)))
pearson_annotation$frequency  <-  df_terms_freq$Frequency[match(row.names(cor.matrix),df_terms_freq$Term)]
png("4A Heatmap with horizontal clustering of Terms pearson correlation.png",width = 300, height = 300, units='mm', res = 300)
  pheatmap(cor.matrix, annotation_row = pearson_annotation, color = viridis(n=100),fontsize=13)  
dev.off() 

#########################
## Below we will show a couple of example Network graphs
## NOTE, with minimum you can remove weaker nodes, but you have to document the setings for maximum reproducability
## Although network graphs look intersting, in many cases heatmaps are more insightfull and suited for publication
#########################

#### VISULATION 4 ####
## Network visualisation using qqgraph
cor.matrix2 <- cor.matrix

png("4A Network of Terms clustering.png",width = 600, height = 600, units='mm', res = 300)
  qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix), vsize=3,shape='ellipse',legend=TRUE,borders=FALSE,layout = "spring",theme="classic",normalize=TRUE,legend=TRUE)
dev.off() 

#### VISULATION 5 ####
## Network visualisation using qqgraph, with groups and using frequency for the node size
group_annotation <- pearson_annotation$cluster
group_annotation <- setNames(group_annotation, rownames(pearson_annotation)) # Create named list of Term as name and cluster as value
scaling_parameter <- 500 # Tweak this variable, higher for smaller nodes, to make your graph readable
size_annotation <- pearson_annotation$frequency**0.5/10 # Take the root value, so the surface area = frequency, keeps text readable
size_annotation<- setNames(size_annotation, rownames(pearson_annotation)) # Create named list of Term as name and cluster as value

## Plot using the group annotation for color and size annotation for node size. So similar to a WordCloud, but with clustering
png("4A Network of Terms clustering_groups_frequency_size1.png",width = 600, height = 600, units='mm', res = 300)
  qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=size_annotation,shape='ellipse',legend=TRUE,borders=FALSE,layout = "spring",theme="classic",normalize=TRUE,legend=TRUE)
dev.off() 

#### VISULATION 7 ####
## Same as above, but using different layout. We recommend trying a few layouts such as 'sprins', 'circle'
png("4A Network of Terms clustering_groups2.png",width = 600, height = 600, units='mm', res = 300)
  qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=3,shape='ellipse',legend=TRUE,borders=FALSE,layout = "circle",theme="classic",normalize=TRUE)
dev.off()    

####################################################################################################################
### Manual grouping based on: https://repositorio.iscte-iul.pt/bitstream/10071/12132/5/24.pdf
####################################################################################################################

# ## Frazen terms of interest groups
# group_stakeholders <- c("students","employers","universities","accreditation","teachers","content providers","board","association","professional","farmers","fishers","government","employees")
# group_technologies <- c("")
# ## OR grouping based on keywords
# learning <- c("literaci","lifelong","learning","value","creation","innovation","diversity","knowledge","empowerment","skills","motivation","wellbeing","competence","social","individual","development","personal","citizenship","values","awareness","relationships","decision","making")
# digital_platform <- c("digital","technology","platform","certificate","asses","blended","online","offline","gamification","learning","active","peer")
# business_model <- c("open","source","free","pay","micro","credit","degree","training","business","adapt")
# selected_terms <- c(learning,digital_platform,business_model )

##keywords, 5 groups
df_keyword <- read.table(file = 'keywords_sustainability.txt', sep = '\t', header = TRUE)
df_keyword$Keyword <- as.character(df_keyword$Keyword)

## Add KEYWORD GROUPING to column 'group2', select only those in the search terms
#df_terms_freq$group_term <- NA
df_terms_freq$Term <- as.character(df_terms_freq$Term)
df_keyword$Keyword <- as.character(df_keyword$Keyword)
df_terms_freq_terms <- df_terms_freq[which(as.character(df_terms_freq$Term) %in% df_keyword$Keyword),]
df_keyword <- df_keyword[which(df_keyword$Keyword %in% df_terms_freq$Term),]
#Match order, and then simply copy group columns
df_keyword <- df_keyword[order(df_keyword$Keyword), ]
df_terms_freq_terms <- df_terms_freq_terms[order(df_terms_freq_terms$Term),]
df_keyword <- df_keyword[!duplicated(df_keyword$Keyword), ] #drop keywords duplicates
df_terms_freq_terms <- df_terms_freq_terms[!duplicated(df_terms_freq_terms$Term), ] #drop keywords duplicates
df_terms_freq_terms$Group <- df_keyword$Group
df_terms_freq_terms <- df_terms_freq_terms[order(df_terms_freq_terms$prop_term_to_total_terms,decreasing = TRUE), ]
df_terms_freq_terms$Term <- factor(df_terms_freq_terms$Term, levels = df_terms_freq_terms$Term)


### Make a bar graph for each group
df <- df_terms_freq_terms[which(as.character(df_terms_freq_terms$Group) == 'Environmental'),]
png("4A_group1_Environmental_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=Term, y=df$prop_term_to_total_terms,options(scipen = 999))) +
  geom_bar(stat="identity",fill="#F8766D")+
  ggtitle("Environmental",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='blue')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()

df <- df_terms_freq_terms[which(as.character(df_terms_freq_terms$Group) == 'Social'),]
png("4A_group2_Social_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=Term, y=df$prop_term_to_total_terms)) +
  geom_bar(stat="identity",fill="#00B0F6")+
  ggtitle("Social",)+ # for the main title
  #scale_x(labels = ~ format(.x, scientific = FALSE))+
  #scale_x_continuous(labels = scales::comma)+
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='blue')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='#F8766D')+ #add linear trend line
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()

df <- df_terms_freq_terms[which(as.character(df_terms_freq_terms$Group) == 'Economic'),]
png("4A_group3_Economic_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=Term, y=df$prop_term_to_total_terms,options(scipen = 999))) +
  geom_bar(stat="identity",fill="#A3A500")+
  ggtitle("Economic",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  geom_smooth(method=lm,level=0.95,color='#00BF7D')+ #add linear trend line+
  #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()




################## SAME WITH TECHNOLOGY KEYWORDS
df_keyword_tech <- read.table(file = 'keywords_technology.txt', sep = '\t', header = TRUE)
df_keyword_tech$Keyword <- as.character(df_keyword_tech$Keyword)

## Add KEYWORD GROUPING to column 'group2', select only those in the search terms
#df_terms_freq$group_term <- NA
df_terms_freq_terms <- df_terms_freq[which(df_terms_freq$Term %in% df_keyword_tech$Keyword),]
df_keyword_tech <- df_keyword_tech[which(df_keyword_tech$Keyword %in% df_terms_freq$Term),]
#Match order, and then simply copy group columns
df_keyword_tech <- df_keyword_tech[order(df_keyword_tech$Keyword), ]
df_terms_freq_terms <- df_terms_freq_terms[order(df_terms_freq_terms$Term),]
df_keyword_tech <- df_keyword_tech[!duplicated(df_keyword_tech$Keyword), ] #drop keywords duplicates
df_terms_freq_terms <- df_terms_freq_terms[!duplicated(df_terms_freq_terms$Term), ] #drop keywords duplicates
df_terms_freq_terms$Group <- df_keyword_tech$Group
df_terms_freq_terms <- df_terms_freq_terms[order(df_terms_freq_terms$prop_term_to_total_terms,decreasing = TRUE), ]
df_terms_freq_terms$Term <- factor(df_terms_freq_terms$Term, levels = df_terms_freq_terms$Term)

#df <- df_terms_freq_terms[which(as.character(df_terms_freq_terms$Group) == df_keyword_tecch),]
df <- df_terms_freq_terms #No selection since no grouping
png("4A_group4_technologies_terms.png",width=7,height=3,units="in",res=1200)
ggplot(data=df, aes(x=Term, y=df$prop_term_to_total_terms,options(scipen = 999))) +
  geom_bar(stat="identity",fill="grey")+
  ggtitle("Technologies",)+ # for the main title
  ylab("relative frequency")+ # for the x axis label
  #geom_smooth(method=loess,level=0.95,color='red')+ #add linear trend line
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#facet_wrap(~Term, ncol = 4, nrow=5,scales='free_y')
dev.off()


##################################################################

words_selection <- c(df_keyword$Keyword,as.character(df_keyword_tech$Keyword))
lookup_df <- rbind(df_keyword,df_keyword_tech)
### Manual grouping heatmap and  network
## Filter both the normal and the binary TDM on words you are interestd in 
tdm.select <- content.tdm[rownames(content.tdm) %in% words_selection,]
tdm.binary.select <- tdm.binary[rownames(tdm.binary) %in% words_selection,]

## Remove NaN, remove rows and columns with only zero values, usefull in case you searched for a Term and it was not found
m <- as.matrix(tdm.binary.select)
m[is.nan(m)] = 0
m <- m[rowSums(m[])>0,]
m <- m[,colSums(m[])>0]

## Look at correlation between terms, first Convert to Term_Term matrix, to see which words are co-occuring a lot
m_backup <- m
term_term_matrix <- m %*% t(m) # Get Term-Term matrix, so how many documents terms co-occur
distance_matrix <- as.matrix(dist(term_term_matrix)) # Euclidian distance, nice for plotting the distance between terms in a heatmap
cor.matrix <- cor(term_term_matrix,method="pearson") # Get the correlation matrix based on correlation/co-occurance of Terms in the the binary matrix

#### VISULATION 2 ####
#### VISULATION 2 ####
## Plot Terms-Terms matrix, get grouping as annotation from the dendrogram from the heatmap itself, so based on Pearson correlation
## NOTE: Heatmaps of clustering are one of the most usefull visualisations for accademic analysis and publishing 
result <-  pheatmap(cor.matrix,fontsize=13)
dev.off()
pearson_annotation <- data.frame(cluster =  as.factor(cutree(result$tree_row, k = 3)))
pearson_annotation$Frequency  <-  df_terms_freq$Frequency[match(rownames(pearson_annotation),df_terms_freq$Term)]
pearson_annotation$Group <- lookup_df$Group[match(rownames(pearson_annotation),lookup_df$Keyword)]

png("4A Heatmap with horizontal clustering of Terms pearson correlation_cusyom_keywords.png",width = 300, height = 300, units='mm', res = 300)
pheatmap(cor.matrix, annotation_row = pearson_annotation, color = viridis(n=100),fontsize=13)  
dev.off() 
#################

#### Custom_nertwork ####
## Network visualisation using qqgraph, with groups and using frequency for the node size
group_annotation <- pearson_annotation$Group
group_annotation <- setNames(group_annotation, rownames(pearson_annotation)) # Create named list of Term as name and cluster as value
scaling_parameter <- 500 # Tweak this variable, higher for smaller nodes, to make your graph readable
size_annotation <- pearson_annotation$Frequency**0.1*3 # Take the root value, so the surface area = frequency, keeps text readable
size_annotation <- setNames(size_annotation, rownames(pearson_annotation)) # Create named list of Term as name and cluster as value

## Plot using the group annotation for color and size annotation for node size. So similar to a WordCloud, but with clustering
png("4A Network of Terms clustering_groups_frequency_custom1.png",width = 600, height = 600, units='mm', res = 300)
qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=size_annotation,shape='ellipse',legend=TRUE,borders=FALSE,layout = "spring",theme="classic",normalize=TRUE,legend=TRUE)
dev.off() 

#### VISULATION 7 ####
## Same as above, but using different layout. We recommend trying a few layouts such as 'sprins', 'circle'
png("4A Network of Terms clustering_groups_custom2.png",width = 600, height = 600, units='mm', res = 300)
qgraph(cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=3,shape='ellipse',legend=TRUE,borders=FALSE,layout = "circle",theme="classic",normalize=TRUE)
dev.off()   

## Plot using the group annotation for color and size annotation for node size. So similar to a WordCloud, but with clustering
png("4A Network of Terms clustering_groups_frequency_custom3.png",width = 600, height = 600, units='mm', res = 300)
qgraph(graph = "pcor",cor.matrix,minimum=0.5,labels =rownames(cor.matrix),groups=group_annotation, vsize=size_annotation,shape='ellipse',legend=TRUE,borders=FALSE,layout = "spring",theme="classic",normalize=TRUE,legend=TRUE)
dev.off() 

