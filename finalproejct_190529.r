library(ggplot2)
library(ggwordcloud)

einstein <- scan( "http://www.gutenberg.org/files/5001/5001-h/5001-h.htm" , what = character() )
galilei <- scan( "http://www.gutenberg.org/cache/epub/37729/pg37729.txt" , what = character() )
huygens <- scan( "http://www.gutenberg.org/cache/epub/14725/pg14725.txt" , what = character() )
tesla <- scan( "http://www.gutenberg.org/cache/epub/13476/pg13476.txt" , what = character() )

remove_punct <- function( str ) {
  tmp <- gsub( "[[:punct:][:blank:]]+" , "" , str )
  tmp <- unlist( apply( as.matrix( tmp ) , c(1,2) , strsplit , split = "\n" ) )
  tmp <- tmp[ tmp != "" ]
  res <- tolower( tmp )
  return( res )
}

tmp_einstein <- remove_punct( einstein )
tmp_galilei <- remove_punct( galilei )
tmp_huygens <- remove_punct( huygens )
tmp_tesla <- remove_punct( tesla )

length( tmp_einstein ); head( tmp_einstein , 20 )
length( tmp_galilei ); head( tmp_galilei , 20 )
length( tmp_huygens ); head( tmp_huygens , 20 )
length( tmp_tesla ); head( tmp_tesla , 20 )

tab_einstein <- table( tmp_einstein ) ; tab_einstein <- sort( tab_einstein , decreasing = TRUE )
tab_galilei <- table( tmp_galilei ) ; tab_galileo <- sort( tab_galilei , decreasing = TRUE )
tab_huygens <- table( tmp_huygens ) ; tab_huygens <- sort( tab_huygens , decreasing = TRUE )
tab_tesla <- table( tmp_tesla ) ; tab_tesla <- sort( tab_tesla , decreasing = TRUE )



author <- rep( c( "Einstein" , "Galilei" , "Huygens" , "Tesla" ) , c( length( tab_einstein ) , length( tab_galilei ) , length( tab_huygens ) , length( tab_tesla ) ) )
word <- c( names( tab_einstein ) , names( tab_galilei ) , names( tab_huygens ) , names( tab_tesla ) )
count <- c( tab_einstein , tab_galilei , tab_huygens , tab_tesla )

tf_einstein <- tab_einstein / sum( tab_einstein )
tf_galilei <- tab_galilei / sum( tab_galilei )
tf_huygens <- tab_huygens / sum( tab_huygens )
tf_tesla <- tab_tesla / sum( tab_tesla )
tf <- c( tf_einstein , tf_galilei , tf_huygens , tf_tesla )

physics <- data.frame( author = author , word = word , count = count , tf = tf , stringsAsFactors = FALSE )

idf_fun <- function( word ) {
  idx <- physics$word %in% word
  idf <- log( 4 / sum(idx) )
  return( idf )
} #tf_idf 가중치 함수 설정

idf <- sapply( physics$word , FUN = idf_fun )

physics$idf <- idf
physics$tf_idf <- physics$tf * physics$idf

idx <- tapply( physics$tf_idf , physics$author , FUN = order , decreasing = TRUE )

tf_idf_einstein <- physics[ physics$author == "Einstein" , ][ idx$Einstein[1:10] ,]
tf_idf_galilei <- physics[ physics$author == "Galilei" , ][ idx$Galilei[1:10] ,]
tf_idf_huygens <- physics[ physics$author == "Huygens" , ][ idx$Huygens[1:10] ,]
tf_idf_tesla <- physics[ physics$author == "Tesla" , ][ idx$Tesla[1:10] ,]

physics <- rbind( tf_idf_einstein , tf_idf_galilei , tf_idf_huygens , tf_idf_tesla )

p <- ggplot( physics , aes( x = factor(word , levels = word ) , y = tf_idf , fill = author ) ) + geom_col( show.legend = FALSE )
p + facet_wrap( ~ author , scales = "free" ) + coord_flip() + theme( axis.title.y = element_blank() )