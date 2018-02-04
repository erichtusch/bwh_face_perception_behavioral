
## read file
get.beh.file.contents <- function(filepath){
  cat('\nreading file contents from',filepath)
  df=data.frame('text'=NA)
  con = file(filepath, "r")
  i=1
  while ( TRUE ) {
    line = readLines(con, n = 1,skipNul=T)
    if ( length(line) == 0 ) {
      break
    }
    #print(line)
    if(line != ""){
      df[i,1]=line
      i=i+1
    }
  }
  close(con)
  return(df)
}