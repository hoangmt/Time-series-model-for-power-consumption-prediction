azure_blob_call <- function(url, verb, key, requestBody=NULL, headers=NULL, ifMatch="", md5="",debug=FALSE) { 
  print('Key is as follows:------------------------')
  #print(key)
  #sak= 'e2VxYkn7AlsHInKUbbjpSWa1qdewR92dOS/loVmvc/KuU8V6CxejJZJQZ8XvL51LpZdAaJi8TqYvBIsH9nk5og=='
  #url ='https://esmartprodstorage.blob.core.windows.net/pings?restype=container&comp=list'
  #saname ='esmartprodstorage'
  #example upload to blob2:
  #azureBlobCall("https://esmartprodstorage.blob.core.windows.net/test1/blob2", "PUT", sak, 
  #headers = c("x-ms-blob-type"="BlockBlob"), requestBody = "Hej faiuaiofiadfudisjfaljfdkajfasjfdsakljlfafkdafldsalfjaslkfdsajkflajflakslfdasjlfadskjffaifu v�rlden!") #upload_file("blob.txt"))
  urlcomponents <- httr::parse_url(url)
  account <- gsub(".blob.core.windows.net", "", urlcomponents$hostname, fixed = TRUE)
  container <- urlcomponents$path
  
  # get timestamp in us locale
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "us")
  `x-ms-date` <- format(Sys.time(),"%a, %d %b %Y %H:%M:%S %Z", tz="GMT")
  Sys.setlocale("LC_TIME", lct)
  
  # if requestBody exist get content length in bytes and content type
  `Content-Length` <- ""; `Content-Type` <- ""
  if(!is.null(requestBody)) {
    if(class(requestBody) == "form_file") {
      `Content-Length` <- (file.info(requestBody$path))$size
      `Content-Type` <- requestBody$type 
    } else {
      requestBody <- enc2utf8(as.character(requestBody))
      `Content-Length` <- nchar(requestBody, "bytes")
      `Content-Type` <- "text/plain; charset=UTF-8" 
    }
  } 
  
  # combine timestamp and version headers with any input headers, order and create the CanonicalizedHeaders
  headers <- setNames(c(`x-ms-date`, "2015-04-05",  unlist(headers)), 
                      c("x-ms-date", "x-ms-version", unclass(names(unlist(headers)))))
  headers <- headers[order(names(headers))]
  CanonicalizedHeaders <- paste(names(headers), headers, sep=":", collapse = "\n")
  
  # create CanonicalizedResource headers and add any queries to it
  if(!is.null(urlcomponents$query)) {
    components <- setNames(unlist(urlcomponents$query), unclass(names(unlist(urlcomponents$query))))
    componentstring <- paste0("\n", paste(names(components[order(names(components))]),
                                          components[order(names(components))], sep=":", collapse = "\n"))
  } else componentstring <- ""
  CanonicalizedResource <- paste0("/",account,"/",container, componentstring)
  
  # create the authorizationtoken
  signaturestring <- paste0(verb, "\n\n\n", `Content-Length`, "\n", md5, "\n", `Content-Type`, "\n\n\n", 
                            ifMatch, "\n\n\n\n", CanonicalizedHeaders, "\n", CanonicalizedResource)
  print('1111111')
  requestspecificencodedkey <- RCurl::base64(
    digest::hmac(key=RCurl::base64Decode(key, mode="raw"),
                 object=enc2utf8(signaturestring),
                 algo= "sha256", raw=TRUE)
  )
  print('2222222222222, requestspecificencodedkey')
  #print(requestspecificencodedkey)
  authorizationtoken <- paste0("SharedKey ", account, ":", requestspecificencodedkey)
  
  # make the call. These two commands also output the data to stdout
  headers_final <- add_headers(Authorization=authorizationtoken, headers, `Content-Type` = `Content-Type`)
  if (debug)
    call <- httr::VERB(verb=verb, url=url, config=headers_final, body=requestBody, verbose())
  else
    call <- httr::VERB(verb=verb, url=url, config=headers_final, body=requestBody)
  print('3333333333333, below his headers_final')
  #print(headers_final)
  #print('url:')
  #print(url)
  #print("signaturestring");print(signaturestring); 
  #print(headers_final); 
  print('Finished accessing the blob')

  return(content(call))
}
