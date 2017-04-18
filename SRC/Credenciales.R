library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "H4w4mdUSP3Fht1MltVnLYtXXX"
consumerSecret <- "lhcUfQoQxpFJSaJsvJdNDHCieFdjVASHAbPNklLNmP4RjmtXXX"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
                             requestURL=requestURL, accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src')
save(my_oauth, file="my_oauth")

tweets <- searchTwitter("from:@deusto", n=1000,since="2016-01-01")

###########################################################################

for (i in 1:20) {
  if (i==1) search = searchTwitter("from:@deusto",n=20, since='2014-04-15')
  else search = searchTwitter("from:@deusto",n=20, since='2014-04-15', sinceID=search[[1]]$id)
  print(search)
  Sys.sleep(20)
}
