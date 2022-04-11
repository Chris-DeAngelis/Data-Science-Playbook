library(RCurl)
library(httr)
library(jsonlite)
#https://martinctc.github.io/blog/vignette-scraping-amazon-reviews-in-r/
#https://developer.amazon.com/docs/reports-promo/reporting-API.html#map-the-security-profile-to-the-api

#Security Profile ID amzn1.application.b57b186553a74c8689ae26c7861fe476
#Client ID amzn1.application-oa2-client.e6570edab728414c82dcd6f7d1140b81
# Client Secret aa747d2ae16bcc9a7fb8f5fb68794f3b7612f2aca99a0fef752f0fa7f6ba34b9

#Client ID	amzn1.application-oa2-client.5c5cf3d4b9c14734b18e26cba3e3c920
#Client Secret	eee2b4ab52c32870f6cff7597ae3ac3c8ec7642ad312a099bb78a4a350da7725


#Security Profile Name	MF_Funnel_Security
#Security Profile Description	Marketing Funnel App Security
#Security Profile ID	amzn1.application.db01475cd3e64e37aed5e27188e7a834
##################################### CDD Attempt #####################################

url <- "https://api.amazon.com/auth/o2/token"
header <- "Content-Type: application/x-www-form-urlencoded"
content <- 
  
check <- POST(url, 
              add_headers(header), 
              encode = "json")

curl -k -X POST -H 'Content-Type: application/x-www-form-urlencoded' -d 'grant_type=client_credentials&client_id=amzn1.application-oa2-client.5c1462ee102c4a57a5224d0c72118741&client_secret=15d1829ddf4f12d1c5d425e57e5ca081d0f7a63bd94c9e142ff8b20d9de880a4&scope=appstore::apps:readwrite' https://api.amazon.com/auth/O2/token
GET(url, add_headers('x-api-key'= 'xQNG1cotzh5oqbiYBLyii4hjIBdmwN8a2WpTB10n'))

  url <- paste("https://ybz56ec10a.execute-api.us-east-1.amazonaws.com/prod/product/",unique_tbs[i])
  con <- GET(url, add_headers('x-api-key'= 'xQNG1cotzh5oqbiYBLyii4hjIBdmwN8a2WpTB10n'))
  res <- con %>% httr::content(con, as = 'text') %>% 
    fromJSON()
  
  {
    "grant_type": "client_credentials",
    "client_id": "amzn1.application-oa2-client.ae941846cdd745e9a53319f7bb98d435",
    "client_secret": "41d135b2b02ce5f2fbf7643a66477c089fcc1d88d11f69d3e4a6285b917ca35d",    
    "scope": "appstore::apps:readwrite"
    }


##################################### Notes #####################################

search.amazon <- function(Keywords, SearchIndex = 'All', AWSAccessKeyId, AWSsecretkey, AssociateTag, ResponseGroup = 'Small', Operation = 'ItemSearch'){
  library(digest)
  library(RCurl)
  
  base.html.string <- "http://ecs.amazonaws.com/onca/xml?"
  SearchIndex <- match.arg(SearchIndex, c('All',
                                          'Apparel',
                                          'Appliances',
                                          'ArtsAndCrafts',
                                          'Automotive',
                                          'Baby',
                                          'Beauty',
                                          'Blended',
                                          'Books',
                                          'Classical',
                                          'DigitalMusic',
                                          'DVD',
                                          'Electronics',
                                          'ForeignBooks',
                                          'Garden',
                                          'GourmetFood',
                                          'Grocery',
                                          'HealthPersonalCare',
                                          'Hobbies',
                                          'HomeGarden',
                                          'HomeImprovement',
                                          'Industrial',
                                          'Jewelry',
                                          'KindleStore',
                                          'Kitchen',
                                          'Lighting',
                                          'Magazines',
                                          'Marketplace',
                                          'Miscellaneous',
                                          'MobileApps',
                                          'MP3Downloads',
                                          'Music',
                                          'MusicalInstruments',
                                          'MusicTracks',
                                          'OfficeProducts',
                                          'OutdoorLiving',
                                          'Outlet',
                                          'PCHardware',
                                          'PetSupplies',
                                          'Photo',
                                          'Shoes',
                                          'Software',
                                          'SoftwareVideoGames',
                                          'SportingGoods',
                                          'Tools',
                                          'Toys',
                                          'UnboxVideo',
                                          'VHS',
                                          'Video',
                                          'VideoGames',
                                          'Watches',
                                          'Wireless',
                                          'WirelessAccessories'))
  Operation <- match.arg(Operation, c('ItemSearch',
                                      'ItemLookup',
                                      'BrowseNodeLookup',
                                      'CartAdd',
                                      'CartClear',
                                      'CartCreate',
                                      'CartGet',
                                      'CartModify',
                                      'SimilarityLookup'))
  ResponseGroup <- match.arg(ResponseGroup, c('Accessories',
                                              'AlternateVersions',
                                              'BrowseNodeInfo',
                                              'BrowseNodes',
                                              'Cart',
                                              'CartNewReleases',
                                              'CartTopSellers',
                                              'CartSimilarities',
                                              'Collections',
                                              'EditorialReview',
                                              'Images',
                                              'ItemAttributes',
                                              'ItemIds',
                                              'Large',
                                              'Medium',
                                              'MostGifted',
                                              'MostWishedFor',
                                              'NewReleases',
                                              'OfferFull',
                                              'OfferListings',
                                              'Offers',
                                              'OfferSummary',
                                              'PromotionSummary',
                                              'RelatedItems',
                                              'Request',
                                              'Reviews',
                                              'SalesRank',
                                              'SearchBins',
                                              'Similarities',
                                              'Small',
                                              'TopSellers',
                                              'Tracks',
                                              'Variations',
                                              'VariationImages',
                                              'VariationMatrix',
                                              'VariationOffers',
                                              'VariationSummary'),
                             several.ok = TRUE)
  version.request = '2011-08-01'
  Service = 'AWSECommerceService'
  if(!is.character(AWSsecretkey)){
    message('The AWSsecretkey should be entered as a character vect, ie be qouted')
  }
  
  pb.txt <- Sys.time()
  
  pb.date <- as.POSIXct(pb.txt, tz = Sys.timezone)
  
  Timestamp = strtrim(format(pb.date, tz = "GMT", usetz = TRUE, "%Y-%m-%dT%H:%M:%S.000Z"), 24)
  
  str = paste('GET\necs.amazonaws.com\n/onca/xml\n',
              'AWSAccessKeyId=', curlEscape(AWSAccessKeyId),
              '&AssociateTag=', AssociateTag,
              '&Keywords=', curlEscape(Keywords),
              '&Operation=', curlEscape(Operation),
              '&ResponseGroup=', curlEscape(ResponseGroup),
              '&SearchIndex=', curlEscape(SearchIndex),
              '&Service=AWSECommerceService',
              '&Timestamp=', gsub('%2E','.',gsub('%2D', '-', curlEscape(Timestamp))),
              '&Version=', version.request,
              sep = '')
  
  ## signature test
  Signature = curlEscape(base64(hmac( enc2utf8((AWSsecretkey)), enc2utf8(str1), algo = 'sha256', serialize = FALSE,  raw = TRUE)))
  
  AmazonURL <- paste(base.html.string,
                     'AWSAccessKeyId=', AWSAccessKeyId,
                     '&AssociateTag=', AssociateTag,
                     '&Keywords=', Keywords,
                     '&Operation=',Operation,
                     '&ResponseGroup=',ResponseGroup,
                     '&SearchIndex=', SearchIndex,
                     '&Service=AWSECommerceService',
                     '&Timestamp=', Timestamp,
                     '&Version=', version.request,
                     '&Signature=', Signature
                     sep = '')
  AmazonResult <- getURL(AmazonURL)
  return(AmazonResult)
}
  
  
################################ UGH
  
request_body <- data.frame(
    grant_type="client_credentials",
    client_id="amzn1.application-oa2-client.5c5cf3d4b9c14734b18e26cba3e3c920",
    client_secret="eee2b4ab52c32870f6cff7597ae3ac3c8ec7642ad312a099bb78a4a350da7725",
    scope="adx_reporting::appstore:marketer"
)
request_body_json <- toJSON(list(documents = request_body), auto_unbox = TRUE)
header2 <- "Content-Type: application/json"
header <- "Content-Type: application/x-www-form-urlencoded"

  POST(url="https://api.amazon.com/auth/o2/token",
       body = request_body_json,
       add_headers=header
)

access_token <- "Atc|MQEBICkdcjBudbrGqc3kCGCICkYrAer54RCGjsnpAq-aKREBPBoM5160WoPBh2ijjHR18XOe4UrH1rR5N9qyH3W_EcmHc2wc7LDRxkmCN90P-dxDRlJCWp0Dhbk8vhta5ma8XnwaTPgY4dYJp2Ax5MMcjhU4bUeGk96QcYrPjcK5sqnDXCEsx9zEbVqNtF9aWszFy2F2LsVGWFJLK0s31o5-2h3_ZUWHiy6tClNiGjufGm-VhjHL11IfVYzu-Oz_K-JhnTC3PKOV6DyUEx3S2wrc94j_JCbKrfuUxUugI0yvhogVYQ"
  
  
  
  