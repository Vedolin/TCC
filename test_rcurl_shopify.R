library(RCurl)
headers = c(
  "X-Shopify-Access-Token" = "f254f1f4be2cfe1f8f81909738683834"
)
res <- getURL("https://mar-mergulhe-suas-ideias.myshopify.com/admin/api/2023-04/webhooks.json", .opts=list(httpheader = headers, followlocation = TRUE))
cat(res)
