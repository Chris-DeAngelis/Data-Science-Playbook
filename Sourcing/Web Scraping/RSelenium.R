# This script highlights how R can use a browser to interact with a website. Furthermore, Tidyverse package rvest can be used to work with HTML
library(RSelenium)

#https://rpubs.com/johndharrison/RSelenium-Basics

# Set up RSelenium session
# IE Driver: https://www.selenium.dev/downloads/
# Chrome driver: https://sites.google.com/chromium.org/driver/
# Reassociate System Path: https://www.itprotoday.com/windows-server/how-can-i-add-new-folder-my-system-path
# Avoiding bot detection: https://stackoverflow.com/questions/33225947/can-a-website-detect-when-you-are-using-selenium-with-chromedriver/41220267
# More info: https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rselenium
driver <- rsDriver(browser="firefox", port = 1331L) #remoteDriver # firefox
remDr <- driver[["client"]]
# saving excel files
# remDr <- remoteDriver(extraCapabilities = makeFirefoxProfile(list(
#   "browser.helperApps.neverAsk.saveToDisk"="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
# ))


# Setting HTML headers
url <- "https://www.google.com"
uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"
session <- session(url, user_agent(uastring))

# Navigate to web page
remDr$navigate("https://www.northamericanstainless.com/NAS_App/Surcharge1?language=E&type=F")

# Update date for form
input <- "11/01/2021 to 11/30/2021"
remDr$findElement(using = "name", value = "yearmonth")$sendKeysToElement(list(input))

# Click on Submit button and wait for page to load
remDr$findElement(using = "name", value = "Submit")$sendKeysToElement(list(input))
#remDr$findElements("id", "btnSub")[[1]]$clickElement()
Sys.sleep(5) # give the page time to fully load

# get main window and store to switch back
currWindow <-  remDr$getCurrentWindowHandle()

# get all windows 
windows <- remDr$getWindowHandles()

# loop through switching child window 
for (window in windows) {
  if (window != currWindow[[1]]) 
    remDr$switchToWindow(window)
}

# now close your child window after doing all stuff
remDr$closeWindow()

# now switch back to main window for further stuff 
remDr$switchToWindow(currWindow[[1]])
remDr$closeWindow()

# Search for IDs
webElem <- remDr$findElement(using = 'name', value = "q")
webElem$getElementAttribute("name")
webElem$getElementAttribute("class")
webElem$getElementAttribute("id")

# Search by ID
webElem <- remDr$findElement(using = 'id', value = "lst-ib")

# Search by class
webElem$getElementAttribute("class")
webElem$getElementAttribute("type")
webElem <- remDr$findElement('css', "[class = 'gsfi lst-d-f']")

# Search by css-selector
webElem <- remDr$findElement(using = 'css', "input[name='q']")
webElem2 <- remDr$findElement(using = 'css', "[name='q']")
webElem <- remDr$findElement(using = 'css', "input#lst-ib")

# Search by xpath
webElem <- remDr$findElement('xpath', "//input[@id = 'lst-ib']")
webElem <- remDr$findElement('xpath', "//input[@class = 'gsfi lst-d-f']")

# Sending text to elements
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran"))

# Sending key presses to elements
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", "\uE007"))
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", key = "enter"))

# Sending mouse events to elements
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", key = "enter"))


# Highlight an element
webElem$highlightElement()