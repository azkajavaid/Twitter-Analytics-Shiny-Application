library(shiny)
library(devtools)
library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(wordcloud)
library(tidytext)
library(reshape2)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(translate)
library(ggmap)
library(maptools)
library(maps)
library(leaflet)
library(stringi)
library(rtimes)
library(igraph)
library(visNetwork)
library(data.table)
library(mosaic)
library(RColorBrewer)
library(tidyr)
library(ggthemes)
library(Rgraphviz)
library(tm)
library(zoo)
library(slam)
library(topicmodels)

# loading emoji data from today-is-a-good-day's emojis data accessible at https://github.com/today-is-a-good-day/emojis
load("emojidata.rda")

# replace with appropriate personal keys
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

api_key <- "KEY"
api_secret <- "SECRET"

set.key("KEY")

setup_twitter_oauth(api_key, api_secret, "", "")


navbarPage(
  "Twitter Analytics", inverse = TRUE,
  # navbarMenu("Main Page",
  tabPanel(
    "Welcome",
    img(src = "Twitter.png", align = "left", style = "margin:10px 10px"),
    h4(strong("Twitter Analytics App outlines trends in a user's twitter timeline,
                                 user's favorited tweets and topic related tweets, communicated via hashtags. 
                                 This platform builds a portfolio of a user/topic through live tweet analysis,
                                 visualized via data tables, word clouds, bar plots, time series and network plots.
                                 In addition to building a user portfolio, this app aims to better understand global trends through features like
                                 geographic trend analysis.")),

    HTML("<br/>"),

    hr(),

    HTML("<br/>"),
    h4(p(
      strong("Tweets Table"), "and", strong("Word Cloud"), "features visualize a 
                            user or topic's tweet corpus through data table and word cloud, respectively.", strong("Tweet Statistics"),
      "performs analysis of a user's preferred tweet time/platform.", strong("Topic Modeling"), "classifies words in topics using Latent Dirichlet Allocation (LDA) modeling.", strong("Sentiment Analysis"), "performs text analysis of user and topic related tweets, 
                            displaying the positive/negative associated tweets via word cloud, bar plots and time series.",
      strong("Geographic Analysis"), "maps a user's followers and followings and  
                            performs geographic level trend analysis, with Google api enabled language translation for multilingual trend exploration.",
      strong("Network Analysis"), "visualizes a user's associations and finds the 
                            most influential people in a domain by retweet analysis. Network feature also 
                            shows global flow of trends and topics overtime. The", strong("Emojis"), "feature, finds the 
                            most prevalent emojis in an individual/topic's tweet corpus."
    ))
  ),
  navbarMenu(
    "Tweets Table",
    tabPanel(
      "User Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          # set app background color
          tags$head(
            tags$style(HTML("
                                                                      body {
                                                                      background-color: #9ADEF5;
                                                                      color: #000000; 
                                                                      }
                                                                      "))
          ),

          # changes color/formatting of the tab panel title
          tags$style(HTML(".tabbable > .nav > li > a[data-value='User Tweets'] {background-color: #333333;   color:white}")),

          tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),

          strong("Feature shows tweets and user specific information (name, description, friends count, status count, favorites count, 
                                                      account creation date, location)."),

          hr(),

          div(
            style = "display: inline-block;vertical-align:right; width: 150px;",
            textInput("twitterUser1", "Enter User", "hadleywickham")
          ),

          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum1", "Tweet Number", 100)
          ),

          actionButton("showUserTweets", "Show User Tweets", style = "color: #000000;background-color: #00aced;margin: 4px;"),
          div(style = "display:inline-block", actionButton("showUserInfo", "Show User Info", style = "color: #000000;background-color: #00aced;margin: 4px;")),

          tags$head(
            tags$style(HTML("#showUserTweets{font-weight:bold;}"))
          ),

          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),

          tags$head(
            tags$style(HTML("#showUserInfo{font-weight:bold;}"))
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "User Tweets",
              dataTableOutput("userTweets2"),
              dataTableOutput("userTweets1")
            )
          )
        )
      )
    ),
    tabPanel(
      "User Favorites",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='User Favorites'] {background-color: #333333;   color:white}")),

          strong("Feature shows favorited tweets by the specified user."),

          hr(),

          div(
            style = "display: inline-block;vertical-align:right; width: 140px;",
            textInput("twitterUser2", "Enter User", "hadleywickham")
          ),

          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("tweetNum2", "Tweet Number", 100)
          ),

          actionButton("showUserFav", "Show User Tweets", style = "color: #000000;background-color: #00aced;margin: 4px;"),

          tags$head(
            tags$style(HTML("#showUserFav{font-weight:bold;}"))
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "User Favorites",
              dataTableOutput("userFavorite")
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Tweets'] {background-color: #333333;   color:white}")),

          strong("Feature shows tweets for the specified topic."),

          hr(),

          div(
            style = "display: inline-block;vertical-align:right; width: 140px;",
            textInput("twitterUser3", "Enter Topic", "rstats")
          ),

          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("tweetNum3", "Tweet Number", 100)
          ),

          actionButton("showTopicMention", "Show User Tweets", style = "color: #000000;background-color: #00aced;margin: 4px;"),


          tags$head(
            tags$style(HTML("#showTopicMention{font-weight:bold;}"))
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Topic Tweets", dataTableOutput("topicTweet"))
          )
        )
      )
    )
  ),
  navbarMenu(
    "Tweet Statistics",
    tabPanel(
      "User Tweets",
      sidebarLayout(
        sidebarPanel(
          strong("Feature assesses a user's twitter use across time metrics like hour, weekday, month and year as well as across twitter platforms 
                                           including Hootsuite, Instagram, iPhone and Web Client."),

          hr(),
          textInput("twitterUser4", "Enter User", "juliasilge"),
          selectInput("chooseTime_Platform1", "Horizontal axis:", choices = c(
            "Hour" = "hour", "Weekday" = "weekday", "Month" = "month",
            "Year" = "year", "Platform" = "platform"
          )),
          selectInput("chooseTime_Platform2", "Vertical axis:", choices = c(
            "Weekday" = "weekday", "Hour" = "hour", "Month" = "month",
            "Year" = "year", "Platform" = "platform"
          )),
          numericInput("tweetNum4", "Tweet Number", 200),

          actionButton("showTweetStats", "Show Trends", style = "color: #000000;background-color: #00aced;"),

          tags$head(
            tags$style(HTML("#showTweetStats{font-weight:bold;}"))
          ),


          downloadButton("downloadTweetStats", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Count Bar Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Count Bar Plot'] {background-color: #333333;   color:white}")),

              plotlyOutput("countBarPlot"),
              dataTableOutput("countStatsClick")
            ),

            tabPanel(
              "Trends Over Time",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Line Plot'] {background-color: #333333;   color:white}")),

              dataTableOutput("overTimeTrends")
            )
          )
        )
      )
    ),
    tabPanel(
      "User Favorites",
      sidebarLayout(
        sidebarPanel(
          strong("Feature assesses a user's tweet favorite behavior across time metrics like hour, weekday, month and year as well as across twitter platforms 
                                           including Hootsuite, Instagram, iPhone and Web Client."),
          hr(),
          textInput("twitterUser4_fav", "Enter User", "juliasilge"),
          selectInput("chooseTime_Platform1_fav", "Horizontal axis:", choices = c(
            "Hour" = "hour", "Weekday" = "weekday", "Month" = "month",
            "Year" = "year", "Platform" = "platform"
          )),
          selectInput("chooseTime_Platform2_fav", "Vertical axis:", choices = c(
            "Weekday" = "weekday", "Hour" = "hour", "Month" = "month",
            "Year" = "year", "Platform" = "platform"
          )),
          numericInput("tweetNum4_fav", "Tweet Number", 200),

          actionButton("showTweetStats_fav", "Show Trends", style = "color: #000000;background-color: #00aced;"),

          tags$head(
            tags$style(HTML("#showTweetStats_fav{font-weight:bold;}"))
          ),


          downloadButton("downloadTweetStats_fav", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Count Bar Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Count Bar Plot'] {background-color: #333333;   color:white}")),

              plotlyOutput("countBarPlot_fav"),
              dataTableOutput("countStatsClick_fav")
            ),

            tabPanel(
              "Trends Over Time",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Line Plot'] {background-color: #333333;   color:white}")),

              dataTableOutput("overTimeTrends_fav")
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Tweets",
      sidebarLayout(
        sidebarPanel(
          strong("Feature assesses a topic's spread across time metrics like hour, weekday, month and year as well as across twitter platforms 
                                           including Hootsuite, Instagram, iPhone and Web Client."),
          hr(),
          textInput("twitterUser4_topic", "Enter Topic", "rstats"),
          selectInput("chooseTime_Platform1_topic", "Horizontal axis:", choices = c(
            "Hour" = "hour", "Weekday" = "weekday", "Month" = "month",
            "Year" = "year", "Platform" = "platform"
          )),
          selectInput("chooseTime_Platform2_topic", "Vertical axis:", choices = c(
            "Weekday" = "weekday", "Hour" = "hour", "Month" = "month",
            "Year" = "year", "Platform" = "platform"
          )),
          numericInput("tweetNum4_topic", "Tweet Number", 200),

          actionButton("showTweetStats_topic", "Show Trends", style = "color: #000000;background-color: #00aced;"),

          tags$head(
            tags$style(HTML("#showTweetStats_topic{font-weight:bold;}"))
          ),

          downloadButton("downloadTweetStats_topic", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Count Bar Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Count Bar Plot'] {background-color: #333333;   color:white}")),

              plotlyOutput("countBarPlot_topic"),
              dataTableOutput("countStatsClick_topic")
            ),

            tabPanel(
              "Trends Over Time",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Line Plot'] {background-color: #333333;   color:white}")),

              dataTableOutput("overTimeTrends_topic")
            )
          )
        )
      )
    )
  ),
  navbarMenu(
    "Word Cloud",
    tabPanel(
      "User Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='User Tweets'] {background-color: #333333;   color:white}")),

          strong("Feature shows word cloud for the specified user, excluding all retweets and replies to get more personalized words.  
                                                      Accompanying dotplot shows word counts. Specify tweet number and minimum 
            number of repeated words (high number of minimum words will yield smaller and more relevant words 
            in the cloud)."),
          hr(),

          textInput("twitterUser5", "Enter User", "juliasilge"),
          tags$head(
            tags$style(HTML("#showTweetCloud{font-weight:bold;}"))
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum5", "Tweet Number", 100)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("minWords", "Min Words", 2)
          ),
          actionButton("showTweetCloud", "Show Cloud", style = "color: #000000;background-color: #00aced;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "User Tweets", plotOutput("userTweetsCloud", height = "700px"),
              plotlyOutput("userTweetsCloudTab", height = "800px")
            )
          ), width = 9
        )
      )
    ),
    tabPanel(
      "User Favorites",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='User Favorites'] {background-color: #333333;   color:white}")),

          strong("Feature shows word cloud for a user's favorited tweets, excluding all retweets and replies to get more personalized words. Accompanying dotplot shows word counts. Specify tweet number and minimum 
                                                      number of repeated words (high number of minimum words will yield smaller and more relevant words in cloud)."),
          hr(),
          textInput("twitterUser6", "Enter User", "juliasilge"),

          tags$head(
            tags$style(HTML("#showFavoriteCloud{font-weight:bold;}"))
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum6", "Tweet Number", 100)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("minWords2", "Min Words", 2)
          ),

          actionButton("showFavoriteCloud", "Show Cloud", style = "color: #000000;background-color: #00aced;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "User Favorites", plotOutput("userFavoritesCloud", height = "500px"),
              plotlyOutput("userFavoritesCloudTab", height = "900px")
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Tweets'] {background-color: #333333;   color:white}")),

          strong("Feature shows word cloud for the specified topic. Accompanying dotplot shows word counts. Specify tweet number and minimum 
                                                      number of repeated words (high number of minimum words will yield smaller and more relevant words in cloud)."),
          hr(),
          textInput("twitterUser7", "Enter Topic", "rstats"),

          tags$head(
            tags$style(HTML("#showTopicCloud{font-weight:bold;}"))
          ),
          div(style = "display: inline-block;vertical-align:top; width: 0.5px;", HTML("<br>")),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum7", "Tweet Number", 100)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 60px;",
            numericInput("minWords3", "Min Words", 2)
          ),
          actionButton("showTopicCloud", "Show Cloud", style = "color: #000000;background-color: #00aced;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Topic Tweets", plotOutput("tweetTopicCloud", height = "700px"),
              plotlyOutput("topicTweetsCloudTab", height = "700px")
            )
          )
        )
      )
    )
  ),
  navbarMenu(
    "Topic Modeling",
    tabPanel(
      "User Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='User Tweets'] {background-color: #333333;   color:white}")),

          strong("Feature performs topic modeling for the specified user, displaying prevalent words for
                                                the specified number of topics in plot and table formats. Specify number of topics and words per topic for display.
            Decrease the number of topics and words per topic for a less crowded display. Search for a term and topic to see related tweets."),

          hr(),
          textInput("twitterUser1_topic", "Enter User", "juliasilge"),
          tags$head(
            tags$style(HTML("#showTopicModelUser{font-weight:bold;}"))
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum1_topic", "Tweet Number", 100)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("topicNumber1_topic", "Number of Topics", 4)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("topWords1User", "Number of Words", 5)
          ),
          actionButton("showTopicModelUser", "Show Plot and Table", style = "color: #000000;background-color: #00aced;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Topic Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Plot'] {background-color: #333333;   color:white}")),
              plotlyOutput("userTweetsTopic", height = "500px"),
              div(
                style = "display: inline-block;vertical-align:right; width: 200px;",
                textInput("topicPlotTerm", "Enter term", "")
              ),
              div(
                style = "display: inline-block;vertical-align:right; width: 200px;",
                textInput("topicPlotTopic", "Enter topic")
              ),
              dataTableOutput("userTweetsTopicRelated")
            ),
            tabPanel(
              "Topic Table",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Table'] {background-color: #333333;   color:white}")),
              dataTableOutput("userTweetsTopicTable")
            ),
            tabPanel(
              "Zoom in Topic",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Zoom in Topic'] {background-color: #333333;   color:white}")),
              numericInput("zoomUserTopic", "Topic Number", ""),
              plotlyOutput("userTweetsTopicSpecific", height = "1700px", width = "800px")
            )
          )
        )
      )
    ),
    tabPanel(
      "User Favorites",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='User Favorites'] {background-color: #333333;   color:white}")),

          strong("Feature performs topic modeling for the specified user's favorited tweets, displaying prevalent words for 
                                                the specified number of topics in plot and table formats. Specify number of topics and words per topic for display.
            Decrease the number of topics and words per topic for a less crowded display. Search for a term and topic to see related tweets."),

          hr(),
          textInput("twitterUser2_topic", "Enter User", "juliasilge"),

          tags$head(
            tags$style(HTML("#showTopicModelFavorite{font-weight:bold;}"))
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum2_topic", "Tweet Number", 100)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("topicNumber2_topic", "Number of Topics", 4)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("topWords2User", "Number of Words", 5)
          ),

          actionButton("showTopicModelFavorite", "Show Plot and Table", style = "color: #000000;background-color: #00aced;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Topic Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Plot'] {background-color: #333333;   color:white}")),
              plotlyOutput("userFavoritesTopic", height = "500px"),
              div(
                style = "display: inline-block;vertical-align:right; width: 200px;",
                textInput("favoritePlotTerm", "Enter term", "")
              ),
              div(
                style = "display: inline-block;vertical-align:right; width: 200px;",
                textInput("favoritePlotTopic", "Enter topic")
              ),
              dataTableOutput("favoritesTweetsTopicRelated")
            ),
            tabPanel(
              "Topic Table",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Table'] {background-color: #333333;   color:white}")),
              dataTableOutput("userFavoritesTopicTable")
            ),
            tabPanel(
              "Zoom in Topic",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Zoom in Topic'] {background-color: #333333;   color:white}")),
              numericInput("zoomFavTopic", "Topic Number", ""),
              plotlyOutput("favTweetsTopicSpecific", height = "1700px", width = "800px")
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,

          tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Tweets'] {background-color: #333333;   color:white}")),

          strong("Feature performs topic modeling for the specified topic, displaying 
                                                prevalent words for the specified number of topics in plot and table formats.
            Specify number of topics and words per topic for display. 
            Decrease the number of topics and words per topic for a less crowded display. Search for a term and topic to see related tweets."),

          hr(),
          textInput("twitterUser3_topic", "Enter Topic", "rstats"),

          tags$head(
            tags$style(HTML("#showTopicModelTweets{font-weight:bold;}"))
          ),
          div(style = "display: inline-block;vertical-align:top; width: 0.5px;", HTML("<br>")),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum3_topic", "Tweet Number", 100)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 60px;",
            numericInput("topicNumber3_topic", "Number of Topics", 4)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("topWords3User", "Number of Words", 5)
          ),
          actionButton("showTopicModelTweets", "Show Plot and Table", style = "color: #000000;background-color: #00aced;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Topic Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Plot'] {background-color: #333333;   color:white}")),
              plotlyOutput("topicTweetCloud", height = "500px"),
              div(
                style = "display: inline-block;vertical-align:right; width: 200px;",
                textInput("topicTweetPlotTerm", "Enter term", "")
              ),
              div(
                style = "display: inline-block;vertical-align:right; width: 200px;",
                textInput("topicTweetPlotTopic", "Enter topic")
              ),
              dataTableOutput("topicsTweetsTopicRelated")
            ),
            tabPanel(
              "Topic Table",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Table'] {background-color: #333333;   color:white}")),
              dataTableOutput("topicTweetCloudTable")
            ),
            tabPanel(
              "Zoom in Topic",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Zoom in Topic'] {background-color: #333333;   color:white}")),
              numericInput("zoomTopicSpecific", "Topic Number", ""),
              plotlyOutput("topicTweetsSpecific", height = "1700px", width = "800px")
            )
          )
        )
      )
    )
  ),
  navbarMenu(
    "Sentiment Analysis",
    tabPanel(
      "User Tweets",
      sidebarLayout(
        sidebarPanel(
          strong("Feature performs sentiment analysis for the specified user. Click 'Show Plots' to show word cloud, bar plot and time series for the user based on his/her tweets.
                                           Click on a point in time series to show positive or negative tweets associated with that point. 
            "),
          hr(),
          textInput("twitterUser8", "Enter User", "juliasilge"),

          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum8", "Tweet Number", 200)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("barplotNum", "Number for Bar Plot", 20)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("wordCloudNum", "Number for Word Cloud", 20)
          ),

          tags$head(
            tags$style(HTML("#showWordBarSentiment{font-weight:bold;}"))
          ),

          tags$head(
            tags$style(
              HTML("#showPointData{margin-right:5px;}")
            )
          ),
          actionButton("showWordBarSentiment", "Show Plots", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Word Cloud",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Word Cloud'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentWordCloud")
            ),

            tabPanel(
              "Bar Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Bar Plot'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentBarPlot", height = "800px")
            ),

            tabPanel(
              "Time Series",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Time Series'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentTimeSeries", height = "600px", click = "plot1_click"),
              dataTableOutput("sentimentInfo")
            )
          )
        )
      )
    ),
    tabPanel(
      "User Favorites",
      sidebarLayout(
        sidebarPanel(
          strong("Feature performs sentiment analysis for the specified user. Click 'Show Plots' to show word cloud, bar plot and time series for the user based on his/her favorited tweets.
                                           Click on a point in time series to show positive or negative tweets associated with that point. 
                 "),
          hr(),
          textInput("twitterUser9", "Enter User", "juliasilge"),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum9", "Tweet Number", 200)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("barplotNum2", "Number for Bar Plot", 20)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("wordCloudNum2", "Number for Word Cloud", 20)
          ),

          tags$head(
            tags$style(
              HTML("#showWordBarSentiment2{margin-left:50px;}")
            )
          ),

          tags$head(
            tags$style(HTML("#showWordBarSentiment2{font-weight:bold;}"))
          ),

          tags$head(
            tags$style(
              HTML("#showPointData2{margin-left:5px;}")
            )
          ),

          actionButton("showWordBarSentiment2", "Show Plots", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Word Cloud",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Word Cloud'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentWordCloud2")
            ),

            tabPanel(
              "Bar Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Bar Plot'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentBarPlot2", height = "900px")
            ),

            tabPanel(
              "Time Series",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Time Series'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentTimeSeries2", height = "900px", click = "plot2_click"),
              dataTableOutput("sentimentInfo2")
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Tweets",
      sidebarLayout(
        sidebarPanel(
          strong("Feature performs sentiment analysis for the specified topic. Click 'Show Plots' to show word cloud, bar plot and time series for the topic.
                                           Click on a point in time series to show positive or negative tweets associated with that point. 
            "),
          hr(),
          textInput("twitterUser9_topic", "Enter Topic", "rstats"),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum9_topic", "Tweet Number", 200)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("barplotNum3", "Number for Bar Plot", 20)
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 70px;",
            numericInput("wordCloudNum3", "Number for Word Cloud", 20)
          ),

          tags$head(
            tags$style(
              HTML("#showWordBarSentiment3{margin-left:50px;}")
            )
          ),

          tags$head(
            tags$style(HTML("#showWordBarSentiment3{font-weight:bold;}"))
          ),

          tags$head(
            tags$style(
              HTML("#showPointData3{margin-left:5px;}")
            )
          ),

          actionButton("showWordBarSentiment3", "Show Plots", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Word Cloud",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Word Cloud'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentWordCloud3")
            ),

            tabPanel(
              "Bar Plot",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Bar Plot'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentBarPlot3", height = "900px")
            ),

            tabPanel(
              "Time Series",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Time Series'] {background-color: #333333;   color:white}")),
              plotOutput("sentimentTimeSeries3", height = "900px", click = "plot3_click"),
              dataTableOutput("sentimentInfo3")
            )
          )
        )
      )
    )
  ),
  navbarMenu(
    "Geographic Analysis",
    tabPanel(
      "Mapping Trends",
      (bootstrapPage(
        leafletOutput("map"),
        strong("Click on the map to show location specific trends. 
                                              Search for tweets related to the specified trend.
                                              Translate multilingual trends/tweets to English using Google Translation API. By default, 10 tweets
                                              of the specified trend are shown."),
        downloadButton("downloadGeographic", "Download Data", style = "color: #000000;background-color: #fff;"),

        HTML("<br/>", "<br/>"), # adding space between panels/inputs

        verbatimTextOutput("lat"),
        verbatimTextOutput("address"),
        fluidRow(
          column(3, wellPanel(textInput("showTweetTrend", "Search Trends", "Analytics"))),
          column(3, wellPanel(actionButton("showTrends", "Show Tweets", style = "color: #000000;background-color: #00aced;"))),

          tags$head(
            tags$style(HTML("#showTrends{font-weight:bold;}"))
          )
        ),
        dataTableOutput("tweetTable"),
        dataTableOutput("trendTable")
      ))
    ),
    tabPanel(
      "Mapping Following",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          strong("Feature globally maps the specified user's followings, with available geolocation data. Click on a point to see following details like name, 
                                                      description, friend count, and user tweets amongst others."),
          hr(),
          div(
            style = "display: inline-block;vertical-align:right; width: 150px;",
            textInput("twitterUser10", "Enter User", "hadleywickham")
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum10", "Enter Following Number", 25)
          ),

          actionButton(
            "showFollowingMap", "Show Mapped Follwing",
            style = "color: #000000;background-color: #00aced;"
          ),

          tags$head(
            tags$style(HTML("#showFollowingMap{font-weight:bold;}"))
          ),

          downloadButton("downloadFollowing", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          leafletOutput("mapFollowing"),
          dataTableOutput("followingDescription"),
          dataTableOutput("followingTweets")
        )
      )
    ),
    tabPanel(
      "Mapping Followers",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          strong("Feature globally maps the specified user's followers, with available geolocation data. Click on a point to see follower details like name, 
                                                      description, friend count, and user tweets amongst others."),
          hr(),

          div(
            style = "display: inline-block;vertical-align:right; width: 150px;",
            textInput("twitterUser11", "Enter User", "hadleywickham")
          ),
          div(
            style = "display: inline-block;vertical-align:right; width: 80px;",
            numericInput("tweetNum11", "Enter Follower Number", 25)
          ),

          actionButton(
            "showFollowerMap", "Show Mapped Followers",
            style = "color: #000000;background-color: #00aced;"
          ),

          tags$head(
            tags$style(HTML("#showFollowerMap{font-weight:bold;}"))
          ),

          downloadButton("downloadFollower", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          leafletOutput("mapFollower"),
          dataTableOutput("followerDescription"),
          dataTableOutput("followerTweets")
        )
      )
    ),
    tabPanel(
      "Mapping Tweets",
      sidebarLayout(
        sidebarPanel(
          strong("Feature facilitates global visualization of location enabled tweets for a specified topic. Vertices' radius 
                                           is indicative of the tweet's retweet count, favorites count, or the tweeting user's friends, followers or 
            status count. The vertices can be colored by hour or weekday status to show the
            global trend/movement of tweets over time. Click on the vertex/band of tweet to show tweet specific information. Decrease
            the circle size threshold to proportionally decrease the radius of the circles associated with the 
            vertices to get a holistic mapped visual."),
          hr(),
          textInput("twitterUser12", "Enter topic", "rstats"),
          numericInput("circleSize", "Circle size threshold", "5"),
          numericInput("tweetNum12", "Number of tweets", "50"),
          selectInput("circleCount", "Circle radius by:", choices = c(
            "RetweetCount" = "retweetCount",
            "FavoriteCount" = "favoritesCount", "FollowerCount" = "followersCount",
            "FriendsCount" = "friendsCount", "StatusCount" = "statusesCount"
          )),
          selectInput("circleColor", "Color by:", choices = c(
            "Hour" = "hour",
            "Weekday" = "weekday"
          )),
          actionButton("showMapTweets", "Show Mapped Tweets", style = "color: #000000;background-color: #00aced;"),

          tags$head(
            tags$style(HTML("#showMapTweets{font-weight:bold;}"))
          ),

          downloadButton("downloadTweetMapping", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          leafletOutput("mapTweets"),
          dataTableOutput("geolocatedTweets")
        )
      )
    )
  ),
  navbarMenu(
    "Network Analysis",
    tabPanel(
      "Retweets Network",
      sidebarLayout(
        sidebarPanel(
          strong("Feature assesses a topic's influence by searching 
                                           mentions of retweets involving that topic. Sentiments
            of those tweets are analyzed and distinguished by emotions like
            anger, anticipation, disgust, fear, joy, negative, positive,
            sadness, surprise, trust and no emotion. Zoom in on the network to take a
            closer look. Click on each individual vertex to explore the user's remarks about
            the searched topic. Change the filter network to show network with greater than the 
            specified number of retweets."),
          hr(),
          textInput("twitterUser13", "Search Topic", "rstats"),
          numericInput("tweetNum13", "Enter Tweet Number", 100),
          numericInput("retweetLimit", "Filter Retweets", 1),
          actionButton("showRetweetNetwork", "Show Retweets Network", style = "color: #000000;background-color: #00aced; margin: 4px;"),

          tags$head(
            tags$style(HTML("#showRetweetNetwork{font-weight:bold;}"))
          )
        ),
        mainPanel(
          visNetworkOutput("retweetNetwork", height = "400px"),
          dataTableOutput("retweetNetworkTable")
        )
      )
    ),
    tabPanel(
      "Following Network",
      sidebarLayout(
        sidebarPanel(
          strong("Feature shows individuals followed by the specified user. The magnitude of the 
                                           following user vertex corresponds to the number of followers for the followed individual (larger
            vertex corresponds to more followers). Change the network display by specifying follower limit
            so users with large followers like celebrities and corporations 
            are balanced with individuals with normal twitter following. Change the range to only
            show following users with followers within that range. Click on a vertex to display user-specific
            information about the following individual as well as tweet mentions involving the searched user 
            and the individual being followed."),
          hr(),
          textInput("twitterUser14", "Search User", "hadleywickham"),
          numericInput("tweetNum14", "Search Following Number", 50),
          numericInput("lowLimitFollowing", "Follower Lower Limit", 10),
          numericInput("upLimitFollowing", "Follower Upper Limit", 1000),
          numericInput("followerLimit", "Follower Limit", 10),
          actionButton("showFollowingNetwork", "Show Following Network", style = "color: #000000;background-color: #00aced;"),

          tags$head(
            tags$style(HTML("#showFollowingNetwork{font-weight:bold;}"))
          ),

          downloadButton("downloadFollowingData", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          visNetworkOutput("followingNetwork"),
          dataTableOutput("followingNetworkUser"),
          dataTableOutput("followingNetworkTweets")
        )
      )
    ),
    tabPanel(
      "Follower Network",
      sidebarLayout(
        sidebarPanel(
          strong("Feature shows followers of the searched user. Magnitude of the 
                                           follower vertex corresponds to the number of followers for the followed user (larger
            vertex corresponds to more followers). Change the network display by specifying follower limit
            so users with large followers like celebrities and corporations 
            are balanced with individuals with normal twitter following. Additionally change the range to only
            show following personalities with followers within the range. Click on a vertex to display user-specific
            information about the follower as well as the tweet mentions involving the searched user 
            and the follower."),
          hr(),
          textInput("twitterUser15", "Search User", "hadleywickham"),
          numericInput("tweetNum15", "Search Follower Number", 50),
          numericInput("lowLimitFollower", "Follower Lower Limit", 10),
          numericInput("upLimitFollower", "Follower Upper Limit", 1000),
          numericInput("followerLimit2", "Follower Limit", 10),
          actionButton("showFollowerNetwork", "Show Follower Network", style = "color: #000000;background-color: #00aced;"),

          tags$head(
            tags$style(HTML("#showFollowerNetwork{font-weight: bold;}"))
          ),

          downloadButton("downloadFollowerData", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
        ),
        mainPanel(
          visNetworkOutput("followerNetwork"),
          dataTableOutput("followerNetworkUser"),
          dataTableOutput("followerNetworkTweets")
        )
      )
    )
  ),
  navbarMenu(
    "Emojis",
    tabPanel(
      "User Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          strong("Feature tracks emoji use for the specified user compared by actual, retweeted and favorited tweets frequencies. 
                                                      Click a point to see the tweets associated with that emoji at that frequency.
                                                      Zoom over each graph for a more granulated visual. See the 'Trends Over time' tab for tracking emoji use in user tweets, retweets 
                                                      and favorites over time."),
          hr(),
          textInput("twitterUser16", "Enter User", "juliasilge"),
          numericInput("tweetNum16", "Enter Tweet Number", 100),
          actionButton("showEmojiAnalysisUser", "Show Emoji Trends", style = "color: #000000;background-color: #00aced;margin: 4px;"),
          tags$head(
            tags$style(HTML("#showEmojiAnalysisUser{font-weight:bold;}"))
          ),
          tags$head(
            tags$style(
              HTML("#dashboard{margin-top:100px;}")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Emoji Trends",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Emoji Trends'] {background-color: #333333;   color:white}")),
              plotlyOutput("plotEmojiTrendsUser"),
              dataTableOutput("emojiUserClick")
            ),
            tabPanel(
              "Trends Over Time",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Trends Over Time'] {background-color: #333333;   color:white}")),
              dataTableOutput("emojiTrendsOverTimeUser")
            )
          )
        )
      )
    ),
    tabPanel(
      "User Favorites",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          strong("Feature tracks emoji use for the specified user's favorited tweets compared by actual, retweeted and favorited tweets frequencies. 
                                                      Click a point to see the tweets associated with that emoji at that frequency.
            Zoom over each graph for a more granulated visual. See the 'Trends Over time' tab for tracking emoji use in user tweets, retweets 
            and favorites over time."),
          hr(),
          textInput("twitterUser17", "Enter User", "juliasilge"),
          numericInput("tweetNum17", "Enter Tweet Number", 100),
          actionButton("showEmojiAnalysisFav", "Show Emoji Trends", style = "color: #000000;background-color: #00aced;margin: 4px;"),

          tags$head(
            tags$style(HTML("#showEmojiAnalysisFav{font-weight:bold;}"))
          ),
          tags$head(
            tags$style(
              HTML("#dashboard{margin-top:100px;}")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Emoji Trends",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Emoji Trends'] {background-color: #333333;   color:white}")),
              plotlyOutput("plotEmojiTrendsFav"),
              dataTableOutput("emojiFavClick")
            ),
            tabPanel(
              "Trends Over Time",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Trends Over Time'] {background-color: #333333;   color:white}")),
              dataTableOutput("emojiTrendsOverTimeFav")
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Tweets",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          strong("Feature tracks emoji use for the specified topic compared by actual, retweeted and favorited tweets frequencies. 
                                                      Click a point to see the tweets associated with that emoji at that frequency.
                                                      Zoom over each graph for a more granulated visual. See the 'Trends Over time' tab for tracking emoji use in user tweets, retweets 
                                                      and favorites over time."),
          hr(),
          textInput("twitterUser18", "Enter Topic", "rstats"),
          numericInput("tweetNum18", "Enter Tweet Number", 100),
          actionButton("showEmojiAnalysisTopic", "Show Emoji Trends", style = "color: #000000;background-color: #00aced;margin: 4px;"),
          tags$head(
            tags$style(HTML("#showEmojiAnalysisTopic{font-weight:bold;}"))
          ),
          tags$head(
            tags$style(
              HTML("#dashboard{margin-top:100px;}")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Emoji Trends",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Emoji Trends'] {background-color: #333333;   color:white}")),
              plotlyOutput("plotEmojiTrendsTopic"),
              dataTableOutput("emojiTopicClick")
            ),
            tabPanel(
              "Trends Over Time",
              tags$style(HTML(".tabbable > .nav > li > a[data-value='Trends Over Time'] {background-color: #333333;   color:white}")),
              dataTableOutput("emojiTrendsOverTimeTopic")
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "References",
    verbatimTextOutput("Reference")
  )
)
