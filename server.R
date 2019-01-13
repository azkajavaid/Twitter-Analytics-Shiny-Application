
function(input, output, session) {

  # Tweets Table Section

  ## User Tweets

  ### Returns tweets for specified user
  userTweetData <- eventReactive(input$showUserTweets, {
    withProgress(message = "Loading application", value = 0, {
      tweets <- userTimeline(input$twitterUser1, n = input$tweetNum1) # selects specified number of user tweets
      incProgress(0.7, detail = "Getting tweets")
      tab <- twListToDF(tweets) # converts tweets and associated metrics in table format
      tab2 <- tab[!duplicated(tab[, c("text")]), ] # removes duplicated text
      tab2 <- tab2 %>% dplyr::select(
        text, favoriteCount, replyToSN, created, truncated, replyToSID, id, replyToUID, statusSource, screenName, retweetCount,
        isRetweet, retweeted, longitude, latitude
      )
      incProgress(0.3, detail = "Finishing...")
      return(tab2)
    })
  })

  output$userTweets1 <- renderDataTable({
    userTweetData()
  })

  ### Returns info for specified twitter user
  userInfoData <- eventReactive(input$showUserInfo, {
    withProgress(message = "Loading application", value = 0, {
      search.string <- input$twitterUser1
      user <- getUser(search.string) # retrieve information about a Twitter user
      incProgress(0.4, detail = "Getting user data")
      ScreenName <- user$screenName
      Name <- user$name
      Description <- user$description
      FavoritesCount <- user$favoritesCount
      FriendsCount <- user$friendsCount
      StatusCount <- user$statusesCount
      AccountCreated <- date(user$created)
      AccountCreated <- as.character(AccountCreated)
      Location <- user$location
      incProgress(0.3, detail = "Processing")
      userInfo <- cbind(
        ScreenName, Name, Description,
        FriendsCount, StatusCount, FavoritesCount,
        AccountCreated, Location
      )
      incProgress(0.3, detail = "Finishing")
      return(userInfo)
    })
  })

  output$userTweets2 <- renderDataTable({
    userInfoData()
  })

  ## User Favorites
  userFavoriteData <- eventReactive(input$showUserFav, {
    withProgress(message = "Application loading", value = 0, {
      tweets <- favorites(input$twitterUser2, n = input$tweetNum2)
      incProgress(0.7, detail = "Getting favorite tweets")
      tab <- twListToDF(tweets)
      tab2 <- tab[!duplicated(tab[, c("text")]), ]
      tab2 <- tab2 %>% dplyr::select(
        text, favoriteCount, replyToSN, created, truncated, replyToSID, id, replyToUID, statusSource, screenName, retweetCount,
        isRetweet, retweeted, longitude, latitude
      )
      incProgress(0.3, detail = "Finishing...")
      return(tab2)
    })
  })

  output$userFavorite <- renderDataTable({
    userFavoriteData()
  })

  ## Topic Tweets
  topicTweetsData <- eventReactive(input$showTopicMention, {
    withProgress(message = "Loading application", value = 0, {
      tweets <- searchTwitter(input$twitterUser3, n = input$tweetNum3)
      incProgress(0.7, detail = "Getting tweets")
      tab <- twListToDF(tweets)
      tab2 <- tab[!duplicated(tab[, c("text")]), ]
      tab2 <- tab2 %>% dplyr::select(
        text, favoriteCount, replyToSN, created, truncated, replyToSID, id, replyToUID, statusSource, screenName, retweetCount,
        isRetweet, retweeted, longitude, latitude
      )
      incProgress(0.3, detail = "Finishing...")
      return(tab2)
    })
  })

  output$topicTweet <- renderDataTable({
    topicTweetsData()
  })

  # End of Tweets Table Section

  # Tweet Statistics

  ## Tweet statistics for user

  tweetStatisticsData <- suppressWarnings(eventReactive(input$showTweetStats, {
    tweets <- userTimeline(input$twitterUser4, n = input$tweetNum4)
    tab <- twListToDF(tweets)
    tab$hour <- hour(with_tz(tab$created, "EST"))
    tab$date <- as.Date(tab$created)
    tab$year <- year(tab$date)
    tab$year <- as.factor(tab$year)
    tab$month <- as.factor(months(tab$date))
    tab$weekday <- as.factor(weekdays(tab$date))
    tab$weekday <- factor(tab$weekday, levels = c(
      "Sunday", "Monday",
      "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
    ))
    tab$hour <- as.factor(tab$hour)

    tab$statusSource <- as.factor(tab$statusSource)
    tab$statusSource <- regmatches(tab$statusSource, gregexpr("(?<=>).*?(?=<)", tab$statusSource, perl = TRUE))
    tab$platform <- as.character(unlist(tab$statusSource))
    tab$platform <- as.factor(tab$platform)

    tab$platform <- gsub("Twitter Web Client", "Web Client", tab$platform)
    tab$platform <- gsub("Twitter for iPhone", "iPhone", tab$platform)
    tab$platform <- gsub("Twitter for Android", "Android", tab$platform)
    tab$platform <- gsub("Twitter for Mac", "Mac", tab$platform)
    tab$platform <- gsub("Twitter for iOS", "iOS", tab$platform)
    tab$platform <- gsub("Twitter for iPad", "iPad", tab$platform)
    tab$platform <- gsub("Tweetbot for Mac", "Mac bot", tab$platform)
    tab$platform <- gsub("Tweetbot for iOS", "iOS bot", tab$platform)
    tab$platform <- gsub("Meme_Twitterbot", "Meme bot", tab$platform)

    tab <- subset(tab, platform != "rtapp315156161")
    tab$weekday <- mapvalues(tab$weekday, from = c(
      "Sunday", "Monday", "Tuesday",
      "Wednesday", "Thursday", "Friday",
      "Saturday"
    ), to = c(
      "Su", "M", "Tu", "W",
      "Th", "F", "Sa"
    ))
    return(tab)
  }))

  ## Count Bar Plot to show twitter use trends over specified time/platform input
  output$countBarPlot <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Getting tweets")
      tab <- tweetStatisticsData()
      join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1, input$chooseTime_Platform2) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      tab <- join5
      tab$count <- as.factor(as.character(tab$n))
      incProgress(0.3, detail = "Plotting")
      if ((input$chooseTime_Platform2) == "hour") {
        tab <- as.data.frame(tab)
        tab[, 2] <- as.numeric(as.character(tab[, 2]))
        p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1, y = "n", fill = input$chooseTime_Platform2)) +
          geom_bar(stat = "identity") + theme_bw()
        ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot")
      }
      else {
        p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1, y = "n", fill = input$chooseTime_Platform2)) +
          geom_bar(stat = "identity") + theme_bw()
        ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot")
      }
    })
  })

  ## Click on bar plot to show associated tweets at the specifed time/platform for user
  output$countStatsClick <- renderDataTable({
    d <- event_data("plotly_click", source = "countBarPlot")
    if (is.null(d)) {
      dat <- data.frame("Click on a bar to show associated tweets")
      colnames(dat) <- "Click on a bar to show associated tweets"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      tab <- tweetStatisticsData()
      tab$weekday <- mapvalues(tab$weekday, from = c(
        "Su", "M", "Tu", "W",
        "Th", "F", "Sa"
      ), to = c(
        "Sunday", "Monday", "Tuesday",
        "Wednesday", "Thursday", "Friday",
        "Saturday"
      ))
      join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1, input$chooseTime_Platform2) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      tab <- join5
      tab$count <- as.factor(as.character(tab$n))
      tab1 <- tab[, c(1, 2)]
      tabChoice1 <- tab[, 1]
      tabChoice1 <- unique(tabChoice1)
      tabChoice1 <- as.data.frame(tabChoice1)
      tabChoice2 <- cbind(as.data.frame(tabChoice1), 1:length(tabChoice1[, 1]))
      colnames(tabChoice2) <- c(input$chooseTime_Platform1, "Variable1Val")
      tabChoice3 <- tabChoice1 %>% inner_join(tabChoice2)
      tabChoice3 <- tab %>% inner_join(tabChoice3)
      tabChoice3 <- as.data.frame(tabChoice3)
      if (input$chooseTime_Platform2 == "hour") {
        if (is.null(d)) {
          dat <- as.data.frame("Click events appear here (double-click to clear)")[1]
          return(dat)
        }
        else {
          hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
          hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
          dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
          dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
          dat5 <- dat4[, c(1, 2, 4, 8)]
          return(dat5)
        }
      }
      else {
        if (is.null(d)) {
          dat <- as.data.frame("Click events appear here (double-click to clear)")[1][, 1]
          return(dat)
        }
        else {
          string <- paste("desc(", input$chooseTime_Platform2, ")", sep = "")
          tabChoice3 <- tabChoice3 %>% arrange_(input$chooseTime_Platform1, string)
          # return(tabChoice3)
          hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
          hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
          if (is.null(hour5)) {
            dat <- data.frame("Click on a bar to show associated tweets")
            colnames(dat) <- "Click on a bar to show associated tweets"
            dat[, 1] <- ""
            return(dat)
          }
          else {
            dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
            dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
            dat5 <- dat4[, c(1, 2, 4, 8)]
            return(dat5)
          }
        }
      }
    }
  })

  ## Table to show trends in twitter use over time
  output$overTimeTrends <- renderDataTable({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Getting tweets")
      tab <- tweetStatisticsData()
      tab$weekday <- revalue(tab$weekday, c(
        "W" = "Wednesday", "Th" = "Thursday", "Sa" = "Saturday",
        "F" = "Friday", "Tu" = "Tuesday", "Su" = "Sunday", "M" = "Monday"
      ))
      tab1 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1, input$chooseTime_Platform2) %>% dplyr::summarise(n = n())
      incProgress(0.3, detail = "Plotting")
      return(data.frame(tab1))
    })
  })

  output$downloadTweetStats <- downloadHandler(
    filename = function() {
      paste("StatsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweetStatisticsData(), file)
    }
  )

  ## Twitter statistics for user's favorited tweets
  tweetStatisticsData_fav <- suppressWarnings(eventReactive(input$showTweetStats_fav, {
    tweets <- favorites(input$twitterUser4_fav, n = input$tweetNum4_fav)
    tab <- twListToDF(tweets)
    tab$hour <- hour(with_tz(tab$created, "EST"))
    tab$date <- as.Date(tab$created)
    tab$year <- year(tab$date)
    tab$year <- as.factor(tab$year)
    tab$month <- as.factor(months(tab$date))
    tab$weekday <- as.factor(weekdays(tab$date))
    tab$weekday <- factor(tab$weekday, levels = c(
      "Sunday", "Monday",
      "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
    ))
    tab$hour <- as.factor(tab$hour)

    tab$statusSource <- as.factor(tab$statusSource)
    tab$statusSource <- regmatches(tab$statusSource, gregexpr("(?<=>).*?(?=<)", tab$statusSource, perl = TRUE))
    tab$platform <- as.character(unlist(tab$statusSource))
    tab$platform <- as.factor(tab$platform)

    tab$platform <- gsub("Twitter Web Client", "Web Client", tab$platform)
    tab$platform <- gsub("Twitter for iPhone", "iPhone", tab$platform)
    tab$platform <- gsub("Twitter for Android", "Android", tab$platform)
    tab$platform <- gsub("Twitter for Mac", "Mac", tab$platform)
    tab$platform <- gsub("Twitter for iOS", "iOS", tab$platform)
    tab$platform <- gsub("Twitter for iPad", "iPad", tab$platform)
    tab$platform <- gsub("Tweetbot for Mac", "Mac bot", tab$platform)
    tab$platform <- gsub("Tweetbot for iOS", "iOS bot", tab$platform)
    tab$platform <- gsub("Meme_Twitterbot", "Meme bot", tab$platform)

    tab <- subset(tab, platform != "rtapp315156161")
    tab$weekday <- mapvalues(tab$weekday, from = c(
      "Sunday", "Monday", "Tuesday",
      "Wednesday", "Thursday", "Friday",
      "Saturday"
    ), to = c(
      "Su", "M", "Tu", "W",
      "Th", "F", "Sa"
    ))
    return(tab)
  }))

  ## Count Bar Plot to show twitter use trends over specified time/platform input
  output$countBarPlot_fav <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Getting tweets")
      tab <- tweetStatisticsData_fav()
      join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_fav, input$chooseTime_Platform2_fav) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      tab <- join5
      tab$count <- as.factor(as.character(tab$n))
      incProgress(0.3, detail = "Plotting")
      if ((input$chooseTime_Platform2_fav) == "hour") {
        tab <- as.data.frame(tab)
        tab[, 2] <- as.numeric(as.character(tab[, 2]))
        p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1_fav, y = "n", fill = input$chooseTime_Platform2_fav)) +
          geom_bar(stat = "identity") + theme_bw()
        ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot_fav")
      }
      else {
        p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1_fav, y = "n", fill = input$chooseTime_Platform2_fav)) +
          geom_bar(stat = "identity") + theme_bw()
        ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot_fav")
      }
    })
  })

  ## Click on bar plot to show associated tweets at the specifed time/platform for favorited tweets
  output$countStatsClick_fav <- renderDataTable({
    d <- event_data("plotly_click", source = "countBarPlot_fav")
    if (is.null(d)) {
      dat <- data.frame("Click on a bar to show associated tweets")
      colnames(dat) <- "Click on a bar to show associated tweets"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      tab <- tweetStatisticsData_fav()
      tab$weekday <- mapvalues(tab$weekday, from = c(
        "Su", "M", "Tu", "W",
        "Th", "F", "Sa"
      ), to = c(
        "Sunday", "Monday", "Tuesday",
        "Wednesday", "Thursday", "Friday",
        "Saturday"
      ))
      join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_fav, input$chooseTime_Platform2_fav) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      tab <- join5
      tab$count <- as.factor(as.character(tab$n))
      tab1 <- tab[, c(1, 2)]
      tabChoice1 <- tab[, 1]
      tabChoice1 <- unique(tabChoice1)
      tabChoice1 <- as.data.frame(tabChoice1)
      tabChoice2 <- cbind(as.data.frame(tabChoice1), 1:length(tabChoice1[, 1]))
      colnames(tabChoice2) <- c(input$chooseTime_Platform1_fav, "Variable1Val")
      tabChoice3 <- tabChoice1 %>% inner_join(tabChoice2)
      tabChoice3 <- tab %>% inner_join(tabChoice3)
      tabChoice3 <- as.data.frame(tabChoice3)
      if (input$chooseTime_Platform2_fav == "hour") {
        if (is.null(d)) {
          dat <- as.data.frame("Click events appear here (double-click to clear)")[1]
          return(dat)
        }
        else {
          hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
          hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
          dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
          dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
          dat5 <- dat4[, c(1, 2, 4, 8)]
          return(dat5)
        }
      }
      else {
        if (is.null(d)) {
          dat <- as.data.frame("Click events appear here (double-click to clear)")[1][, 1]
          return(dat)
        }
        else {
          string <- paste("desc(", input$chooseTime_Platform2_fav, ")", sep = "")
          tabChoice3 <- tabChoice3 %>% arrange_(input$chooseTime_Platform1_fav, string)
          # return(tabChoice3)
          hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
          hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
          if (is.null(hour5)) {
            dat <- data.frame("Click on a bar to show associated tweets")
            colnames(dat) <- "Click on a bar to show associated tweets"
            dat[, 1] <- ""
            return(dat)
          }
          else {
            dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
            dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
            dat5 <- dat4[, c(1, 2, 4, 8)]
            return(dat5)
          }
        }
      }
    }
  })

  ## Table to show trends in twitter use over time
  output$overTimeTrends_fav <- renderDataTable({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Getting tweets")
      tab <- tweetStatisticsData_fav()
      tab$weekday <- revalue(tab$weekday, c(
        "W" = "Wednesday", "Th" = "Thursday", "Sa" = "Saturday",
        "F" = "Friday", "Tu" = "Tuesday", "Su" = "Sunday", "M" = "Monday"
      ))
      tab1 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_fav, input$chooseTime_Platform2_fav) %>% dplyr::summarise(n = n())
      incProgress(0.3, detail = "Plotting")
      return(data.frame(tab1))
    })
  })

  output$downloadTweetStats_fav <- downloadHandler(
    filename = function() {
      paste("StatsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweetStatisticsData_fav(), file)
    }
  )

  ## Twitter statistics for topic tweets
  tweetStatisticsData_topic <- suppressWarnings(eventReactive(input$showTweetStats_topic, {
    tweets <- searchTwitter(input$twitterUser4_topic, n = input$tweetNum4_topic)
    tab <- twListToDF(tweets)
    tab$hour <- hour(with_tz(tab$created, "EST"))
    tab$date <- as.Date(tab$created)
    tab$year <- year(tab$date)
    tab$year <- as.factor(tab$year)
    tab$month <- as.factor(months(tab$date))
    tab$weekday <- as.factor(weekdays(tab$date))
    tab$weekday <- factor(tab$weekday, levels = c(
      "Sunday", "Monday",
      "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
    ))
    tab$hour <- as.factor(tab$hour)

    tab$statusSource <- as.factor(tab$statusSource)
    tab$statusSource <- regmatches(tab$statusSource, gregexpr("(?<=>).*?(?=<)", tab$statusSource, perl = TRUE))
    tab$platform <- as.character(unlist(tab$statusSource))
    tab$platform <- as.factor(tab$platform)

    tab$platform <- gsub("Twitter Web Client", "Web Client", tab$platform)
    tab$platform <- gsub("Twitter for iPhone", "iPhone", tab$platform)
    tab$platform <- gsub("Twitter for Android", "Android", tab$platform)
    tab$platform <- gsub("Twitter for Mac", "Mac", tab$platform)
    tab$platform <- gsub("Twitter for iOS", "iOS", tab$platform)
    tab$platform <- gsub("Twitter for iPad", "iPad", tab$platform)
    tab$platform <- gsub("Tweetbot for Mac", "Mac bot", tab$platform)
    tab$platform <- gsub("Tweetbot for iOS", "iOS bot", tab$platform)
    tab$platform <- gsub("Meme_Twitterbot", "Meme bot", tab$platform)

    tab <- subset(tab, platform != "rtapp315156161")
    tab$weekday <- mapvalues(tab$weekday, from = c(
      "Sunday", "Monday", "Tuesday",
      "Wednesday", "Thursday", "Friday",
      "Saturday"
    ), to = c(
      "Su", "M", "Tu", "W",
      "Th", "F", "Sa"
    ))
    return(tab)
  }))

  ## Count Bar Plot to show twitter use trends over specified time/platform input
  output$countBarPlot_topic <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Getting tweets")
      tab <- tweetStatisticsData_topic()
      join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_topic, input$chooseTime_Platform2_topic) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      tab <- join5
      tab$count <- as.factor(as.character(tab$n))
      incProgress(0.3, detail = "Plotting")
      if ((input$chooseTime_Platform2_fav) == "hour") {
        tab <- as.data.frame(tab)
        tab[, 2] <- as.numeric(as.character(tab[, 2]))
        p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1_topic, y = "n", fill = input$chooseTime_Platform2_topic)) +
          geom_bar(stat = "identity") + theme_bw()
        ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot_fav")
      }
      else {
        p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1_topic, y = "n", fill = input$chooseTime_Platform2_topic)) +
          geom_bar(stat = "identity") + theme_bw()
        ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot_fav")
      }
    })
  })

  ## Click on bar plot to show associated tweets at the specifed time/platform for user
  output$countStatsClick_topic <- renderDataTable({
    d <- event_data("plotly_click", source = "countBarPlot_fav")
    if (is.null(d)) {
      dat <- data.frame("Click on a bar to show associated tweets")
      colnames(dat) <- "Click on a bar to show associated tweets"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      tab <- tweetStatisticsData_topic()
      tab$weekday <- mapvalues(tab$weekday, from = c(
        "Su", "M", "Tu", "W",
        "Th", "F", "Sa"
      ), to = c(
        "Sunday", "Monday", "Tuesday",
        "Wednesday", "Thursday", "Friday",
        "Saturday"
      ))
      join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_topic, input$chooseTime_Platform2_topic) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      tab <- join5
      tab$count <- as.factor(as.character(tab$n))
      tab1 <- tab[, c(1, 2)]
      tabChoice1 <- tab[, 1]
      tabChoice1 <- unique(tabChoice1)
      tabChoice1 <- as.data.frame(tabChoice1)
      tabChoice2 <- cbind(as.data.frame(tabChoice1), 1:length(tabChoice1[, 1]))
      colnames(tabChoice2) <- c(input$chooseTime_Platform1_topic, "Variable1Val")
      tabChoice3 <- tabChoice1 %>% inner_join(tabChoice2)
      tabChoice3 <- tab %>% inner_join(tabChoice3)
      tabChoice3 <- as.data.frame(tabChoice3)
      if (input$chooseTime_Platform2_topic == "hour") {
        if (is.null(d)) {
          dat <- as.data.frame("Click events appear here (double-click to clear)")[1]
          return(dat)
        }
        else {
          hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
          hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
          dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
          dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
          dat5 <- dat4[, c(1, 2, 4, 8)]
          return(dat5)
        }
      }
      else {
        if (is.null(d)) {
          dat <- as.data.frame("Click events appear here (double-click to clear)")[1][, 1]
          return(dat)
        }
        else {
          string <- paste("desc(", input$chooseTime_Platform2_topic, ")", sep = "")
          tabChoice3 <- tabChoice3 %>% arrange_(input$chooseTime_Platform1_topic, string)
          # return(tabChoice3)
          hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
          hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
          if (is.null(hour5)) {
            dat <- data.frame("Click on a bar to show associated tweets")
            colnames(dat) <- "Click on a bar to show associated tweets"
            dat[, 1] <- ""
            return(dat)
          }
          else {
            dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
            dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
            dat5 <- dat4[, c(1, 2, 4, 8)]
            return(dat5)
          }
        }
      }
    }
  })

  ## Table to show trends in twitter use over time
  output$overTimeTrends_topic <- renderDataTable({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.7, detail = "Getting tweets")
      tab <- tweetStatisticsData_topic()
      tab$weekday <- revalue(tab$weekday, c(
        "W" = "Wednesday", "Th" = "Thursday", "Sa" = "Saturday",
        "F" = "Friday", "Tu" = "Tuesday", "Su" = "Sunday", "M" = "Monday"
      ))
      tab1 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_topic, input$chooseTime_Platform2_topic) %>% dplyr::summarise(n = n())
      incProgress(0.3, detail = "Plotting")
      return(data.frame(tab1))
    })
  })

  output$downloadTweetStats_topic <- downloadHandler(
    filename = function() {
      paste("StatsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweetStatisticsData_topic(), file)
    }
  )

  # End of Tweet Statistics

  # Word Cloud

  ## Tweet word cloud for the specified user
  tweetsCloudData <- eventReactive(input$showTweetCloud, {
    tweets <- userTimeline(
      input$twitterUser5, n = input$tweetNum5, includeRts = FALSE,
      excludeReplies = TRUE
    )
    tab <- twListToDF(tweets)
    reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    tab2 <- tab %>%
      dplyr::filter(!str_detect(text, "^RT")) %>%
      dplyr::mutate(text = str_replace_all(text, "http\\w+", "")) %>%
      unnest_tokens(word, text, token = "regex", pattern = reg) %>%
      dplyr::filter(
        !word %in% stop_words$word,
        str_detect(word, "[a-z]")
      )
    tab2 <- tab2 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
    return(tab2)
  })

  ## Plotting word cloud for topic tweets
  output$userTweetsCloud <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      col <- list(colorRampPalette(brewer.pal(9, "Blues"))(50))[[1]][37:50]
      incProgress(0.3, detail = "Building word cloud")
      tab <- tweetsCloudData()
      incProgress(0.7, detail = "Finishing...")
      tab2 <- subset(tab, n >= input$minWords)
      wordcloud(
        words = tab2$word, freq = tab2$n, min.freq = input$minWords, scale = c(3, 2), rot.per = 0,
        random.order = T, colors = col
      )
    })
  })

  ## Table of raw word cloud counts for topic tweets
  output$userTweetsCloudTab <- renderPlotly({
    tab <- tweetsCloudData()
    incProgress(0.3, detail = "Building table")
    tab2 <- subset(tab, n >= input$minWords)
    tab2 <- plyr::rename(tab2, replace = c("word" = "Word"))
    tab2 <- plyr::rename(tab2, replace = c("n" = "WordCount"))
    incProgress(0.7, detail = "Finishing...")
    y.text <- element_text(size = 8)

    tab2$Word <- factor(tab2$Word, levels = tab2$Word[order(tab2$WordCount)])
    userTweets <- ggplot(tab2, aes(x = WordCount, y = Word)) + geom_point(size = 2, color = "#2575B7") +
      ggtitle("Word Cloud Count") + theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = y.text)
    ggplotly(userTweets, tooltip = c("x", "y"))
  })

  ## Favorites tweet word cloud for the specified user
  favoritesCloudData <- eventReactive(input$showFavoriteCloud, {
    tweets <- favorites(input$twitterUser6, n = input$tweetNum6)
    tab <- twListToDF(tweets)
    reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    tab2 <- tab %>%
      dplyr::filter(!str_detect(text, "^RT")) %>%
      dplyr::mutate(text = str_replace_all(text, "http\\w+", "")) %>%
      unnest_tokens(word, text, token = "regex", pattern = reg) %>%
      dplyr::filter(
        !word %in% stop_words$word,
        str_detect(word, "[a-z]")
      )
    tab2 <- tab2 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
    return(tab2)
  })

  ## Plotting word cloud for favorite tweets
  output$userFavoritesCloud <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      col <- list(colorRampPalette(brewer.pal(9, "Blues"))(50))[[1]][37:50]
      incProgress(0.3, detail = "Building word cloud")
      tab <- favoritesCloudData()
      incProgress(0.7, detail = "Finishing...")
      tab2 <- subset(tab, n >= input$minWords2)
      wordcloud(
        words = tab2$word, freq = tab2$n, min.freq = input$minWords2, scale = c(3, 2), rot.per = 0,
        random.order = T, colors = col
      )
    })
  })

  ## Table of raw word cloud counts for favorite tweets
  output$userFavoritesCloudTab <- renderPlotly({
    tab <- favoritesCloudData()
    incProgress(0.3, detail = "Building table")
    tab2 <- subset(tab, n >= input$minWords2)
    tab2 <- plyr::rename(tab2, replace = c("word" = "Word"))
    tab2 <- plyr::rename(tab2, replace = c("n" = "WordCount"))
    incProgress(0.7, detail = "Finishing...")
    y.text <- element_text(size = 8)

    tab2$Word <- factor(tab2$Word, levels = tab2$Word[order(tab2$WordCount)])
    favTweets <- ggplot(tab2, aes(x = WordCount, y = Word)) + geom_point(size = 2, color = "#2575B7") +
      ggtitle("Word Cloud Count") + theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = y.text)
    ggplotly(favTweets, tooltip = c("x", "y"))
  })

  ## Tweet word cloud for the specified topic
  topicCloudData <- eventReactive(input$showTopicCloud, {
    tweets <- searchTwitter(input$twitterUser7, n = input$tweetNum7)
    tab <- twListToDF(tweets)
    reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    tab2 <- tab %>%
      dplyr::filter(!str_detect(text, "^RT")) %>%
      dplyr::mutate(text = str_replace_all(text, "http\\w+", "")) %>%
      unnest_tokens(word, text, token = "regex", pattern = reg) %>%
      dplyr::filter(
        !word %in% stop_words$word,
        str_detect(word, "[a-z]")
      )
    tab2 <- tab2 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
    return(tab2)
  })

  output$tweetTopicCloud <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      col <- list(colorRampPalette(brewer.pal(9, "Blues"))(50))[[1]][37:50]
      incProgress(0.3, detail = "Building word cloud")
      tab <- topicCloudData()
      incProgress(0.7, detail = "Finishing...")
      tab2 <- subset(tab, n >= input$minWords3)
      wordcloud(
        words = tab2$word, freq = tab2$n, min.freq = input$minWords3, scale = c(3, 2), rot.per = 0,
        colors = col
      )
    })
  })

  ## Table of raw word cloud counts for topic tweets
  output$topicTweetsCloudTab <- renderPlotly({
    tab <- topicCloudData()
    incProgress(0.3, detail = "Building table")
    tab2 <- subset(tab, n >= input$minWords3)
    tab2 <- plyr::rename(tab2, replace = c("word" = "Word"))
    tab2 <- plyr::rename(tab2, replace = c("n" = "WordCount"))
    incProgress(0.7, detail = "Finishing...")
    y.text <- element_text(size = 8)

    tab2$Word <- factor(tab2$Word, levels = tab2$Word[order(tab2$WordCount)])
    favTweets <- ggplot(tab2, aes(x = WordCount, y = Word)) + geom_point(size = 2, color = "#2575B7") +
      ggtitle("Word Cloud Count") + theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = y.text)
    ggplotly(favTweets, tooltip = c("x", "y"))
  })

  # End of Word Cloud

  # Topic Modeling

  ## User Tweets

  userTweetsTopicData <- eventReactive(input$showTopicModelUser, {
    withProgress(message = "Application loading", value = 0, {
      tweets <- userTimeline(input$twitterUser1_topic, n = input$tweetNum1_topic)
      tab <- twListToDF(tweets)

      reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
      tab2 <- tab %>%
        dplyr::filter(!str_detect(text, "^RT")) %>%
        dplyr::mutate(text = str_replace_all(text, "http\\w+", ""))

      tweets <- sapply(tab2$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

      incProgress(0.6, detail = "Collecting Tweets...")
      corpus <- Corpus(VectorSource(tweets))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      tdm <- DocumentTermMatrix(corpus)

      term_tfidf <- tapply(tdm$v / row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm) / col_sums(tdm > 0))
      incProgress(0.4, detail = "Finishing...")
      tdm <- tdm[, term_tfidf >= 0.1]
      tdm <- tdm[row_sums(tdm) > 0, ]
      return(tdm)
    })
  })

  output$userTweetsTopic <- renderPlotly({
    tdm <- userTweetsTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    top_terms <- chapter_topics %>%
      group_by(topic) %>%
      top_n(input$topWords1User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms$term <- as.factor(top_terms$term)
    topicPlot <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) +
      facet_wrap(~ topic, scales = "free") + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 8))

    ggplotly(topicPlot)
  })

  output$userTweetsTopicSpecific <- renderPlotly({
    tdm <- userTweetsTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    chapter_topic <- dplyr::filter(chapter_topics, topic == input$zoomUserTopic)
    top_terms <- chapter_topic %>%
      top_n(input$topWords1User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms$term <- as.factor(top_terms$term)
    topicPlot <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 7), axis.title.y = element_blank())
  })

  output$userTweetsTopicRelated <- renderDataTable({
    tdm <- userTweetsTopicData()
    chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")
    tidiedTDM <- tidy(tdm)

    topicTweets <- chapter_topics %>% dplyr::inner_join(tidiedTDM)
    data <- dplyr::filter(topicTweets, term == input$topicPlotTerm & topic == input$topicPlotTopic)
    return(data)
  })

  output$userTweetsTopicTable <- renderDataTable({
    tdm <- userTweetsTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    top_terms <- chapter_topics %>%
      group_by(topic) %>%
      top_n(input$topWords1User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    return(top_terms)
  })

  ## User Favorites

  userFavTopicData <- eventReactive(input$showTopicModelFavorite, {
    withProgress(message = "Application loading", value = 0, {
      tweets <- favorites(input$twitterUser2_topic, n = input$tweetNum2_topic)
      tab <- twListToDF(tweets)

      reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
      tab2 <- tab %>%
        dplyr::filter(!str_detect(text, "^RT")) %>%
        dplyr::mutate(text = str_replace_all(text, "http\\w+", ""))

      tweets <- sapply(tab2$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

      incProgress(0.6, detail = "Collecting Tweets...")
      corpus <- Corpus(VectorSource(tweets))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      tdm <- DocumentTermMatrix(corpus)

      term_tfidf <- tapply(tdm$v / row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm) / col_sums(tdm > 0))

      incProgress(0.4, detail = "Finishing...")
      tdm <- tdm[, term_tfidf >= 0.1]
      tdm <- tdm[row_sums(tdm) > 0, ]
      return(tdm)
    })
  })

  output$userFavoritesTopic <- renderPlotly({
    tdm <- userFavTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber2_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    top_terms <- chapter_topics %>%
      group_by(topic) %>%
      top_n(input$topWords2User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms$term <- as.factor(top_terms$term)
    topicPlot <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) +
      facet_wrap(~ topic, scales = "free") + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 8))

    ggplotly(topicPlot)
  })

  output$favTweetsTopicSpecific <- renderPlotly({
    tdm <- userFavTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber2_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    chapter_topic <- dplyr::filter(chapter_topics, topic == input$zoomFavTopic)
    top_terms <- chapter_topic %>%
      top_n(input$topWords2User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms$term <- as.factor(top_terms$term)
    topicPlot <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 7), axis.title.y = element_blank())
  })


  output$favoritesTweetsTopicRelated <- renderDataTable({
    tdm <- userFavTopicData()
    chapters_lda <- LDA(tdm, k = input$topicNumber2_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")
    tidiedTDM <- tidy(tdm)

    topicTweets <- chapter_topics %>% dplyr::inner_join(tidiedTDM)
    data <- dplyr::filter(topicTweets, term == input$favoritePlotTerm & topic == input$favoritePlotTopic)
    return(data)
  })

  output$userFavoritesTopicTable <- renderDataTable({
    tdm <- userFavTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber2_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    top_terms <- chapter_topics %>%
      group_by(topic) %>%
      top_n(input$topWords2User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    return(top_terms)
  })

  ## Topic Tweets

  topicTweetsTopicData <- eventReactive(input$showTopicModelTweets, {
    withProgress(message = "Application loading", value = 0, {
      tweets <- searchTwitter(input$twitterUser3_topic, n = input$tweetNum3_topic)
      tab <- twListToDF(tweets)

      reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
      tab2 <- tab %>%
        dplyr::filter(!str_detect(text, "^RT")) %>%
        dplyr::mutate(text = str_replace_all(text, "http\\w+", ""))

      tweets <- sapply(tab2$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
      incProgress(0.6, detail = "Collecting Tweets...")

      corpus <- Corpus(VectorSource(tweets))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      tdm <- DocumentTermMatrix(corpus)

      term_tfidf <- tapply(tdm$v / row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm) / col_sums(tdm > 0))
      incProgress(0.4, detail = "Finishing...")

      tdm <- tdm[, term_tfidf >= 0.1]
      tdm <- tdm[row_sums(tdm) > 0, ]
      return(tdm)
    })
  })

  output$topicTweetCloud <- renderPlotly({
    tdm <- topicTweetsTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    top_terms <- chapter_topics %>%
      group_by(topic) %>%
      top_n(input$topWords3User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms$term <- as.factor(top_terms$term)
    topicPlot <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) +
      facet_wrap(~ topic, scales = "free") + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 8))

    ggplotly(topicPlot)
  })

  output$topicTweetsSpecific <- renderPlotly({
    tdm <- topicTweetsTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    chapter_topic <- dplyr::filter(chapter_topics, topic == input$zoomTopicSpecific)
    top_terms <- chapter_topic %>%
      top_n(input$topWords3User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms$term <- as.factor(top_terms$term)
    topicPlot <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 7), axis.title.y = element_blank())
  })

  output$topicsTweetsTopicRelated <- renderDataTable({
    tdm <- topicTweetsTopicData()
    chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")
    tidiedTDM <- tidy(tdm)

    topicTweets <- chapter_topics %>% dplyr::inner_join(tidiedTDM)
    data <- dplyr::filter(topicTweets, term == input$topicTweetPlotTerm & topic == input$topicTweetPlotTopic)
    return(data)
  })

  output$topicTweetCloudTable <- renderDataTable({
    tdm <- topicTweetsTopicData()

    chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
    chapter_topics <- tidy(chapters_lda, matrix = "beta")

    top_terms <- chapter_topics %>%
      group_by(topic) %>%
      top_n(input$topWords3User, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    return(top_terms)
  })

  # End of Topic Modeling

  # Sentiment Analysis

  ## Prepares data for building sentiment word cloud, bar plot and time series
  userSentimentData <- eventReactive(input$showWordBarSentiment, {
    tweets <- userTimeline(input$twitterUser8, n = input$tweetNum8)
    incProgress(0.3, detail = "Getting topic tweets")
    tab <- twListToDF(tweets)
    tab3 <- unique(tab)
    reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
    tab4 <- tab3 %>%
      dplyr::filter(!str_detect(text, "^RT")) %>%
      dplyr::mutate(text2 = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", ""))
    tab4 <- tab4 %>%
      unnest_tokens(word, text2, token = "regex", pattern = reg) %>%
      dplyr::filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))
    tab4$date <- as.Date(tab4$created)
    return(tab4)
  })

  ## Building sentiment word cloud
  output$sentimentWordCloud <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- userSentimentData()
      wordTab <- join7 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
      incProgress(0.6, detail = "Building word cloud")
      join1 <- inner_join(wordTab, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::mutate(score = ifelse(sentiment == "negative", -1 * n, n))
      incProgress(0.4, detail = "Finishing...")
      join4 %>%
        dplyr::arrange(desc(n)) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(
          colors = c("#F8766D", "#00BFC4"),
          max.words = input$wordCloudNum
        )
    })
  })

  ## Building sentiment bar plot
  output$sentimentBarPlot <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- userSentimentData()
      wordTab <- join7 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
      incProgress(0.6, detail = "Building bar plot")
      join1 <- inner_join(wordTab, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::mutate(score = ifelse(sentiment == "negative", -1 * n, n))
      incProgress(0.4, detail = "Finishing...")
      join4 %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::group_by(sentiment) %>%
        top_n(n = input$barplotNum, abs(score)) %>%
        dplyr::mutate(word = reorder(word, score)) %>%
        ggplot(aes(x = reorder(word, -score), score, fill = score > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() + theme_bw()
    })
  })

  ## Preparing data for sentiment time series
  userBarPlotData <- eventReactive(input$showTimeSeriesSentiment, {
    withProgress(message = "Application loading", value = 0, {
      join7 <- userSentimentData()
      incProgress(0.6, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.4, detail = "Finishing...")
      return(join5)
    })
  })

  ## Building sentiment time series
  output$sentimentTimeSeries <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- userSentimentData()
      incProgress(0.6, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.2, detail = "Finishing...")
      join5 <- plyr::rename(join5, replace = c("n" = "Tweets"))
      incProgress(0.2, detail = "Plotting...")
      p <- ggplot(data = join5, aes(x = date, y = Tweets)) + geom_line(aes(colour = sentiment)) +
        geom_point(alpha = 0.6, size = 1.5, aes(text = word)) + theme_bw()
      p + (scale_x_date(labels = date_format("%b %y")))
      p
    })
  })

  ## Preparing to show point/sentiment specific tweets
  output$sentimentInfo <- renderDataTable({
    withProgress(message = "Application loading", value = 0, {
      join7 <- userSentimentData()
      incProgress(0.4, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.3, detail = "Processing...")
      join5 <- plyr::rename(join5, replace = c("n" = "Tweets"))
      if (is.null(input$plot1_click) == TRUE) {
        dat <- data.frame("Click a point to see relevant tweets by sentiment")
        colnames(dat) <- "Click a point to see relevant tweets by sentiment"
        dat[, 1] <- ""
        return(dat)
      }
      else {
        dat <- nearPoints(join5, input$plot1_click, addDist = TRUE)
        dat1 <- as.data.frame(dat)
        dat2 <- dat1 %>% dplyr::select(sentiment, date, word)
        dat4 <- as.data.frame(dat2) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
        dat5 <- dat4 %>% dplyr::select(sentiment, date, V3)
        incProgress(0.3, detail = "Finishing...")
        dat5 <- plyr::rename(dat5, replace = c("V3" = "tweets"))
        return(unique(dat5))
      }
    })
  })

  ## Preparing data for user favorite tweet sentiment analysis
  favoriteSentimentData <- eventReactive(input$showWordBarSentiment2, {
    tweets <- favorites(input$twitterUser9, n = input$tweetNum9)
    table <- twListToDF(tweets)
    reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

    table2 <- table %>%
      dplyr::filter(!str_detect(text, "^RT")) %>%
      dplyr::mutate(text2 = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", ""))

    tab3 <- table2 %>%
      unnest_tokens(word, text2, token = "regex", pattern = reg) %>%
      dplyr::filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))
    tab3$date <- as.Date(tab3$created)
    return(tab3)
  })

  ## Building sentiment word cloud for user favorite tweets
  output$sentimentWordCloud2 <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- favoriteSentimentData()
      wordTab <- join7 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
      incProgress(0.6, detail = "Building word cloud")
      join1 <- inner_join(wordTab, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::mutate(score = ifelse(sentiment == "negative", -1 * n, n))
      incProgress(0.4, detail = "Finishing...")
      join4 %>%
        dplyr::arrange(desc(n)) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(
          colors = c("#F8766D", "#00BFC4"),
          max.words = input$wordCloudNum2
        )
    })
  })

  ## Building bar plot for user favorite tweets
  output$sentimentBarPlot2 <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- favoriteSentimentData()
      wordTab <- join7 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
      incProgress(0.6, detail = "Building bar plot")
      join1 <- inner_join(wordTab, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::mutate(score = ifelse(sentiment == "negative", -1 * n, n))
      incProgress(0.4, detail = "Finishing...")
      join4 %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::group_by(sentiment) %>%
        top_n(n = input$barplotNum2, abs(score)) %>%
        dplyr::mutate(word = reorder(word, score)) %>%
        ggplot(aes(x = reorder(word, -score), score, fill = score > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() + theme_bw()
    })
  })

  ## Building time series for user favorite tweets
  output$sentimentTimeSeries2 <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- favoriteSentimentData()
      incProgress(0.6, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.2, detail = "Finishing...")
      join5 <- plyr::rename(join5, replace = c("n" = "Tweets"))
      incProgress(0.2, detail = "Plotting...")
      p <- ggplot(data = join5, aes(x = date, y = Tweets)) + geom_line(aes(colour = sentiment)) +
        geom_point(alpha = 0.6, size = 1.5) + theme_bw()
      p + (scale_x_date(labels = date_format("%b %y")))
      p
    })
  })

  ## Preparing to show point/sentiment specific favorite tweets
  output$sentimentInfo2 <- renderDataTable({
    withProgress(message = "Application loading", value = 0, {
      join7 <- favoriteSentimentData()
      incProgress(0.4, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.3, detail = "Processing...")
      join5 <- plyr::rename(join5, replace = c("n" = "Tweets"))
      if (is.null(input$plot2_click) == TRUE) {
        dat <- data.frame("Click a point to see relevant tweets by sentiment")
        colnames(dat) <- "Click a point to see relevant tweets by sentiment"
        dat[, 1] <- ""
        return(dat)
      }
      else {
        dat <- nearPoints(join5, input$plot2_click, addDist = TRUE)
        dat1 <- as.data.frame(dat)
        dat2 <- dat1 %>% dplyr::select(sentiment, date, word)
        dat4 <- as.data.frame(dat2) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
        dat5 <- dat4 %>% dplyr::select(sentiment, date, V3)
        incProgress(0.3, detail = "Finishing...")
        dat5 <- plyr::rename(dat5, replace = c("V3" = "tweets"))
        return(unique(dat5))
      }
    })
  })

  ## Preparing data for topic tweet sentiment analysis
  topicSentimentData <- eventReactive(input$showWordBarSentiment3, {
    tweets <- searchTwitter(input$twitterUser9_topic, n = input$tweetNum9_topic)
    table <- twListToDF(tweets)
    reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

    table2 <- table %>%
      dplyr::filter(!str_detect(text, "^RT")) %>%
      dplyr::mutate(text2 = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", ""))

    tab3 <- table2 %>%
      unnest_tokens(word, text2, token = "regex", pattern = reg) %>%
      dplyr::filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))
    tab3$date <- as.Date(tab3$created)
    return(tab3)
  })

  ## Building sentiment word cloud for topic tweets
  output$sentimentWordCloud3 <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- topicSentimentData()
      wordTab <- join7 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
      incProgress(0.6, detail = "Building word cloud")
      join1 <- inner_join(wordTab, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::mutate(score = ifelse(sentiment == "negative", -1 * n, n))
      incProgress(0.4, detail = "Finishing...")
      join4 %>%
        dplyr::arrange(desc(n)) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(
          colors = c("#F8766D", "#00BFC4"),
          max.words = input$wordCloudNum3
        )
    })
  })

  ## Building bar plot for topic tweets
  output$sentimentBarPlot3 <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- topicSentimentData()
      wordTab <- join7 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
      incProgress(0.6, detail = "Building bar plot")
      join1 <- inner_join(wordTab, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::mutate(score = ifelse(sentiment == "negative", -1 * n, n))
      incProgress(0.4, detail = "Finishing...")
      join4 %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::group_by(sentiment) %>%
        top_n(n = input$barplotNum3, abs(score)) %>%
        dplyr::mutate(word = reorder(word, score)) %>%
        ggplot(aes(x = reorder(word, -score), score, fill = score > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() + theme_bw()
    })
  })

  output$sentimentTimeSeries3 <- renderPlot({
    withProgress(message = "Application loading", value = 0, {
      join7 <- topicSentimentData()
      incProgress(0.6, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.2, detail = "Finishing...")
      join5 <- plyr::rename(join5, replace = c("n" = "Tweets"))
      incProgress(0.2, detail = "Plotting...")
      p <- ggplot(data = join5, aes(x = date, y = Tweets)) + geom_line(aes(colour = sentiment)) +
        geom_point(alpha = 0.6, size = 1.5) + theme_bw()
      p + (scale_x_date(labels = date_format("%b %y")))
      p
    })
  })

  ## Preparing to show point/sentiment specific topic tweets
  output$sentimentInfo3 <- renderDataTable({
    withProgress(message = "Application loading", value = 0, {
      join7 <- topicSentimentData()
      incProgress(0.4, detail = "Constructing time series")
      join1 <- inner_join(join7, sentiments, by = c("word" = "word"))
      join2 <- subset(join1, lexicon == "nrc")
      join3 <- dplyr::filter(join2, sentiment %in% c("positive", "negative"))
      join4 <- join3 %>% dplyr::select(word, sentiment, date, text)
      join4$word <- as.character(join4$word)
      join4$sentiment <- as.character(join4$sentiment)
      join5 <- join4 %>% dplyr::group_by(sentiment, date) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
      join5$n <- as.numeric(join5$n)
      for (i in 1:nrow(join5))
      {
        join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
      }
      incProgress(0.3, detail = "Processing...")
      join5 <- plyr::rename(join5, replace = c("n" = "Tweets"))
      if (is.null(input$plot3_click) == TRUE) {
        dat <- data.frame("Click a point to see relevant tweets by sentiment")
        colnames(dat) <- "Click a point to see relevant tweets by sentiment"
        dat[, 1] <- ""
        return(dat)
      }
      else {
        dat <- nearPoints(join5, input$plot3_click, addDist = TRUE)
        dat1 <- as.data.frame(dat)
        dat2 <- dat1 %>% dplyr::select(sentiment, date, word)
        dat4 <- as.data.frame(dat2) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
        dat5 <- dat4 %>% dplyr::select(sentiment, date, V3)
        incProgress(0.3, detail = "Finishing...")
        dat5 <- plyr::rename(dat5, replace = c("V3" = "tweets"))
        return(unique(dat5))
      }
    })
  })

  # End of Sentiment Analysis

  # Geographic Trend, Follower and Tweet Mapping Analysis

  ## Mapping Trends

  ### Retrieves trends for the specified map location
  trendsData <- eventReactive(input$map_click, {
    address <- try(revgeocode(c(click$lng, click$lat)))
    click <- input$map_click
    clng <- click$lng
    clat <- click$lat
    leafletProxy("map") %>%
      addCircles(
        lng = clng, lat = clat, group = "circles",
        weight = 20, radius = 100, color = "#0033FF",
        popup = address, fillOpacity = 0.5, opacity = 1
      )

    addressClick <- click

    progress4 <- shiny::Progress$new()
    on.exit(progress4$close())
    progress4$set(message = "Calculating trends", value = 0)

    clat <- addressClick$lat
    clng <- addressClick$lng
    obs <- closestTrendLocations(clat, clng)
    woe <- obs["woeid"]$woeid
    trends <- getTrends(woe)
    trend <- trends$name

    empt <- list()
    for (i in 1:nrow(trends))
    {
      trends$name[i] <- gsub("([[:upper:]])", " \\1", trends$name[i])
      trans <- translate(trends$name[i], detect.source(trends$name[i])[[1]], "en")
      if (length(trans) == 0) {
        trends$name[i] <- str_replace(gsub("\\s+", " ", str_trim(trends$name[i])), "B", "b")
        trends$name[i] <- gsub(" ", "", trends$name[i])
        empt <- rbind(empt, trends$name[i])
      }
      else {
        trans <- str_replace(gsub("\\s+", " ", str_trim(trans)), "B", "b")
        trans <- gsub(" ", "", trans)
        empt <- rbind(empt, trans)
      }
      progress4$inc(1 / i, detail = paste("Calculating trend", i))
    }

    empt2 <- data.frame(empt)
    total <- cbind(trend, empt2)
    total <- plyr::rename(total, c("empt" = "translation"))

    # total <- data.frame(trend)
    clat <- addressClick$lat
    clng <- addressClick$lng
    latitude <- paste("Latitude:", round(clat, 3), "Longitude:", round(clng, 3))

    address <- try(revgeocode(c(clng, clat)))
    address1 <- paste("Address:", address)

    total2 <- cbind(total, latitude)
    total3 <- cbind(total2, address1)
    total3$translation <- as.factor(as.character(total3$translation))
    total3$trend <- as.factor(as.character(total3$trend))
    return(total3)
  })

  output$trendTable <- renderDataTable({
    total3 <- trendsData()
    addressClick <- total3 %>% dplyr::select(trend, translation)
    return(addressClick)
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -73.9442, lat = 40.6782, zoom = 8) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE))
  })

  output$lat <- renderText({
    total3 <- trendsData()
    lat1 <- as.character(unique(total3$latitude)[1])
    return(lat1)
  })

  output$address <- renderText({
    total3 <- trendsData()
    address2 <- as.character(unique(total3$address1))[1]
    return(address2)
  })

  output$downloadGeographic <- downloadHandler(
    filename = function() {
      paste("TrendData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(trendsData(), file)
    }
  )

  ### Retrieve 10 tweets for the specified trend
  mapTrendsTweet <- eventReactive(input$showTrends, {
    withProgress(message = "Application loading", value = 0, {
      tweets <- searchTwitter(input$showTweetTrend, n = 10)
      incProgress(0.6, detail = "Getting tweets")
      tab <- twListToDF(tweets)
      text <- unique(tab$text)
      incProgress(0.3, detail = "Translating tweets")
      transl <- list()
      for (i in 1:length(text))
      {
        trans <- (translate(text[i], detect.source(text[i])[[1]], "en"))
        if (length(trans) == 0) {
          transl <- rbind(transl, text[i])
        }
        else {
          transl <- rbind(transl, trans)
        }
      }
      transl <- data.frame(transl)
      trendData <- cbind(text, transl)
      trendData <- plyr::rename(trendData, replace = c("text" = "Tweet"))
      trendData <- plyr::rename(trendData, replace = c("transl" = "Translated"))
      incProgress(0.1, detail = "Finishing...")
      return(trendData)
    })
  })

  output$tweetTable <- renderDataTable({
    return(mapTrendsTweet())
  })

  ## Mapping Following

  ### Retrieve location data for the specified user's following
  datasetFollow1 <- eventReactive(input$showFollowingMap, {
    user <- getUser(input$twitterUser10)
    follower <- user$getFriends(n = input$tweetNum10)
    progress22 <- shiny::Progress$new()
    on.exit(progress22$close())
    progress22$set(message = "Calculating following data", value = 0)

    datFrame <- data.frame()
    datFrame <- twListToDF(follower)
    datFrame <- datFrame %>% dplyr::select(screenName, location)
    empt <- subset(datFrame, location != "")
    empt$location <- as.character(empt$location)

    lon <- vector("double", nrow(empt))
    lat <- vector("double", nrow(empt))
    for (i in 1:nrow(empt))
    {
      lon[[i]] <- try(geocode(empt$location[i])$lon)
      lat[[i]] <- try(geocode(empt$location[i])$lat)
      progress22$inc(1 / nrow(empt), detail = paste("Data for following", i))
    }
    ll.visited <- as.data.frame(cbind(lon, lat))
    Tab2 <- ll.visited
    Tab3 <- cbind(Tab2, empt)
    return(Tab3)
  })

  ### Map the following of the specified user
  output$mapFollowing <- renderLeaflet({
    Tab2 <- datasetFollow1()
    Tab2 <- na.omit(Tab2)
    visit.x <- as.numeric(as.character(Tab2$lon))
    visit.y <- as.numeric(as.character(Tab2$lat))
    map2 <- leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng = visit.x, lat = visit.y, group = "circles",
        weight = 6, radius = 4, color = "#00aced",
        opacity = 1
      )
    return(map2)
  })

  ### Retrieve user attribute information for the chosen following
  observeEvent(input$mapFollowing_marker_click, {
    loc2 <- datasetFollow1()
    loc2 <- na.omit(loc2)
    p <- input$mapFollowing_marker_click
    lat1 <- p$lat
    lon1 <- p$lng
    loc2$lon2 <- as.factor(loc2$lon)
    loc2$lat2 <- as.factor(loc2$lat)
    datsmall <- subset(loc2, lat2 == lat1 & lon2 == lon1)
    datsmall <- datsmall[, !(colnames(datsmall) %in% c("lon2", "lat2"))]
    Name <- vector("character", nrow(datsmall))
    Description <- vector("character", nrow(datsmall))
    FavoritesCount <- vector("double", nrow(datsmall))
    FriendsCount <- vector("double", nrow(datsmall))
    StatusCount <- vector("double", nrow(datsmall))
    AccountCreated <- vector("character", nrow(datsmall))
    ScreenName <- vector("character", nrow(datsmall))
    Location <- vector("character", nrow(datsmall))
    for (i in 1:nrow(datsmall))
    {
      user <- getUser(datsmall$screenName[i])
      Name[[i]] <- user$name
      Description[[i]] <- user$description
      FavoritesCount[[i]] <- user$favoritesCount
      FriendsCount[[i]] <- user$friendsCount
      StatusCount[[i]] <- user$statusesCount
      AccountCreated[[i]] <- as.character(try(date(user$created)))
      ScreenName[[i]] <- datsmall$screenName[i]
      Location[[i]] <- datsmall$location[i]
    }
    userdes <- data.frame(cbind(
      ScreenName, Name, Description, Location,
      FriendsCount, StatusCount, FavoritesCount,
      AccountCreated
    ))
    output$followingDescription <- renderDataTable({
      return(userdes)
    })
  })

  ### Retrieve tweets for the chosen following
  observeEvent(input$mapFollowing_marker_click, {
    loc2 <- datasetFollow1()
    loc2 <- na.omit(loc2)
    p <- input$mapFollowing_marker_click
    lat1 <- p$lat
    lon1 <- p$lng
    loc2$lon2 <- as.factor(loc2$lon)
    loc2$lat2 <- as.factor(loc2$lat)
    datsmall <- subset(loc2, lat2 == lat1 & lon2 == lon1)
    datsmall <- datsmall[, !(colnames(datsmall) %in% c("lon2", "lat2"))]
    total <- data.frame()
    for (i in 1:nrow(datsmall))
    {
      dat <- data.frame(cbind(
        twListToDF(userTimeline(datsmall$screenName[i], n = 10))$text,
        datsmall$screenName[i]
      ))
      colnames(dat) <- c("Tweets", "ScreenName")
      dat2 <- dplyr::select(dat, ScreenName, Tweets)
      total <- rbind(total, dat2)
    }
    output$followingTweets <- renderDataTable({
      return(unique(total))
    })
  })

  output$downloadFollowing <- downloadHandler(
    filename = function() {
      paste("FollowingData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetFollow1(), file)
    }
  )

  ## Mapping Follower

  ### Retrieve location data for the specified user's followers
  datasetFollow <- eventReactive(input$showFollowerMap, {
    user <- getUser(input$twitterUser11)
    follower <- user$getFollowers(n = input$tweetNum11)
    progress2 <- shiny::Progress$new()
    on.exit(progress2$close())
    progress2$set(message = "Calculating follow data", value = 0)

    datFrame <- data.frame()
    datFrame <- twListToDF(follower)
    datFrame <- datFrame %>% dplyr::select(screenName, location)
    empt <- subset(datFrame, location != "")
    empt$location <- as.character(empt$location)
    lon <- vector("double", nrow(empt))
    lat <- vector("double", nrow(empt))
    for (i in 1:nrow(empt))
    {
      lon[[i]] <- try(geocode(empt$location[i])$lon)
      lat[[i]] <- try(geocode(empt$location[i])$lat)
      progress2$inc(1 / nrow(empt), detail = paste("Data for following", i))
    }
    ll.visited <- as.data.frame(cbind(lon, lat))
    Tab2 <- ll.visited
    Tab3 <- cbind(Tab2, empt)
    return(Tab3)
  })

  ### Map the followers of the specified user
  output$mapFollower <- renderLeaflet({
    Tab2 <- datasetFollow()
    Tab2 <- na.omit(Tab2)
    visit.x <- as.numeric(as.character(Tab2$lon))
    visit.y <- as.numeric(as.character(Tab2$lat))
    map2 <- leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng = visit.x, lat = visit.y, group = "circles",
        weight = 6, radius = 4, color = "#00aced",
        opacity = 1
      )
    return(map2)
  })

  ### Retrieve user attribute information for the chosen follower
  observeEvent(input$mapFollower_marker_click, {
    loc2 <- datasetFollow()
    loc2 <- na.omit(loc2)
    p <- input$mapFollower_marker_click
    lat1 <- p$lat
    lon1 <- p$lng
    loc2$lon2 <- as.factor(loc2$lon)
    loc2$lat2 <- as.factor(loc2$lat)
    datsmall <- subset(loc2, lat2 == lat1 & lon2 == lon1)
    datsmall <- datsmall[, !(colnames(datsmall) %in% c("lon2", "lat2"))]
    Name <- vector("character", nrow(datsmall))
    Description <- vector("character", nrow(datsmall))
    FavoritesCount <- vector("double", nrow(datsmall))
    FriendsCount <- vector("double", nrow(datsmall))
    StatusCount <- vector("double", nrow(datsmall))
    AccountCreated <- vector("character", nrow(datsmall))
    ScreenName <- vector("character", nrow(datsmall))
    Location <- vector("character", nrow(datsmall))
    for (i in 1:nrow(datsmall))
    {
      user <- getUser(datsmall$screenName[i])
      Name[[i]] <- user$name
      Description[[i]] <- user$description
      FavoritesCount[[i]] <- user$favoritesCount
      FriendsCount[[i]] <- user$friendsCount
      StatusCount[[i]] <- user$statusesCount
      AccountCreated[[i]] <- as.character(try(date(user$created)))
      ScreenName[[i]] <- datsmall$screenName[i]
      Location[[i]] <- datsmall$location[i]
    }
    userdes <- data.frame(cbind(
      ScreenName, Name, Description, Location,
      FriendsCount, StatusCount, FavoritesCount,
      AccountCreated
    ))
    output$followerDescription <- renderDataTable({
      return(userdes)
    })
  })

  ### Retrieve 10 tweets for the chosen follower
  observeEvent(input$mapFollower_marker_click, {
    loc2 <- datasetFollow()
    loc2 <- na.omit(loc2)
    p <- input$mapFollower_marker_click
    lat1 <- p$lat
    lon1 <- p$lng
    loc2$lon2 <- as.factor(loc2$lon)
    loc2$lat2 <- as.factor(loc2$lat)
    datsmall <- subset(loc2, lat2 == lat1 & lon2 == lon1)
    datsmall <- datsmall[, !(colnames(datsmall) %in% c("lon2", "lat2"))]
    total <- data.frame()
    for (i in 1:nrow(datsmall))
    {
      dat <- data.frame(cbind(
        twListToDF(userTimeline(datsmall$screenName[i], n = 10))$text,
        datsmall$screenName[i]
      ))
      colnames(dat) <- c("Tweets", "ScreenName")
      dat2 <- dplyr::select(dat, ScreenName, Tweets)
      total <- rbind(total, dat2)
    }
    output$followerTweets <- renderDataTable({
      return(unique(total))
    })
  })

  output$downloadFollower <- downloadHandler(
    filename = function() {
      paste("FollowerData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetFollow(), file)
    }
  )

  ### Retrieve mapped tweets data
  mapTweetsData <- eventReactive(input$showMapTweets, {
    tweets <- searchTwitter(input$twitterUser12, n = 500)
    tweetFrame <- twListToDF(tweets)

    userInfo <- lookupUsers(tweetFrame$screenName)
    userFrame <- twListToDF(userInfo)

    full <- inner_join(tweetFrame, userFrame, by = c("screenName" = "screenName"))
    full$location <- iconv(full$location, to = "ASCII", sub = NA)
    full2 <- subset(full, location != "NA" & location != "")

    userFrame <- full2
    user2 <- sample(userFrame, input$tweetNum12)
    user2$date <- as.Date(user2$created.x)
    user2$year <- year(user2$date)
    user2$year <- as.factor(user2$year)
    user2$month <- as.factor(months(user2$date))
    user2$weekday <- as.factor(weekdays(user2$date))
    user2$hour <- hour(with_tz(user2$created.x, "EST"))

    progress3 <- shiny::Progress$new()
    on.exit(progress3$close())
    progress3$set(message = "Calculating table", value = 1)

    loc <- data.frame()
    amt <- nrow(user2)
    for (i in 1:amt)
    {
      dat1 <- data.frame(try(geocode(user2[i, ]$location)))
      progress3$inc(1 / amt, detail = paste("Data for user", i))
      loc <- rbind(loc, dat1)
    }
    loc$location <- user2$location
    loc$retweetCount <- user2$retweetCount
    loc$year <- user2$year
    loc$text <- user2$text
    loc$weekday <- user2$weekday
    loc$hour <- user2$hour
    loc$month <- user2$month
    loc$screenName <- user2$screenName
    loc$favoritesCount <- user2$favoritesCount
    loc$followersCount <- user2$followersCount
    loc$friendsCount <- user2$friendsCount
    loc$statusesCount <- user2$statusesCount
    loc2 <- subset(loc, lon != "NA" & lat != "NA")
    incProgress(0.2, detail = "Finishing...")
    loc3 <- loc2 %>% dplyr::select(
      lon, lat, location, screenName, text, hour, weekday, month, year,
      retweetCount, favoritesCount, statusesCount, followersCount, friendsCount
    )
    return(loc3)
  })

  ### Plotting geolocated tweets and associated data
  output$mapTweets <- renderLeaflet({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.8, detail = "Getting tweets")
      loc2 <- mapTweetsData()
      var <- input$circleColor
      factpal <- colorFactor("viridis", loc2[, var])
      leaf <- leaflet() %>%
        addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(
          lng = loc2$lon, lat = loc2$lat,
          group = "circles",
          color = as.factor(factpal(loc2[, var])),
          weight = loc2[, input$circleCount] * input$circleSize,
          fillColor = "black", radius = 5
        )
      if (var == "hour") {
        leaf <- leaf %>% addLegend(pal = factpal, values = as.factor(loc2[, var]))
      }
      else {
        leaf <- leaf %>% addLegend(
          pal = factpal, values = (loc2[, var]),
          position = "bottomleft"
        )
      }
      incProgress(0.2, detail = "Finishing...")
      return(leaf)
    })
  })

  observeEvent(input$mapTweets_marker_click, {
    loc2 <- mapTweetsData()
    p <- input$mapTweets_marker_click
    lat1 <- p$lat
    lon1 <- p$lng
    loc2$lon2 <- as.factor(loc2$lon)
    loc2$lat2 <- as.factor(loc2$lat)
    datsmall <- subset(loc2, lat2 == lat1 & lon2 == lon1)
    datsmall <- datsmall[, !(colnames(datsmall) %in% c("lon2", "lat2"))]
    output$geolocatedTweets <- renderDataTable({
      return(datsmall)
    })
  })

  output$downloadTweetMapping <- downloadHandler(
    filename = function() {
      paste("TweetsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mapTweetsData(), file)
    }
  )

  # End of Geographic Trend, Follower and Tweet Mapping Analysis

  # Network Analysis

  ## Retweet data for network analysis
  retweetNetworkData <- eventReactive(input$showRetweetNetwork, {
    withProgress(message = "Application loading", value = 0, {
      tweets <- searchTwitter(input$twitterUser13, n = input$tweetNum13)
      incProgress(0.8, detail = "Getting tweets")
      tweet_txt <- sapply(tweets, function(x) x$getText())
      rt_patterns <- grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweet_txt, ignore.case = TRUE)

      who_retweet <- as.list(1:length(rt_patterns))
      who_post <- as.list(1:length(rt_patterns))
      what_text <- as.list(1:length(rt_patterns))
      tweet <- as.list(1:length(rt_patterns))
      for (i in 1:length(rt_patterns))
      {
        twit <- tweets[[rt_patterns[i]]]
        tweet[[i]] <- twit
        poster <- str_extract_all(
          twit$getText(),
          "(RT|via)((?:\\b\\W*@\\w+)+)"
        )
        poster <- gsub(":", "", unlist(poster))
        who_post[[i]] <- gsub("(RT @|via @)", "", poster, ignore.case = TRUE)
        who_post[[i]] <- sapply(who_post[[i]], function(row) iconv(row, "latin1", "ASCII", sub = ""))
        who_retweet[[i]] <- rep(twit$getScreenName(), length(poster))
        who_retweet[[i]] <- sapply(who_retweet[[i]], function(row) iconv(row, "latin1", "ASCII", sub = ""))
        what_text[[i]] <- gsub("^[^:]+:\\s*", "", twit$text)
        what_text[[i]] <- sapply(what_text[[i]], function(row) iconv(row, "latin1", "ASCII", sub = ""))
        what_text[[i]] <- str_replace_all(what_text[[i]], "[^[:alnum:]]", " ")
      }
      tweet2 <- twListToDF(tweet)
      d3 <- cbind(who_post, who_retweet, what_text, tweet2$text)
      d4 <- as.data.frame(d3)
      d4$who_retweet <- as.factor(as.character(d4$who_retweet))
      d4$who_post <- as.factor(as.character(d4$who_post))
      d4$what_text <- as.factor(as.character(d4$what_text))
      d4$V4 <- as.factor(as.character(d4$V4))
      incProgress(0.2, detail = "Finishing...")
      return(d4)
    })
  })

  ## Plots retweet network
  output$retweetNetwork <- renderVisNetwork({
    d4 <- retweetNetworkData()

    progress12 <- shiny::Progress$new()
    on.exit(progress12$close())
    progress12$set(message = "Calculating data", value = 0)

    who_retweet <- unlist(as.character(d4$who_retweet))
    who_post <- unlist(as.character(d4$who_post))
    tabC <- cbind(who_retweet, who_post)
    tabC <- as.data.frame(tabC)

    dat3 <- tabC %>% dplyr::group_by(who_post) %>% dplyr::mutate(count = n())
    small <- dat3
    small <- dat3 %>% dplyr::filter(count > input$retweetLimit)

    dat32 <- d4 %>% dplyr::select(who_post, what_text)

    unwho <- (unique(who_post))

    datSent <- data.frame()
    nrcjoy <- get_sentiments("nrc")

    unwho <- gsub(" ", "", unwho)
    for (i in 1:length(unwho))
    {
      gpdat <- subset(dat32, who_post == unwho[i])
      if (nrow(gpdat) == 0) {
        dat2 <- as.data.frame(cbind(unwho[i], "Null"))
        datSent <- rbind(dat2, datSent)
      }
      else {
        dat <- gpdat
        reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
        dat$text <- dat$what_text
        tidy_tweets <- dat %>%
          dplyr::filter(!str_detect(text, "^RT"))
        if (nrow(tidy_tweets) == 0) {
          dat2 <- as.data.frame(cbind(unwho[i], "Null"))
          datSent <- rbind(dat2, datSent)
        }
        else {
          tidy_tweets <- tidy_tweets %>%
            dplyr::mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", ""))
          if (nrow(tidy_tweets) == 0) {
            dat2 <- as.data.frame(cbind(unwho[i], "Null"))
            datSent <- rbind(dat2, datSent)
          }
          else {
            tidy_tweets <- tidy_tweets %>%
              unnest_tokens(word, text, token = "regex", pattern = reg)
            if (nrow(tidy_tweets) == 0) {
              dat2 <- as.data.frame(cbind(unwho[i], "Null"))
              datSent <- rbind(dat2, datSent)
            }
            else {
              tidy_tweets <- tidy_tweets %>%
                dplyr::filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))
              if (nrow(tidy_tweets) == 0) {
                dat2 <- as.data.frame(cbind(unwho[i], "Null"))
                datSent <- rbind(dat2, datSent)
              }
              else {
                tidy_tweets$word <- str_replace_all(tidy_tweets$word, "[[:punct:]]", "")
                wordc <- tidy_tweets %>%
                  inner_join(nrcjoy) %>%
                  dplyr::group_by(sentiment) %>%
                  dplyr::summarise(n = n()) %>%
                  subset(n == max(n))
                if (length(wordc$sentiment) == 0) {
                  dat2 <- as.data.frame(cbind(unwho[i], "Null"))
                  datSent <- rbind(dat2, datSent)
                }
                else {
                  dat2 <- as.data.frame(cbind(unwho[i], wordc$sentiment))
                  datSent <- rbind(dat2, datSent)
                }
              }
            }
          }
        }
      }
    }

    tab <- tally(datSent$V1)
    tall <- as.data.frame(tab)
    tall <- rename(tall, V1 = X)

    datSent2 <- datSent %>% inner_join(tall)
    dat3 <- subset(datSent2, Freq < 2)
    dat4 <- subset(dat3, V2 != "Null")

    dat4 <- rename(dat4, who_post = V1)
    dat5 <- dat4 %>% inner_join(small, by = c("who_post" = "who_post"))
    dat6 <- dat5 %>% dplyr::select(who_post, who_retweet)
    dat6 <- cbind(dat6, dat5$V2)
    colnames(dat6)[3] <- "sent"

    edges <- dat6 %>% dplyr::select(who_retweet, who_post)
    nodes <- dat6 %>% dplyr::select(who_post, sent)
    nodes <- rename(nodes, id = who_post)
    nodes <- rename(nodes, group = sent)

    col <- brewer.pal(12, "Set3")[as.factor(nodes$group)]
    nodes$color <- col
    nodes <- unique(nodes)

    who_retweet <- edges$who_retweet
    diffcol <- setdiff(who_retweet, nodes$id)
    bind1 <- cbind(diffcol, "Null")
    datb <- as.data.frame(bind1)
    colnames(datb)[1] <- "id"
    colnames(datb)[2] <- "group"
    fullNodes <- nodes %>% dplyr::select(id, group)
    fullNodes <- rbind(datb, fullNodes)
    fullNodes <- unique(fullNodes)
    edges <- rename(edges, from = who_retweet)
    edges <- rename(edges, to = who_post)

    fullNodes2 <- fullNodes
    if (nrow(fullNodes2) != nrow(edges)) {
      if (nrow(fullNodes2) > nrow(edges)) {
        length <- nrow(fullNodes2) - nrow(edges)
        edges$to <- as.character(edges$to)
        edges$from <- as.character(edges$from)
        for (i in 1:length)
        {
          edges <- rbind(edges, c("Null", "Null"))
        }
        fullDat <- cbind(edges, fullNodes2)
      }
      else {
        length <- nrow(edges) - nrow(fullNodes2)
        edges$to <- as.character(edges$to)
        edges$from <- as.character(edges$from)
        for (i in 1:length)
        {
          fullNodes2 <- rbind(fullNodes2, c("Null", "Null"))
        }
        fullDat <- cbind(edges, fullNodes2)
      }
    }
    else {
      fullDat <- cbind(edges, fullNodes2)
    }

    fullDat2 <- fullDat
    edges <- fullDat2 %>% dplyr::select(from, to)
    edges <- subset(edges, from != "Null" & to != "Null")
    fullNodes2 <- fullDat2 %>% dplyr::select(id, group)
    fullNodes2 <- subset(fullNodes2, id != "Null")
    edges$from <- as.factor(edges$from)
    edges$to <- as.factor(edges$to)

    nodeSelect <- visNetwork(fullNodes2, edges) %>%
      visEdges(width = 5, color = "#663300") %>%
      visGroups(groupname = "positive", color = "#E69F00") %>%
      visGroups(groupname = "disgust", color = "#CCCC99") %>%
      visGroups(groupname = "trust", color = "#CC79A7") %>%
      visGroups(groupname = "fear", color = "#CC9933") %>%
      visGroups(groupname = "negative", color = "#56B4E9") %>%
      visGroups(groupname = "anticipation", color = "#66CC00") %>%
      visGroups(groupname = "surprise", color = "#CC00CC") %>%
      visGroups(groupname = "anger", color = "#000033") %>%
      visGroups(groupname = "sadness", color = "#0033CC") %>%
      visGroups(groupname = "joy", color = "#D55E00") %>%
      visGroups(groupname = "Null", color = "#FFFFFF") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend() %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>%
      visInteraction(zoomView = TRUE)
  })

  ## Retrives tweets with point click
  output$retweetNetworkTable <- renderDataTable({
    d4 <- retweetNetworkData()
    subTweet <- subset(d4, who_post == input$current_node_id | who_retweet == input$current_node_id)
    text <- subTweet$V4
    text <- unique(text)
    as.data.table(text)
  })

  ## End retweet network

  ## Following Network

  ### Following data for network analysis
  followingNetworkData <- eventReactive(input$showFollowingNetwork, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating following data", value = 0)

    user <- getUser(input$twitterUser14)
    fcount <- user$friendsCount

    num <- input$tweetNum14

    friends <- user$getFriends(num)
    full <- data.frame()

    for (i in 1:length(friends))
    {
      user2 <- getUser(friends[i])
      fcount <- user2$followersCount
      small <- cbind(user2$screenName, fcount)
      full <- rbind(small, full)
      progress$inc(1 / length(friends), detail = paste("Data for follower", i))
    }
    full2 <- list(rep(user$screenName, nrow(full)))
    names(full2)[1] <- "from"
    full3 <- cbind(full, full2)

    full3 <- plyr::rename(full3, replace = c("V1" = "to"))
    full3 <- plyr::rename(full3, replace = c("fcount" = "follower"))
    full3 <- full3 %>% dplyr::select(from, to, follower)

    return(full3)
  })

  ### Plots following network
  output$followingNetwork <- renderVisNetwork({
    full3 <- followingNetworkData()
    full3 <- plyr::rename(full3, replace = c("follower" = "fcount"))

    full3$num <- as.numeric(as.character(full3$fcount))
    full4 <- full3 %>% dplyr::filter(num > input$lowLimitFollowing & num < input$upLimitFollowing)
    full3 <- full4

    full4 <- full3 %>% dplyr::select(from, to, fcount)
    full4$fcount <- as.numeric(as.character(full4$fcount))
    nodes <- full4$to
    nodes <- as.data.frame(nodes)
    names(nodes)[1] <- "id"
    nodes2 <- as.data.frame(setdiff(full3$from, nodes))
    names(nodes2)[1] <- "id"
    nodes <- rbind(nodes, nodes2)
    edges <- full4
    net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)
    V(net)$size <- E(net)$fcount / input$followerLimit
    visNet <- toVisNetworkData(net, idToLabel = TRUE)
    visNetwork(nodes = visNet$nodes, edges = visNet$edges) %>%
      visNodes(color = list(highlight = "#FFFFFF")) %>%
      visEvents(select = "function(nodes) {
              Shiny.onInputChange('current_node_id3', nodes.nodes);
              ;}")
  })

  ### Retrives tweets with point click
  output$followingNetworkTweets <- renderDataTable({
    if (is.null(input$current_node_id3)) {
      dat <- data.frame("Click vertex for follower info and related tweets")
      colnames(dat) <- "Click vertex for follower info and related tweets"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      full3 <- followingNetworkData()
      full4 <- full3 %>% dplyr::select(from, to, follower)
      full4$follower <- as.numeric(as.character(full4$follower))
      full4 <- plyr::rename(full4, replace = c("follower" = "NumberFriends"))
      f5 <- subset(full4, to == input$current_node_id3)
      person <- f5 %>% dplyr::select(to)
      tweets <- userTimeline(person, n = 10, excludeReplies = TRUE)
      tweetFr <- twListToDF(tweets)
      texts <- tweetFr$text
      create <- date(tweetFr$created)
      texts <- (as.data.frame(texts))
      create <- (as.data.frame(create))
      f6 <- cbind(f5, texts)
      f6 <- cbind(f6, create)
      as.data.table(f6)
    }
  })

  ### Retrieves user specific information
  output$followingNetworkUser <- renderDataTable({
    if (is.null(input$current_node_id3) == TRUE) {
      dat <- data.frame("Click vertex for follower info and related tweets")[1][, 1]
      # colnames(dat) <- "Click vertex for follower info and related tweets"
      # dat[,1] <- ""
      return(dat)
    }
    else {
      toNode <- input$current_node_id3
      user <- getUser(toNode)
      Categories <- c(
        "ScreenName", "Name", "Created", "Description", "Location", "FavoritesCount",
        "FollowersCount", "FriendsCount", "StatusesCount"
      )
      Values <- c(
        as.character(user$screenName), as.character(user$name),
        as.character(user$created),
        as.character(user$description), as.character(user$location),
        as.numeric(user$favoritesCount), as.character(user$followersCount),
        as.character(user$friendsCount), as.numeric(user$statusesCount)
      )
      return(as.data.frame(cbind(Categories, Values)))
    }
  })

  output$downloadFollowingData <- downloadHandler(
    filename = function() {
      paste("FollowingData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(followingNetworkData(), file)
    }
  )

  ## End following network

  ## Follower network

  ### Retweet data for network analysis
  followerNetworkData <- eventReactive(input$showFollowerNetwork, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating follower data", value = 0)
    user <- getUser(input$twitterUser15)

    num <- input$tweetNum15

    follower <- user$getFollowers(num)
    full <- data.frame()

    for (i in 1:length(follower))
    {
      user2 <- getUser(follower[i])
      fcount <- user2$followersCount
      small <- cbind(user2$screenName, fcount)
      full <- rbind(small, full)
      progress$inc(1 / length(follower), detail = paste("Data for follower", i))
    }
    full2 <- list(rep(user$screenName, nrow(full)))
    names(full2)[1] <- "from"
    full3 <- cbind(full, full2)
    full3 <- plyr::rename(full3, replace = c("V1" = "to"))
    full3 <- plyr::rename(full3, replace = c("fcount" = "follower"))
    full3 <- full3 %>% dplyr::select(from, to, follower)
    return(full3)
  })

  ### Plots follower network
  output$followerNetwork <- renderVisNetwork({
    full3 <- followerNetworkData()

    full3 <- plyr::rename(full3, replace = c("follower" = "fcount"))
    full3$num <- as.numeric(as.character(full3$fcount))
    full4 <- full3 %>% dplyr::filter(num > input$lowLimitFollower & num < input$upLimitFollower)

    full3 <- full4

    full4 <- full3 %>% dplyr::select(from, to, fcount)
    full4$fcount <- as.numeric(as.character(full4$fcount))
    nodes <- full4$to
    nodes <- as.data.frame(nodes)
    names(nodes)[1] <- "id"
    nodes2 <- as.data.frame(setdiff(full3$from, nodes))
    names(nodes2)[1] <- "id"
    nodes <- rbind(nodes, nodes2)
    edges <- full4
    net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)
    V(net)$size <- E(net)$fcount / input$followerLimit2
    visNet <- toVisNetworkData(net, idToLabel = TRUE)
    visNetwork(nodes = visNet$nodes, edges = visNet$edges) %>%
      visNodes(color = list(highlight = "#FFFFFF")) %>%
      visEvents(select = "function(nodes) {
              Shiny.onInputChange('current_node_id2', nodes.nodes);
              ;}")
  })

  ### Retrives tweets with point click
  output$followerNetworkTweets <- renderDataTable({
    if (is.null(input$current_node_id2) == TRUE) {
      dat <- data.frame("Click vertex for follower info and related tweets")[1][, 1]
      colnames(dat) <- "Click vertex for follower info and related tweets"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      full3 <- followerNetworkData()
      full4 <- full3 %>% dplyr::select(from, to, follower)
      full4$follower <- as.numeric(as.character(full4$follower))
      full4 <- plyr::rename(full4, replace = c("follower" = "NumberFriends"))
      f5 <- subset(full4, to == input$current_node_id2)
      person <- f5 %>% dplyr::select(to)
      tweets <- userTimeline(person, n = 10, excludeReplies = TRUE)
      tweetFr <- twListToDF(tweets)
      texts <- tweetFr$text
      create <- date(tweetFr$created)
      texts <- (as.data.frame(texts))
      create <- (as.data.frame(create))
      f6 <- cbind(f5, texts)
      f6 <- cbind(f6, create)
      as.data.table(f6)
    }
  })

  ### Retrieves user specific information for specified user
  output$followerNetworkUser <- renderDataTable({
    if (is.null(input$current_node_id2)) {
      dat <- data.frame("Click vertex for follower info and related tweets")[1][, 1]
      return(dat)
    }
    else {
      toNode <- input$current_node_id2
      user <- getUser(toNode)
      Categories <- c(
        "ScreenName", "Name", "Created", "Description", "Location", "FavoritesCount",
        "FollowersCount", "FriendsCount", "StatusesCount"
      )
      Values <- c(
        as.character(user$screenName), as.character(user$name),
        as.character(user$created),
        as.character(user$description), as.character(user$location),
        as.numeric(user$favoritesCount), as.character(user$followersCount),
        as.character(user$friendsCount), as.numeric(user$statusesCount)
      )
      return(as.data.frame(cbind(Categories, Values)))
    }
  })

  output$downloadFollowerData <- downloadHandler(
    filename = function() {
      paste("FollowerData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(followerNetworkData(), file)
    }
  )

  ## End of Follower Network
  # End of Network Analysis

  # Emoji Analysis

  ## Retrieves emoji trend data
  emojiTrendsDataUser <- eventReactive(input$showEmojiAnalysisUser, {
    progress43 <- shiny::Progress$new()
    on.exit(progress43$close())
    progress43$set(message = "Getting emoji data", value = 0)
    userTweets <- userTimeline(input$twitterUser16, n = input$tweetNum16)
    tab <- twListToDF(userTweets)
    tweet43 <- unique(tab)
    text <- data.frame(
      text = iconv(tweet43$text, "latin1", "ASCII", "byte"),
      stringsAsFactors = FALSE
    )
    tweet43$translate <- iconv(tweet43$text, "latin1", "ASCII", "byte")
    emoj <- list()
    des <- list()
    num <- list()
    act <- list()
    tweetnum <- list()
    dateC <- list()
    retweet3 <- list()
    favorite3 <- list()

    fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
    for (j in 1:nrow(tweet43))
    {
      progress43$inc(1 / nrow(tweet43), detail = paste("Emoji", j))
      str6 <- tweet43[j, ] %>% dplyr::select(translate)
      str5 <- str6$translate
      date <- tweet43[j, ] %>% dplyr::select(created)
      date2 <- date$created
      retweet <- tweet43[j, ] %>% dplyr::select(retweetCount)
      favorite <- tweet43[j, ] %>% dplyr::select(favoriteCount)
      for (i in fil8$R.encoding)
      {
        if (grepl(i, str5) == TRUE) {
          without <- suppressWarnings(gsub(i, "", str5))
          numT <- (nchar(str5) - nchar(without)) / nchar(i)
          sub <- subset(fil8, R.encoding == i)
          sub <- plyr::rename(sub, replace = c("Description" = "Emoji"))
          descr <- sub %>% dplyr::select(Emoji)
          actual <- sub %>% dplyr::select(Native)
          des <- rbind(des, descr)
          num <- rbind(num, numT)
          act <- rbind(act, actual)
          tweetnum <- rbind(tweetnum, str5)
          dateC <- rbind(dateC, as.character(date(date2)))
          retweet3 <- rbind(retweet3, retweet$retweetCount)
          favorite3 <- rbind(favorite3, favorite$favoriteCount)
        }
      }
    }

    num <- as.data.frame(num)
    dat1 <- cbind(des, act, num, tweetnum, dateC, retweet3, favorite3)

    df.expanded <- dat1[rep(row.names(dat1), dat1$V1), ]

    df.expanded$dateC <- as.character(df.expanded$dateC)
    df.expanded$dateC <- as.factor(df.expanded$dateC)

    df.expanded$V1 <- as.numeric(df.expanded$V1)
    df.expanded$retweet3 <- as.numeric(df.expanded$retweet3)
    df.expanded$favorite3 <- as.numeric(df.expanded$favorite3)
    return(df.expanded)
  })

  ## Plotting emoji trends data
  output$plotEmojiTrendsUser <- renderPlotly({
    df.expanded <- emojiTrendsDataUser()
    if (nrow(df.expanded) == 0) {
      g <- ggplot() + ggtitle("No Emojis for the specified user/topic")
      return(ggplotly(g, source = "emojiPlot"))
    }
    else {
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        talDat <- small %>% dplyr::group_by(Emoji) %>% dplyr::summarise(
          EmojiCount = sum(NumTweets), RetweetCount = sum(NumReTweets),
          FavCount = sum(NumFavorite)
        ) %>% dplyr::arrange(desc(EmojiCount), desc(RetweetCount), desc(FavCount))
        talDat <- plyr::rename(talDat, replace = c("Emoji" = "Description"))
        talDat2 <- talDat %>% dplyr::inner_join(fil8)
        talDat3 <- talDat2 %>% dplyr::select(Description, EmojiCount, Native, RetweetCount, FavCount)
        talDat4 <- unique(talDat3)
        talDat5 <- plyr::rename(talDat4, replace = c("Native" = "Emoji"))
        div <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(EmojiCount)))
        div2 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(RetweetCount)))
        div3 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(FavCount)))
        talDat5 <- dplyr::mutate(talDat5, EmojiPercent = EmojiCount / div)
        talDat5 <- dplyr::mutate(talDat5, RetweetPercent = RetweetCount / div2)
        talDat5 <- dplyr::mutate(talDat5, FavoritePercent = FavCount / div3)
        talDat6 <- talDat5 %>% dplyr::select(
          Description, Emoji, EmojiCount, EmojiPercent, RetweetCount, RetweetPercent, FavCount,
          FavoritePercent
        )
        talDat6$EmojiPercent <- round(talDat6$EmojiPercent, 4)
        talDat6$RetweetPercent <- round(talDat6$RetweetPercent, 4)
        talDat6$FavoritePercent <- round(talDat6$FavoritePercent, 4)
        talDat7 <- melt(talDat6, id.vars = c(
          "Description",
          "Emoji", "EmojiCount", "RetweetCount", "FavCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "EmojiPercent" = "TweetCount", "RetweetPercent" = "RetweetCount",
          "FavoritePercent" = "FavoriteCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "TweetCount" = "TweetPercent", "RetweetCount" = "RetweetPercent",
          "FavoriteCount" = "FavoritePercent"
        ))

        ft <- ggplot(talDat7, aes(x = Emoji, y = value, group = variable, colour = variable, text = EmojiCount)) + geom_point(size = 3) +
          ggtitle("Tracking Overall Emoji, Retweet and Favorite Percentage") + theme_bw() + theme(
            plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 11),
            axis.title.y = element_text(size = 9),
            legend.title = element_text(size = 9)
          )

        ggplotly(ft, tooltip = c("x", "y", "group"), source = "emojiPlot")
      }
      else {
        ft5 <- ggplot(small) + theme_bw()
        ggplotly(ft5, source = "emojiPlot")
      }
    }
  })

  ## Tweets data for the selected emoji
  output$emojiUserClick <- renderDataTable({
    d <- event_data("plotly_click", source = "emojiPlot")
    if (is.null(d)) {
      dat <- as.data.frame("Click a point to see emoji data")
      colnames(dat) <- "Click a point to see emoji data"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      ycord <- d$y
      df.expanded <- emojiTrendsDataUser()
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        talDat <- small %>% dplyr::group_by(Emoji) %>% dplyr::summarise(
          EmojiCount = sum(NumTweets), RetweetCount = sum(NumReTweets),
          FavCount = sum(NumFavorite)
        ) %>% dplyr::arrange(desc(EmojiCount), desc(RetweetCount), desc(FavCount))
        talDat <- plyr::rename(talDat, replace = c("Emoji" = "Description"))
        talDat2 <- talDat %>% dplyr::inner_join(fil8)
        talDat3 <- talDat2 %>% dplyr::select(Description, EmojiCount, Native, RetweetCount, FavCount)
        talDat4 <- unique(talDat3)
        talDat5 <- plyr::rename(talDat4, replace = c("Native" = "Emoji"))
        div <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(EmojiCount)))
        div2 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(RetweetCount)))
        div3 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(FavCount)))
        talDat5 <- dplyr::mutate(talDat5, EmojiPercent = EmojiCount / div)
        talDat5 <- dplyr::mutate(talDat5, RetweetPercent = RetweetCount / div2)
        talDat5 <- dplyr::mutate(talDat5, FavoritePercent = FavCount / div3)
        talDat6 <- talDat5 %>% dplyr::select(
          Description, Emoji, EmojiCount, EmojiPercent, RetweetCount, RetweetPercent, FavCount,
          FavoritePercent
        )
        talDat6$EmojiPercent <- round(talDat6$EmojiPercent, 4)
        talDat6$RetweetPercent <- round(talDat6$RetweetPercent, 4)
        talDat6$FavoritePercent <- round(talDat6$FavoritePercent, 4)
        talDat7 <- melt(talDat6, id.vars = c(
          "Description",
          "Emoji", "EmojiCount", "RetweetCount", "FavCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "EmojiPercent" = "TweetCount", "RetweetPercent" = "RetweetCount",
          "FavoritePercent" = "FavoriteCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "TweetCount" = "TweetPercent", "RetweetCount" = "RetweetPercent",
          "FavoriteCount" = "FavoritePercent"
        ))
        talDat71 <- dplyr::select(talDat7, Description, Emoji, value)
        talDat71 <- plyr::rename(talDat71, replace = c("Emoji" = "Native"))
        talDat71 <- plyr::rename(talDat71, replace = c("Description" = "Emoji"))
        df.expanded1 <- dplyr::select(df.expanded, Emoji, Native, tweetnum, dateC)
        talDat2 <- talDat71 %>% inner_join(df.expanded1)
        talDat3 <- subset(talDat2, value == ycord)
        for (i in 1:nrow(talDat3))
        {
          replace <- talDat3[i, ]$Native
          talDat3[i, ]$tweetnum <- suppressWarnings(gsub("\\<[^\\]]*\\>", toString(replace), talDat3[i, ]$tweetnum, perl = TRUE))
        }
        talDat3 <- plyr::rename(talDat3, replace = c("value" = "Value"))
        talDat3 <- plyr::rename(talDat3, replace = c("tweetnum" = "Tweets"))
        talDat3 <- plyr::rename(talDat3, replace = c("dateC" = "Date"))
        return(unique(talDat3))
      }
      else {
        return(small)
      }
    }
  })

  ## Retrieves emoji trends over time data
  output$emojiTrendsOverTimeUser <- renderDataTable({
    df.expanded <- emojiTrendsDataUser()
    if (nrow(df.expanded) == 0) {
      return(df.expanded)
    }
    else {
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        fil8 <- plyr::rename(fil8, replace = c("Description" = "Emoji"))
        smaller2 <- as.data.frame(small) %>% dplyr::inner_join(fil8)
        small3 <- c("Date", "Emoji", "NumTweets", "NumFavorite", "NumReTweets", "Native")
        small4 <- smaller2[small3]
        small4 <- unique(small4)

        div <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumTweets)))
        div2 <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumReTweets)))
        div3 <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumFavorite)))

        small4 <- dplyr::mutate(small4, EmojiPercent = NumTweets / div)
        small4 <- dplyr::mutate(small4, RetweetPercent = NumReTweets / div2)
        small4 <- dplyr::mutate(small4, FavoritePercent = NumFavorite / div3)

        small6 <- small4

        small6$EmojiPercent <- round(small6$EmojiPercent, 4)
        small6$RetweetPercent <- round(small6$RetweetPercent, 4)
        small6$FavoritePercent <- round(small6$FavoritePercent, 4)

        small55 <- small6 %>% dplyr::select(Date, Emoji, Native, EmojiPercent, RetweetPercent, FavoritePercent)
        return(data.frame(small55))
      }
      else {
        return(data.frame(small))
      }
    }
  })

  ## Retrieves emoji trend data for favorited tweets
  emojiTrendsDataFav <- eventReactive(input$showEmojiAnalysisFav, {
    progress43 <- shiny::Progress$new()
    on.exit(progress43$close())
    progress43$set(message = "Getting emoji data", value = 0)
    userTweets <- favorites(input$twitterUser17, n = input$tweetNum17)
    tab <- twListToDF(userTweets)
    tweet43 <- unique(tab)
    text <- data.frame(
      text = iconv(tweet43$text, "latin1", "ASCII", "byte"),
      stringsAsFactors = FALSE
    )
    tweet43$translate <- iconv(tweet43$text, "latin1", "ASCII", "byte")
    emoj <- list()
    des <- list()
    num <- list()
    act <- list()
    tweetnum <- list()
    dateC <- list()
    retweet3 <- list()
    favorite3 <- list()

    fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
    for (j in 1:nrow(tweet43))
    {
      progress43$inc(1 / nrow(tweet43), detail = paste("Emoji", j))
      str6 <- tweet43[j, ] %>% dplyr::select(translate)
      str5 <- str6$translate
      date <- tweet43[j, ] %>% dplyr::select(created)
      date2 <- date$created
      retweet <- tweet43[j, ] %>% dplyr::select(retweetCount)
      favorite <- tweet43[j, ] %>% dplyr::select(favoriteCount)
      for (i in fil8$R.encoding)
      {
        if (grepl(i, str5) == TRUE) {
          without <- suppressWarnings(gsub(i, "", str5))
          numT <- (nchar(str5) - nchar(without)) / nchar(i)
          sub <- subset(fil8, R.encoding == i)
          sub <- plyr::rename(sub, replace = c("Description" = "Emoji"))
          descr <- sub %>% dplyr::select(Emoji)
          actual <- sub %>% dplyr::select(Native)
          des <- rbind(des, descr)
          num <- rbind(num, numT)
          act <- rbind(act, actual)
          tweetnum <- rbind(tweetnum, str5)
          dateC <- rbind(dateC, as.character(date(date2)))
          retweet3 <- rbind(retweet3, retweet$retweetCount)
          favorite3 <- rbind(favorite3, favorite$favoriteCount)
        }
      }
    }

    num <- as.data.frame(num)
    dat1 <- cbind(des, act, num, tweetnum, dateC, retweet3, favorite3)

    df.expanded <- dat1[rep(row.names(dat1), dat1$V1), ]

    df.expanded$dateC <- as.character(df.expanded$dateC)
    df.expanded$dateC <- as.factor(df.expanded$dateC)

    df.expanded$V1 <- as.numeric(df.expanded$V1)
    df.expanded$retweet3 <- as.numeric(df.expanded$retweet3)
    df.expanded$favorite3 <- as.numeric(df.expanded$favorite3)
    return(df.expanded)
  })

  ## Plotting emoji trends data
  output$plotEmojiTrendsFav <- renderPlotly({
    df.expanded <- emojiTrendsDataFav()
    if (nrow(df.expanded) == 0) {
      g <- ggplot() + ggtitle("No Emojis for the specified user/topic")
      return(ggplotly(g, source = "emojiPlot"))
    }
    else {
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        talDat <- small %>% dplyr::group_by(Emoji) %>% dplyr::summarise(
          EmojiCount = sum(NumTweets), RetweetCount = sum(NumReTweets),
          FavCount = sum(NumFavorite)
        ) %>% dplyr::arrange(desc(EmojiCount), desc(RetweetCount), desc(FavCount))
        talDat <- plyr::rename(talDat, replace = c("Emoji" = "Description"))
        talDat2 <- talDat %>% dplyr::inner_join(fil8)
        talDat3 <- talDat2 %>% dplyr::select(Description, EmojiCount, Native, RetweetCount, FavCount)
        talDat4 <- unique(talDat3)
        talDat5 <- plyr::rename(talDat4, replace = c("Native" = "Emoji"))
        div <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(EmojiCount)))
        div2 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(RetweetCount)))
        div3 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(FavCount)))
        talDat5 <- dplyr::mutate(talDat5, EmojiPercent = EmojiCount / div)
        talDat5 <- dplyr::mutate(talDat5, RetweetPercent = RetweetCount / div2)
        talDat5 <- dplyr::mutate(talDat5, FavoritePercent = FavCount / div3)
        talDat6 <- talDat5 %>% dplyr::select(
          Description, Emoji, EmojiCount, EmojiPercent, RetweetCount, RetweetPercent, FavCount,
          FavoritePercent
        )
        talDat6$EmojiPercent <- round(talDat6$EmojiPercent, 4)
        talDat6$RetweetPercent <- round(talDat6$RetweetPercent, 4)
        talDat6$FavoritePercent <- round(talDat6$FavoritePercent, 4)
        talDat7 <- melt(talDat6, id.vars = c(
          "Description",
          "Emoji", "EmojiCount", "RetweetCount", "FavCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "EmojiPercent" = "TweetCount", "RetweetPercent" = "RetweetCount",
          "FavoritePercent" = "FavoriteCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "TweetCount" = "TweetPercent", "RetweetCount" = "RetweetPercent",
          "FavoriteCount" = "FavoritePercent"
        ))

        ft <- ggplot(talDat7, aes(x = Emoji, y = value, group = variable, colour = variable, text = EmojiCount)) + geom_point(size = 3) +
          ggtitle("Tracking Overall Emoji, Retweet and Favorite Percentage") + theme_bw() + theme(
            plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 11),
            axis.title.y = element_text(size = 9),
            legend.title = element_text(size = 9)
          )

        ggplotly(ft, tooltip = c("x", "y", "group"), source = "emojiPlot")
      }
      else {
        ft5 <- ggplot(small) + theme_bw()
        ggplotly(ft5, source = "emojiPlot")
      }
    }
  })

  ## Tweets data for the selected emoji
  output$emojiFavClick <- renderDataTable({
    d <- event_data("plotly_click", source = "emojiPlot")
    if (is.null(d)) {
      dat <- as.data.frame("Click a point to see emoji data")
      colnames(dat) <- "Click a point to see emoji data"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      ycord <- d$y
      df.expanded <- emojiTrendsDataFav()
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        talDat <- small %>% dplyr::group_by(Emoji) %>% dplyr::summarise(
          EmojiCount = sum(NumTweets), RetweetCount = sum(NumReTweets),
          FavCount = sum(NumFavorite)
        ) %>% dplyr::arrange(desc(EmojiCount), desc(RetweetCount), desc(FavCount))
        talDat <- plyr::rename(talDat, replace = c("Emoji" = "Description"))
        talDat2 <- talDat %>% dplyr::inner_join(fil8)
        talDat3 <- talDat2 %>% dplyr::select(Description, EmojiCount, Native, RetweetCount, FavCount)
        talDat4 <- unique(talDat3)
        talDat5 <- plyr::rename(talDat4, replace = c("Native" = "Emoji"))
        div <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(EmojiCount)))
        div2 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(RetweetCount)))
        div3 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(FavCount)))
        talDat5 <- dplyr::mutate(talDat5, EmojiPercent = EmojiCount / div)
        talDat5 <- dplyr::mutate(talDat5, RetweetPercent = RetweetCount / div2)
        talDat5 <- dplyr::mutate(talDat5, FavoritePercent = FavCount / div3)
        talDat6 <- talDat5 %>% dplyr::select(
          Description, Emoji, EmojiCount, EmojiPercent, RetweetCount, RetweetPercent, FavCount,
          FavoritePercent
        )
        talDat6$EmojiPercent <- round(talDat6$EmojiPercent, 4)
        talDat6$RetweetPercent <- round(talDat6$RetweetPercent, 4)
        talDat6$FavoritePercent <- round(talDat6$FavoritePercent, 4)
        talDat7 <- melt(talDat6, id.vars = c(
          "Description",
          "Emoji", "EmojiCount", "RetweetCount", "FavCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "EmojiPercent" = "TweetCount", "RetweetPercent" = "RetweetCount",
          "FavoritePercent" = "FavoriteCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "TweetCount" = "TweetPercent", "RetweetCount" = "RetweetPercent",
          "FavoriteCount" = "FavoritePercent"
        ))
        talDat71 <- dplyr::select(talDat7, Description, Emoji, value)
        talDat71 <- plyr::rename(talDat71, replace = c("Emoji" = "Native"))
        talDat71 <- plyr::rename(talDat71, replace = c("Description" = "Emoji"))
        df.expanded1 <- dplyr::select(df.expanded, Emoji, Native, tweetnum, dateC)
        talDat2 <- talDat71 %>% inner_join(df.expanded1)
        talDat3 <- subset(talDat2, value == ycord)
        for (i in 1:nrow(talDat3))
        {
          replace <- talDat3[i, ]$Native
          talDat3[i, ]$tweetnum <- suppressWarnings(gsub("\\<[^\\]]*\\>", toString(replace), talDat3[i, ]$tweetnum, perl = TRUE))
        }
        talDat3 <- plyr::rename(talDat3, replace = c("value" = "Value"))
        talDat3 <- plyr::rename(talDat3, replace = c("tweetnum" = "Tweets"))
        talDat3 <- plyr::rename(talDat3, replace = c("dateC" = "Date"))
        return(unique(talDat3))
      }
      else {
        return(small)
      }
    }
  })

  ## Retrieves emoji trends over time data
  output$emojiTrendsOverTimeFav <- renderDataTable({
    df.expanded <- emojiTrendsDataFav()
    if (nrow(df.expanded) == 0) {
      return(df.expanded)
    }
    else {
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        fil8 <- plyr::rename(fil8, replace = c("Description" = "Emoji"))
        smaller2 <- as.data.frame(small) %>% dplyr::inner_join(fil8)
        small3 <- c("Date", "Emoji", "NumTweets", "NumFavorite", "NumReTweets", "Native")
        small4 <- smaller2[small3]
        small4 <- unique(small4)

        div <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumTweets)))
        div2 <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumReTweets)))
        div3 <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumFavorite)))

        small4 <- dplyr::mutate(small4, EmojiPercent = NumTweets / div)
        small4 <- dplyr::mutate(small4, RetweetPercent = NumReTweets / div2)
        small4 <- dplyr::mutate(small4, FavoritePercent = NumFavorite / div3)

        small6 <- small4

        small6$EmojiPercent <- round(small6$EmojiPercent, 4)
        small6$RetweetPercent <- round(small6$RetweetPercent, 4)
        small6$FavoritePercent <- round(small6$FavoritePercent, 4)

        small55 <- small6 %>% dplyr::select(Date, Emoji, Native, EmojiPercent, RetweetPercent, FavoritePercent)
        return(data.frame(small55))
      }
      else {
        return(data.frame(small))
      }
    }
  })

  ## Retrieves emoji trend data for specified topic
  emojiTrendsDataTopic <- eventReactive(input$showEmojiAnalysisTopic, {
    progress43 <- shiny::Progress$new()
    on.exit(progress43$close())
    progress43$set(message = "Getting emoji data", value = 0)
    userTweets <- searchTwitter(input$twitterUser18, n = input$tweetNum18)
    # userTweets <- favorites(input$twitterUser18, n = input$tweetNum18)
    tab <- twListToDF(userTweets)
    tweet43 <- unique(tab)
    text <- data.frame(
      text = iconv(tweet43$text, "latin1", "ASCII", "byte"),
      stringsAsFactors = FALSE
    )
    tweet43$translate <- iconv(tweet43$text, "latin1", "ASCII", "byte")
    emoj <- list()
    des <- list()
    num <- list()
    act <- list()
    tweetnum <- list()
    dateC <- list()
    retweet3 <- list()
    favorite3 <- list()

    fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
    for (j in 1:nrow(tweet43))
    {
      progress43$inc(1 / nrow(tweet43), detail = paste("Emoji", j))
      str6 <- tweet43[j, ] %>% dplyr::select(translate)
      str5 <- str6$translate
      date <- tweet43[j, ] %>% dplyr::select(created)
      date2 <- date$created
      retweet <- tweet43[j, ] %>% dplyr::select(retweetCount)
      favorite <- tweet43[j, ] %>% dplyr::select(favoriteCount)
      for (i in fil8$R.encoding)
      {
        if (grepl(i, str5) == TRUE) {
          without <- suppressWarnings(gsub(i, "", str5))
          numT <- (nchar(str5) - nchar(without)) / nchar(i)
          sub <- subset(fil8, R.encoding == i)
          sub <- plyr::rename(sub, replace = c("Description" = "Emoji"))
          descr <- sub %>% dplyr::select(Emoji)
          actual <- sub %>% dplyr::select(Native)
          des <- rbind(des, descr)
          num <- rbind(num, numT)
          act <- rbind(act, actual)
          tweetnum <- rbind(tweetnum, str5)
          dateC <- rbind(dateC, as.character(date(date2)))
          retweet3 <- rbind(retweet3, retweet$retweetCount)
          favorite3 <- rbind(favorite3, favorite$favoriteCount)
        }
      }
    }

    num <- as.data.frame(num)
    dat1 <- cbind(des, act, num, tweetnum, dateC, retweet3, favorite3)

    df.expanded <- dat1[rep(row.names(dat1), dat1$V1), ]

    df.expanded$dateC <- as.character(df.expanded$dateC)
    df.expanded$dateC <- as.factor(df.expanded$dateC)

    df.expanded$V1 <- as.numeric(df.expanded$V1)
    df.expanded$retweet3 <- as.numeric(df.expanded$retweet3)
    df.expanded$favorite3 <- as.numeric(df.expanded$favorite3)
    return(df.expanded)
  })

  ## Plotting emoji trends data
  output$plotEmojiTrendsTopic <- renderPlotly({
    df.expanded <- emojiTrendsDataTopic()
    if (nrow(df.expanded) == 0) {
      g <- ggplot() + ggtitle("No Emojis for the specified user/topic")
      return(ggplotly(g, source = "emojiPlot"))
    }
    else {
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        talDat <- small %>% dplyr::group_by(Emoji) %>% dplyr::summarise(
          EmojiCount = sum(NumTweets), RetweetCount = sum(NumReTweets),
          FavCount = sum(NumFavorite)
        ) %>% dplyr::arrange(desc(EmojiCount), desc(RetweetCount), desc(FavCount))
        talDat <- plyr::rename(talDat, replace = c("Emoji" = "Description"))
        talDat2 <- talDat %>% dplyr::inner_join(fil8)
        talDat3 <- talDat2 %>% dplyr::select(Description, EmojiCount, Native, RetweetCount, FavCount)
        talDat4 <- unique(talDat3)
        talDat5 <- plyr::rename(talDat4, replace = c("Native" = "Emoji"))
        div <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(EmojiCount)))
        div2 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(RetweetCount)))
        div3 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(FavCount)))
        talDat5 <- dplyr::mutate(talDat5, EmojiPercent = EmojiCount / div)
        talDat5 <- dplyr::mutate(talDat5, RetweetPercent = RetweetCount / div2)
        talDat5 <- dplyr::mutate(talDat5, FavoritePercent = FavCount / div3)
        talDat6 <- talDat5 %>% dplyr::select(
          Description, Emoji, EmojiCount, EmojiPercent, RetweetCount, RetweetPercent, FavCount,
          FavoritePercent
        )
        talDat6$EmojiPercent <- round(talDat6$EmojiPercent, 4)
        talDat6$RetweetPercent <- round(talDat6$RetweetPercent, 4)
        talDat6$FavoritePercent <- round(talDat6$FavoritePercent, 4)
        talDat7 <- melt(talDat6, id.vars = c(
          "Description",
          "Emoji", "EmojiCount", "RetweetCount", "FavCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "EmojiPercent" = "TweetCount", "RetweetPercent" = "RetweetCount",
          "FavoritePercent" = "FavoriteCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "TweetCount" = "TweetPercent", "RetweetCount" = "RetweetPercent",
          "FavoriteCount" = "FavoritePercent"
        ))

        ft <- ggplot(talDat7, aes(x = Emoji, y = value, group = variable, colour = variable, text = EmojiCount)) + geom_point(size = 3) +
          ggtitle("Tracking Overall Emoji, Retweet and Favorite Percentage") + theme_bw() + theme(
            plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 11),
            axis.title.y = element_text(size = 9),
            legend.title = element_text(size = 9)
          )

        ggplotly(ft, tooltip = c("x", "y", "group"), source = "emojiPlot")
      }
      else {
        ft5 <- ggplot(small) + theme_bw()
        ggplotly(ft5, source = "emojiPlot")
      }
    }
  })

  ## Tweets data for the selected emoji
  output$emojiTopicClick <- renderDataTable({
    d <- event_data("plotly_click", source = "emojiPlot")
    if (is.null(d)) {
      dat <- as.data.frame("Click a point to see emoji data")
      colnames(dat) <- "Click a point to see emoji data"
      dat[, 1] <- ""
      return(dat)
    }
    else {
      ycord <- d$y
      df.expanded <- emojiTrendsDataTopic()
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        talDat <- small %>% dplyr::group_by(Emoji) %>% dplyr::summarise(
          EmojiCount = sum(NumTweets), RetweetCount = sum(NumReTweets),
          FavCount = sum(NumFavorite)
        ) %>% dplyr::arrange(desc(EmojiCount), desc(RetweetCount), desc(FavCount))
        talDat <- plyr::rename(talDat, replace = c("Emoji" = "Description"))
        talDat2 <- talDat %>% dplyr::inner_join(fil8)
        talDat3 <- talDat2 %>% dplyr::select(Description, EmojiCount, Native, RetweetCount, FavCount)
        talDat4 <- unique(talDat3)
        talDat5 <- plyr::rename(talDat4, replace = c("Native" = "Emoji"))
        div <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(EmojiCount)))
        div2 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(RetweetCount)))
        div3 <- as.numeric(talDat5 %>% dplyr::summarise(tot = sum(FavCount)))
        talDat5 <- dplyr::mutate(talDat5, EmojiPercent = EmojiCount / div)
        talDat5 <- dplyr::mutate(talDat5, RetweetPercent = RetweetCount / div2)
        talDat5 <- dplyr::mutate(talDat5, FavoritePercent = FavCount / div3)
        talDat6 <- talDat5 %>% dplyr::select(
          Description, Emoji, EmojiCount, EmojiPercent, RetweetCount, RetweetPercent, FavCount,
          FavoritePercent
        )
        talDat6$EmojiPercent <- round(talDat6$EmojiPercent, 4)
        talDat6$RetweetPercent <- round(talDat6$RetweetPercent, 4)
        talDat6$FavoritePercent <- round(talDat6$FavoritePercent, 4)
        talDat7 <- melt(talDat6, id.vars = c(
          "Description",
          "Emoji", "EmojiCount", "RetweetCount", "FavCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "EmojiPercent" = "TweetCount", "RetweetPercent" = "RetweetCount",
          "FavoritePercent" = "FavoriteCount"
        ))

        talDat7$variable <- revalue(talDat7$variable, c(
          "TweetCount" = "TweetPercent", "RetweetCount" = "RetweetPercent",
          "FavoriteCount" = "FavoritePercent"
        ))
        talDat71 <- dplyr::select(talDat7, Description, Emoji, value)
        talDat71 <- plyr::rename(talDat71, replace = c("Emoji" = "Native"))
        talDat71 <- plyr::rename(talDat71, replace = c("Description" = "Emoji"))
        df.expanded1 <- dplyr::select(df.expanded, Emoji, Native, tweetnum, dateC)
        talDat2 <- talDat71 %>% inner_join(df.expanded1)
        talDat3 <- subset(talDat2, value == ycord)
        for (i in 1:nrow(talDat3))
        {
          replace <- talDat3[i, ]$Native
          talDat3[i, ]$tweetnum <- suppressWarnings(gsub("\\<[^\\]]*\\>", toString(replace), talDat3[i, ]$tweetnum, perl = TRUE))
        }
        talDat3 <- plyr::rename(talDat3, replace = c("value" = "Value"))
        talDat3 <- plyr::rename(talDat3, replace = c("tweetnum" = "Tweets"))
        talDat3 <- plyr::rename(talDat3, replace = c("dateC" = "Date"))
        return(unique(talDat3))
      }
      else {
        return(small)
      }
    }
  })

  ## Retrieves emoji trends over time data
  output$emojiTrendsOverTimeTopic <- renderDataTable({
    df.expanded <- emojiTrendsDataTopic()
    if (nrow(df.expanded) == 0) {
      return(df.expanded)
    }
    else {
      small <- df.expanded %>% dplyr::group_by(dateC, Emoji) %>% dplyr::summarise(
        sum1 = sum(V1),
        sumReT = sum(retweet3), sumFav = sum(favorite3)
      ) %>% dplyr::arrange(
        dateC, desc(sum1),
        desc(sumReT), desc(sumFav)
      )
      small <- plyr::rename(small, replace = c("sum1" = "NumTweets"))
      small <- plyr::rename(small, replace = c("sumFav" = "NumFavorite"))
      small <- plyr::rename(small, replace = c("sumReT" = "NumReTweets"))
      small <- plyr::rename(small, replace = c("dateC" = "Date"))
      fil8 <- read.csv("https://raw.githubusercontent.com/ajavaid17/emojis/master/emDict.csv", sep = ";")
      if (nrow(small) != 0) {
        fil8 <- plyr::rename(fil8, replace = c("Description" = "Emoji"))
        smaller2 <- as.data.frame(small) %>% dplyr::inner_join(fil8)
        small3 <- c("Date", "Emoji", "NumTweets", "NumFavorite", "NumReTweets", "Native")
        small4 <- smaller2[small3]
        small4 <- unique(small4)

        div <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumTweets)))
        div2 <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumReTweets)))
        div3 <- as.numeric(small4 %>% dplyr::summarise(tot = sum(NumFavorite)))

        small4 <- dplyr::mutate(small4, EmojiPercent = NumTweets / div)
        small4 <- dplyr::mutate(small4, RetweetPercent = NumReTweets / div2)
        small4 <- dplyr::mutate(small4, FavoritePercent = NumFavorite / div3)

        small6 <- small4

        small6$EmojiPercent <- round(small6$EmojiPercent, 4)
        small6$RetweetPercent <- round(small6$RetweetPercent, 4)
        small6$FavoritePercent <- round(small6$FavoritePercent, 4)

        small55 <- small6 %>% dplyr::select(Date, Emoji, Native, EmojiPercent, RetweetPercent, FavoritePercent)
        return(data.frame(small55))
      }
      else {
        return(data.frame(small))
      }
    }
  })
  # End of Emoji Analysis

  output$Reference <- renderPrint({
    sessionInfo()
  })
}