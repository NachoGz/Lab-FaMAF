package utils;

import utils.FeedsData;
import feed.Article;
import utils.Config;
import feed.FeedParser;
import java.util.List;

public class FeedManager {
    private List<FeedsData> feedsDataArray;
    private List<Article> allArticles;
    private Config config;

    public FeedManager(List<FeedsData> feedsDataArray, List<Article> allArticles, Config config) {
        this.feedsDataArray = feedsDataArray;
        this.allArticles = allArticles;
        this.config = config;
    }

    public void processFeed(String feedKey) {
        if (feedKey != null) {
            processSpecificFeed(feedKey);
        } else {
            processAllFeeds();
        }
        if (config.getPrintFeed()) {
            printFeeds();
        }
    }

    private void processSpecificFeed(String feedKey) {
        boolean feedFound = false;
        for (FeedsData feed : feedsDataArray) {
            if (feed.getLabel().equals(feedKey)) {
                feedFound = true;
                try {
                    String xmlString = FeedParser.fetchFeed(feed.getUrl());
                    List<Article> feedArticles = FeedParser.parseXML(xmlString);
                    allArticles.addAll(feedArticles);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        if (!feedFound) {
            System.err.println("El feed ingresado no existe. Intente de nuevo.");
            System.exit(0);
        }
    }

    private void processAllFeeds() {
        for (FeedsData feed : feedsDataArray) {
            try {
                String xmlString = FeedParser.fetchFeed(feed.getUrl());
                List<Article> feedArticles = FeedParser.parseXML(xmlString);
                allArticles.addAll(feedArticles);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void printFeeds() {
        System.out.println("Printing feed(s)...");
        for (Article article : allArticles) {
            article.print();
        }
    }
}
