import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.IOException;

import feed.Article;
import feed.FeedParser;

import utils.Config;
import utils.FeedsData;
import utils.JSONParser;
import utils.UserInterface;
import named_entities.NamedEntity;
import named_entities.heuristics.CapitalizedConsonantsHeuristic;
import named_entities.heuristics.CapitalizedVowelsHeuristic;
import named_entities.heuristics.CapitalizedWordHeuristic;
import named_entities.utils.NameEntityProcessor;

public class App {

    public static void main(String[] args) {

        List<FeedsData> feedsDataArray = new ArrayList<>();
        try {
            feedsDataArray = JSONParser.parseJsonFeedsData("./src/data/feeds.json");
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        UserInterface ui = new UserInterface();
        Config config = ui.handleInput(args);

        run(config, feedsDataArray);
    }

    // TODO: Change the signature of this function if
    private static void run(Config config, List<FeedsData> feedsDataArray) {

        if (feedsDataArray == null || feedsDataArray.size() == 0) {
            System.out.println("No feeds data found");
            return;
        }

        if (config.getPrintHelp()) {
            config.printHelp(feedsDataArray);
            System.exit(0);
        }
        String feedKey = config.getFeedKey();
        List<Article> allArticles = new ArrayList<>();
        boolean feedFound = false;
        
        if (feedKey != null) {
            for (FeedsData feed : feedsDataArray) {
                if (feed.getLabel().equals(feedKey)) {
                    feedFound = true;
                    try {
                        String xmlString = FeedParser.fetchFeed(feed.getUrl());
                        List<Article> feedArticles = FeedParser.parseXML(xmlString);
                        for (Article article : feedArticles) {
                            allArticles.add(article);
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
            if (!feedFound) {
                System.err.println("El feed ingresado no existe. Intente de nuevo.");
                System.exit(0);
            }
            if (config.getPrintFeed()) {
                System.out.println("Printing feed(s)...");
                for (Article article : allArticles) {
                    article.print();
                }
            }
        } else {
            for (FeedsData feed : feedsDataArray) {
                try {
                    String xmlString = FeedParser.fetchFeed(feed.getUrl());
                    List<Article> feedArticles = FeedParser.parseXML(xmlString);
                    for (Article article : feedArticles) {
                        allArticles.add(article);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            if (config.getPrintFeed()) {
                System.out.println("Printing feed(s)...");
                for (Article article : allArticles) {
                    article.print();
                }
            }
        }
        if (config.getComputeNamedEntities()) {
            String text = "";
            for (Article article : allArticles) {
                text += article.getTitle() + " " + article.getDescription();
            }

            String heuristics = config.getOptionsValue("-ne");
            List<String> candidates = new ArrayList<>();
            switch (heuristics) {
                case "cwh":
                    System.out.println("Computing named entities using CapitalWordHeuristic");
                    CapitalizedWordHeuristic cwh = new CapitalizedWordHeuristic();
                    candidates = cwh.extractCandidates(text);
                    break;
                case "cvh":
                    System.out.println("Computing named entities using CapitalVowelsHeuristic");
                    CapitalizedVowelsHeuristic cvh = new CapitalizedVowelsHeuristic();
                    candidates = cvh.extractCandidates(text);
                    break;
                case "cch":
                    System.out.println("Computing named entities using CapitalConsonantsHeuristic");
                    CapitalizedConsonantsHeuristic cch = new CapitalizedConsonantsHeuristic();
                    candidates = cch.extractCandidates(text);
                    break;
                default:
                    System.err.println("La heuristica ingresada no existe. Intente otra.");
                    System.exit(0);
                    // break;
            }
            System.out.println("\nStats: ");
            System.out.println("-".repeat(80));
            String statsFormatKey = config.getStatsFormatkey();
            if (statsFormatKey == null) {
                statsFormatKey = "cat";
            }
            switch (statsFormatKey) {
                case "cat":
                    HashMap<NamedEntity.Category, List<NamedEntity>> namedEntitiesByCategory = new HashMap<>();
                    HashMap<NamedEntity.Category, HashMap<String, Integer>> namedEntitiesStatisticsByCategory = new HashMap<>();
                    try {
                        namedEntitiesByCategory = NameEntityProcessor.extractNamedEntitiesByCategory(text,
                                candidates);
                        namedEntitiesStatisticsByCategory = NameEntityProcessor
                                .countEntitiesByCategory(namedEntitiesByCategory);
                    } catch (IOException e) {
                        e.printStackTrace();
                        System.exit(1);
                    }

                    for (Map.Entry<NamedEntity.Category, HashMap<String, Integer>> entry : namedEntitiesStatisticsByCategory
                            .entrySet()) {
                        NamedEntity.Category category = entry.getKey();
                        HashMap<String, Integer> frequency = entry.getValue();
                        System.out.println("Category: " + category.name());
                        for (Map.Entry<String, Integer> freq : frequency.entrySet()) {
                            System.out.println("\t" + freq.getKey() + " (" + freq.getValue() + ")");
                        }
                    }
                    break;
                case "topic":
                    HashMap<NamedEntity.Topic, List<NamedEntity>> namedEntitiesByTopic = new HashMap<>();
                    HashMap<NamedEntity.Topic, HashMap<String, Integer>> namedEntitiesStatisticsByTopic = new HashMap<>();
                    try {
                        namedEntitiesByTopic = NameEntityProcessor.extractNamedEntitiesByTopic(text, candidates);
                        namedEntitiesStatisticsByTopic = NameEntityProcessor
                                .countEntitiesByTopic(namedEntitiesByTopic);
                    } catch (IOException e) {
                        e.printStackTrace();
                        System.exit(1);
                    }

                    for (Map.Entry<NamedEntity.Topic, HashMap<String, Integer>> entry : namedEntitiesStatisticsByTopic
                            .entrySet()) {
                        NamedEntity.Topic topic = entry.getKey();
                        HashMap<String, Integer> frequency = entry.getValue();
                        System.out.println("Topic: " + topic.name());
                        for (Map.Entry<String, Integer> freq : frequency.entrySet()) {
                            System.out.println("\t" + freq.getKey() + " (" + freq.getValue() + ")");
                        }
                    }
                    break;
                default:
                    System.out.println("La opcion " + statsFormatKey + " no existe. Intente de nuevo");
                    System.exit(0);
                    break;
            }

        } else {
            System.out.println("Printing feed(s)...");
            for (Article article : allArticles) {
                article.print();
            }
        }
    }
}
