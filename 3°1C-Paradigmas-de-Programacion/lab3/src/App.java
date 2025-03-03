// package paradigmas.lab3_computacion_distribuida.src;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import feed.Article;

import utils.Config;
import utils.FeedsData;
import utils.JSONParser;
import utils.UserInterface;
import utils.FeedManager;
import named_entities.utils.SparkNamedEntityProcessor;

import org.apache.spark.api.java.JavaRDD;

public class App {

    public static void main(String[] args) {

        List<FeedsData> feedsDataArray = new ArrayList<>();
        try {
            feedsDataArray = JSONParser.parseJsonFeedsData("feeds.json");
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
        
        // inicializo feedManager
        List<Article> allArticles = new ArrayList<>();
        FeedManager feedManager = new FeedManager(feedsDataArray, allArticles, config);

        String sparkKey = config.getsparkKey();
        
        String dataFile = "";
        
        if (sparkKey != null) {
            dataFile = config.getOptionsValue("-s");
        } else {
            String feedKey = config.getFeedKey();
            
            // solo proceso el feed si es necesario
            feedManager.processFeed(feedKey);
            
            dataFile = "feed.txt";    
            String feed = "";
            for (Article article : allArticles) {
                feed += article.getTitle() + " " + article.getDescription();
            }

            try {
                PrintWriter writer = new PrintWriter(dataFile, "UTF-8");
                writer.println(feed);
                writer.close();
            } catch (IOException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }
        SparkNamedEntityProcessor processor = new SparkNamedEntityProcessor(config, dataFile);
        
        if (config.getComputeNamedEntities()) {
            processor.processNamedEntities();
        } else { 
            feedManager.printFeeds();
        }
    }
}
