package utils;

import java.util.HashMap;
import java.util.List;

public class Config {
    private boolean printFeed = false;
    private boolean computeNamedEntities = false;
    private String feedKey;
    private boolean printHelp = false;
    private HashMap<String, String> optionDict;
    private String heuristicKey;
    private String statsFormatKey;

    // TODO: A reference to the used heuristic will be needed here

    public Config(boolean printFeed, boolean computeNamedEntities, String feedKey, boolean printHelp,
            HashMap<String, String> optionDict,
            String heuristicKey, String statsFormatKey) {
        this.printFeed = printFeed;
        this.computeNamedEntities = computeNamedEntities;
        this.feedKey = feedKey;
        this.printHelp = printHelp;
        this.optionDict = optionDict;
        this.heuristicKey = heuristicKey;
        this.statsFormatKey = statsFormatKey;
    }

    public boolean getPrintFeed() {
        return printFeed;
    }

    public boolean getComputeNamedEntities() {
        return computeNamedEntities;
    }

    public String getFeedKey() {
        return feedKey;
    }

    public boolean getPrintHelp() {
        return printHelp;
    }

    public String getHeuristicKey() {
        return heuristicKey;
    }

    public String getStatsFormatkey() {
        return statsFormatKey;
    }

    public void printHelp(List<FeedsData> feedsDataArray) {
        System.out.println("Usage: make run ARGS=\"[OPTION]\"");
        System.out.println("Options:");
        System.out.println("  -h, --help: Show this help message and exit");
        System.out.println("  -f, --feed <feedKey>:                Fetch and process the feed with");
        System.out.println("                                       the specified key");
        System.out.println("                                       Available feed keys are: ");
        for (FeedsData feedData : feedsDataArray) {
            System.out.println("                                       " + feedData.getLabel());
        }
        System.out.println("  -ne, --named-entity <heuristicName>: Use the specified heuristic to extract");
        System.out.println("                                       named entities");
        System.out.println("                                       Available heuristic names are: ");
        // TODO: Print the available heuristics with the following format
        System.out.println(
                "                                       cwh (CapitalWordHeuristic): Selecciona aquellas palabras que empiezan con mayuscula.");
        System.out.println(
                "                                       cvh (CapitalVowelsHeuristic): Selecciona aquellas palabras que empiezan con una letra vocal mayuscula.");
        System.out.println(
                "                                       cch (CapitalConsonantsHeuristic): Selecciona aquellas palabras que empiezan con una letra consonante mayuscula.");
        System.out.println("  -pf, --print-feed:                   Print the fetched feed");
        System.out.println("  -sf, --stats-format <format>:        Print the stats in the specified format");
        System.out.println("                                       Available formats are: ");
        System.out.println("                                       cat: Category-wise stats");
        System.out.println("                                       topic: Topic-wise stats");
    }

    public void printFeed(List<FeedsData> feedsDataArray) {
        for (FeedsData feedData : feedsDataArray) {
            feedData.print();
        }
    }

    public String getOptionsValue(String option) {
        return optionDict.get(option);
    }
}
