package named_entities.utils;

import utils.Config;
import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.sql.SparkSession;
import scala.Tuple2;

import named_entities.NamedEntity;
import named_entities.NamedEntity.Category;
import named_entities.NamedEntity.Topic;
import named_entities.heuristics.CapitalizedConsonantsHeuristic;
import named_entities.heuristics.CapitalizedVowelsHeuristic;
import named_entities.heuristics.CapitalizedWordHeuristic;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.io.PrintWriter;
import java.lang.StringBuilder;

public class SparkNamedEntityProcessor extends NameEntityProcessor {
    private Config config;
    private String dataFile;
    private static final Pattern SPACE = Pattern.compile(" ");

    public SparkNamedEntityProcessor(Config config, String dataFile) {
        this.config = config;
        this.dataFile = dataFile;
    }

    public void processNamedEntities() {
        SparkSession spark = createSparkSession();
        JavaSparkContext sc = new JavaSparkContext(spark.sparkContext());

        JavaRDD<String> textRDD = sc.textFile(dataFile);

        JavaRDD<List<String>> candidatesRDD = computeCandidates(textRDD);

        computeNamedEntities(candidatesRDD, sc);

        sc.close();
        spark.close();
    }

    private SparkSession createSparkSession() {
        return SparkSession.builder()
                .appName("ComputingNamedEntities")
                .getOrCreate();
    }

    private JavaRDD<List<String>> computeCandidates(JavaRDD<String> textRDD) {
        String heuristics = config.getOptionsValue("-ne");
        
        // para cada pedazo de texto de textRDD, extraigo candidatos en ese pedazo
        switch (heuristics) {
            case "cwh":
                System.out.println("Computing named entities using CapitalWordHeuristic");
                return textRDD.map(textChunk -> {
                    CapitalizedWordHeuristic cwh = new CapitalizedWordHeuristic();
                    return cwh.extractCandidates(textChunk);
                });
            case "cvh":
                System.out.println("Computing named entities using CapitalVowelsHeuristic");

                return textRDD.map(textChunk -> {
                    CapitalizedVowelsHeuristic cvh = new CapitalizedVowelsHeuristic();
                    return cvh.extractCandidates(textChunk);
                });
            case "cch":
                System.out.println("Computing named entities using CapitalConsonantsHeuristic");
                return textRDD.map(textChunk -> {
                    CapitalizedConsonantsHeuristic cch = new CapitalizedConsonantsHeuristic();
                    return cch.extractCandidates(textChunk);
                });
            default:
                System.err.println("Unknown Heuristic: " + heuristics);
                System.out.println("La heuristica ingresada no existe. Intente otra.");
                System.exit(0);
        }
        return null; // no se debería llegar acá
    }

    public static JavaPairRDD<Category, Tuple2<String, Integer>> countEntitiesByCategory(
            JavaRDD<NamedEntity> namedEntitiesRDD) {
        // convierto NamedEntity al par (Category, name)
        JavaPairRDD<Category, String> categoryEntityPairs = namedEntitiesRDD
                .mapToPair(entity -> new Tuple2<>(entity.getCategory(), entity.getName()));

        // Mapeo a los pares ((Category, name), 1)
        JavaPairRDD<Tuple2<Category, String>, Integer> entityPairs = categoryEntityPairs
                .mapToPair(pair -> new Tuple2<>(pair, 1));

        // Reduzco para contar
        JavaPairRDD<Tuple2<Category, String>, Integer> entityCounts = entityPairs.reduceByKey(Integer::sum);

        // Mapeo a (Category, (name, count))
        return entityCounts.mapToPair(pair -> new Tuple2<>(pair._1._1, new Tuple2<>(pair._1._2, pair._2)));
    }

    public static JavaPairRDD<Topic, Tuple2<String, Integer>> countEntitiesByTopic(
            JavaRDD<NamedEntity> namedEntitiesRDD) {
        // convierto NamedEntity al par (Topic, name)
        JavaPairRDD<Topic, String> topicEntityPairs = namedEntitiesRDD.flatMapToPair(entity -> {
            List<Tuple2<Topic, String>> pairs = new ArrayList<>();
            for (Topic topic : entity.getTopics()) {
                pairs.add(new Tuple2<>(topic, entity.getName()));
            }
            return pairs.iterator();
        });

        // Mapeo a los pares ((Topic, name), 1)
        JavaPairRDD<Tuple2<Topic, String>, Integer> entityPairs = topicEntityPairs
                .mapToPair(pair -> new Tuple2<>(pair, 1));

        // Reduzco para contar
        JavaPairRDD<Tuple2<Topic, String>, Integer> entityCounts = entityPairs.reduceByKey(Integer::sum);

        // Mapeo a (Topic (name, count))
        return entityCounts.mapToPair(pair -> new Tuple2<>(pair._1._1, new Tuple2<>(pair._1._2, pair._2)));
    }

    private void computeNamedEntities(JavaRDD<List<String>> candidatesRDD, JavaSparkContext sc) {
        String statsFormatKey = config.getStatsFormatkey();
        if (statsFormatKey == null) {
            statsFormatKey = "cat";
        }

        switch (statsFormatKey) {
            case "cat":
                JavaPairRDD<NamedEntity.Category, Iterable<Tuple2<String, Integer>>> namedEntitiesStatisticsByCategoryRDD = computeStatisticsByCategory(
                        candidatesRDD);
                printStatisticsByCategory(namedEntitiesStatisticsByCategoryRDD, sc);
                break;
            case "topic":
                JavaPairRDD<NamedEntity.Topic, Iterable<Tuple2<String, Integer>>> namedEntitiesStatisticsByTopicRDD = computeStatisticsByTopic(
                        candidatesRDD);
                printStatisticsByTopic(namedEntitiesStatisticsByTopicRDD);
                break;
            default:
                System.out.println("La opcion " + statsFormatKey + " no existe. Intente de nuevo");
                System.exit(0);
        }
    }

    private JavaPairRDD<NamedEntity.Category, Iterable<Tuple2<String, Integer>>> computeStatisticsByCategory(
            JavaRDD<List<String>> candidatesRDD) {
        try {
            // ahora para cada pedazo en candidatesRDD, extraigo named entities
            JavaRDD<HashMap<NamedEntity.Category, List<NamedEntity>>> namedEntitiesByCategoryRDD = candidatesRDD
                    .map(candidateChunk -> {
                        try {
                            return extractNamedEntitiesByCategory(candidateChunk);
                        } catch (IOException e) {
                            e.printStackTrace();
                            return new HashMap<>();
                        }
                    });
            
            // extraigo todas las entidades nombradas del HashMap
            JavaRDD<NamedEntity> allNamedEntitiesByCategoryRDD = namedEntitiesByCategoryRDD
                    .flatMap(map -> map.values().stream()
                            .flatMap(List::stream)
                            .iterator());
            
            // computo las estadisticas
            JavaPairRDD<NamedEntity.Category, Iterable<Tuple2<String, Integer>>> namedEntitiesStatisticsByCategoryRDD = countEntitiesByCategory(
                    allNamedEntitiesByCategoryRDD).groupByKey();
            return namedEntitiesStatisticsByCategoryRDD;
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        return null;
    }

    private void printStatisticsByCategory(
            JavaPairRDD<NamedEntity.Category, Iterable<Tuple2<String, Integer>>> namedEntitiesStatisticsByCategoryRDD,
            JavaSparkContext sc) {
        namedEntitiesStatisticsByCategoryRDD.foreach(pair -> {
            NamedEntity.Category category = pair._1();
            Iterable<Tuple2<String, Integer>> tuple = pair._2;
            synchronized (System.out) { // para que no haya race conditions entre los worker threads ó los workers
                System.out.println("Category: " + category);
                for (Tuple2<String, Integer> par : tuple) {
                    System.out.println("\t" + par._1() + " (" + par._2() + ")");
                }
            }
        });

        // Para que el output se vea en modo cluster pero ojo con el uso de collect en
        // big data muy grandes
        // List<Tuple2<NamedEntity.Category, Iterable<Tuple2<String, Integer>>>>
        // collectedStatistics = namedEntitiesStatisticsByCategoryRDD.collect();

        // for (Tuple2<NamedEntity.Category, Iterable<Tuple2<String, Integer>>> pair :
        // collectedStatistics) {
        // NamedEntity.Category category = pair._1();
        // Iterable<Tuple2<String, Integer>> tuples = pair._2();
        // synchronized(System.out) {
        // System.out.println("Category: " + category);

        // for (Tuple2<String, Integer> tuple : tuples) {
        // System.out.println("\t" + tuple._1() + " (" + tuple._2() + ")");
        // }
        // }

        // }
    }

    private JavaPairRDD<NamedEntity.Topic, Iterable<Tuple2<String, Integer>>> computeStatisticsByTopic(
            JavaRDD<List<String>> candidatesRDD) {
        try {
            // ahora para cada pedazo en candidatesRDD, extraigo named entities
            JavaRDD<HashMap<NamedEntity.Topic, List<NamedEntity>>> namedEntitiesByTopicRDD = candidatesRDD
                    .map(candidateChunk -> {
                        try {
                            return extractNamedEntitiesByTopic(candidateChunk);
                        } catch (IOException e) {
                            e.printStackTrace();
                            return new HashMap<>();
                        }
                    });
            
            // extraigo todas las entidades nombradas del HashMap
            JavaRDD<NamedEntity> allNamedEntitiesByTopicRDD = namedEntitiesByTopicRDD
                    .flatMap(map -> map.values().stream()
                            .flatMap(List::stream)
                            .iterator());
                            
            // computo las estadisticas
            JavaPairRDD<NamedEntity.Topic, Iterable<Tuple2<String, Integer>>> namedEntitiesStatisticsByTopicRDD = countEntitiesByTopic(
                    allNamedEntitiesByTopicRDD).groupByKey();

            return namedEntitiesStatisticsByTopicRDD;
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        return null;

    }

    private void printStatisticsByTopic(
            JavaPairRDD<NamedEntity.Topic, Iterable<Tuple2<String, Integer>>> namedEntitiesStatisticsByTopicRDD) {
        namedEntitiesStatisticsByTopicRDD.foreach(pair -> {
            NamedEntity.Topic topic = pair._1();
            Iterable<Tuple2<String, Integer>> tuple = pair._2;
            synchronized (System.out) {
                System.out.println("Topic: " + topic);

                for (Tuple2<String, Integer> par : tuple) {
                    System.out.println("\t" + par._1() + " (" + par._2() + ")");
                }
            }
        });
    }

    // Para que el output se vea en modo cluster pero ojo con el uso de collect en
    // big data muy grandes
    // List<Tuple2<NamedEntity.Topic, Iterable<Tuple2<String, Integer>>>>
    // collectedStatistics = namedEntitiesStatisticsByTopicRDD.collect();

    // for (Tuple2<NamedEntity.Topic, Iterable<Tuple2<String, Integer>>> pair :
    // collectedStatistics) {
    // NamedEntity.Topic topic = pair._1();
    // Iterable<Tuple2<String, Integer>> tuples = pair._2();
    // synchronized(System.out) {
    // System.out.println("Topic: " + topic);

    // for (Tuple2<String, Integer> tuple : tuples) {
    // System.out.println("\t" + tuple._1() + " (" + tuple._2() + ")");
    // }
    // }

    // }
}
