package named_entities.utils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import named_entities.NamedEntity;
import named_entities.NamedEntity.Category;
import named_entities.NamedEntity.Topic;

public class NameEntityProcessor {
    private static final String dictionaryPath = "dictionary.json";

    public static HashMap<Category, List<NamedEntity>> extractNamedEntitiesByCategory(
            List<String> candidates) throws IOException {
        HashMap<Category, List<NamedEntity>> categorizedEntities = new HashMap<>();
        Boolean found = false;

        // me fijo para cada candidato si se encuentra en el diccionarios
        InputStream inputStream = NameEntityProcessor.class.getClassLoader().getResourceAsStream(dictionaryPath);
        if (inputStream == null) {
            throw new IOException("File not found: " + dictionaryPath);
        }

        // Read the input stream into a string
        String jsonData = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
        JSONArray jsonArray = new JSONArray(jsonData);
        for (String candidate : candidates) {
            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                String label = jsonObject.getString("label");
                Category category = Category.valueOf(jsonObject.getString("Category"));

                List<Topic> topics = new ArrayList<>();
                Object topicsObject = jsonObject.get("Topics");
                JSONArray topicsArray = (JSONArray) topicsObject;

                for (int j = 0; j < topicsArray.length(); j++) {
                    topics.add(Topic.valueOf(topicsArray.getString(j)));
                }

                List<String> keywords = new ArrayList<>();
                Object keywordsObject = jsonObject.get("keywords");
                JSONArray keywordsArray = (JSONArray) keywordsObject;
                for (int j = 0; j < keywordsArray.length(); j++) {
                    keywords.add(keywordsArray.getString(j));
                }
                if (keywords.contains(candidate)) {
                    NamedEntity namedEntity = null;
                    // NamedEntity ne = new NamedEntity(label, category, topics);
                    if (category == Category.PERSON) {
                        namedEntity = new Person(label, topics, null, "Argentino/a", null, null);
                    } else if (category == Category.ORGANIZATION) {
                        namedEntity = new Organization(label, topics, null, null, null);
                    } else if (category == Category.EVENT) {
                        namedEntity = new Event(label, topics, null);
                    } else if (category == Category.LOCATION) { // saque if
                        namedEntity = new Location(label, topics, null, null, null);
                    }
                    // else {
                    // namedEntity = new Other(label, topics, null);
                    // }
                    // categorizedEntities.computeIfAbsent(category, k -> new
                    // ArrayList<>()).add(ne);
                    if (namedEntity != null) {
                        categorizedEntities.computeIfAbsent(category, k -> new ArrayList<>()).add(namedEntity);
                        found = true;
                    }

                } else {
                    found = found || false;
                }
            }
            // if (!found) {
            // List<Topic> other = new ArrayList<>();
            // other.add(Topic.OTHER);
            // // NamedEntity ne = new NamedEntity(candidate, Category.OTHER, other);
            // Other namedEntity = new Other(candidate, other, null);
            // // categorizedEntities.computeIfAbsent(Category.OTHER, k -> new
            // ArrayList<>()).add(ne);
            // categorizedEntities.computeIfAbsent(Category.OTHER, k -> new
            // ArrayList<>()).add(namedEntity);
            // }
        }
        return categorizedEntities;
    }

    public static HashMap<Topic, List<NamedEntity>> extractNamedEntitiesByTopic(List<String> candidates)
            throws IOException {
        HashMap<Topic, List<NamedEntity>> categorizedEntities = new HashMap<>();

        InputStream inputStream = NameEntityProcessor.class.getClassLoader().getResourceAsStream(dictionaryPath);
        if (inputStream == null) {
            throw new IOException("File not found: " + dictionaryPath);
        }

        // Read the input stream into a string
        String jsonData = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
        JSONArray jsonArray = new JSONArray(jsonData);

        // me fijo para cada candidato si se encuentra en el diccionarios
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            String label = jsonObject.getString("label");
            Category category = Category.valueOf(jsonObject.getString("Category"));
            List<Topic> topics = new ArrayList<>();
            Object topicsObject = jsonObject.get("Topics");
            JSONArray topicsArray = (JSONArray) topicsObject;

            for (int j = 0; j < topicsArray.length(); j++) {
                topics.add(Topic.valueOf(topicsArray.getString(j)));
            }

            List<String> keywords = new ArrayList<>();
            Object keywordsObject = jsonObject.get("keywords");
            JSONArray keywordsArray = (JSONArray) keywordsObject;
            for (int j = 0; j < keywordsArray.length(); j++) {
                keywords.add(keywordsArray.getString(j));
            }
            for (String candidate : candidates) {
                if (keywords.contains(candidate)) {
                    // NamedEntity ne = new NamedEntity(label, category, topics);
                    NamedEntity namedEntity = null;
                    if (category == Category.PERSON) {
                        namedEntity = new Person(label, topics, null, null, null, null);
                    } else if (category == Category.ORGANIZATION) {
                        namedEntity = new Organization(label, topics, null, null, null);
                    } else if (category == Category.EVENT) {
                        namedEntity = new Event(label, topics, null);
                    } else if (category == Category.LOCATION) {
                        namedEntity = new Location(label, topics, null, null, null);
                    } else {
                        namedEntity = new Other(label, topics, null);
                    }
                    for (Topic topic : topics) {
                        // categorizedEntities.computeIfAbsent(topic, k -> new ArrayList<>()).add(ne);
                        categorizedEntities.computeIfAbsent(topic, k -> new ArrayList<>()).add(namedEntity);
                    }
                }
            }
        }
        return categorizedEntities;
    }

    public static HashMap<Category, HashMap<String, Integer>> countEntitiesByCategory(
            HashMap<Category, List<NamedEntity>> categorizedEntities) {
        HashMap<Category, HashMap<String, Integer>> countE = new HashMap<>();
        for (Map.Entry<Category, List<NamedEntity>> entry : categorizedEntities.entrySet()) {
            Category category = entry.getKey();
            List<NamedEntity> namedEntities = entry.getValue();
            HashMap<String, Integer> count = new HashMap<>();
            for (NamedEntity entity : namedEntities) {
                if (count.containsKey(entity.getName())) {
                    count.put(entity.getName(), count.get(entity.getName()) + 1);
                } else {
                    count.put(entity.getName(), 1);
                }
            }
            countE.put(category, count);
        }
        return countE;
    }

    public static HashMap<Topic, HashMap<String, Integer>> countEntitiesByTopic(
            HashMap<Topic, List<NamedEntity>> categorizedEntities) {
        HashMap<Topic, HashMap<String, Integer>> countE = new HashMap<>();
        for (Map.Entry<Topic, List<NamedEntity>> entry : categorizedEntities.entrySet()) {
            Topic topic = entry.getKey();
            List<NamedEntity> namedEntities = entry.getValue();
            HashMap<String, Integer> count = new HashMap<>();
            for (NamedEntity entity : namedEntities) {
                if (count.containsKey(entity.getName())) {
                    count.put(entity.getName(), count.get(entity.getName()) + 1);
                } else {
                    count.put(entity.getName(), 1);
                }
            }
            countE.put(topic, count);
        }
        return countE;
    }
}
