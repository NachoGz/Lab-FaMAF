package utils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

public class JSONParser {

    static public List<FeedsData> parseJsonFeedsData(String jsonFilePath) throws IOException {
        // Load the file from the classpath
        InputStream inputStream = JSONParser.class.getClassLoader().getResourceAsStream(jsonFilePath);
        if (inputStream == null) {
            throw new IOException("File not found: " + jsonFilePath);
        }

        // Read the input stream into a string
        String jsonData = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);

        List<FeedsData> feedsList = new ArrayList<>();

        // Parse the JSON data
        JSONArray jsonArray = new JSONArray(jsonData);
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            String label = jsonObject.getString("label");
            String url = jsonObject.getString("url");
            String type = jsonObject.getString("type");
            feedsList.add(new FeedsData(label, url, type));
        }
        return feedsList;
    }
}
