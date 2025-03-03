package named_entities.utils;

import java.util.List;

import named_entities.NamedEntity;

public class Location extends NamedEntity {
    private String location_name;
    private String longitude;
    private String latitude;

    public Location(String name, List<Topic> topics, String location_name, String longitude, String latitude) {
        super(name, topics);
        this.location_name = location_name;
        this.longitude = longitude;
        this.latitude = latitude;
    }

    @Override
    public Category getCategory() {
        return Category.PERSON;
    }

    public String getLocationName() {
        return location_name;
    }

    public String getLongitude() {
        return longitude;
    }

    public String getLatitude() {
        return latitude;
    }
}