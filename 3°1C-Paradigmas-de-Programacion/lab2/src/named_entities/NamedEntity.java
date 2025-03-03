package named_entities;

import java.util.List;

public abstract class NamedEntity {
    private String name;
    private List<Topic> topics;

    public NamedEntity(String name, List<Topic> topics) {
        this.name = name;
        this.topics = topics;
    }

    public String getName() {
        return name;
    }

    public abstract Category getCategory();

    public List<Topic> getTopics() {
        return topics;
    }

    public enum Category {
        PERSON,
        LOCATION,
        ORGANIZATION,
        EVENT,
        OTHER
    }

    public enum Topic {
        POLITICS,
        SPORTS,
        ECONOMICS,
        BUSINESS,
        HEALTH,
        TECHNOLOGY,
        ENTERTAINMENT,
        OTHER
    }
}