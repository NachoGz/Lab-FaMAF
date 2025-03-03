package named_entities;

import java.io.Serializable;
import java.util.List;
// hago la clase serializable para que pueda ser mandada a través de la red
public abstract class NamedEntity implements Serializable { 
    private String name;
    private List<Topic> topics;
    private static final long serialVersionUID = 1L;

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