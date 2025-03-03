package named_entities.utils;

import java.util.List;

import named_entities.NamedEntity;

public class Event extends NamedEntity {
    private String event_name;
    private static final long serialVersionUID = 1L;

    public Event(String name, List<Topic> topics, String event_name) {
        super(name, topics);
        this.event_name = event_name;
    }

    @Override
    public Category getCategory() {
        return Category.EVENT;
    }

    public String getEventName() {
        return event_name;
    }
}
