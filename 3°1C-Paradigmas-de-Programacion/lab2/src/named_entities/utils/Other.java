package named_entities.utils;

import java.util.List;

import named_entities.NamedEntity;

public class Other extends NamedEntity {
    private String text;

    public Other(String name, List<Topic> topics, String text) {
        super(name, topics);
        this.text = text;
    }

    @Override
    public Category getCategory() {
        return Category.PERSON;
    }

    public String getText() {
        return text;
    }
}