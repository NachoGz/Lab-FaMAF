package named_entities.utils;

import java.util.List;

import named_entities.NamedEntity;

public class Organization extends NamedEntity {
    private String full_name;
    private String siglas;
    private String environment; // sports, politics, economy, etc.
    private static final long serialVersionUID = 1L;

    public Organization(String name, List<Topic> topics, String full_name, String siglas, String environment) {
        super(name, topics);
        this.full_name = full_name;
        this.siglas = siglas;
    }

    @Override
    public Category getCategory() {
        return Category.ORGANIZATION;
    }

    public String getFullName() {
        return full_name;
    }

    public String getSiglas() {
        return siglas;
    }

    public String getEnvironment() {
        return environment;
    }
}
