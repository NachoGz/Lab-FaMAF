package named_entities.utils;

import java.util.List;

import named_entities.NamedEntity;

public class Person extends NamedEntity {
    private String job;
    private String nationality;
    private String firstname;
    private String lastname;

    public Person(String name, List<Topic> topics, String job, String nationality, String firstname, String lastname) {
        super(name, topics);
        this.job = job;
        this.nationality = nationality;
        this.firstname = firstname;
        this.lastname = lastname;
    }

    @Override
    public Category getCategory() {
        return Category.PERSON;
    }

    public String getJob() {
        return job;
    }

    public String getNationality() {
        return nationality;
    }

    public String getFirstName() {
        return firstname;
    }

    public String getLastName() {
        return lastname;
    }
}