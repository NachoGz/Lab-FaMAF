package feed;

public class Article {
    private String title;
    private String description;
    private String pubDate;
    private String link;

    public Article(String title, String description, String pubDate, String link) {
        this.title = title;
        this.description = description;
        this.pubDate = pubDate;
        this.link = link;
    }

    public String getTitle() {
        return this.title;
    }

    public String getDescription() {
        return description;
    }

    public String getPubDate() {
        return pubDate;
    }

    public String getLink() {
        return link;
    }

    public void print() {
        System.out.println("Title: " + title);
        System.out.println("Descripcion: " + description);
        System.out.println("Publication Date: " + pubDate);
        System.out.println("Link:\n" + link);
        System.out.println("********************************************************************************");
    }
}