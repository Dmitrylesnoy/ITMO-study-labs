package lab.programming.textCreator.words;



public class Predicate extends SentenceMember {
    private Speed velocity;
    private Subject place;
    private Subject subject;

    Predicate() {
    }

    Predicate(String name) {
        super(name);
    }

    Predicate(String name, String defenition, Speed velocity) {
        super(name, defenition);
        this.velocity = velocity;
    }

    void addSubject(SentenceMember member) {
        this.subject = new Subject(member.getName(), member.getDefenition());
    }

    void setVelocity(Speed velocity) {
        this.velocity = velocity;
    }

    void setPlace(Subject place) {
        this.place = place;
    }

}
