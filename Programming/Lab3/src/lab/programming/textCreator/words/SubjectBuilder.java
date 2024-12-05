package lab.programming.textCreator.words;

public class SubjectBuilder extends SentenceMember {
    private Subject subject;

    public SubjectBuilder(Subject subject) {
        this.subject = subject;
        super(subject.name(), subject.defenition());
    }

    public void addSubject(SentenceMember m) {
        throw new IllegalCallerException("Method is not aviable in this class");
    }

    public SubjectBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public SubjectBuilder setDefenition(String defenition) {
        this.defenition = defenition;
        return this;
    }

    public SentenceMember build() {
        return this;
    }

    @Override
    public String toString() {
        return subject.defenition() + " " + subject.name();
    }

}