package lab.programming.textCreator.words;

import lab.programming.textCreator.exceptions.NonAviableMethod;

public class SubjectBuilder extends SentenceMember {
    private Subject subject;

    public SubjectBuilder(Subject subject) {
        this.subject = subject;
        super(subject.name(), subject.defenition());
    }

    public SubjectBuilder(String name, String defenition) {
        super(name, defenition);
        subject = new Subject(name, defenition);
    }

    public void addSubject(SentenceMember m) throws NonAviableMethod {
        try{
            throw new NonAviableMethod("Input SentenceMember will defind like a Subject variable");
        } catch (NonAviableMethod e) {
            this.subject = new Subject(m.name, m.defenition);
        }
    }

    public Subject getSubject() {
        return this.subject;
    }

    public SentenceMember build() {
        return this;
    }

    @Override
    public String toString() {
        return subject.toString();
    }

    @Override
    public int hashCode() {
        int code = defenition == null ? 0 : defenition.hashCode();
        code = 31 * code + name.hashCode();
        code = 31 * code + (subject == null ? 0 : subject.hashCode());
        return code;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;

        SubjectBuilder objs = (SubjectBuilder) obj;

        if (!name.equals(objs.name))
            return false;
        else if (!defenition.equals(objs.defenition))
            return false;
        else if (!subject.equals(objs.subject))
            return false;

        return true;
    }

}