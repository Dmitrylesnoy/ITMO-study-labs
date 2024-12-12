package lab.programming.textCreator.words;

public class Predicate extends SentenceMember {
    private Speed velocity;
    private Subject place;
    private String tug;
    private Subject subject;

    public Predicate() {
    }

    public Predicate(String name) {
        super(name);
    }

    public Predicate(String name, Subject subject) {
        this.name = name;
        this.subject = subject;
    }

    public Predicate(String name, String defenition, Speed velocity) {
        super(name, defenition);
        this.velocity = velocity;
    }

    public void addSubject(SentenceMember member) throws ArrayStoreException{
        try {
            if ((member instanceof SubjectBuilder)) {
                // SubjectBuilder memberB = (SubjectBuilder)member;
                this.subject=((SubjectBuilder)member).getSubject();

            } else {
                throw new ArrayStoreException();
            }
        } catch (ArrayStoreException e) {
            this.subject = new Subject(member.getName(), member.getDefenition());
        }
    }

    public void setVelocity(Speed velocity) {
        this.velocity = velocity;
    }

    public void setPlace(Subject place, String tug) {
        this.place = place;
        this.tug = tug;
    }

    @Override
    public String toString() {
        String str = "";

        if (subject != null) {
            str = subject.toString();
        }
        if (velocity != null) {
            if (velocity != Speed.NOMOVE) {
                str = str + " " + velocity.toString();
            }
        }
        str = str + " " + name;
        if (place != null) {
            if (place.toString() != "") {
                str = str + " " + tug + " " + place;
            }
        }
        if (defenition != null) {
            if (defenition != "") {
                str = str + " " + defenition;
            }
        }
        return str;
    }

    @Override
    public int hashCode() {
        int code = defenition == null ? 0 : defenition.hashCode();
        code = 31 * code + name.hashCode();
        code = 31 * code + (velocity == null ? 0 : velocity.hashCode());
        code = 31 * code + (place == null ? 0 : place.hashCode());
        code = 31 * code + (tug == null ? 0 : tug.hashCode());
        code = 31 * code + (subject == null ? 0 : subject.hashCode());
        return code;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;

        Predicate objs = (Predicate) obj;

        if (!name.equals(objs.name))
            return false;
        else if (!defenition.equals(objs.defenition))
            return false;
        else if (!velocity.equals(objs.velocity))
            return false;
        else if (!place.equals(objs.place))
            return false;
        else if (!tug.equals(objs.tug))
            return false;
        else if (!subject.equals(objs.subject))
            return false;

        return true;
    }

}
