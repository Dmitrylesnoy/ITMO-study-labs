package lab.programming.textCreator.words;

import lab.programming.textCreator.exceptions.NonAviableMethod;

public abstract class SentenceMember {
    String name;
    String defenition;

    public SentenceMember() {
    }

    public SentenceMember(String name) {
        this.name = name;
    }

    public SentenceMember(String name, String defenition) {
        this.name = name;
        this.defenition = defenition;
    }

    public String getName() {
        return name;
    }

    public String getDefenition() {
        return defenition;
    }

    public void setDefenition(String defenition) {
        this.defenition = defenition;
    }

    public String toString() {
        if (defenition==null || defenition == "") {
            return name;
        } else {
            return name + " " + defenition;
        }
    }

    abstract void addSubject(SentenceMember member) throws NonAviableMethod;

    @Override
    public int hashCode() {
        int code = defenition == null ? 0 : defenition.hashCode();
        code = 31 * code + name.hashCode();
        return code;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;

        SentenceMember objs = (SentenceMember) obj;

        if (!name.equals(objs.name)) return false;
        else if (!defenition.equals(objs.defenition)) return false;

        return true;
    }

}
