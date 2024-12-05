package lab.programming.textCreator.words;

public abstract class SentenceMember {
    String name, defenition;
    
    SentenceMember(){}
    SentenceMember(String name) {
        this.name = name;
    }
    SentenceMember(String name, String defenition){
        this.name = name;
        this.defenition=defenition;
    }

    String getName() {
        return name;
    }

    String getDefenition() {
        return name;
    }

    void addDefenition(String defenition) {
        this.defenition = defenition;
    }

    public String toString(){
        return name+" "+defenition;
    }

    abstract void addSubject(SentenceMember member);

}
