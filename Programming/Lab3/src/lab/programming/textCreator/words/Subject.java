package lab.programming.textCreator.words;

public record Subject(String name, String defenition) {
    @Override
    public String toString() {
        if (defenition == "") {
            return name;}
        else {
            return defenition + " " + name;}
    }
}
