package lab.programming.textCreator.words;

public enum Speed {
    STAY,
    NOMOVE,
    SLOW,
    AVERAGE,
    FAST,
    ALWAYS;

    @Override
    public String toString(){
        if (this.equals(Speed.STAY)) {
            return "неподвижно";
        } else if (this.equals(Speed.SLOW)) {
            return "медленно";
        } else if (this.equals(Speed.AVERAGE)) {
            return "со средней скоростью";
        } else if (this.equals(Speed.FAST)) {
            return "быстро";
        } else if (this.equals(Speed.ALWAYS)) {
            return "всегда";
        } else {
            return "";
        }
    }
}
