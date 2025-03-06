package lab5.system.exceptions;

public class NullArgumetnException extends IllegalArgumentException {
    public NullArgumetnException(String message) {
        super(message);
    }
    @Override
    public String toString(){
        return "Wrong entered argument. Input mustn't be Null." + this.toString();
    }
}
