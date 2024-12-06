package lab.programming.textCreator.exceptions;

public class NonaviableMethod extends Exception {
    private int errorCode;

    public NonaviableMethod(String message){
        this(0, message);
    }

    public NonaviableMethod(int errorCode, String message){
        super(message);
        this.errorCode = errorCode;
    }

    public int getErrorCode() {
        return errorCode;
    }

    @Override
    public String getMessage() {
        return "This method not aviable in this class (Error code: {"+errorCode+"})";
    }
}
