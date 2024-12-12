package lab.programming.textCreator.exceptions;

public class NonAviableMethod extends Exception {
    private int errorCode;

    public NonAviableMethod(String message){
        this(0, message);
    }

    public NonAviableMethod(int errorCode, String message){
        super(message);
        this.errorCode = errorCode;
    }

    public int getErrorCode() {
        return errorCode;
    }

    @Override
    public String getMessage() {
        return "This method is not aviable in this class (Error code: {"+errorCode+"})";
    }
}
