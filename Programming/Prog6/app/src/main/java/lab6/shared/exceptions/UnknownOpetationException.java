package lab6.shared.exceptions;

public class UnknownOpetationException extends IllegalArgumentException{
    public UnknownOpetationException(String message){
        super(message);
    }

    @Override
    public String toString() {
        return "Wrong entered opetation. Input mustn't be Null." + super.toString();
    }
}
