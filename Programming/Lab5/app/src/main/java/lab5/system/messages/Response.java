package lab5.system.messages;

public class Response {
    private Status status;
    private String output;
    private Exception e;

    public Response(Status status, String output) {
        this.output = output;
        this.status = status;
    }

    public Response(Status status, Exception e){
        this.status = status;
        this.e=e;
    }

    @Override
    public String toString() {
        switch (status) {
            case COMPLETE:
                if (output.isEmpty()) {
                    return status.toString();
                }
                return output;
            case WARNING:
                return status.toString()+"\n"+output;
            default:
                return status.toString() +"\n"+e.toString();
        }
    }
}
