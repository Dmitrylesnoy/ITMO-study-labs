package lab5.system.messages;

public class Response {
    private String name;
    private Status status;
    private String output;
    private Exception e;

    public Response(String name, Status status, String output) {
        this.name = name;
        this.output = output;
        this.status = status;
    }

    public Response(String name, Status status, Exception e) {
        this.name = name;
        this.status = status;
        this.e = e;
    }

    @Override
    public String toString() {
        switch (status) {
            case COMPLETE:
                if (output.isEmpty()) {
                    return name + "  " + status.toString() + "\n=> ";
                }
                return output + "\n=>";
            case WARNING:
                return name + "  " + status.toString() + "\n " + output + "\n=> ";
            default:
                return name + "  " + status.toString() + "\n " + e.toString() + "\n=> ";
        }
    }
}
