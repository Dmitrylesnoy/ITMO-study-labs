package lab5.system.messages;

public enum Status {
    COMPLETE("Сompleted successfully"),
    WARNING("Completed with warnings"),
    FAILED("Execuiton failed");
    
    private Status(String string) {
    }
}
