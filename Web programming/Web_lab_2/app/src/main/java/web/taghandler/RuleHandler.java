package web.taghandler;

import jakarta.servlet.jsp.JspException;
import jakarta.servlet.jsp.PageContext;
import jakarta.servlet.jsp.tagext.SimpleTagSupport;

public class RuleHandler extends SimpleTagSupport {

    private String type;
    private String value;
    private String message;

    public void setType(String type) {
        this.type = type;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public void doTag() throws JspException {
        PageContext pageContext = (PageContext) getJspContext();
        FieldHandler parentField = (FieldHandler) pageContext.getAttribute("currentField");

        if (parentField != null) {
            parentField.addRule(type, value, message != null ? message : getDefaultMessage(type));
        }
    }

    private String getDefaultMessage(String type) {
        return switch (type) {
            case "required" -> "This field is required";
            case "pattern" -> "Invalid format";
            case "minValue" -> "Value too low. Min value is " + value;
            case "maxValue" -> "Value too high. Max value is " + value;
            case "range" -> "Value out of range [" + value + "].";
            case "maxLength" -> "Too long. Max length is " + value;
            default -> "Invalid input";
        };
    }
}