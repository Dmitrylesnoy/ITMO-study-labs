package web.taghandler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jakarta.servlet.jsp.JspException;
import jakarta.servlet.jsp.JspWriter;
import jakarta.servlet.jsp.PageContext;
import jakarta.servlet.jsp.tagext.SimpleTagSupport;

public class FormValidatorHandler extends SimpleTagSupport {

    private String formId;
    private String onSubmit;
    private String action;
    private String method;
    private List<FieldHandler> fields = new ArrayList<>();

    public void setFormId(String formId) {
        this.formId = formId;
    }

    public void setOnSubmit(String onSubmit) {
        this.onSubmit = onSubmit;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public void addField(FieldHandler field) {
        fields.add(field);
    }

    @Override
    public void doTag() throws JspException, IOException {
        JspWriter out = getJspContext().getOut();
        PageContext pageContext = (PageContext) getJspContext();

        pageContext.setAttribute("currentForm", this);

        String formTag = "<form id=\"" + formId + "\"";
        if (action != null) {
            formTag += " action=\"" + action + "\"";
        }
        if (method != null) {
            formTag += " method=\"" + method + "\"";
        }
        if (onSubmit != null) {
            formTag += " onsubmit=\"" + onSubmit + "\"";
        }
        formTag += ">";
        out.println(formTag);
        getJspBody().invoke(null);
        out.println("</form>");
        pageContext.removeAttribute("currentForm");

        generateValidationJS(out);
    }

    private void generateValidationJS(JspWriter out) throws IOException {
        out.println("<script>");
        out.println("$(document).ready(function () {");
        out.println("$('#formCoords').submit(function (event) {");
        out.println("event.preventDefault();");

        out.println("window.validate" + formId + "= function () {");
        out.println("  let isFormValid = true;");
        for (FieldHandler field : fields) {
            out.println("  if (!validate" + field.getName() + "()) {");
            out.println("    isFormValid = false;");
            out.println("  }");
        }
        out.println("  return isFormValid;");
        out.println("}");

        out.println("if (!validate" + formId + "()) {");
        out.println("return false;");
        out.println("} else {");
        out.println("this.submit();");
        out.println("return true;");
        out.println("}");
        out.println("});");
        out.println("});");
        out.println("</script>");
    }
}
