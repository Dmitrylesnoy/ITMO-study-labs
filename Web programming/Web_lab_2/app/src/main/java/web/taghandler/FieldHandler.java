package web.taghandler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jakarta.servlet.jsp.JspException;
import jakarta.servlet.jsp.JspWriter;
import jakarta.servlet.jsp.PageContext;
import jakarta.servlet.jsp.tagext.SimpleTagSupport;
import web.model.HitData;

public class FieldHandler extends SimpleTagSupport {

    private String name;
    private String label;
    private Integer minValue = -10;
    private Integer maxValue = 10;
    private List<Rule> rules = new ArrayList<>();

    public void setName(String name) {
        this.name = name;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public void setMinValue(int minValue) {
        this.minValue = minValue;
    }

    public void setMaxValue(int maxValue) {
        this.maxValue = maxValue;
    }

    public String getName() {
        return name;
    }

    public void addRule(String type, String value, String message) {
        rules.add(new Rule(type, value, message));
    }

    @Override
    public void doTag() throws JspException, IOException {
        JspWriter out = getJspContext().getOut();

        PageContext pageContext = (PageContext) getJspContext();
        FormValidatorHandler currentForm = (FormValidatorHandler) pageContext.getAttribute("currentForm");
        if (currentForm != null) {
            currentForm.addField(this);
        }

        pageContext.setAttribute("currentField", this);
        if (getJspBody() != null)
            getJspBody().invoke(null);
        pageContext.removeAttribute("currentField");

        String displayLabel = label != null ? label : name;
        out.println("<div class=\"input-group\">");
        out.println("<label for=\"" + name + "\" class=\"required-field\">" + displayLabel + ":</label>");

        switch (name) {
            case "x":
                generateXField(out);
                break;
            case "y":
                generateYField(out);
                break;
            case "r":
                generateRField(out);
                break;
            case "submit":
                generateSubmitField(out);
                break;
            default:
                break;
        }
        out.println("</div>");
        generateValidationJS(out);
    }

    private int[] getRangeFromRules() {
        for (Rule rule : rules) {
            try {
                switch (rule.type()) {
                    case "range":
                        String[] parts = rule.value().split(",");
                        if (parts.length == 2) {
                            minValue = Integer.parseInt(parts[0].trim());
                            maxValue = Integer.parseInt(parts[1].trim());
                        }
                        break;
                    case "minValue":
                        minValue = Integer.parseInt(rule.value().trim());
                        break;
                    case "maxValue":
                        maxValue = Integer.parseInt(rule.value().trim());
                        break;
                }
            } catch (NumberFormatException e) {
            }
        }
        return new int[] { minValue, maxValue };
    }

    private void generateXField(JspWriter out) throws IOException {
        out.println("<div id=\"" + name + "\">");
        int[] range = getRangeFromRules();
        for (int i = range[0]; i <= range[1]; i++) {
            out.println("<label><input type=\"checkbox\" name=\"x\" value=\"" + i + "\" />" + i + "</label>");
        }
        out.println("</div>");
    }

    private void generateYField(JspWriter out) throws IOException {
        getRangeFromRules();
        out.println("<input type=\"text\" id=\"y\" name=\"y\" min=\"" + minValue + "\" max=\"" + maxValue
                + "\" step=\"any\" placeholder=\"От " + minValue + " до "
                + maxValue + "\" />");
    }

    private void generateRField(JspWriter out) throws IOException {
        getRangeFromRules();

        PageContext pageContext = (PageContext) getJspContext();
        Integer lastR = (Integer) pageContext.getSession().getAttribute("lastR");
        int initR = (lastR != null && lastR >= minValue && lastR <= maxValue) ? lastR : -1;

        out.println("<input type=\"hidden\" id=\"r\" name=\"r\" value=\"" + (initR != -1 ? initR : "") + "\" />");
        out.println("<button type=\"button\" id=\"r-btn\">" + (initR != -1 ? initR : "null") + "</button>");
        out.println("<script>");
        out.println("const rBtn = document.getElementById('r-btn');");
        out.println("const rInput = document.getElementById('r');");

        out.println("const values = [");
        for (int i = minValue; i <= maxValue; i++) {
            out.print(i == maxValue ? i + "];" : i + ",");
        }
        out.println("let currentIndex = -1;");
        if (initR != -1) {
            out.println("currentIndex = " + (initR - 1) + ";");
            out.println("rBtn.textContent = " + initR + ";");
            out.println("rInput.value = " + initR + ";");
        }
        out.println("rBtn.addEventListener('click', function () {");
        out.println("currentIndex = (currentIndex + 1) % values.length;");
        out.println("const newValue = values[currentIndex];");
        out.println("rInput.value = newValue;");
        out.println("rBtn.textContent = newValue;");
        out.println("});");
        out.println("</script>");
    }

    private void generateSubmitField(JspWriter out) throws IOException {
        out.println("<button type=\"submit\" id=\"submitBtn\">Check</button>");
    }

    private void generateXNumericValidation(JspWriter out, String condition, Rule rule) throws IOException {
        out.println("let checkedX = document.querySelectorAll('input[name=\"x\"]:checked');");
        out.println("for (let cb of checkedX) {");
        generateNumericValidation(out, "cb.value", condition, rule);
        out.println("}");
    }

    private void generateNumericValidation(JspWriter out, String valueExpression, String condition, Rule rule)
            throws IOException {
        out.println("{");
        out.println("let val = parseFloat(" + valueExpression + ");");
        out.println("if (isNaN(val)) {");
        out.println("createToast('warning', 'Field " + name + ": Set the value');");
        out.println("isValid = false;");
        out.println("} else if (" + condition + ") {");
        out.println("createToast('warning', 'Field " + name + ": " + rule.message() + "');");
        out.println("isValid = false;");
        out.println("}");
        out.println("}");
    }

    private void generateValidationJS(JspWriter out) throws IOException {
        out.println("<script>");
        out.println("function validate" + name + "() {");
        out.println("let field = document.getElementById('" + name + "');");
        out.println("let isValid = true;");

        for (Rule rule : rules) {
            switch (rule.type()) {
                case "required":
                    if ("x".equals(name)) {
                        out.println("let checked = document.querySelectorAll('input[name=\"x\"]:checked');");
                        out.println("if (checked.length === 0) {");
                        out.println("createToast('warning', 'Field " + name + ": " + rule.message() + "');");
                        out.println("isValid = false;");
                        out.println("}");
                    } else {
                        out.println("if (!field.value.trim()) {");
                        out.println("createToast('warning', 'Field " + name + ": " + rule.message() + "');");
                        out.println("isValid = false;");
                        out.println("}");
                    }
                    break;
                case "maxLength":
                    out.println("if (field.value.length > " + rule.value() + ") {");
                    out.println("createToast('warning', 'Field " + name + ": " + rule.message() + "');");
                    out.println("isValid = false;");
                    out.println("}");
                    break;
                case "pattern":
                    if ("y".equals(name)) {
                        out.println("if (!/" + rule.value() + "/.test(field.value)) {");
                        out.println("createToast('warning', 'Field " + name + ": " + rule.message() + "');");
                        out.println("isValid = false;");
                        out.println("}");
                    }
                    break;
                case "minValue":
                    if ("x".equals(name)) {
                        generateXNumericValidation(out, "val < " + rule.value(), rule);
                    } else {
                        generateNumericValidation(out, "field.value", "val < " + rule.value(), rule);
                    }
                    break;
                case "maxValue":
                    if ("x".equals(name)) {
                        generateXNumericValidation(out, "val > " + rule.value(), rule);
                    } else {
                        generateNumericValidation(out, "field.value", "val > " + rule.value(), rule);
                    }
                    break;
                case "range":
                    if ("x".equals(name)) {
                        generateXNumericValidation(out, "val < " + minValue + " || val > " + maxValue, rule);
                    } else {
                        generateNumericValidation(out, "field.value",
                                "val < " + minValue + " || val > " + maxValue, rule);
                    }
                    break;
                default:
                    break;
            }
        }

        out.println("  return isValid;");
        out.println("}");
        out.println("</script>");
    }
}
