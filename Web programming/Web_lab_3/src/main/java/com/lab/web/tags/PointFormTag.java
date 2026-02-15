package com.lab.web.tags;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.lab.web.beans.Point;

import jakarta.el.MethodExpression;
import jakarta.faces.application.Application;
import jakarta.faces.component.UICommand;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.component.UIInput;
import jakarta.faces.component.UIMessage;
import jakarta.faces.component.UIOutput;
import jakarta.faces.component.UISelectItems;
import jakarta.faces.component.UISelectOne;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.SelectItem;
import jakarta.faces.validator.MethodExpressionValidator;
import jakarta.inject.Inject;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PointFormTag extends UIComponentBase {
    private String field;
    private String type = "text";
    private String range;
    private Float step;
    private String label;
    private Float value;

    @Override
    public String getFamily() {
        return "pointForm";
    }

    @Override
    public boolean getRendersChildren() {
        return true;
    }

    @Override
    public void encodeBegin(FacesContext context) throws IOException {
        context.getResponseWriter().startElement("div", this);
        context.getResponseWriter().writeAttribute("class", "input-group", null);

        Application app = context.getApplication();

        float[] effectiveRange = null;
        if (range != null) {
            String[] parts = range.split(",");
            if (parts.length == 2) {
                try {
                    effectiveRange = new float[] { Float.parseFloat(parts[0].trim()),
                            Float.parseFloat(parts[1].trim()) };
                } catch (NumberFormatException e) {
                }
            }
        }

        Float effectiveStep = step != null ? step : 1.0f;
        String effectiveType = type != null ? type : "text";

        renderInputComponent(context, app, field, effectiveType, effectiveRange, effectiveStep);
    }

    @Override
    public void encodeEnd(FacesContext context) throws IOException {
        context.getResponseWriter().endElement("div");
    }

    private void renderInputComponent(FacesContext context, Application app, String field, String type, float[] range,
            float step) throws IOException {
        String elVar = "point." + field;

        String labelId = field + "-label";
        if (!componentExists(labelId)) {
            UIOutput labelComp = (UIOutput) app.createComponent(UIOutput.COMPONENT_TYPE);
            labelComp.setId(labelId);
            String effectiveLabel = label != null ? label : field.toUpperCase();
            labelComp.setValue(effectiveLabel + ": ");
            this.getChildren().add(labelComp);
        }

        switch (type) {
            case "radio":
                createRadioComponent(context, app, field, elVar, range, step);
                break;
            case "button":
                createButtonComponent(context, app, field, elVar);
                break;
            default:
                createTextComponent(context, app, field, elVar);
                break;
        }

        createErrorMessageComponent(context, app, field);
    }

    private void createRadioComponent(FacesContext context, Application app, String field, String elVar, float[] range,
            float step) {
        String radioId = this.getId() + "_" + field;
        if (!componentExists(radioId)) {
            UISelectOne radio = (UISelectOne) app.createComponent(UISelectOne.COMPONENT_TYPE);
            radio.setRendererType("jakarta.faces.Radio");
            radio.setId(radioId);

            if ("x".equals(field)) {
                String hiddenId = this.getId() + "_xHidden";
                if (!componentExists(hiddenId)) {
                    UIInput hiddenInput = (UIInput) app.createComponent(UIInput.COMPONENT_TYPE);
                    hiddenInput.setRendererType("jakarta.faces.Hidden");
                    hiddenInput.setId(hiddenId);
                    hiddenInput.setValueExpression("value",
                            context.getApplication().getExpressionFactory().createValueExpression(
                                    context.getELContext(),
                                    "#{" + elVar + "}", Float.class));

                    MethodExpression validator = context.getApplication().getExpressionFactory()
                            .createMethodExpression(context.getELContext(),
                                    "#{validator.validateX}", Void.class,
                                    new Class[] { FacesContext.class, UIComponent.class, Object.class });
                    hiddenInput.addValidator(new MethodExpressionValidator(validator));

                    this.getChildren().add(hiddenInput);
                }
            }

            List<SelectItem> items = new ArrayList<>();
            if (range != null) {
                for (float val = range[0]; val <= range[1]; val += step) {
                    String displayValue = (val == (int) val) ? String.valueOf((int) val) : String.valueOf(val);
                    items.add(new SelectItem(val, displayValue));
                }
            }
            UISelectItems selectItems = (UISelectItems) app.createComponent(UISelectItems.COMPONENT_TYPE);
            selectItems.setValue(items);
            radio.getChildren().add(selectItems);

            if ("x".equals(field)) {
                radio.getAttributes().put("onchange", "updateXHidden(this.value)");
            }

            this.getChildren().add(radio);
        }
    }

    private void createButtonComponent(FacesContext context, Application app, String field, String elVar) {
        String buttonId = field + "-button";
        if (!componentExists(buttonId)) {
            UICommand button = (UICommand) app.createComponent(UICommand.COMPONENT_TYPE);
            button.setRendererType("jakarta.faces.Button");
            button.setId(buttonId);

            MethodExpression action = context.getApplication().getExpressionFactory()
                    .createMethodExpression(context.getELContext(),
                            "#{point.updateValue}", Void.class, new Class[0]);
            button.setActionExpression(action);

            button.setValueExpression("value", context.getApplication().getExpressionFactory()
                    .createValueExpression(context.getELContext(), "#{" + elVar + "}", Float.class));

            this.getChildren().add(button);
        }
    }

    private void createTextComponent(FacesContext context, Application app, String field, String elVar) {
        String inputId = field + "-text";
        if (!componentExists(inputId)) {
            UIInput input = (UIInput) app.createComponent(UIInput.COMPONENT_TYPE);
            input.setRendererType("jakarta.faces.Text");
            input.setId(inputId);
            input.getAttributes().put("maxlength", "10");
            input.setValueExpression("value", context.getApplication().getExpressionFactory()
                    .createValueExpression(context.getELContext(), "#{" + elVar + "}", Float.class));

            if ("y".equals(field)) {
                MethodExpression validator = context.getApplication().getExpressionFactory()
                        .createMethodExpression(context.getELContext(),
                                "#{validator.validateY}", Void.class,
                                new Class[] { FacesContext.class, UIComponent.class, Object.class });
                input.addValidator(new MethodExpressionValidator(validator));
            }

            this.getChildren().add(input);
        }
    }

    private void createErrorMessageComponent(FacesContext context, Application app, String field) {
        String messageId = field + "-message";
        if (!componentExists(messageId)) {
            UIMessage messageComp = (UIMessage) app.createComponent(UIMessage.COMPONENT_TYPE);
            messageComp.setId(messageId);

            if ("x".equals(field)) {
                messageComp.setFor(this.getId() + "_xHidden");
            } else {
                messageComp.setFor(
                        field + (field.equals("x") ? "_" + field : "-" + (field.equals("y") ? "text" : "button")));
            }

            messageComp.getAttributes().put("styleClass", "error-message");
            this.getChildren().add(messageComp);
        }
    }

    private boolean componentExists(String id) {
        for (UIComponent child : this.getChildren()) {
            if (id.equals(child.getId())) {
                return true;
            }
        }
        return false;
    }
}
