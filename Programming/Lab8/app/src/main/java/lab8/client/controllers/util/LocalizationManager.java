package lab8.client.controllers.util;

import java.util.Locale;
import java.util.ResourceBundle;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.StringBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;

public class LocalizationManager {
    private static final ObjectProperty localeProperty = new SimpleObjectProperty<>(Locale.getDefault());
    private static final String BUNDLE_NAME = "locales.Messages";

    public static ObjectProperty<Locale> localeProperty() {
        return localeProperty;
    }

    public static Locale getLocale() {
        return (Locale) localeProperty.get();
    }

    public static void setLocale(Locale locale) {
        localeProperty.set(locale);
    }

    public static ResourceBundle getBundle() {
        return ResourceBundle.getBundle(BUNDLE_NAME, getLocale());
    }

    public static StringBinding createStringBinding(String key) {
        return Bindings.createStringBinding(() -> getBundle().getString(key), localeProperty);
    }

    public static String getString(String key) {
        try {
            return getBundle().getString(key);
        } catch (Exception e) {
            return key; 
        }
    }

    public static void switchToRussian() {
        setLocale(new Locale("ru"));
    }

    public static void switchToDutch() {
        setLocale(new Locale("nl"));
    }

    public static void switchToGreek() {
        setLocale(new Locale("el"));
    }

    public static void switchToSpanishPR() {
        setLocale(new Locale("es", "PR"));
    }

}